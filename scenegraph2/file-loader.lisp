(in-package 3bgl-sg2)

;; controls post-processing done by assimp while loading files. not
;; included in cache index, so probably shouldn't be changed much at
;; runtime unless all uses of any give file are known to be with same
;; settings
(defparameter *ai-loader-post-processing*
  ;; other code assumes triangles only, so should include at least
  ;; that much if data isn't previously validated/processed. Also
  ;; currently assuming 16bit indices in meshes, max 4 bones per vert,
  ;; probably some other things.
  '(:ai-process-preset-target-realtime-max-quality))
(defparameter *ai-loader-properties*
  ;; todo: add point/line support
  '(:pp-sbp-remove (:point :line)
    ;; todo: decide if we should support larger meshes or just
    ;; hard-code short indices?
    :pp-slm-vertex-limit 65535))

(defparameter *ai-attributes*
  `(((:vertex :vec3 :location 0) ai:vertices t)
    ((:normal :vec3 :location 1) ai:normals t)
    ((:tangent :vec3 :location 2) ai:tangents t)
    ((:bitangent :vec3 :location 3) ai:bitangents t)
    #++((:color :vec4u8 :location 4) ai:colors t)
    ((:color :vec4u8 :location 4) ,(lambda (x) (let ((c (ai:colors x)))
                                                 (if (zerop (length c))
                                                     nil
                                                     (aref c 0))))
     t)
    ((:uv :vec2 :location 5) ,(lambda (x) (let ((tc (ai:texture-coords x)))
                                            (if (zerop (length tc))
                                                nil
                                                (aref tc 0))))
     t)
    ((:bones :vec4u8 :location 6) ai:bones nil) ;; :ivec4ub?
    ((:weights :vec4u8 :location 7) ai:bones nil)
    ;; todo: more UVs/more components?
    ))

(defvar *ai-shader-program*)
(defparameter *ai-material-defaults* '(:blend-func (:one :one-minus-src-alpha)
                                       :blend nil
                                       :depth-test t
                                       :cull-face :back))

(defun ai-material-name (blend format)
  (append '(ai-material)
          (loop for (a nil) on format by #'cddr collect a)
          (list blend))
  #++(intern (format nil "~:@(ai-material/~{~a/~}~a~)"
                  (loop for (a nil) on format by #'cddr collect a)
                  blend)
          '3bgl-sg2))

(defun translate-ai-nodes (root &key sg parent-node)
  (let ((i 0)
        ;; name -> index + node
        (h (make-hash-table :test 'equalp))
        ;; set of meshes -> names of nodes using that set
        (objects (make-hash-table :test 'equalp))
        (work nil)
        (sg (or sg (make-instance 'scenegraph))))
    (labels ((add (w)
               (let* ((p (first w))
                      (n (second w))
                      (name (ai:name n))
                      (pname (or (and p (ai:name p))
                                 parent-node)))
                 (setf (gethash name h)
                       (list i n))
                 (add-node sg 'transform (ai:name n) pname
                           :matrix (sb-cga:transpose-matrix (ai:transform n)))
                 (push name (gethash (sort (copy-seq (ai:meshes n)) '<)
                                     objects))
                 (incf i)))
             (r (w)
               (setf work nil)
               (labels ((n (n) (cond
                                 ((consp n) (mapcar #'n n))
                                 (n (ai:name n))
                                 (t n))))
                 (when w
                   (map nil #'add w)
                   (loop for (p n) in w
                         unless (zerop (length (ai:children n)))
                           #+do (format t "add children1: ~s~%"
                                      (map 'list #'n (ai:children n)))
                         do (loop for c across (ai:children n)
                                  do (push (list n c) work)))
                   (r (reverse work))))))
      (setf work (list (list nil root)))
      (r work)
      (list sg objects))))

(defun expand-material (format material)
  (let* ((state (scenegraph::make-state (list* :vertex-format
                                               (mapcar 'cdr
                                                       (alexandria:plist-alist
                                                        format))
                                               :sample-alpha-to-coverage
                                               (getf material :blend)
                                               *ai-material-defaults*)))
         (name (ai-material-name (getf material :blend) format))
         (mat (or (get-material name)
                  (make-material name state
                                 :count-var '3bgl-ai-shaders::count))))
    (list name (intern-material mat (getf material :properties)))))

(defun translate-ai-mesh (m skel materials)
  (declare (ignore skel))
  (let ((layout nil))
    (flet ((a (aa)
             (destructuring-bind (a f x) aa
               (declare (ignore x))
               (unless (zerop (length (funcall f m)))
                 (setf layout (push a layout))))))
      ;; add any active components to layout
      (map nil #'a *ai-attributes*)
      (setf layout (nreverse layout)))
    (let* ((format (buffer-builder::vertex-format-for-layout layout :named t))
           (count (length (ai:vertices m)))
           (index-count (* 3 (length (ai:faces m))))
           (stride (car (last (getf format :vertex))))
           (ret nil))
      ;; fill buffer with index data and upload to GPU
      (cffi:with-foreign-object (index :unsigned-short
                                 index-count)
        (loop for tri across (ai:faces m)
              for i from 0 by 3
              do (loop for j below 3
                       do (setf (cffi:mem-aref index :unsigned-short (+ i j))
                                (aref tri j))))
        (setf (getf ret :index)
              (upload-index-data (index-buffer *resource-manager*)
                                 index index-count :unsigned-short)))
      ;; fill buffer with vertex data and upload to GPU
      (cffi:with-foreign-pointer (data (* (1+ count) stride))
        (loop
          for (n channel) on format by #'cddr
          for (nil nil ecount gltype norm offset) = channel
          for ctype = (gl::symbolic-type->real-type gltype)
          for (layout f simple) = (assoc n *ai-attributes* :key 'car)
          when simple
            do (loop
                 for v across (funcall f m)
                 for i from offset by stride
                 do (loop
                      for j below ecount
                      do (assert (< (+ i
                                        (* (1+ j)
                                           (cffi:foreign-type-size ctype)))
                                    (* (1+ count) stride)))
                      do (setf (cffi:mem-aref (cffi:inc-pointer data i)
                                              ctype j)
                               (ecase gltype
                                 (:byte (floor (* (aref v j) 127)))
                                 (:unsigned-byte (if norm
                                                     (floor
                                                      (* 255 (aref v j)))
                                                     (aref v j)))
                                 (:float (aref v j)))))))
        (let ((bs (get-buffer-set (mapcar 'cdr
                                          (alexandria:plist-alist format)))))
          (setf (getf ret :vertex)
                (list* bs (buffer-geometry bs count data))))
        (setf (getf ret :material)
              (expand-material format
                               (aref materials (ai:material-index m)))))
      ret)))

(defparameter *mat-props*
  (alexandria:plist-hash-table
   '("?mat.name" (name nil)
     ;; not sure if this should default to on or off?
     "$mat.twosided" (3bgl-ai-shaders::mat-two-sided :int 0)
     "$mat.shadingm" (3bgl-ai-shaders::mat-shading-model :shading-mode :ai-shading-mode-phong)
     "$mat.wireframe" (3bgl-ai-shaders::mat-wireframe :int 0)
     "$mat.blend" (3bgl-ai-shaders::mat-blend :float 0) ;; 0=normal alpha, 1=additive
     "$mat.opacity" (3bgl-ai-shaders::mat-opacity :float 1.0)
     "$mat.bumpscaling" (3bgl-ai-shaders::mat-bump-scaling :float 1.0)
     "$mat.shininess" (3bgl-ai-shaders::mat-shininess :float 0.0) ;; ?
     "$mat.reflectivity" (3bgl-ai-shaders::mat-reflectivity :float 0.0)
     "$mat.shinpercent" (3bgl-ai-shaders::mat-shininess-strength :float 1.0) ;; ?
     "$mat.refracti" (3bgl-ai-shaders::mat-index-of-refraction :float 1.0)
     "$clr.diffuse" (3bgl-ai-shaders::clr-diffuse :vec3 #(0.577 0.577 0.577))
     "$clr.ambient" (3bgl-ai-shaders::clr-ambient :vec3 #(0.577 0.577 0.577))
     "$clr.specular" (3bgl-ai-shaders::clr-specular :vec3 #(0.0 0.0 0.0))
     "$clr.emissive" (3bgl-ai-shaders::clr-emissive :vec3 #(0.0 0.0 0.0))
     "$clr.transparent" (3bgl-ai-shaders::clr-transparent :int 0)
     "$clr.reflective" (3bgl-ai-shaders::clr-reflective :int 0)
     "?bg.global" (:warn) ;;(global-background-image)
     "$tex.file" (tex-file :textures nil)
     "$tex.uvwsrc" (:warn)   ;;(tex-uvwsrc)
     "$tex.op" (:warn)       ;;(tex-op)
     "$tex.mapping" (:warn)  ;;(tex-mapping)
     "$tex.blend" (:warn)    ;;(tex-blend)
     "$tex.mapmodeu" (:warn) ;;(tex-mapmodeu)
     "$tex.mapmodev" (:warn) ;;(tex-mapmodev)
     "$tex.mapaxis" (:warn)  ;;(tex-mapaxis)
     "$tex.uvtrafo" (:warn)  ;;(tex-uvtrafo)
     "$tex.flags" (:warn))   ;;(tex-flags)
   :test 'equalp))

(defparameter *ai-shading-modes*
  '(:ai-shading-mode-flat 1
    :ai-shading-mode-gouraud 2
    :ai-shading-mode-phong 3
    :ai-shading-mode-blinn 4
    :ai-shading-mode-toon 5
    :ai-shading-mode-oren-nayar 6
    :ai-shading-mode-minnaert 7
    :ai-shading-mode-cook-torrance 8
    :ai-shading-mode-no-shading 9
    :ai-shading-mode-fresnel 10))

(defparameter *ai-texture-type*
  '(:ai-texture-type-none 0
    :ai-texture-type-diffuse 1
    :ai-texture-type-specular 2
    :ai-texture-type-ambient 3
    :ai-texture-type-emissive 4
    :ai-texture-type-height 5
    :ai-texture-type-normals 6
    :ai-texture-type-shininess 7
    :ai-texture-type-opacity 8
    :ai-texture-type-displacement 9
    :ai-texture-type-lightmap 10
    :ai-texture-type-reflection 11
    :ai-texture-type-unknown 12))

(defparameter *texture-slots* #(3bgl-ai-shaders::tex-none
                                3bgl-ai-shaders::tex-diffuse
                                3bgl-ai-shaders::tex-specular
                                3bgl-ai-shaders::tex-ambient
                                3bgl-ai-shaders::tex-emissive
                                3bgl-ai-shaders::tex-height
                                3bgl-ai-shaders::tex-normals
                                3bgl-ai-shaders::tex-shininess
                                3bgl-ai-shaders::tex-opacity
                                3bgl-ai-shaders::tex-displacement
                                3bgl-ai-shaders::tex-lightmap
                                3bgl-ai-shaders::tex-reflection
                                3bgl-ai-shaders::tex-unknown))

(defparameter *tx* nil)

(defun find-texture (n type)
  ;; todo: add other options, like stripping prefix from given filenames, etc
  (or (probe-file (merge-pathnames n))
      (asdf:system-relative-pathname '3bgl-misc
                                     (case type
                                       ((1 2) "data/debug-texture.png")
                                       ;; todo: more debug textures
                                       ;; (or black/white/grey)
                                       ((3 4) nil)
                                       (5 "data/debug-bump-texture1.png")
                                       (6 "data/debug-bump-texture.png")
                                       ;; todo:
                                       ((7 8 9 10 11 12) nil)
                                       (t nil)))))

(defun normalize-material (h)
  (labels ((2< (a b)
             (or (< (car a) (car b))
                 (and (= (car a) (car b))
                      (< (cadr a) (cadr b)))))
           (normalize-value (value type)
             (ecase type
               (:int value)
               (:float (coerce value 'single-float))
               (:vec3 (coerce (map 'vector
                                   (lambda (a) (coerce a 'single-float))
                                   value)
                              '(simple-array single-float (3))))
               (:shading-mode (getf *ai-shading-modes* value))
               (:handle value)
               (:textures
                ;;sort by texture type then uv channel (though if
                ;;we have multiple textures with same type we
                ;;probably aren't rendering correctly since it
                ;;probably uses the $tex.op/blend stuff)
                (sort
                 (loop for (a b tx) in (copy-list value)
                       collect (list a b (find-texture tx a)))
                 #'2<)
                ))))

    (let ((a (alexandria:hash-table-alist h))
          (name nil))
      (alexandria:alist-hash-table
       (loop for (n . tv) in (sort a 'string<
                                   :key (lambda (a) (symbol-name (car a))))
             for (type value) = (when (consp tv) tv)
             when (eql n 'name)
               do (setf name tv)
             else collect (cons n (normalize-value value type)))))))

(defun ai-translate-material (m)
  (let ((h (make-hash-table)))
    (loop for (n type d) in (alexandria:hash-table-values *mat-props*)
          when d
            do (setf (gethash n h) (list type d)))
    (flet ((tx (tx)
             (destructuring-bind (&optional tt tc fn)
                 tx
               (list (getf *ai-texture-type* tt tt)
                     tc
                     (substitute #\/ #\\ fn)))))
      ;; set textures to 0 by default (possibly should (optionally) zero buffers
      ;; before filling instead?)
      (loop for i from 0
            for tx across *texture-slots*
            do (setf (gethash tx h) '(:handle 0)))
      (loop with tex-present = 0
            for k being the hash-keys of m using (hash-value v)
            for (name type default) = (gethash k *mat-props*)
            when (eql type :textures)
              do (setf v (mapcar #'tx v))
                 (loop for (tt uv name) in v
                       do (unless (= uv 0)
                            (cerror "continue"
                                    "material uses non-zero uv channel?"))
                          (setf (ldb (byte 1 tt) tex-present) 1)
                          (setf (gethash (aref *texture-slots* tt) h)
                                (list
                                 :int
                                 (handle
                                  (get-handle (get-texture name)
                                              (get-sampler 'ai-sampler
                                                           :max-anisotropy 16)
                                              :resident t)))))
            when (eql name :warn)
              do (format t "using unsupported material parameter ~s = ~s~%"
                         k v)
            else
              do (setf (gethash name h)
                       (if type (list type v) v))
            finally (setf (gethash '3bgl-ai-shaders::tex-present h)
                          (list :int tex-present))))
    (let* ((n (normalize-material h))
           ;; todo: check for alpha channel in diffuse texture?...
           ;; hard to do here without doing a bunch of extra
           ;; filesystem work though... probably have to add some
           ;; external metadata if that case ends up mattering
           (blend (or (plusp (gethash '3bgl-ai-shaders::tex-opacity n))
                      (< (gethash '3bgl-ai-shaders::mat-opacity n) 1.0))))
      (list :blend blend :properties n))))

(defun unique-names (root)
  (let ((c 0))
    (labels ((r (n)
               (setf (ai:name n) (format nil "~a.~a" (incf c) (ai:name n)))
               (map nil #'r (ai:children n))))
      (r root))
    root))

(defun translate-ai-scene (s &key sg parent-node)
  ;; todo: animations
  ;; todo: lights
  ;; todo: cameras
  (let* ((nodes/groups (translate-ai-nodes (unique-names
                                            (ai:root-node s))
                                           :sg sg :parent-node parent-node))
         (sg (first nodes/groups))
         (groups (second nodes/groups))
         ;; todo: build a 'skeleton' for each node = hash of each node
         ;; used as bone by specific object, with index of bone in
         ;; that particular skeleton instead of whole tree
         #++ (skeleton (extract-skeletons nodes))
         (materials (map 'vector 'ai-translate-material (ai:materials s)))
         (objects
           ;; possibly some duplication this way, but if 2 distinct
           ;; objects share meshes they might still have different
           ;; skeletons, so just duplicating for now. (presumably
           ;; pretty rare to reuse meshes either way though)
           (loop with h = (make-hash-table :test 'equalp)
                 for o in (alexandria:hash-table-keys groups)
                 for meshes = (progn    ;remove nil
                                (loop for m# across o
                                      for m = (aref (ai:meshes s) m#)
                                      collect (translate-ai-mesh
                                               m nil materials)))
                 when meshes
                   do (assert (not (gethash o h)))
                      (setf (gethash o h) meshes)
                 finally (return h))))
    (loop for k being the hash-keys of objects using (hash-value v)
          for names = (gethash k groups)
          do (loop for n in names
                   do (convert-transform-to-instance sg n v)))
    sg))


(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; eval at compile time as well so vbo-builder macro can see it
  (defparameter *vnc-layout* '((vertex :vec4)
                               (normal :vec3)
                               (uv :vec2)
                               (color :vec4u8))))

(defparameter *vnc-bindings* (multiple-value-list
                              (buffer-builder::calc-vbo-layout *vnc-layout*)))

(defmethod load-object ((loader (eql :file)) name &key sg parent-node)
  ;; todo: load files on another thread, just draw nothing until loaded
  ;; (possibly draw debug scene until fully loaded?)
  (let ((*ai-material-defaults*
          (list* :program (get-program :vertex '3bgl-ai-shaders::vertex
                                       :fragment '3bgl-ai-shaders::fragment)
                 *ai-material-defaults*)))
    (let ((o (assimp:import-into-lisp
              (merge-pathnames name)
              :processing-flags *ai-loader-post-processing*
              :properties *ai-loader-properties*)))
      (translate-ai-scene o :sg sg :parent-node parent-node))))


#++
(let ((scenegraph::*runtime-values-cache* (make-hash-table))
      (scenegraph::*known-states* (make-hash-table :test #'equalp))
      (scenegraph::*state-defaults* (make-hash-table)))
  ;(scenegraph::init-defaults scenegraph::*state-defaults*)
 (with-resource-manager ()
   (make-material 'scene2test::ai-shader
                  (scenegraph::make-state*))
   (list
    (load-object :file
                 (asdf:system-relative-pathname '3bgl-misc "data/teapot/teapot.obj"))
    *resource-manager*)))
