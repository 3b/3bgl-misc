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
  `(((:vertex :vec3) ai:vertices t)
    ((:normal :vec3) ai:normals t)
    ((:tangent :vec3) ai:tangents t)
    ((:bitangent :vec3) ai:bitangents t)
    ((:color :vec4u8) ai:colors t)
    ((:uv :vec2) ,(lambda (x) (let ((tc (ai:texture-coords x)))
                                (if (zerop (length tc))
                                    nil
                                    (aref tc 0))))
     t)
    ((:bones :vec4u8) ai:bones nil) ;; :ivec4ub?
    ((:weights :vec4u8) ai:bones nil)
    ;; todo: more UVs/more components?
    ))



(defun translate-ai-nodes (root)
  (let ((i 0)
        ;; name -> index + node
        (h (make-hash-table :test 'equalp))
        ;; set of meshes -> names of nodes using that set
        (objects (make-hash-table :test 'equalp))
        (work nil)
        (sg (make-instance 'scenegraph)))
    (labels ((add (w)
               (let* ((p (first w))
                      (n (second w))
                      (name (ai:name n))
                      (pname (and p (ai:name p))))
                 (format t "~& add node ~a @ ~a~%"
                         name pname)
                 (setf (gethash name h)
                       (list i n))
                 (add-node sg 'transform (ai:name n) pname
                           :matrix (ai:transform n))
                 (push name (gethash (sort (copy-seq (ai:meshes n)) '<)
                                     objects))
                 (incf i)))
             (r (w)
               (setf work nil)
               (labels ((n (n) (cond
                                 ((consp n) (mapcar #'n n))
                                 (n (ai:name n))
                                 (t n))))
                 (format t "~%recurse: ~s~%" (mapcar #'n w))
                 (when w
                   (map nil #'add w)
                   (loop for (p n) in w
                         unless (zerop (length (ai:children n)))
                           do (format t "add children1: ~s~%"
                                      (map 'list #'n (ai:children n)))
                         do (loop for c across (ai:children n)
                                  do (push (list n c) work)))
                   (r (reverse work))))))
      (setf work (list (list nil root)))
      (r work)
      (list sg objects))))

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
      (format t "layout = ~s~%format = ~s~%" layout format)
      (unless (eql 4 (car (getf format :uv)))
        (break "uv"))
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
                                 (:byte (* (aref v j) 127))
                                 (:unsigned-byte (if norm
                                                     (* 255 (aref v j))
                                                     (aref v j)))
                                 (:float (aref v j)))))))
        (let ((bs (get-buffer-set (mapcar 'cdr
                                          (alexandria:plist-alist format)))))
          (setf (getf ret :vertex)
                (list* bs (buffer-geometry bs count data))))
        (setf (getf ret :material) (aref materials (ai:material-index m))))
      ret)))

(defparameter *mat-props*
  (alexandria:plist-hash-table
   '("?mat.name" (name nil)
     ;; not sure if this should default to on or off?
     "$mat.twosided" (mat-two-sided :int 0)
     "$mat.shadingm" (mat-shading-model :shading-mode :ai-shading-mode-phong)
     "$mat.wireframe" (mat-wireframe :int 0)
     "$mat.blend" (mat-blend :float 0) ;; 0=normal alpha, 1=additive
     "$mat.opacity" (mat-opacity :float 1.0)
     "$mat.bumpscaling" (mat-bump-scaling :float 1.0)
     "$mat.shininess" (mat-shininess :float 0.0) ;; ?
     "$mat.reflectivity" (mat-reflectivity :float 0.0)
     "$mat.shinpercent" (mat-shininess-strength :float 1.0) ;; ?
     "$mat.refracti" (mat-index-of-refraction :float 1.0)
     "$clr.diffuse" (clr-diffuse :vec3 #(0.577 0.577 0.577))
     "$clr.ambient" (clr-ambient :vec3 #(0.577 0.577 0.577))
     "$clr.specular" (clr-specular :vec3 #(0.0 0.0 0.0))
     "$clr.emissive" (clr-emissive :vec3 #(0.0 0.0 0.0))
     "$clr.transparent" (clr-transparent :int 0)
     "$clr.reflective" (clr-reflective :int 0)
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

(defparameter *texture-slots* #(tex-none
                                tex-diffuse
                                tex-specular
                                tex-ambient
                                tex-emissive
                                tex-height
                                tex-normals
                                tex-shininess
                                tex-opacity
                                tex-displacement
                                tex-lightmap
                                tex-reflection
                                tex-unknown))

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
             ;;(format t "normalize ~s ~s~%" value type)
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
                                        ;do (format t "~s -> ~s~%" n tv)
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
            finally (setf (gethash 'tex-present h) (list :int tex-present))))
    (format t "~&mat: ~{~s ~s~%~}~%" (alexandria:hash-table-plist h))
    (format t "= ~{~s~%~^  ~}~%" (alexandria:hash-table-alist
                                  (normalize-material h)))
    ;; possibly should load textures now instead of on use
    (intern-material 'ai-shaders (normalize-material h))))

(defun translate-ai-scene (s)
  ;; todo: animations
  ;; todo: lights
  ;; todo: cameras
  (let* ((nodes/groups (translate-ai-nodes (ai:root-node s)))
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
    (format t "objects=~s~%" (alexandria:hash-table-alist objects))
    (loop for k being the hash-keys of objects using (hash-value v)
          for names = (gethash k groups)
          do (format t "~s @~s-> ~s~%" k names v)
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

(defun ensure-ai-material ()
  (let* ((p (3bgl-shaders::shader-program :vertex '3bgl-ai-shaders::vertex
                                          :fragment '3bgl-ai-shaders::fragment))
         (state (scenegraph::make-state*
                 :program p
                 :vertex-format (buffer-builder::vertex-format-for-layout
                                 *vnc-layout*)
                 :blend-func '(:one :one-minus-src-alpha)
                 :blend nil
                 :depth-test t
                 :cull-face :back))
         (mat (make-material 'ai-shaders state
                             :count-var '3bgl-ai-shaders::count)))
    (make-material 'ai-shaders/blend
                   (scenegraph::make-state*
                    :program p
                    :vertex-format (buffer-builder::vertex-format-for-layout
                                    *vnc-layout*)
                    :blend-func '(:one :one-minus-src-alpha)
                    :blend t
                    :depth-test t
                    :cull-face :back)
                   :count-var '3bgl-ai-shaders::count)
    'ai-shaders))

(defmethod load-object ((loader (eql :file)) name)
  ;; todo: load files on another thread, just draw nothing until loaded
  ;; (possibly draw debug scene until fully loaded?)
  (ensure-ai-material)
  (let ((o (assimp:import-into-lisp
            (merge-pathnames name)
            :processing-flags *ai-loader-post-processing*
            :properties *ai-loader-properties*)))
    (translate-ai-scene o)))


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
