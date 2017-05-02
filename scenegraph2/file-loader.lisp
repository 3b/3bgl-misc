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



(defun translate-ai-skeleton (root)
  (let ((i 0)
        ;; name -> index + node
        (h (make-hash-table :test 'equalp))
        ;; set of meshes -> names of nodes using that set
        (objects (make-hash-table :test 'equalp))
        (work nil))
    (labels ((add (n)
               (let ((name (ai:name n)))
                 (format t "add node ~a~%" name)
                 (setf (gethash name h)
                       (list i n))
                 (push name (gethash (sort (copy-seq (ai:meshes n)) '<)
                                     objects))
                 (incf i)))
             (r (w)
               (when w
                 (map nil #'add w)
                 (loop for n in work
                       do (loop for c across (ai:children n)
                                do (push c work)))
                 (r (reverse (shiftf work nil))))))
      (setf work (list root))
      (r work)
      (list h objects))))

(defun translate-ai-mesh (m skel)
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
                (list* bs (buffer-geometry bs count data)))))
      ret)))

(defun translate-ai-scene (s)
  ;; todo: animations
  ;; todo: lights
  ;; todo: cameras
  (let* ((skeleton (translate-ai-skeleton (ai:root-node s)))
         (meshes (loop for m across (ai:meshes s)
                       collect (translate-ai-mesh m skeleton))))
    (list skeleton meshes)))

(defmethod load-object ((loader (eql :file)) name)
  (let ((o (assimp:import-into-lisp
            (merge-pathnames name)
            :processing-flags *ai-loader-post-processing*
            :properties *ai-loader-properties*)))
    (translate-ai-scene o)))


#++
(sg::load-object :file
                 (asdf:system-relative-pathname '3bgl-misc "data/teapot.obj"))
