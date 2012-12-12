(in-package #:3bgl-mesh)

;; generic skinned mesh lib (gl 3.2+ (maybe 3.3?))
;; (just geometry for now? only one piece, etc.
;;  textures, multiple parts, skeletons anim etc to be added by
;;  higher level class)

;; for now, assumes following attribute layout:
;;   :position 0 = vec3
;;   :uv 1 = vec3 (uv + sign of bitangent)
;;   :normal 2 = vec3
;;   :tangent 3 = vec3
;;   :bone-weights 4 = vec4
;;   :bone-indices 5 = vec4 (u8)

(defclass mesh ()
  ((vbos :accessor vbos :initform nil :initarg :vbos)
   (vao :accessor vao :initform nil :initarg :vao)
   (element-count :accessor element-count :initform nil :initarg :element-count)
   (index-count :accessor index-count :initform nil :initarg :index-count)
   ;; fixme: possibly should store numeric value rather than keyword for
   ;; index type?
   (index-type :accessor index-type :initform :unsigned-int
               :initarg :index-type)))

(defmethod draw (mesh &key)
  (when mesh
    (error "don't know how to draw ~s (~s)?" mesh (type-of mesh))))

(defmethod draw ((mesh mesh) &key (mode :triangles))
  (when (vao mesh)
    (gl:bind-vertex-array (vao mesh))
    (cond
      ((index-count mesh)
       (gl:point-size 12)
       (%gl:draw-elements mode (index-count mesh)
                          (index-type mesh)
                          (cffi:null-pointer)))
      ((element-count mesh)
       (%gl:draw-arrays mode 0 (element-count mesh))))))

(defmethod free-mesh ((mesh mesh))
  (when (vbos mesh)
    (gl:delete-buffers (vbos mesh))
    (setf (vbos mesh) nil))
  (when (vao mesh)
    (gl:delete-vertex-arrays (list (vao mesh)))
    (setf (vao mesh) nil)))

(defun vec (x y z)
  (sb-cga:vec (float x 1f0) (float y 1f0) (float z 1f0)))

(defun svec (seq)
  (map-into (sb-cga:vec 0.0 0.0 0.0)
            (lambda (x) (float x 1f0))
            seq))

(defun vec4 (x y z w)
  (make-array 4
              :element-type 'single-float
              :initial-contents (list (float x 1f0) (float y 1f0)
                                      (float z 1f0) (float w 1f0))))
(defun ub8vec4 (x y z w)
  (make-array 4
              :element-type '(unsigned-byte 8)
              :initial-contents (list x y z w)))

(defun floats (l &key (initial-element 0f0))
  (make-array l :element-type 'single-float :initial-element (or initial-element 0f0)))

;;; some simple shapes
(defmacro with-static-vectors (((var length &optional (gl-type :float))
                                &rest more-bindings) &body body)
  (let ((lisp-type (ecase gl-type
                    (:float 'single-float)
                    (:unsigned-byte '(unsigned-byte 8))
                    (:byte '(signed-byte 8))
                    )))
    `(static-vectors:with-static-vector (,var ,length :element-type ',lisp-type)
       ,@(if more-bindings
             `((with-static-vectors (,@more-bindings)
                 ,@body))
             body))))

(defun %make-mesh (indices verts uvs normals tangents bone-weights bone-indices
                   &key (index-type :unsigned-int) (index-size 4))
  (let ((vbos (gl:gen-buffers 7))
        (vao (gl:gen-vertex-array)))
    (unwind-protect
         (progn
           (gl:bind-vertex-array vao)
           (gl:bind-buffer :element-array-buffer (car vbos))
           (%gl:buffer-data :element-array-buffer (* (length indices)
                                                     index-size)
                           (static-vectors:static-vector-pointer indices)
                           :static-draw)
           (loop
              for vbo in (cdr vbos)
              for buffer in (list verts uvs normals tangents bone-weights
                                  bone-indices)
              for name in '(verts uvs normals tangents bone-weights
                                  bone-indices)
              for count in (list 3 3 3 3 4 4)
              for size in (list 4 4 4 4 4 1)
              for type in (list :float :float :float :float :float
                                :unsigned-byte)
              for loc from 0
              do
                (gl:enable-vertex-attrib-array loc)
                (gl:bind-buffer :array-buffer vbo)
                (%gl:buffer-data :array-buffer (* (length buffer) size)
                                 (static-vectors:static-vector-pointer buffer)
                                 :static-draw)
                (gl:vertex-attrib-pointer loc count type nil 0 (cffi:null-pointer)))
           (prog1
               (make-instance 'mesh
                              :vbos vbos :vao vao
                              :index-count (length indices)
                              :element-count (length verts)
                              :index-type index-type)
             (setf vao nil)
             (setf vbos nil)))
      ;; clean stuff up (we set VBOS and VAO to nil on normal exit,
      ;; delete them otherwise)
      (when vbos
        (gl:delete-buffers vbos))
      (when vao
        (gl:delete-vertex-arrays (list vao)))
      (gl:bind-vertex-array 0)
      (gl:bind-buffer :array-buffer 0)
      (gl:bind-buffer :element-array-buffer 0))))

(defun cube-mesh (&key (center '(0.0 0.0 0.0))
                  (width 1.0)
                  (height width)
                  (depth width))
  (declare (optimize debug))
  ;; possibly should add option to control UV mapping? just doing 0..1
  ;; on each face for now...
  (let ((center (svec center))
        (size (vec (* 0.5 width) (* 0.5 height) (* 0.5 depth)))
        (index 0))
    (with-static-vectors ((verts (* 6 4 3))
                          (uvs (* 6 4 3))
                          (normals (* 6 4 3))
                          (tangents (* 6 4 3))
                          (bone-weights (* 6 4 4))
                          (bone-indices (* 6 4 4) :unsigned-byte))
      ;; generate vertices
      (labels ((add1 (a x &optional (count 3))
                 (loop for i below count
                      do (setf (aref a (+ i (* index count)))
                               (aref x i))))
               (add (vert uv norm tan bw bi)
                 (add1 verts vert 3)
                 (add1 uvs uv 3)
                 (add1 normals norm 3)
                 (add1 tangents tan 3)
                 (add1 bone-weights bw 4)
                 (add1 bone-indices bi 4)
                 (incf index)))
        (loop for axis in (list (vec 1 0 0)
                                (vec 0 1 0)
                                (vec 0 0 1)
                                (vec -1 0 0)
                                (vec 0 -1 0)
                                (vec 0 0 -1))
           for ax2 = (vec (aref axis 2) (aref axis 0) (aref axis 1))
           for ax3 = (vec (aref axis 1) (aref axis 2) (aref axis 0))
           for rot = (sb-cga:matrix
                      (aref axis 0) (aref axis 1) (aref axis 2) 0.0
                      (aref ax2 0) (aref ax2 1) (aref ax2 2) 0.0
                      (aref ax3 0) (aref ax3 1) (aref ax3 2) 0.0
                      0.0 0.0 0.0 1.0)
           for scale = (sb-cga:scale size)
           for xlate = (sb-cga:translate center)
           for xform = (sb-cga:matrix* rot scale xlate)
           do (loop
                 for y in '(-1 1)
                 for u in '(0 1)
                 do (loop
                       for z in '(-1 1)
                       for v in '(0 1)
                       for p = (sb-cga:transform-point (vec 1 y z) xform)
                       for n = (sb-cga:transform-direction (vec 1 0 0) rot)
                       for t1 = (sb-cga:transform-direction (vec 0 1 0) rot)
                       do (add p (vec u v 1)
                               n t1
                               (vec4 1 0 0 0) (ub8vec4 0 0 0 0))))))
      ;; generate indices
      (static-vectors:with-static-vector (indices
                                          (* 6 6)
                                          :element-type '(unsigned-byte 16))
        (loop with index = 0
           for i below (* 6 4) by 4
           do (flet ((add (x)
                 (setf (aref indices index) x)
                 (incf index)))
                (add (+ i 0))
                (add (+ i 1))
                (add (+ i 3))
                (add (+ i 0))
                (add (+ i 2))
                (add (+ i 3))))

        (%make-mesh indices
                    verts uvs normals tangents bone-weights bone-indices
                    :index-type :unsigned-short :index-size 2)))))

(defun cylinder-mesh (&key (center '(0.0 0.0 0.0))
                      (radius 1.0)
                      (length 2.0)
                      (divisions 16)
                      (segments 16)
                      #++(bones 2))
  (declare (optimize debug))
  (let ((center (svec center))
        (vcount (+ (* (1+ segments) (1+ divisions)) ;; sides
                   (* 2 (+ 1 divisions)))) ;; caps
        (icount (+ (* 2 (* 3 segments))
                   (* 6 (1+ segments) (1+ divisions))))
        (vindex 0)
        (iindex 0))
    (with-static-vectors ((verts (* vcount 3))
                          (uvs (* vcount 3))
                          (normals (* vcount 3))
                          (tangents (* vcount 3))
                          (bone-weights (* vcount 4))
                          (bone-indices (* vcount 4) :unsigned-byte))
      (static-vectors:with-static-vector (indices
                                          icount
                                          :element-type '(unsigned-byte 16)
                                          :initial-element 0)
        (labels ((add1 (a x &optional (count 3))
                   (loop for i below count
                      do (setf (aref a (+ i (* vindex count)))
                               (aref x i))))
                 (add (vert uv norm tan bw bi)
                   (add1 verts vert 3)
                   (add1 uvs uv 3)
                   (add1 normals norm 3)
                   (add1 tangents tan 3)
                   (add1 bone-weights bw 4)
                   (add1 bone-indices bi 4)
                   (incf vindex))
                 (addi (x)
                   (setf (aref indices iindex) x)
                   (incf iindex)))
          ;; generate cylinder
          (loop for s to segments
             for v = (float (/ s segments) 1f0)
             for y = (+ (* -0.5 length)
                        (* length v))
             ;; we wrap all the way around so UVs on last column of
             ;; tris go to 1 instead of wrapping back to 0
             do (loop for r to divisions
                   for u = (float (/ r divisions) 1f0)
                   for a = (float (* r (/ pi divisions 0.5)) 1f0)
                   for x = (* radius (sin a))
                   for z = (* radius (cos a))
                   for tx = (* radius (sin (+ a (/ pi 2))))
                   for tz = (* radius (cos (+ a (/ pi 2))))
                   ;; fixme: reuse first xyz for last point in circle to make
                   ;; sure they match (should be same pos but different uv)
                   do (add (sb-cga:vec+ center (vec x y z))
                           (vec u v 1)
                           (sb-cga:normalize (vec x 0 z))
                           (sb-cga:normalize (vec tx 0  tz))
                           ;; todo: more bones
                           (vec4 v (- 1 v) 0 0)
                           (ub8vec4 0 1 0 0))))
          (loop for s below segments
             do (loop for r below divisions
                   for p1 = (+ (* s (1+ divisions)) r)
                   for p2 = (+ (* (1+ s) (1+ divisions)) r)
                   do (addi p1)
                     (addi p2)
                     (addi (1+ p2))
                     (addi p1)
                     (addi (1+ p2))
                     (addi (1+ p1))))

          ;; generate caps
          (loop
             for s in '(-1 1)
             for b in '(0 1)
             do (let ((l (* s (/ length 2.0)))
                      (c1 vindex))
                  (add (vec 0 l 0)
                       (vec 0.5 0.5 1)
                       (vec 0 s 0)
                       (vec s 0 0)
                       (vec4 1 0 0 0)
                       (ub8vec4 b 0 0 0))
                  (loop for r below divisions
                     for a = (float (* r (/ pi divisions 0.5)) 1f0)
                     for x = (* radius (sin a))
                     for z = (* radius (cos a))
                     do (add (vec x l z)
                             (vec (* 0.5 (+ (sin a) 1))
                                  (* 0.5 (+ (cos a) 1))
                                  1
                                  )
                             (vec 0 s 0)
                             (vec s 0 0)
                             ;; todo: more bones
                             (vec4 1 0 0 0)
                             (ub8vec4 b 0 0 0))
                       (unless (zerop r)
                         (addi c1)
                         (addi (+ c1 1 r -1))
                         (addi (+ c1 1 r))))
                  (addi c1)
                  (addi (+ c1 divisions))
                  (addi (+ c1 1)))))
        (%make-mesh indices
                    verts uvs normals tangents bone-weights bone-indices
                    :index-type :unsigned-short :index-size 2)))))





