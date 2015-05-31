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
               :initarg :index-type)
   (index-size :accessor index-size :initform 4
               :initarg :index-size)))

(defmethod draw (mesh &key)
  (when mesh
    (error "don't know how to draw ~s (~s)?" mesh (type-of mesh))))

(defmethod draw ((mesh mesh) &key (mode :triangles) (start 0) end)
  (when (vao mesh)
    (gl:bind-vertex-array (vao mesh))
    (cond
      ((index-count mesh)
       #++(assert (<= 0 start (index-count mesh)))
       (cond
         ((>= start (index-count mesh))
          (return-from draw nil))
         ((< start 0)
          (setf start 0)))
       (when end
         (cond
          ((> end (index-count mesh))
           (setf end nil))
          ((<= end start)
           (return-from draw nil))))
       #++(when end (assert (<= start end (index-count mesh))))
       (%gl:draw-elements mode (if end
                                   (- end start)
                                   (- (index-count mesh) start))
                          (index-type mesh)
                          (* start (index-size mesh))))
      ((element-count mesh)
       (unless end (setf end (element-count mesh)))
       (assert (<= 0 start (element-count mesh)))
       (when end (assert (<= start end (element-count mesh))))
       (%gl:draw-arrays mode start (if end
                                       (- end start)
                                       (- (element-count mesh) start)))))))

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
                    (:unsigned-int '(unsigned-byte 32))
                    (:signed-int '(signed-byte 32))
                    (:int '(signed-byte 32))
                    (:unsigned-short '(unsigned-byte 16))
                    (:signed-short '(signed-byte 16))
                    (:unsigned-byte '(unsigned-byte 8))
                    (:byte '(signed-byte 8))
                    )))
    `(static-vectors:with-static-vector (,var ,length :element-type ',lisp-type)
       ,@(if more-bindings
             `((with-static-vectors (,@more-bindings)
                 ,@body))
             body))))

(defun calculate-normals (indices verts normals)
  ;; assumes any sharp edges are already split into separate points
  (let* ((vcount (/ (length verts) 3))
         (tcount (/ (length indices) 3))
         ;(tri-normals (make-array tcount))
         (vert-sums (make-array vcount
                                :initial-element (sb-cga:vec 0.0 0.0 0.0)))
         (vert-counts (make-array vcount :element-type '(unsigned-byte 32)
                                  :initial-element 0)))
    ;; calculate triangle normals
    ;; sum/count vertex normals
    (labels ((i (i)
               (aref indices i))
             (v (i)
               (sb-cga:vec (aref verts (+ (* 3 i) 0))
                           (aref verts (+ (* 3 i) 1))
                           (aref verts (+ (* 3 i) 2))))
             (add (i0 n)
               (loop for i from i0 below (+  i0 3)
                     do (setf (aref vert-sums (i i))
                              (sb-cga:vec+ (aref vert-sums (i i)) n))
                        (incf (aref vert-counts (i i))))))
      (loop for tri below tcount
            for i = (* tri 3)
            for v0 = (v (i i))
            for e1 = (sb-cga:vec- (v (i (+ i 1))) v0)
            for e2 = (sb-cga:vec- (v (i (+ i 2))) v0)
            for n = (sb-cga:cross-product e1 e2)
            do (when (and n (> (sb-cga:vec-length n) 0.001))
                 (add i (sb-cga:normalize n))))
      ;; divide and copy into result vector
      (loop for i below vcount
            for n1 across vert-sums
            for c across vert-counts
            for n = (unless (zerop (sb-cga:vec-length n1))
                      (sb-cga:normalize n1))
            when n
              do (setf (aref normals (+ 0 (* i 3))) (aref n 0)
                       (aref normals (+ 1 (* i 3))) (aref n 1)
                       (aref normals (+ 2 (* i 3))) (aref n 2))))))

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
                (format t "upload ~s: ~sx~s=~s / ~s ~s ~s~%"
                        name (length buffer) size (* (length buffer) size)
                        loc count type)
                (%gl:buffer-data :array-buffer (* (length buffer) size)
                                 (static-vectors:static-vector-pointer buffer)
                                 :static-draw)
                (if (eq type :float)
                    (gl:vertex-attrib-pointer loc count type nil 0 (cffi:null-pointer))
                    (gl:vertex-attrib-ipointer loc count type 0 (cffi:null-pointer))))
           (prog1
               (make-instance 'mesh
                              :vbos vbos :vao vao
                              :index-count (length indices)
                              :element-count (length verts)
                              :index-type index-type
                              :index-size index-size)
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




(defun icosahedron1 ()
  (let ((v (make-array 12))
        (i 0)
        (y1 (sin (atan 2 1))))
    (flet ((v (x y z)
             (setf (aref v i) (vec x y z))
             (incf i)))
      (v 0 1 0)
      (loop for a from 0 by (* 72 (/ pi 180))
            repeat 5
            do (v (sin a) y1 (cos a)))
      (loop for a from 36 by (* 72 (/ pi 180))
            repeat 5
            do (v (sin a) (- y1) (cos a)))
      (v 0 -1 0))
    (list v
          #(1 0 2  2 0 3  3 0 4  4 0 5  5 0 1
            1 2 6  6 2 7  2 3 7  7 3 8  3 4 8
            8 4 9  4 5 9  9 5 10  5 1 10  10 1 6
            6 7 11  7 8 11  8 9 11  9 10 11  10 6 11))))

(defun icosahedron ()
  (let ((p (make-array 22))
        (i 0)
        (y1 (sin (atan 1/2))))
    (flet ((v (x y z u v w)
             (setf (aref p i) (list (sb-cga:normalize (vec x y z))
                                    (vec u v w)))
             (incf i)))
      ;; split poles so we can have separate UVs
      (v 0 1 0 0.1 0 1)
      (v 0 1 0 0.3 0 1)
      (v 0 1 0 0.5 0 1)
      (v 0 1 0 0.7 0 1)
      (v 0 1 0 0.9 0 1)
      (loop for a from 0 by (* 72 (/ pi 180))
            for u from 0 by 0.2
            repeat 6
            do (v (sin a) y1 (cos a) u 1/3 1))
      (loop for a from (* 36 (/ pi 180)) by (* 72 (/ pi 180))
            for u from 0.1 by 0.2
            repeat 6
            do (v (sin a) (- y1) (cos a)
                  u 2/3 1))
      (v 0 -1 0 0.2 1 1)
      (v 0 -1 0 0.4 1 1)
      (v 0 -1 0 0.6 1 1)
      (v 0 -1 0 0.8 1 1)
      (v 0 -1 0 1.0 1 1))
    (list p
          ; 0..4 5..10 11..16 17..21
          #(5 0 6  6 1 7  7 2 8  8 3 9  9 4 10
            5 6 11  6 7 12  7 8 13  8 9 14  9 10 15
            11 6 12  12 7 13  13 8 14  14 9 15  15 10 16
            11 12 17  12 13 18  13 14 19  14 15 20  15 16 21))))



(defun geodesic-sphere-mesh (&key
                               (center '(0.0 0.0 0.0))
                               (radius 1.0)
                               ;; divisions 0 = icosahedron (with smoothed normals)
                               (divisions 2))
  (declare (optimize debug))
  (let* ((center (svec center))
         (radius (float radius 1f0))
         (icos (icosahedron))
         vin iin vout iout new-indices)
    (labels ((v (i1 &optional i2)
               (cond
                 ;; existing midpoint
                 ((and i1 i2 (gethash (list (min i1 i2) (max i1 i2))
                                      new-indices)))
                 ;; existing vertex
                 ((and (not i2) (gethash i1 new-indices)))
                 ;; new midpoint
                 (i2
                  (if (< i2 i1) (rotatef i1 i2))
                  (let* ((p1 (aref vin i1))
                         (p2 (aref vin i2))
                         ;; average points, then normalize to push out to
                         ;; surface of sphere
                         (new (list
                               (sb-cga:normalize
                                (sb-cga:vec* (sb-cga:vec+ (first p1)
                                                          (first p2))
                                             0.5))
                               (sb-cga:vec* (sb-cga:vec+ (second p1)
                                                         (second p2))
                                            0.5))))
                    (setf (gethash (list i1 i2) new-indices)
                          (vector-push-extend new vout))))
                 ;; old vertex not seen yet
                 (t
                  (setf (gethash i1 new-indices)
                        (vector-push-extend (aref vin i1) vout)))
                 ))
             (tri (i1 i2 i3)
               (vector-push-extend i1 iout)
               (vector-push-extend i2 iout)
               (vector-push-extend i3 iout))
             (sub1 (a b c)
               (let ((p1 (v a))
                     (p2 (v a b))
                     (p3 (v a c))
                     (p4 (v b))
                     (p5 (v b c))
                     (p6 (v c)))
                 (tri p2 p1 p3)
                 (tri p4 p2 p5)
                 (tri p2 p3 p5)
                 (tri p5 p3 p6)))
             (sub ()
               (loop for i below (length iin) by 3
                     for p1 = (aref iin (+ i 0))
                     for p2 = (aref iin (+ i 1))
                     for p3 = (aref iin (+ i 2))
                     do (sub1 p1 p2 p3))))
      (setf vin (first icos)
            iin (second icos))
      (loop for i below (or divisions 0)
            do (setf vout (make-array (length vin)
                                      :adjustable t
                                      :element-type 'sb-cga:vec
                                      :fill-pointer 0)
                     iout (make-array (length iin)
                                      :adjustable t
                                      :element-type '(unsigned-byte 32)
                                      :fill-pointer 0)
                     new-indices (make-hash-table :test #'equal))
               (sub)
               (setf vin vout iin iout))

  (let ((vcount (length vin))
        (icount (length iin))
        (vindex 0)
        (short (< (length iin) 65535)))
    (with-static-vectors ((verts (* vcount 3))
                          (uvs (* vcount 3))
                          (normals (* vcount 3))
                          (tangents (* vcount 3))
                          (bone-weights (* vcount 4))
                          (bone-indices (* vcount 4) :unsigned-byte))
      (static-vectors:with-static-vector (indices
                                          icount
                                          :element-type (if short
                                                            '(unsigned-byte 16)
                                                            '(unsigned-byte 32))
                                          :initial-element 0)
        (labels ((add1 (a x &optional (count 3))
                   (loop for i below count
                         do (setf (aref a (+ i (* vindex count)))
                                  (aref x i)))))
          (loop with bw = (vec4 1 0 0 0)
                with bi = (ub8vec4 0 0 0 0)
                for (v1 uv) across vin
                for v = (sb-cga:vec+ center (sb-cga:vec* v1 radius))
                for n = (sb-cga:normalize v1)
                for tan = (let ((x (sb-cga:cross-product n (vec 0 1 0))))
                            (if (< (sb-cga:vec-length x) 0.1)
                                (vec (sin (* (+ 1/2 (elt uv 0)) (/ pi 2)))
                                     0
                                     (cos (* (+ 1/2 (elt uv 0)) (/ pi 2))))
                                x))
                do (add1 verts v 3)
                   (add1 uvs uv 3)
                   (add1 normals n 3)
                   (add1 tangents tan 3)
                   (add1 bone-weights bw 4)
                   (add1 bone-indices bi 4)
                   (incf vindex)))
        (replace indices iin)
        (%make-mesh indices
                    verts uvs normals tangents bone-weights bone-indices
                    :index-type (if short :unsigned-short :unsigned-int)
                    :index-size (if short 2 4))))))))

(defun ai-mesh-1 (mesh bone-map)
  (assert (= 4 (ai:primitive-types mesh))) ;; must be triangulated
  (let* ((vcount (length (ai:vertices mesh)))
         (icount (* 3 (length (ai:faces mesh))))
         (vindex 0)
         (short (< vcount 65536))
         (non-orthogonal-bitangents nil)
         (bw (make-array vcount :initial-element nil))
         (bi (make-array vcount :initial-element nil))
         (uv (make-array vcount :initial-element nil)))
    (with-static-vectors ((verts (* vcount 3))
                          (uvs (* vcount 3))
                          (normals (* vcount 3))
                          (tangents (* vcount 3))
                          (bone-weights (* vcount 4))
                          (bone-indices (* vcount 4) :unsigned-byte))
      (static-vectors:with-static-vector (indices
                                          icount
                                          :element-type (if short
                                                            '(unsigned-byte 16)
                                                            '(unsigned-byte 32))
                                          :initial-element 0)
        (labels ((add1 (a x &optional (count 3))
                   (loop for i below count
                         do (setf (aref a (+ i (* vindex count)))
                                  (aref (aref x vindex) i)))))
          (loop for bone across (ai:bones mesh)
                for index = (gethash (ai:name bone) bone-map)
                do (assert index)
                   (loop for w across (ai:weights bone)
                         do (push index (aref bi (ai:id w)))
                            (push (ai:weight w) (aref bw (ai:id w)))))
          (flet ((v4 (x init)
                   (assert x)
                   (map-into (make-array 4 :initial-element init)
                             #'identity (reverse x))))
            (loop for v below vcount
                  for w across bw
                  for i across bi
                  do (setf (aref bw v) (v4 w 0.0))
                  do (setf (aref bi v) (v4 i 0))))
          (loop for uv2 across (aref (ai:texture-coords mesh) 0)
                for bt across (ai:bitangents mesh)
                for n across (ai:normals mesh)
                for tan across (ai:tangents mesh)
                for dot = (sb-cga:dot-product bt (sb-cga:cross-product n tan))
                for uv3 = (sb-cga:vec (aref uv2 0) (aref uv2 1) 1.0)
                for i from 0
                when (minusp dot)
                  do (setf (aref uv3 2) -1.0)
                when (< (abs dot) 0.9)
                  do (push (list dot n tan bt) non-orthogonal-bitangents)
                do (setf (aref uv i) uv3))
          (loop for i below (length (ai:vertices mesh))
                do (add1 verts (ai:vertices mesh) 3)
                   (add1 uvs uv 3)
                   (add1 normals (ai:normals mesh) 3)
                   (add1 tangents (ai:tangents mesh) 3)
                   (add1 bone-weights bw 4)
                   (add1 bone-indices bi 4)
                   (incf vindex)))
        (loop for iindex from 0
              for face across (ai:faces mesh)
              do (loop for i below 3
                       do (setf (aref indices (+ i (* iindex 3)))
                                (aref face i))))
        (when non-orthogonal-bitangents
          (warn "non-orthogonal-bitangents")
          #++(with-simple-restart (continue "continue1")
            (error "non-orthogonal bitangents in input?"
                   non-orthogonal-bitangents)))
        (when (loop for i from 0
                    for c across (ai:components-per-texture-coord mesh)
                      thereis (and (zerop i) (/= c 2))
                        thereis (and (plusp i) (/= c 0)))
          (with-simple-restart (continue "continue1")
            (error "unsupported number of texcoords ~s"
                   (ai:components-per-texture-coord mesh))))
       (%make-mesh indices
                   verts uvs normals tangents bone-weights bone-indices
                   :index-type (if short :unsigned-short :unsigned-int)
                   :index-size (if short 2 4))))))

(defun ai-mesh (scene bone-map)
  "return a list of MESH objects from the AI:MESHES in scene"
  (loop for m across (ai:meshes scene)
        collect (ai-mesh-1 m bone-map) ))

