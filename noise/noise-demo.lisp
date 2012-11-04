(in-package #:3bgl-noise)

(defclass noise-demo (basecode::basecode-glut
                      basecode:perspective-projection
                      basecode:basecode-clear
                      basecode:fps-graph
                      basecode:basecode-draw-axes
                      basecode:basecode-draw-ground-plane
                      basecode:freelook-camera)
  ((texture :initform nil :accessor texture))
  (:default-initargs :look-at-eye '(8 1 4)))

(defparameter *grid* nil)
(defparameter *period* 7d0)

(defmethod basecode:basecode-draw ((w noise-demo))
  (gl:enable :depth-test)
  (when (texture w)
   (gl:with-pushed-matrix* (:modelview)
     (gl:disable :cull-face :lighting)
     (gl:enable :texture-2d)
     (gl:bind-texture :texture-2d (texture w))
       (gl:color 1 1 1 1)
       (gl:scale 4 4 4)
     (gl:with-primitives :quads
       (gl:tex-coord 0 0)
       (gl:vertex -1 0.1 -1)

       (gl:tex-coord 0 2)
       (gl:vertex -1 0.1 1)

       (gl:tex-coord 2 2)
       (gl:vertex 1 0.1 1)

       (gl:tex-coord 2 0)
       (gl:vertex 1 0.1 -1))))
  (gl:disable :texture-2d)
  (when *grid*
    (gl:with-pushed-matrix* (:modelview)
      (gl:translate 2 0.5 2)
      ;(gl:scale 0.25 0.25 0.25)
      (let ((/x (- (/ (sqrt 2.0)))))
        (declare (ignorable /x))
        (gl:with-pushed-matrix* (:modelview)
          #++(gl:scale  (/ 16.0 15.0) 1  (/ 16.0 15.0))
          (let ((s (/ 4 (/ 256 *period*)) #++ 16.0 #++(/ 16.0 11.0)))
            (gl:scale s 1 s))
          (gl:color 1 0 0)
          (gl:line-width 2)
          (gl:enable :line-smooth :blend)
          (gl:blend-func :src-alpha :one-minus-src-alpha)
          (gl:with-primitives :lines
            (loop for i from -32 to 16
                                        ;with /x = (- (- 1 (/ (sqrt 2.0) 1.6)))
                                        ;with /x = (- (- 1 (/ (sqrt 3d0) 2)))
               with /x = (- (/ (- 3.0 (sqrt 3d0)) 6d0))
                  ;/x = (+ (/ 3d0))
               do
                 (gl:vertex (+ i (* (+ i -32) /x))
                            0
                            (+ -32 (* (+ i -32) /x)))
                 (gl:vertex (+ i (* (+ i 32) /x))
                            0
                            (+ 32 (* (+ i 32) /x)))

                 (gl:vertex (+ -32 (* (+ i -32) /x))
                            0
                            (+ i (* (+ i -32) /x)))
                 (gl:vertex (+ 32 (* (+ i 32) /x))
                            0
                            (+ i (* (+ i 32) /x)))
                 ))
          )
        (gl:with-primitives :lines
          #++(loop for i to 16
                do
                  (gl:vertex (+ i (* i /x)) 0 (* i /x))
                  (gl:vertex (+ i (* (+ i 16) /x))
                             0
                             (+ 16 (* (+ i 16) /x)))
                  (gl:vertex (* i /x) 0 (+ i (* i /x)))
                  (gl:vertex (+ 16 (* (+ i 16) /x))
                             0
                             (+ i (* (+ i 16) /x)))
                  )
          (gl:color 1 0 0)
          #++(loop for i to 16
                do
                  (gl:vertex (* i /x) 0 (+ i (* i /x)))
                  (gl:vertex (+ i (* i /x)) 0 (* i /x))
                  (gl:vertex (+ i (* (+ i 16) /x))
                             0
                             (+ 16 (* (+ i 16) /x)))
                  (gl:vertex (+ 16 (* (+ i 16) /x))
                             0
                             (+ i (* (+ i 16) /x)))
                  )
          (gl:color 1 1 0)
          (loop for i from -16 to 16
             do (gl:vertex i 0 -16)
               (gl:vertex i 0 16)
               (gl:vertex -16 0 i)
               (gl:vertex 16 0 i)
               )
          (gl:color 0 1 1)
          (loop for i to 32
             do (gl:vertex i 0 0)
               (gl:vertex i 0 32)
               (gl:vertex 0 0 i)
               (gl:vertex 32 0 i)
               )))))
)

(defun update-texture (w fun wx wy wu wv &key wrap)
  (unless (texture w)
    (setf (texture w) (car (gl:gen-textures 1))))
  (let ((tex (texture w))
        (a (make-array (* wx wy) :element-type '(unsigned-byte 8)
                       :initial-element 0)))
    (time
     (loop
        for v from (- (/ wv 2.0)) by (/ wv wy)
        for y below wy
        do (loop for u from (- (/ wu 2.0)) by (/ wu wx)
              for x below wx
              do (setf (aref a (+ (* wx y) x))
                       (mod (floor (+ 128 (* 128
                                             (if wrap
                                                 (funcall fun u v 0.0 wrap)
                                                 (funcall fun u v 0.0))
                                             ))) 256)))))
    #++(loop
       for v from (- (/ wv 2.0)) by (/ wv wy)
       for y below wy
       do (loop for u from (- (/ wu 2.0)) by (/ wu wx)
             for x below wx
             do (setf (aref a (+ (* wx y) x))
                      (mod (floor (+ 128 (* 128 (/ (* y x) 111) ))) 256))))

    (gl:bind-texture :texture-2d tex)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-image-2d :texture-2d 0 :luminance wx wy 0
                     :luminance :unsigned-byte a)
    (gl:generate-mipmap :texture-2d)
    (gl:bind-texture :texture-2d 0)))

(defmethod glut:keyboard :after ((w noise-demo) k x y)
  (declare (optimize (debug 2)))
  (case k
    (#\1
     (update-texture w #'simplex-3d-perlin-reference 512 512 *period* *period*))
    (#\2
     #++(update-texture w #'simplex-3d-perlin-opt 512 512 *period* *period*))
    (#\3
     (update-texture w #'simplex-3d-gustavson 512 512 *period* *period*))
    (#\4
     (update-texture w (lambda (x y z) (declare (ignore z))
                         (simplex-2d-gustavson x y)) 512 512 *period* *period*))
    (#\5
     (update-texture w (lambda (x y z)
                         (simplex-4d x y z 0.0)) 512 512 *period* *period*))
    (#\+
     ;; 3 6 9 15 30
     (princ (incf *period* 0.25d0))
     #++(update-texture w #'simplex-3d-perlin-reference 512 512 *period* *period*)
     (update-texture w (lambda (x y z) (declare (ignore z))
                         (simplex-2d-gustavson x y)) 512 512
                         (/ 256 *period*) (/ 256 *period*)))
    (#\-
     ;; 3 6 9 15 30
     (decf *period* 0.1)
     #++ (update-texture w #'simplex-3d-perlin-reference 512 512 *period* *period*)
          (update-texture w (lambda (x y z) (declare (ignore z))
                         (simplex-2d-gustavson x y)) 512 512 *period* *period*))
    (#\g (setf *grid* (not *grid*)))))



; (basecode:basecode-run (make-instance 'noise-demo))
