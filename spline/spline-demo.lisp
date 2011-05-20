(in-package #:basecode)

(defclass spline ()
  ((p1 :initarg :p1 :accessor p1)
   (pc :initarg :pc :accessor pc)
   (p2 :initarg :p2 :accessor p2)))

(defun random-vec (r)
  (sb-cga:vec (float (- (random (* 2.0 r)) r) 1.0)
              (float (- (random (* 2.0 r)) r) 1.0)
              (float (- (random (* 2.0 r)) r) 1.0)))

(defun make-spline (&key (s 3) (r 10))
  (let* ((c (random-vec r))
         (p1 (sb-cga:vec+ c (random-vec s)))
         (p2 (sb-cga:vec+ c (random-vec s))))
    (make-instance 'spline :p1 p1 :pc c :p2 p2)))

(defparameter *points* 0)
(defparameter *lines* 0)

(defmethod draw ((s spline))
  (declare (optimize debug))
  (let ((p (sb-cga:normalize
            (sb-cga:cross-product (sb-cga:vec- (pc s) (p1 s))
                                  (sb-cga:vec- (pc s) (p2 s))))))
    (flet ((v (v)
             (gl:vertex (aref v 0) (aref v 1) (aref v 2)))
           (x (v)
             (gl:translate (aref v 0) (aref v 1) (aref v 2))))
      (gl:enable :line-smooth :point-smooth :blend :depth-test)
      (gl:disable :lighting :texture-2d)
      (gl:line-width 1.5)
      (gl:with-primitives :line-strip
        (incf *lines* 2)
        (gl:color 1.0 1.0 1.0 1.0)
        (v (p1 s))
        (v (pc s))
        (v (p2 s)))
      (gl:point-size 2)
      (multiple-value-bind (points normals tangents)
          (3bgl-splines:subdivide-quadratic
           (p1 s) (pc s) (p2 s) :normals t)
        (gl:with-primitives :points
          (gl:color 1.0 0.0 1.0 1.0)
          (incf *points* (length points))
          (loop for i across points
             do (v i)))
        (gl:with-pushed-matrix* (:modelview)
          (gl:point-size 1)
          (gl:with-primitives :points
            (gl:color 1.0 1.0 1.0 1.0)
            (incf *points* 128)
            (loop with d = 128
               for i below d
               for p = (3bgl-splines:evaluate-quadratic
                        (p1 s) (pc s) (p2 s) (float (/ i d) 1.0))
               do (v p)))
          (x (sb-cga:vec* p 0.2))

          (gl:line-width 1.0)
          (gl:with-primitives :lines
            (incf *lines* 8)
            (gl:color 0.0 1.0 0.0 1.0)
            (loop for i below 8
               for p = (3bgl-splines:evaluate-quadratic
                        (p1 s) (pc s) (p2 s) (/ i 8.0))
               for n = (3bgl-splines:evaluate-quadratic-normal
                        (p1 s) (pc s) (p2 s) (/ i 8.0))
               do (v p)  (v (sb-cga:vec+ p (sb-cga:vec* n 0.3)))))
          (gl:with-primitives :lines
            (gl:color 1.0 1.0 0.0 1.0)
            (incf *lines* 8)
            (loop for i below 8
               for p = (3bgl-splines:evaluate-quadratic
                        (p1 s) (pc s) (p2 s) (/ i 8.0))
               for n = (3bgl-splines:evaluate-quadratic-tangent
                        (p1 s) (pc s) (p2 s) (/ i 8.0))
               do (v p) (v (sb-cga:vec+ p (sb-cga:vec* n 0.3))))))
        (gl:with-primitives :lines
          (incf *lines* (length points))
          (loop for p across points
             for n across normals
             for tan across tangents
             do
               (gl:color 0.0 1.0 0.0 1.0)
               (v p)
               (v (sb-cga:vec+ p (sb-cga:vec* n 0.3)))
               (gl:color 1.0 1.0 0.0 1.0)
               (v p)
               (v (sb-cga:vec+ p (sb-cga:vec* tan 0.3)))))
        (gl:line-width 1.5)
        (gl:with-primitives :line-strip
          (incf *lines* (1- (length points)))
          (gl:color 1.0 0.0 0.0 1.0)
          (loop for i across points
             do (v i)))))))

(defclass spline-demo (basecode-glut perspective-projection basecode-clear
                                     fps-graph basecode-draw-ground-plane
                                     freelook-camera)
  ((splines :accessor splines :initform (loop for i below 100
                                           collect (make-spline)))
   (animate :initform nil :accessor animate))
  (:default-initargs :look-at-eye '(3 2 15)))


(defmethod basecode-draw ((w spline-demo))
  (gl:with-pushed-matrix* (:modelview)
    (setf *points* 0 *lines* 0)
    (loop for i in (splines w)
       do (draw i)
         (when (animate w)
           (setf (pc i) (sb-cga:vec+ (pc i) (random-vec 0.05)))
           (setf (p1 i) (sb-cga:vec+ (p1 i) (random-vec 0.05)))
           (setf (p2 i) (sb-cga:vec+ (p2 i) (random-vec 0.05)))))))

(defmethod glut:keyboard :after ((w spline-demo) k x y)
  (when (eql k #\m)
    (setf (animate w) (not (animate w)))))



; (basecode-run (make-instance 'spline-demo))



(defclass spline-demo2 (basecode-glut perspective-projection basecode-clear
                                      fps-graph basecode-draw-ground-plane
                                      freelook-camera)
  ((points :accessor points
           :initform (coerce (loop for i below 100
                                collect (random-vec 15))
                             'vector))
   (colors :accessor colors
           :initform (coerce (loop for i below 100
                                collect (sb-cga:vec (random 1.0)
                                                    (random 1.0)
                                                    (random 1.0)))
                             'vector))
   (fracs :accessor fracs
          :initform (coerce (loop for i below 100 collect (random 1.0))
                            'vector)))
  (:default-initargs :look-at-eye '(5 12 -20)))

(defparameter *counts* nil)
(defmethod basecode-draw ((w spline-demo2))
  (gl:with-pushed-matrix* (:modelview)
    (setf *counts* (list *points* *lines*))
    (setf *points* 0 *lines* 0)
    (flet ((v (v)
             (gl:vertex (aref v 0) (aref v 1) (aref v 2)))
           (c (v)
             (gl:color (aref v 0) (aref v 1) (aref v 2) 1.0)))
      (gl:point-size 4)
      (gl:line-width 2)

      (gl:with-primitives :line-strip
        (gl:color 1 0 0 1)
        (loop with points = (points w)
           with fracs = (fracs w)
           with colors = (colors w)
           for i from 1 below (1- (length points))
           for pc = (aref points i)
           for p1 = (sb-cga:vec-lerp (aref points (1- i)) (aref points i)
                                     (aref fracs (1- i)))
           for p2 = (sb-cga:vec-lerp (aref points i) (aref points (1+ i))
                                     (aref fracs i))
           do
           ;;(v p1)
           ;;(c (aref colors i))
           ;;(v pc)
           ;;(v p2)
           ;;
             (loop with c = (3bgl-splines:subdivide-quadratic
                             p1 pc p2
                             :angle-tolerance-rad (* 1 (/ pi 180)))
                for p across c
                for u from 0.0 by (float (/ (length c)))
                do (incf *lines*)
                  (c (sb-cga:vec-lerp (aref colors (1- i))
                                      (aref colors  i)
                                      u))
                  (v p)))))

    (loop for i below (length (points w))
       do (setf (aref (points w) i)
                (sb-cga:vec+ (aref (points w) i) (random-vec 0.05)))
         #++(setf (aref (fracs w) i)
                  (/ (loop repeat 100 sum (random 1.0)) 100)
                  #++(+ (aref (fracs w) i) (- (random 0.01) 0.005))))))

; (basecode-run (make-instance 'spline-demo2))


