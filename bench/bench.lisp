(in-package #:3bgl-bench)

(defclass im-bench (basecode-glut perspective-projection basecode-clear
                    ;;fps-graph basecode-draw-ground-plane
                    basecode-exit-on-esc
                    freelook-camera)
  ((count :accessor vertex-count :initform 1))
  (:default-initargs :look-at-eye '(5 12 -20)))

(defparameter *points* 0)
(defparameter *frames* 0)
(defparameter *start-time* 0)
(defun update-points (c)
  (let ((now (get-internal-real-time)))
    (when (zerop *start-time*)
      (setf *start-time* now
            *frames* 0
            *points* 0))
    (incf *points* c)
    (incf *frames* 1)
    (when (> (- now *start-time*) (* 2 internal-time-units-per-second))
      (let ((s (/ (float (- now *start-time*)) internal-time-units-per-second)))
        (format t "~s frames in ~s seconds = ~s fps~%"
                *frames* s (/ *frames* s))
        (format t "~s points per frame, ~s points in ~s seconds = ~:d pps~%"
                c *points* s (floor (/ *points* s)))
        (setf *start-time* 0)))))

(defmethod basecode-draw ((w im-bench))
  (declare (optimize speed))
  (let* ((start (get-internal-real-time))
         (c (vertex-count w))
         (fc (float c 1.0))
         (d 1.0))
    (declare  (single-float fc))
    (update-points (* c c d))
    (loop for z single-float from 0.0 below fc
          for z/c = (/ z fc)
          do (loop for y single-float from 0.0 below fc
                   for y/c = (/ y fc)
                   do (gl:color z/c y/c 1.0)
                      (%gl::begini #.(cffi:foreign-enum-value '%gl:enum :points))
                      (loop for x single-float from 0.0 below d
                            for x/c = (/ x d)
                            #+do (gl:color z/c y/c x/c)
                            do (gl:vertex x/c y/c z/c))
                      (%gl:end)))
    #++(gl:with-primitives :points
         (loop with c = (float (vertex-count w) 1.0)
               with w = (float (expt c 1/3) 1.0)
               with w^2 = (float (expt w 2) 1.0)
               for i below c
               for z = (/ i w^2)
               for xy of-type (single-float 0.0 1000000.0) = (float (mod i w^2) 1.0)
               for x = (/ xy w)
               for y = (mod xy w)
               do ))
    #++
    (let* ((end (get-internal-real-time))
           (dt (/ (float (- end start)) internal-time-units-per-second)))
      (cond
        ((< dt 0.1)
         (setf (vertex-count w) (min 100000000 (1+ (vertex-count w)))))
        ((> dt 0.13)
         (setf (vertex-count w) (max 1 (1- (vertex-count w)))))))))


(defmethod mouse-down ((w im-bench) button x y)
  )

(defmethod key-down ((w im-bench) k)
  (case k
    ((#\z :z)
     (setf (vertex-count w) 1))
    ((#\q :q)
     (setf (vertex-count w) 636))))

#++
(basecode-run
 (make-instance
  'im-bench))
