(in-package #:basecode)

;;; fixme: clean this up...
(defclass fps-graph ()
  ((fps-history :accessor fps-history
                :initform (make-array 120 :initial-element 0))
   (frame-time-history :accessor frame-time-history
                       :initform (make-array 120 :initial-element 0))
   (draw-time-history :accessor draw-time-history
                      :initform (make-array 120 :initial-element 0))
   (fps-history-index :accessor fps-history-index :initform 0)
   (last-frame-time :accessor last-frame-time :initform 0)
   (last-fps-time :accessor last-fps-time :initform 0)
   (current-fps-sample :accessor current-fps-sample :initform 0)
   (current-fps-sample-count :accessor current-fps-sample-count :initform 0)))

;;; fixme: higher res timer?
(defun now ()
  ;; single float runs out of precision, so use doubles here and cast to single
  ;; if needed after calculating a delta or whatever
  (float (/ (get-internal-real-time) internal-time-units-per-second) 1d0))

(defmethod basecode-draw :around ((w fps-graph))
  (let ((start (now)))
    (call-next-method)
    (let ((stop (now)))
      (with-pixel-ortho-projection (w)
        (gl:with-pushed-matrix* (:modelview)
          (gl:load-identity)
          (gl:disable :lighting :cull-face :texture-1d :texture-2d  :texture-3d
                      :depth-test)
          (gl:with-primitives :triangles
            (gl:color 0.3 0.3 0.6 0.7)
            (gl:vertex 0 0 0)
            (gl:vertex 120 0 0)
            (gl:vertex 0 100 0)
            (gl:vertex 0 100 0)
            (gl:vertex 120 0 0)
            (gl:vertex 120 100 0))
          (gl:color 1 0 0 1)
          (gl:line-width 1)
          (gl:with-primitives :line-strip
            (loop
               with l = (length (fps-history w))
               for x from 0 below l
               for y = (aref (fps-history w) (mod (+ x (fps-history-index w))
                                                  l))
               do (gl:vertex x (- 100 y) 0)))
          (gl:color 0 1 0 1)
          (gl:with-primitives :line-strip
            (loop
               with l = (length (fps-history w))
               for x from 0 below l
               for y = (aref (frame-time-history w)
                             (mod (+ x (fps-history-index w)) l))
               do (gl:vertex x (- 100 y) 0)))
          (gl:color 0 0 1 1)
          (gl:with-primitives :line-strip
            (loop
               with l = (length (fps-history w))
               for x from 0 below l
               for y = (aref (draw-time-history w)
                             (mod (+ x (fps-history-index w)) l))
               do (gl:vertex x (- 100 y) 0)))))
      (when (> (- stop (last-fps-time w)) 0.03)
        (let ((d (- stop (last-fps-time w))))
          (setf (aref (fps-history w) (fps-history-index w))
                (/ (current-fps-sample-count w)
                   d))
          (setf (aref (frame-time-history w) (fps-history-index w))
                (* 1000 (- stop (last-frame-time w))))
          (setf (aref (draw-time-history w) (fps-history-index w))
                (* 1000 (- stop start)))
          (setf (current-fps-sample-count w) 0)
          (setf (fps-history-index w)
                (mod (1+ (fps-history-index w))
                     (length (fps-history w))))
          (setf (last-fps-time w) stop)
          )
        )
      (incf (current-fps-sample-count w))
      (setf (last-frame-time w) stop)

)
    )
)
