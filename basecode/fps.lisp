(in-package #:basecode)

(defclass fps ()
  ((fps-history :accessor fps-history
                :initform (make-array 120 :initial-element 0))
   (frame-time-history :accessor frame-time-history
                       :initform (make-array 120 :initial-element 0))
   (draw-time-history :accessor draw-time-history
                      :initform (make-array 120 :initial-element 0))
   (fps-history-index :accessor fps-history-index :initform 0)
   (last-frame-time :accessor last-frame-time :initform 0)
   (last-fps-time :accessor last-fps-time :initform 0)
   (average-fps :accessor average-fps :initform 0)
   (current-fps-sample :accessor current-fps-sample :initform 0)
   (current-fps-sample-count :accessor current-fps-sample-count :initform 0)))

;;; fixme: clean this up...
(defclass fps-graph (fps)
  ())

;;; fixme: higher res timer?
(defun now ()
  ;; single float runs out of precision, so use doubles here and cast to single
  ;; if needed after calculating a delta or whatever
  ;;#+(and sbcl (not win32))
  ;;(multiple-value-bind (s us) (sb-ext:get-time-of-day)
  ;;   (+ s (* us 1d-6)))
  ;;#+(not (and sbcl (not win32)))
  ;;(float (/ (get-internal-real-time) internal-time-units-per-second) 1d0)
  ;; just use gl timestamp for now, gl3.2+
  (/ (gl:get* :timestamp)
     1000000000d0))

(defun update-fps (w start stop)
  (when (> (- start (last-fps-time w)) 0.03)
    (let* ((d (- stop (last-fps-time w)))
           (fps (/ (current-fps-sample-count w)
                   d)))
      (setf (aref (fps-history w) (fps-history-index w)) fps)
      (let ((o (average-fps w)))
        (setf (average-fps w) (+ (* 0.9 (average-fps w))
                                 (* 0.1 fps)))
        (when (= o (average-fps w))
          (setf (average-fps w) fps)))
      (setf (aref (frame-time-history w) (fps-history-index w))
            (* 1000 (- start (last-frame-time w))))
      (setf (aref (draw-time-history w) (fps-history-index w))
            (* 1000 (- stop start)))
      (setf (current-fps-sample-count w) 0)
      (setf (fps-history-index w)
            (mod (1+ (fps-history-index w))
                 (length (fps-history w))))
      (setf (last-fps-time w) stop)))
  (incf (current-fps-sample-count w))
  (setf (last-frame-time w) start))


(defmethod basecode-draw :around ((w fps))
  (let ((start (%frame-start-time w)))
    (call-next-method)
    (update-fps w start (now))))

(defmethod basecode-draw :around ((w fps-graph))
  (let ((start (%frame-start-time w)))
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
          (gl:line-stipple 2 #b0011001100110011)
          (gl:enable :line-stipple)
          (gl:with-primitives :lines
            (gl:color 0.5 0 0 1)
            (gl:vertex 0 50 0)
            (gl:vertex 120 50 0)
            (gl:color 0.3 0 0 1)
            (gl:vertex 0 25 0)
            (gl:vertex 120 25 0)
            (gl:vertex 0 75 0)
            (gl:vertex 120 75 0)
            (gl:color 0 0.5 0 1)
            (gl:vertex 0 40 0)
            (gl:vertex 120 40 0))
          (gl:disable :line-stipple)
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
              do (gl:vertex x (- 100 y) 0))))))))
