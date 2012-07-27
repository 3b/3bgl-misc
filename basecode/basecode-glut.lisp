(in-package #:basecode)


;;;; not sure if this should inherit from BASECODE, might be better as
;;;; a mixin, so we can have an app derived from basecode, then add in
;;;; specific display libs without modifying the original app code?
(defclass basecode-glut (basecode glut:window)
  ()
  (:default-initargs :width 640 :height 480 :title "..."
                     :mode '(:double :rgba :depth :multisample)
                     :tick-interval 10))




;;; declaring optimize debug to increase odds of having restartable frames
;;; if we hit debugger
;;; and adding a continue restart in case they still aren't
(defmethod glut:display-window :before ((w basecode-glut))
  (declare (optimize debug))
  (with-continue-restart (basecode-init w)))


(defmethod glut:tick ((w basecode-glut))
  (declare (optimize debug))
  (with-continue-restart (basecode-tick w))
  (glut:post-redisplay))


(defmethod glut:display ((w basecode-glut))
  (declare (optimize debug))
  (with-continue-restart (basecode-draw w))
  (glut:swap-buffers))

(defmethod glut:reshape ((w basecode-glut) width height)
  (declare (optimize debug))
  (gl:viewport 0 0 width height)
  (setf (slot-value w '%width) width
        (slot-value w '%height) height
        (slot-value w '%aspect) (float (/ width height) 1.0))
  (with-continue-restart (basecode-reshape w)))

(defmethod glut:keyboard ((w basecode-glut) key x y)
  (declare (ignore x y)
           (optimize debug))
  (setf (gethash key (key-state w)) t)
  (key-down w key))

(defmethod glut:keyboard-up ((w basecode-glut) key x y)
  (declare (ignore x y)
           (optimize debug))
  (setf (gethash key (key-state w)) nil)
  (key-up w key))

(defmethod glut:special ((w basecode-glut) key x y)
  (declare (ignore x y)
           (optimize debug))
  (setf (gethash key (key-state w)) t)
  (key-down w key))

(defmethod glut:special-up ((w basecode-glut) key x y)
  (declare (ignore x y)
           (optimize debug))
  (setf (gethash key (key-state w)) nil)
  (key-up w key))

(defmethod glut:mouse ((w basecode-glut) button state x y)
  (declare (optimize debug))
  (setf (slot-value w '%mouse-position)
        (list x y))
  (setf (gethash button (mouse-buttons w)) state)
  (if state
      (mouse-down w button x y)
      (mouse-up w button x y)))

(defmethod glut:motion ((w basecode-glut) x y)
  (declare (optimize debug))
  (setf (slot-value w '%mouse-position)
        (list x y)))

(defmethod glut:passive-motion ((w basecode-glut) x y)
  (declare (optimize debug))
  (setf (slot-value w '%mouse-position)
        (list x y)))


(defmethod glut:close ((w basecode-glut))
  (declare (optimize debug))
  (with-continue-restart (basecode-cleanup w)))


(defmethod basecode-run ((w basecode-glut))
  (let ((glut:*run-main-loop-after-display* nil))
    (glut:display-window w)
    (run-main-loop w)))

(defmethod exit-main-loop ((w basecode-glut))
  (glut:leave-main-loop))

(defmethod run-main-loop ((w basecode-glut))
  (glut:main-loop)

)