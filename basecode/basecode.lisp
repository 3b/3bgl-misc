(in-package #:basecode)

;;; set of classes for building a simple cl-glut (or eventually other
;;; libs) demo out of predefined components

;;; demo subclasses BASECODE-GLUT and adds mixins like PERSPECTIVE or ORTHO
;;; to configure display, FREELOOK-CAM to add movement controls, etc
;;; then adds methods on INIT if needed, and DRAW to draw a frame

(defclass basecode ()
  ((%width :reader width)
   (%height :reader height)
   (%aspect :reader aspect)
   ;; todo: handle multiple devices (mice,keyboards,etc)?
   (%key-states :reader key-state :initform (make-hash-table))
   (%mouse-buttons :reader mouse-buttons :initform (make-hash-table))
   (%mouse-position :reader mouse-position :initform (list 0 0))))

(defgeneric basecode-init (w)
  (:method (w)))
(defgeneric basecode-cleanup (w)
  (:method (w)))
(defgeneric basecode-tick (w)
  (:method (w)))
;;; possibly should add (keyword?) arg for time since last draw call?
(defgeneric basecode-draw (w)
  (:method (w)))
(defgeneric basecode-key-state (w key)
  (:method (w key)
    (gethash key (key-state w) nil)))
(defgeneric basecode-button-state (w button)
  (:method (w button)
    (gethash button (mouse-buttons w) nil)))

;;; defining some extra methods, so the mixins don't need to care
;;; which library made the window if they don't have to (projection
;;; mixins for example)
(defgeneric basecode-reshape (w)
  ;; called after window is resized, get new w/h from w
  (:method (w)))


;;;; not sure if this should inherit from BASECODE, might be better as
;;;; a mixin, so we can have an app derived from basecode, then add in
;;;; specific display libs without modifying the original app code?
(defclass basecode-glut (basecode glut:window)
  ()
  (:default-initargs :width 640 :height 480 :title "..."
                     :mode '(:double :rgba :depth :multisample)
                     :tick-interval 10))


(defmacro with-continue-restart (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue"  )))


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
  (case key
    (#\Esc (glut:leave-main-loop))))

(defmethod glut:keyboard-up ((w basecode-glut) key x y)
  (declare (ignore x y)
           (optimize debug))
  (setf (gethash key (key-state w)) nil))

(defmethod glut:special ((w basecode-glut) key x y)
  (declare (ignore x y)
           (optimize debug))
  (setf (gethash key (key-state w)) t))

(defmethod glut:special-up ((w basecode-glut) key x y)
  (declare (ignore x y)
           (optimize debug))
  (setf (gethash key (key-state w)) nil))

(defmethod glut:mouse ((w basecode-glut) button state x y)
  (declare (optimize debug))
  (setf (slot-value w '%mouse-position)
        (list x y))
  (setf (gethash button (mouse-buttons w)) state))

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
  (glut:display-window w))