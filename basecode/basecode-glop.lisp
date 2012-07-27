(in-package #:basecode)

;;; need separate window class for glop since it wants to create the
;;; instance itself...
(defclass %basecode-glop-window (glop:window)
  ((%basecode-window :accessor %basecode-window)))

(defclass basecode-glop (basecode)
  ((%glop-window :accessor %glop-window)
   (%exit-main-loop :initform nil)
   ;; not sure if these shoujld have actual slots mirroring state of
   ;; window or just be methods that read values off of the window
   ;; object?
   ;; -- separate slots lets us read/set them when window doesn't exist,
   ;;    so leaving them in for now
   (title :reader title :initarg :title)
   (width :reader width :initarg :width)
   (height :reader height :initarg :height)
   (mode :initarg :mode)
   )
  (:default-initargs :width 640 :height 480 :title "..."
   ;;not sure how to pass extra params to window creation yet...
                     :mode '()))



;;; declaring optimize debug to increase odds of having restartable frames
;;; if we hit debugger
;;; and adding a continue restart in case they still aren't
#++
(defmethod glop:display-window :before ((w basecode-glop))
  (declare (optimize debug))
  (with-continue-restart (basecode-init w)))


#++
(defmethod glop:tick ((w basecode-glop))
  (declare (optimize debug))
  (with-continue-restart (basecode-tick w))
  (glop:post-redisplay))


(defmethod glop:on-event ((w %basecode-glop-window) (event glop:resize-event))
  (declare (optimize debug))
  (let ((width (glop:width event))
        (height (glop:height event))
        (bw (%basecode-window w)))
    (gl:viewport 0 0 width height)
    (setf (slot-value bw '%width) width
          (slot-value bw '%height) height
          (slot-value bw '%aspect) (float (/ width height) 1.0))
    (with-continue-restart (basecode-reshape bw))))

(defmethod glop:on-event ((w %basecode-glop-window)
                          (event glop:key-press-event))
  (declare (optimize debug))
  (let ((key (glop:keysym event)))
    (unless (shiftf (gethash key (key-state (%basecode-window w))) t)
      (key-down (%basecode-window w) key))))

(defmethod glop:on-event ((w %basecode-glop-window)
                          (event glop:key-release-event))
  (declare (optimize debug))
  (let ((key (glop:keysym event)))
    (setf (gethash key (key-state (%basecode-window w))) nil)
    (key-up (%basecode-window w) key)))


(defmethod glop:on-event ((w %basecode-glop-window)
                          (event glop:mouse-motion-event))
  (declare (optimize debug))
  (setf (slot-value (%basecode-window w) '%mouse-position)
        (list (glop:x event) (glop:y event))))

(defmethod glop:on-event ((w %basecode-glop-window)
                          (event glop:button-press-event))
  (declare (optimize debug))
  #++(setf (slot-value (%basecode-window w) '%mouse-position)
        (list (glop:x event) (glop:y event)))
  (setf (gethash (glop:button event) (mouse-buttons (%basecode-window w)))
        (glop:pressed event))
  (apply #'mouse-down (%basecode-window w) (glop:button event)
         (slot-value (%basecode-window w) '%mouse-position)))

(defmethod glop:on-event ((w %basecode-glop-window)
                          (event glop:button-release-event))
  (declare (optimize debug))
  #++(setf (slot-value (%basecode-window w) '%mouse-position)
        (list (glop:x event) (glop:y event)))
  (setf (gethash (glop:button event) (mouse-buttons (%basecode-window w)))
        (glop:pressed event))
  (apply #'mouse-up (%basecode-window w) (glop:button event)
         (slot-value (%basecode-window w) '%mouse-position))
)



#++(defmethod glop:close ((w basecode-glop))
  (declare (optimize debug))
  (with-continue-restart (basecode-cleanup w)))


(defmethod basecode-run ((bw basecode-glop))
  (declare (optimize debug))
  (with-continue-restart
    (basecode-init bw))
  (%gl::with-context ()
    (glop:with-window (gw (title bw) (width bw) (height bw)
                         :win-class '%basecode-glop-window)
     (setf (%basecode-window gw) bw)
     (setf (%glop-window bw) gw)
     (setf (slot-value bw '%exit-main-loop) nil)
     (run-main-loop bw)
     ;; should this be in an unwind-protect cleanup?
     (basecode-cleanup bw))))

(defmethod exit-main-loop ((w basecode-glop))
  (setf (slot-value w '%exit-main-loop) t))

(defmethod run-main-loop ((w basecode-glop))
  (declare (optimize debug))
  (loop
    until (slot-value w '%exit-main-loop)
    while (glop:dispatch-events (%glop-window w) :blocking nil :on-foo nil)
    do
       (with-continue-restart
         (basecode-draw w))
       (glop:swap-buffers (%glop-window w))))
