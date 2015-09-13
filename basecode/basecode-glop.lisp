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
   (x-pos :reader x-pos :initarg :x)
   (y-pos :reader y-pos :initarg :y)
   (fullscreen :reader fullscreen :initarg :fullscreen :initform nil)
   (mode :initarg :mode)
   (swap-buffers :initform t :initarg :swap-buffers
                 :accessor swap-buffers)
   )
  (:default-initargs :width 640 :height 480 :title "..."
                     :x 0 :y 0
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


(defmethod basecode-repaint ((w basecode-glop))
  (gl:clear :color-buffer-bit)
  (glop:swap-buffers (%glop-window w)))

(defmethod glop:on-event ((w %basecode-glop-window) (event glop:resize-event))
  (declare (optimize debug))
  (let ((width (glop:width event))
        (height (glop:height event))
        (bw (%basecode-window w)))
    (when (or (/= (width bw) width) (/= height (height bw)))
      (setf (%resized bw) t))
    (gl:viewport 0 0 width height)
    (setf (slot-value bw '%width) width
          (slot-value bw 'width) width
          (slot-value bw '%height) height
          (slot-value bw 'height) height
          (slot-value bw '%aspect) (float (/ width height) 1.0))
    (with-continue-restart (basecode-reshape bw))))


(defmethod glop:on-event ((w %basecode-glop-window)
                          (event glop:key-press-event))
  (declare (optimize debug))
  (let ((key (glop:keysym event))
        (*%key-extra* event))
    (unless (shiftf (gethash key (key-state (%basecode-window w))) t)
      (with-continue-restart (key-down (%basecode-window w) key)))))

(defmethod glop:on-event ((w %basecode-glop-window)
                          (event glop:key-release-event))
  (declare (optimize debug))
  (let ((key (glop:keysym event))
        (*%key-extra* event))
    (setf (gethash key (key-state (%basecode-window w))) nil)
    (with-continue-restart (key-up (%basecode-window w) key))))


(defmethod glop:on-event ((w %basecode-glop-window)
                          (event glop:mouse-motion-event))
  (declare (optimize debug))
  (setf (slot-value (%basecode-window w) '%mouse-position)
        (list (glop:x event) (glop:y event)))
  (with-continue-restart
    (apply #'mouse-move (%basecode-window w)
           (slot-value (%basecode-window w) '%mouse-position))))

(defmethod glop:on-event ((w %basecode-glop-window)
                          (event glop:button-press-event))
  (declare (optimize debug))
  #++(setf (slot-value (%basecode-window w) '%mouse-position)
        (list (glop:x event) (glop:y event)))
  (setf (gethash (glop:button event) (mouse-buttons (%basecode-window w)))
        (glop:pressed event))
  (with-continue-restart
    (apply #'mouse-down (%basecode-window w) (glop:button event)
          (slot-value (%basecode-window w) '%mouse-position))))

(defmethod glop:on-event ((w %basecode-glop-window)
                          (event glop:button-release-event))
  (declare (optimize debug))
  #++(setf (slot-value (%basecode-window w) '%mouse-position)
        (list (glop:x event) (glop:y event)))
  (setf (gethash (glop:button event) (mouse-buttons (%basecode-window w)))
        (glop:pressed event))
  (with-continue-restart
    (apply #'mouse-up (%basecode-window w) (glop:button event)
          (slot-value (%basecode-window w) '%mouse-position)))
)

(defmethod glop:on-event ((w %basecode-glop-window)
                          (event glop:expose-event))
  ;; ignore for now, assuming we will usually be drawing on idle anyway
  (declare (optimize debug) (ignore w event)))

(defmethod glop:on-event ((w %basecode-glop-window)
                          (event glop:visibility-obscured-event))
  ;; fixme: possibly should pass this on so apps can pause when hidden?
  (declare (optimize debug) (ignore w event)))

(defmethod glop:on-event ((w %basecode-glop-window)
                          (event glop:visibility-unobscured-event))
  ;; fixme: possibly should pass this on so apps can pause when hidden?
  (declare (optimize debug) (ignore w event)))

(defmethod glop:on-event ((w %basecode-glop-window)
                          (event glop:focus-out-event))
  ;; probably should have a basecode event so things like mouselook
  ;; can update state? (or else they should look at key-state instead
  ;; of watching press/release events?
  (declare (optimize debug) (ignore w event))
  (clrhash (key-state (%basecode-window w))))

(defmethod glop:on-event ((w %basecode-glop-window)
                          (event glop:focus-in-event))
  (declare (optimize debug) (ignore w event)))

(defgeneric basecode-child-mapped (w child)
  (:method (w c)))

(defgeneric basecode-child-unmapped (w child)
  (:method (w c)))

(defgeneric basecode-child-resized (w child width height)
  (:method (w c width height)))

(defgeneric basecode-child-reparent (w child parent x y)
  (:method (w c p x y)))

#-win32
(defmethod glop:on-event ((w %basecode-glop-window)
                          (event glop::child-visibility-unobscured-event))
  (declare (optimize debug))
  (with-continue-restart
    (basecode-child-mapped (%basecode-window w) (glop::child event))))

#-win32
(defmethod glop:on-event ((w %basecode-glop-window)
                          (event glop::child-visibility-obscured-event))
  (declare (optimize debug))
  (with-continue-restart
    (basecode-child-unmapped (%basecode-window w) (glop::child event))))

#-win32
(defmethod glop:on-event ((w %basecode-glop-window)
                          (event glop::child-resize-event))
  (declare (optimize debug))
  (with-continue-restart
    (basecode-child-resized (%basecode-window w) (glop::child event)
                             (glop::width event) (glop::height event))))
#-win32
(defmethod glop:on-event ((w %basecode-glop-window)
                          (event glop::child-reparent-event))
  (declare (optimize debug))
  (with-continue-restart
    (basecode-child-reparent (%basecode-window w)
                             (glop::child event) (glop::parent event)
                             (glop::x event) (glop::y event))))


#++(defmethod glop:close ((w basecode-glop))
  (declare (optimize debug))
  (with-continue-restart (basecode-cleanup w)))


(defmethod basecode-run ((bw basecode-glop))
  (declare (optimize debug))
  (with-continue-restart
    (basecode-init bw))
  (let ((%gl::*in-begin* nil))
    (glop:with-window (gw (title bw) (width bw) (height bw)
                          :x (x-pos bw) :y (y-pos bw)
                          :win-class '%basecode-glop-window
                          :depth-size 16
                          :fullscreen (fullscreen bw)
                          )
     (setf (%basecode-window gw) bw)
     (setf (%glop-window bw) gw)
     (setf (slot-value bw '%exit-main-loop) nil)
     (run-main-loop bw)
     ;; should this be in an unwind-protect cleanup?
     (basecode-cleanup bw))))

(defmethod exit-main-loop ((w basecode-glop))
  (setf (slot-value w '%exit-main-loop) t))

(defmethod run-main-loop ((w basecode-glop))
  (declare (optimize debug)
           (notinline glop:swap-buffers))
  (loop
    until (slot-value w '%exit-main-loop)
    while (glop:dispatch-events (%glop-window w) :blocking nil :on-foo nil)
    do
       (with-continue-restart
         (basecode-draw w))
       (when (swap-buffers w)
         (glop:swap-buffers (%glop-window w)))))

(defmethod run-nested-loop ((w basecode-glop) lock-and-var)
  (declare (optimize debug))
  (loop
    until (and (bt:acquire-lock (car lock-and-var))
               (prog1 (cdr lock-and-var)
                 (bt:release-lock (car lock-and-var))))
    do (format t "dispatch events~%")
    while (glop:dispatch-events (%glop-window w) :blocking nil :on-foo nil)
    do
       (with-continue-restart
         (format t "basecode draw~%")
         (basecode-draw w))
       (format t "swap buffers~%")
       (glop:swap-buffers (%glop-window w)))
  (format t "exit nested loop~%"))
