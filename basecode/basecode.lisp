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

;;; these don't do any translation for now, so generic code will
;;; have to check multiple possible names for a given key
;;; (ex: #\x vs :x or :key-up vs :up)
;; fixme: should these be named basecode-* ?
(defgeneric key-down (w key)
  (:method (w key)))
(defgeneric key-up (w key)
  (:method (w key)))

;;; buttons are also not translated, possibly should though?
;;; glop: 1, 2, 3, 4, 5, 6..?
;;; glut: :left-button, :middle-button, :right-button, :wheel-up, :wheel-down, :button[4..?]
(defgeneric mouse-down (w button x y)
  (:method (w button x y)))
(defgeneric mouse-up (w button x y)
  (:method (w button x y)))
(defgeneric mouse-move (w x y)
  (:method (w x y)))



;;; hook for things that want to wrap the main loop in a dynamic scope
;;; (let bindings, unwind-protect etc)
;;; which can define an :around method on this
(defgeneric run-main-loop (w))

(defgeneric exit-main-loop (w))

(defmacro with-continue-restart (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue"  )))