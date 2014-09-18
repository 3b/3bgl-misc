(defpackage #:basecode-shader-helper
  (:use #:cl #:basecode))
(in-package #:basecode-shader-helper)

;; mixin for handling shader reloading with 3bgl-shader
(defclass basecode-shader-helper ()
  ((modified-shaders :accessor modified-shaders :initform nil)))

;; todo: add a mutex for this
(defparameter *running-loops* nil)
;; bound to a hash table within an running loop
(defvar *live-programs*)


(defun shader-program-hook (p functions)
  (loop for f in functions
        do (pushnew p (gethash f *live-programs*))))

(defmethod run-main-loop :around ((w basecode-shader-helper))
  (let ((*live-programs* (make-hash-table))
        (3bgl-shaders::*shader-program-hook* 'shader-program-hook))
    ;; todo: add mutex for this
    (push w *running-loops*)
    (unwind-protect
         (call-next-method)
      ;; todo: add mutex for this
      (setf *running-loops* (remove w *running-loops*)))))


(defun modified-shader-hook (modified)
  (format t "saw modified functions ~s~%" modified)
  ;; todo: add a mutex for this
  (loop for w in *running-loops*
        do (setf (modified-shaders w)
                 (union (modified-shaders w)
                        modified))))

(pushnew 'modified-shader-hook 3bgl-shaders::*modified-function-hook*)

(defun recompile-modified-shaders (w)
  ;; todo: add a mutex for this
  (loop for f in (shiftf (modified-shaders w) nil)
        do (loop for p in (gethash f *live-programs*)
                 do (3bgl-shaders::flag-shader p f))))


(defmethod basecode-draw :around ((w basecode-shader-helper))
  (recompile-modified-shaders w)
  (call-next-method))

