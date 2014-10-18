(defpackage #:compute-test
  (:use :cl :basecode))
(in-package #:compute-test)

(defclass compute-test (basecode-glop perspective-projection basecode-clear
                        fps-graph basecode-draw-ground-plane
                        freelook-camera
                        basecode-exit-on-esc
                        basecode-shader-helper::basecode-shader-helper)
  ((program :accessor program :initform nil)
   (texture :accessor texture :initform nil)
   (texture2 :accessor texture2 :initform nil)
   (twiddle :accessor twiddle :initform nil)
   (odd :accessor odd :initform nil))
  (:default-initargs :look-at-eye '(3 2 15)))

(defparameter *w* nil)

(defmethod run-main-loop :before ((w compute-test))
  (setf (program w) (3bgl-shaders::shader-program
                     :compute 'compute-test-shaders::compute)))

(defun gen-texture (&key (w 512) (h 512)
                      data)
  (let ((tex (car (gl:gen-textures 1 ))))
    (gl:bind-texture :texture-2d tex)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-image-2d :texture-2d 0 :rg32f w h 0 :rg :float data)
    ;;(%gl:bind-image-texture 0 tex 0 nil 0 :write-only :rg32f)
    tex))

(defmethod basecode-draw ((w compute-test))
  (setf *w* w)
  (sleep 0.01)
  (when (program w)
    #++
    (when (texture w)
      (gl:delete-textures (list (texture w)))
      (setf (texture w) nil))
    (unless (texture w)
      (setf (texture w) (gen-texture)))
    (unless (texture2 w)
      (setf (texture2 w) (gen-texture)))
    (unless (twiddle w)
      (setf (twiddle w) (gen-texture :w 512 :h 2)))
    (let ((d (make-array (* 512 2 2) :element-type 'single-float
                                     :initial-element 0.0)))

      (loop for i below 512
            ;;for i2 = (floor i 8)
            for j = (floor i 64)
            for k = (mod i 64)
            for l = (mod k 8)
            for m = (floor k 8)
            for x = (* i 2)             ; (+ k (* 64 j))
            for x2 = (+ 1024 (* (+ j (* 8 (+ l (* m 8)))) 2))
            for t1 = (exp (complex 0 (* -2/512 pi j k)))
            for t2 = (exp (complex 0 (* -2/64 pi l m)))
            do (setf (aref d x) (float (realpart t1) 1.0)
                     (aref d (1+ x)) (float (imagpart t1) 1.0)
                     (aref d x2) (float (realpart t2) 1.0)
                     (aref d (1+ x2)) (float (imagpart t2) 1.0)))
      (gl:bind-texture :texture-2d (twiddle w))
      (gl:tex-image-2d :texture-2d 0 :rg32f 512 2 0 :rg :float d))

    (let ((p (program w)))
      (setf (3bgl-shaders::uniform p 'compute-test-shaders::angle)
            (float (mod (/ (get-internal-real-time)
                           internal-time-units-per-second)
                        (* pi 2))
                   1.0))
      (let ((t1 (texture w))
            (t2 (texture2 w)))
        (when (odd w)
          #++(rotatef t1 t2))
        (setf (odd w) (not (odd w)))
        (%gl:bind-image-texture 0 t1 0 nil 0 :read-only :rg32f)
        (%gl:bind-image-texture 1 t2 0 nil 0 :write-only :rg32f)
        (gl:bind-texture :texture-2d t2))
      (setf (3bgl-shaders::uniform p 'compute-test-shaders::tex)
            0)
      (setf (3bgl-shaders::uniform p 'compute-test-shaders::out1)
            1)
      (%gl:bind-image-texture 2 (twiddle w) 0 nil 0 :read-only :rg32f)
      (setf (3bgl-shaders::uniform p 'compute-test-shaders::twiddle)
            2)
      (3bgl-shaders::use-program (program w))
      (%gl:memory-barrier #.(cffi:foreign-enum-value '%GL:ENUM :texture-fetch-barrier-bit))
      (%gl:dispatch-compute 512/8 1 1)
      (gl:use-program 0))
    #++(gl:bind-texture :texture-2d (texture2 w))
    (%gl:memory-barrier #.(cffi:foreign-enum-value '%GL:ENUM :texture-fetch-barrier-bit))
    (gl:bind-texture :texture-2d (texture2 w))
    (gl:with-pushed-matrix* (:modelview)
      (gl:enable :depth-test :texture-2d)
      (gl:color 1 1 1 1)
      (gl:scale 2 2 2)
      (gl:rotate 90 0 0 1)
      (gl:translate 1 0 0)
      (gl:with-primitives :quads
        (gl:tex-coord 1 1)
        (gl:vertex -1 0.1 -1)

        (gl:tex-coord 0 1)
        (gl:vertex -1 0.1 1)

        (gl:tex-coord 0 0)
        (gl:vertex 1 0.1 1)

        (gl:tex-coord 1 0)
        (gl:vertex 1 0.1 -1)))))

(defmethod key-down :after ((w compute-test) k)
  (case k
    ((#\m :m)
     )))


(setf 3bgl-shaders::*print-shaders* t)
; (basecode-run (make-instance 'compute-test))
