(defpackage #:3bgl-smoothlife
  (:use :cl :basecode))
(in-package #:3bgl-smoothlife)

(defclass smoothlife (basecode-glop perspective-projection basecode-clear
                        fps-graph basecode-draw-ground-plane
                        freelook-camera
                        basecode-exit-on-esc
                        basecode-shader-helper::basecode-shader-helper)
  ((programs :accessor programs :initform nil)
   ;; textures
   ;; need:
   ;;   fft of data = 2 component f16?
   ;;      -- probably need 32bits for 3d, could easily go beyond
   ;;         range of fp16 at 256^3
   ;;   temp space = same
   ;;   convolution kernels = 2 x 2 component f16?
   ;;      -- scaled to sum to 1, so might be able to use normalized
   ;;         s16 instead?
   ;;   real data = 2 x 1 component f16?
   ;;     possibly can reuse one of the fft buffers?
   (texture1 :accessor texture1 :initform nil)
   (texture2 :accessor texture2 :initform nil)
   (texture3 :accessor texture3 :initform nil)
   ;; fft of convolution kernels
   (kernel1 :accessor kernel1 :initform nil)
   (kernel2 :accessor kernel2 :initform nil)
   ;; twiddle factors for FFTs (todo: move to constants in shader?)
   (twiddle :accessor twiddle :initform nil)
   ;; counter to see if no pixels are live
   (counter :accessor counter :initform nil))
  (:default-initargs :look-at-eye '(3 2 15)))

;; process:
;;   initialize kernels (kernel1, kernel2)
;;   initialize real data  (in texture1.x?)
;;   fft real data
;;      fft pass 1 (texture1 -> texture2)
;;      fft pass 2 (texture2 -> texture1)
;;      fft pass 3 (texture1 -> texture2)
;;   convolve
;;      inner (texture2 + kernel1 -> texture1)
;;      outer (texture2 + kernel2 -> texture3)
;;   ifft
;;      inner pass 1 (texture1 -> texture2)
;;      inner pass 2 (texture2 -> texture1)
;;      inner pass 3 (texture1 -> texture2)
;;      outer pass 1 (texture3 -> texture1)
;;      outer pass 2 (texture1 -> texture3)
;;      outer pass 3 (texture3 -> texture1 or texture2.y?)
;;   birth/death
;;      texture2 + texture3 -> texture1?
;;  - possibly want an extra copy, to allow interpolation or continuous
;;    time steps?

(defparameter *wx* 256)
(defparameter *wy* 256) ;; wy/wz should be same for now
(defparameter *wz* *wy*)

(defparameter *w* nil)

(defmethod run-main-loop :before ((w smoothlife))
  (setf (programs w)
        (list :fft-x (3bgl-shaders::shader-program
                      :compute 'smoothlife-shaders::fft-x)
              :fft-y (3bgl-shaders::shader-program
                      :compute 'smoothlife-shaders::fft-y)
              :ifft-x (3bgl-shaders::shader-program
                       :compute 'smoothlife-shaders::ifft-x)
              :ifft-y (3bgl-shaders::shader-program
                       :compute 'smoothlife-shaders::ifft-y)
              #+:fft-z (3bgl-shaders::shader-program
                        :compute 'smoothlife-shaders::fft-z)
              :convolve (3bgl-shaders::shader-program
                         :compute 'smoothlife-shaders::convolve)
              :rules (3bgl-shaders::shader-program
                         :compute 'smoothlife-shaders::rules)
              :count (3bgl-shaders::shader-program
                      :compute 'smoothlife-shaders::count-pixels))))

(defun gen-texture (&key (w *wx*) (h *wy*) #++(d *wz*)
                      data)
  (let ((tex (car (gl:gen-textures 1 ))))
    (gl:bind-texture :texture-2d tex)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-image-2d :texture-2d 0 :rg16f w h 0 :rg :float data)
    tex))


(defun fft-pass (p source dest x y z)
  ;; assumes twiddle is bound to imageunit 2
  (%gl:bind-image-texture 0 source 0 nil 0 :read-only :rg16f)
  (%gl:bind-image-texture 1 dest 0 nil 0 :write-only :rg16f)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::tex)
        0)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::out1)
        1)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::twiddle)
        2)
  (3bgl-shaders::use-program p)
  (%gl:memory-barrier #.(cffi:foreign-enum-value '%GL:ENUM :texture-fetch-barrier-bit))
  (%gl:dispatch-compute x y z))

(defun convolve-pass (p kernel source dest x y z)
  (%gl:bind-image-texture 0 source 0 nil 0 :read-only :rg16f)
  (%gl:bind-image-texture 1 dest 0 nil 0 :write-only :rg16f)
  (%gl:bind-image-texture 3 kernel 0 nil 0 :read-only :rg16f)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::tex)
        0)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::out1)
        1)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::kernel)
        3)
  (3bgl-shaders::use-program p)
  (%gl:memory-barrier #.(cffi:foreign-enum-value '%GL:ENUM :texture-fetch-barrier-bit))
  (%gl:dispatch-compute x y z))

(defun rules-pass (p source1 source2 dest x y z)
  (%gl:bind-image-texture 0 source1 0 nil 0 :read-only :rg16f)
  (%gl:bind-image-texture 2 source2 0 nil 0 :read-only :rg16f)
  (%gl:bind-image-texture 1 dest 0 nil 0 :write-only :rg16f)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::rule-in1)
        0)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::out1)
        1)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::rule-in2)
        2)
  (3bgl-shaders::use-program p)
  (%gl:memory-barrier #.(cffi:foreign-enum-value '%GL:ENUM :texture-fetch-barrier-bit))
  (%gl:dispatch-compute x y z))

(defun count-pass (p counter source1 x y z)
  (%gl:bind-image-texture 0 source1 0 nil 0 :read-only :rg16f)
  (%gl:bind-buffer-base :atomic-counter-buffer 0 counter)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::tex)
        0)
  (3bgl-shaders::use-program p)
  (%gl:memory-barrier #.(cffi:foreign-enum-value '%GL:ENUM :texture-fetch-barrier-bit))
  (%gl:dispatch-compute x y z))

(defparameter *step* nil)
(defmethod basecode-draw ((w smoothlife))
  (setf *w* w)
;  (sleep 0.1)
  (when (programs w)
    (unless (twiddle w)
      (setf (twiddle w) (gen-texture :w *wy* :h 2))
      (let ((d (make-array (* *wy* 2 2) :element-type 'single-float
                                        :initial-element 0.0)))

        (loop for i below *wy*
              for j = (floor i 16)
              for k = (mod i 16)
              for x = (* i 2)
              for t1 = (exp (complex 0 (* -2/256 pi j k)))
              do (setf (aref d x) (float (realpart t1) 1.0)
                       (aref d (1+ x)) (float (imagpart t1) 1.0)))
        (gl:bind-texture :texture-2d (twiddle w))
        (gl:tex-image-2d :texture-2d 0 :rg16f 256 2 0 :rg :float d)))
    (%gl:bind-image-texture 2 (twiddle w) 0 nil 0 :read-only :rg16f)
    (unless (texture1 w)
      (setf (texture1 w) (gen-texture)))
    (unless (texture2 w)
      (setf (texture2 w) (gen-texture)))
    (unless (texture3 w)
      (setf (texture3 w) (gen-texture)))
    (unless (counter w)
      (setf (counter w) (car (gl:gen-buffers 1)))
      (gl:bind-buffer :atomic-counter-buffer (counter w))
      (%gl:buffer-data :atomic-counter-buffer 4 (cffi:null-pointer)
                       :dynamic-draw )
      (cffi:with-foreign-object (x :uint32)
        (setf (cffi:mem-aref x :uint32) 1)
        (%gl:buffer-sub-data :atomic-counter-buffer 0 4 x))
      (gl:bind-buffer :atomic-counter-buffer 0))
    (flet ((init-world (buf)
             ;; initialize texture1 to random values
             (loop with r = (coerce (loop for i below *wy* collect (random 2))
                                    'vector)
                   for i below *wx*
                   when (zerop (mod i 7))
                     do (setf r (coerce (loop for j below *wy*
                                              collect (if (< (random 1.0) 0.25)
                                                          1 0))
                                        'vector))
                   do (loop for j below *wy*
                            do (setf (aref buf (* 2 (+ i (* j *wx*))))
                                     (float (aref r (floor j 7)) 1.0))))
             (gl:bind-texture :texture-2d (texture1 w))
             (gl:tex-image-2d :texture-2d 0 :rg16f *wx* *wy* 0 :rg :float buf)
             (gl:bind-texture :texture-2d 0)))
      (unless (kernel1 w)
        (setf (kernel1 w) (gen-texture))
        (unless (kernel2 w)
          (setf (kernel2 w) (gen-texture)))
        (assert (and (texture1 w) (texture2 w) (kernel1 w) (kernel2 w)))
        (let* ((ra (/ 21.0 2))
               (ri (/ ra 3.0))
               (aa 1.0)
               (area1 (coerce (* pi (expt ri 2)) 'single-float))
               (area2 (coerce (- (* pi (expt ra 2)) area1) 'single-float))
               (buf (make-array (* *wx* *wy* 2) :element-type 'single-float
                                                :initial-element 0.0)))
          (flet ((fft-x (w) (getf (programs w) :fft-x))
                 (fft-y (w) (getf (programs w) :fft-y))
                 (filltex (rx area &optional rin)
                   (print
                    (loop for i below *wx*
                          sum (loop for j below *wy*
                                    for x = (if (< i (/ *wx* 2) )
                                                i
                                                (- *wx* i))
                                    for y = (if (< j (/ *wy* 2) )
                                                j
                                                (- *wy* j))
                                    for r = (sqrt (+ (* x x) (* y y)))
                                    for v1 = (cond
                                               ((and rin
                                                     (< r (- rin (/ aa 2))))
                                                0)
                                               ((and rin
                                                     (< r (+ rin (/ aa 2))))
                                                (/ (/ (- r (- rin (/ aa 2))) aa)
                                                   area))
                                               ((< r (- rx (/ aa 2)))
                                                (/ 1 area))
                                               ((> r (+ rx (/ aa 2)))
                                                0)
                                               (t
                                                (/ (/ (- (+ rx (/ aa 2)) r) aa)
                                                   area)))
                                    do (setf (aref buf (* 2 (+ i (* j *wx*))))
                                             (coerce v1 'single-float))
                                    sum (* area v1) )))))
            ;;(filltex ri area1)
            (print ri)
            ;;(filltex ri 154.55185) ;; fixme: calculate areas from radii
            (filltex ri 38.85737)
            (gl:bind-texture :texture-2d (texture1 w))
            (gl:tex-image-2d :texture-2d 0 :rg16f *wx* *wy* 0 :rg :float buf)
            (gl:bind-texture :texture-2d 0)
            (fft-pass (fft-x w) (texture1 w) (texture2 w) 256/16 1 1)
            (fft-pass (fft-y w) (texture2 w) (kernel1 w) 256/16 1 1)
            ;;(filltex ra area2 ri)
            (print ra)
            ;;(filltex ra 1231.231 #++ area2 ri)
            (filltex ra 308.53482 ri)
            (gl:bind-texture :texture-2d (texture1 w))
            (gl:tex-image-2d :texture-2d 0 :rg16f *wx* *wy* 0 :rg :float buf)
            (gl:bind-texture :texture-2d 0)
            (fft-pass (fft-x w) (texture1 w) (texture2 w) 256/16 1 1)
            (fft-pass (fft-y w) (texture2 w) (kernel2 w) 256/16 1 1)
            (init-world buf))))

      (when (eql *step* :auto)
        (let ((c 1))
          (gl:bind-buffer :atomic-counter-buffer (counter w))
          (cffi:with-foreign-object (x :uint32)
            (%gl:get-buffer-sub-data :atomic-counter-buffer 0 4 x)
            (setf c (cffi:mem-aref x :uint32))
            (setf (cffi:mem-aref x :uint32) 0)
            (%gl:buffer-sub-data :atomic-counter-buffer 0 4 x))
          (gl:bind-buffer :atomic-counter-buffer 0)
          (when (zerop c)
            (let ((buf (make-array (* *wx* *wy* 2) :element-type 'single-float
                                                   :initial-element 0.0))
                  #++(*random-state* (make-random-state nil)))
              (init-world buf))))))

    (when *step*
      (%gl:bind-image-texture 2 (twiddle w) 0 nil 0 :read-only :rg16f)
      (when (numberp *step*)
        (decf *step*))
      (when (eql *step* 0)
        (setf *step* nil))

      (gl:bind-texture :texture-2d 0)
      ;; fft
      (fft-pass (getf (programs w) :fft-x)
                (texture1 w) (texture3 w) 256/16 1 1)
      (gl:bind-texture :texture-2d 0)
      (fft-pass (getf (programs w) :fft-y)
                (texture3 w) (texture1 w) 256/16 1 1)

      ;; convolve
      (convolve-pass (getf (programs w) :convolve)
                     (kernel1 w) (texture1 w) (texture2 w)
                     (/ *wx* 32) (/ *wy* 32) 1)
      (convolve-pass (getf (programs w) :convolve)
                     (kernel2 w) (texture1 w) (texture3 w)
                     (/ *wx* 32) (/ *wy* 32) 1)

      ;; ifft x2
      (gl:bind-texture :texture-2d 0)
      (fft-pass (getf (programs w) :ifft-x)
                (texture3 w) (texture1 w) 256/16 1 1)
      (gl:bind-texture :texture-2d 0)
      (fft-pass (getf (programs w) :ifft-y)
                (texture1 w) (texture3 w) 256/16 1 1)

      (gl:bind-texture :texture-2d 0)
      (fft-pass (getf (programs w) :ifft-x)
                (texture2 w) (texture1 w) 256/16 1 1)
      (gl:bind-texture :texture-2d 0)
      (fft-pass (getf (programs w) :ifft-y)
                (texture1 w) (texture2 w) 256/16 1 1)


      ;; rules
      (gl:bind-texture :texture-2d 0)
      (rules-pass (getf (programs w) :rules)
                  (texture2 w) (texture3 w) (texture1 w)
                  256/32 256/32 1))




    (when (eql *step* :auto)
      (count-pass (getf (programs w) :count)
                  (counter w) (texture1 w)
                  256/32 256/32 1))
    (gl:bind-texture :texture-2d (texture1 w))
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:use-program 0)
    (gl:with-pushed-matrix* (:modelview)
      (gl:enable :depth-test :texture-2d)
      (gl:color 1 1 1 1)
      (gl:scale 2 2 2)
      (gl:rotate 90 0 0 1)
      (gl:translate 1 0 0)
      (gl:with-primitives :quads
        ;;(gl:tex-coord 1.5 1.5)
        (gl:tex-coord 1 1)
        (gl:vertex -1 0.1 -1)

        ;;(gl:tex-coord 0.5 1.5)
        (gl:tex-coord 0 1)
        (gl:vertex -1 0.1 1)

        ;;(gl:tex-coord 0.5 0.5)
        (gl:tex-coord 0 0)
        (gl:vertex 1 0.1 1)

        ;;(gl:tex-coord 1.5 0.5)
        (gl:tex-coord 1 0)
        (gl:vertex 1 0.1 -1)))))

(defmethod key-down :after ((w smoothlife) k)
  (case k
    ((#\m :m)
     (macrolet ((d (x)
                  `(when (,x w)
                     (gl:delete-textures (list (,x w)))
                     (setf (,x w) nil))))
       (d kernel1)
       (d kernel2)
       (d texture1)
       (d texture2)
       (d texture3)
       (d twiddle)))
    ((#\space :space)
     (setf *step* 1))
    ((:r)
     (setf *step* :foo))
    ((:p)
     (print (setf *step* :auto)))))


(setf 3bgl-shaders::*print-shaders* t)
; (basecode-run (make-instance 'smoothlife))
