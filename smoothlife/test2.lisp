(defpackage #:3bgl-smoothlife-test2
  (:use :cl :basecode))
(in-package #:3bgl-smoothlife-test2)

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
   (texture4 :accessor texture4 :initform nil)
   ;; fft of convolution kernels
   (kernel1 :accessor kernel1 :initform nil)
   (kernel2 :accessor kernel2 :initform nil)
   ;; twiddle factors for FFTs (todo: move to constants in shader?)
   (twiddle :accessor twiddle :initform nil)
   ;; tmp buffer for initializing world
   (world-init :accessor world-init :initform nil)
   ;; counter to see if no pixels are live
   (counter :accessor counter :initform nil)
   ;; timer queries for benchmarking
   (timestamps :accessor timestamps :initform nil)
   (timestamp-masks :accessor timestamp-masks :initform nil)
   (times :accessor times :initform nil))
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
(defparameter *fill* 0.5)

(defparameter *w* nil)
(defparameter *frames* 0)
(defparameter *frame-time* (get-internal-real-time))

(defmethod run-main-loop :before ((w smoothlife))
  (setf (programs w)
        (list :fft-x (3bgl-shaders::shader-program
                      :compute 'smoothlife-shaders::fft-x)
              :fft-y (3bgl-shaders::shader-program
                      :compute 'smoothlife-shaders::fft-y)
              :fft-z (3bgl-shaders::shader-program
                      :compute 'smoothlife-shaders::fft-z)
              :ifft-x (3bgl-shaders::shader-program
                       :compute 'smoothlife-shaders::ifft-x)
              :ifft-y (3bgl-shaders::shader-program
                       :compute 'smoothlife-shaders::ifft-y)
              :ifft-z (3bgl-shaders::shader-program
                       :compute 'smoothlife-shaders::ifft-z)
              :convolve (3bgl-shaders::shader-program
                         :compute 'smoothlife-shaders::convolve)
              :rules (3bgl-shaders::shader-program
                         :compute 'smoothlife-shaders::rules)
              :count (3bgl-shaders::shader-program
                      :compute 'smoothlife-shaders::count-pixels)
              :make-kernel (3bgl-shaders::shader-program
                            :compute 'smoothlife-shaders::make-kernel)
              :scale-kernel (3bgl-shaders::shader-program
                             :compute 'smoothlife-shaders::scale-kernel)
              :init-world (3bgl-shaders::shader-program
                            :compute 'smoothlife-shaders::init-world))))

(defun gen-texture2d (&key (w *wx*) (h *wy*) data)
  (let ((tex (car (gl:gen-textures 1 ))))
    (gl:bind-texture :texture-2d tex)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-image-2d :texture-2d 0 :rg32f w h 0 :rg :float data)
    tex))

(defun gen-texture (&key (w *wx*) (h *wy*) (d *wz*)
                      data)
  (let ((tex (car (gl:gen-textures 1 ))))
    (gl:bind-texture :texture-2d 0)
    (gl:bind-texture :texture-3d tex)
    (gl:tex-parameter :texture-3d :texture-min-filter :linear)
    (gl:tex-parameter :texture-3d :texture-mag-filter :linear)
    (gl:tex-image-3d :texture-3d 0 :rg32f w h d 0 :rg :float data)
    (gl:bind-texture :texture-3d 0)
    tex))


;; not sure why these need 'layered' flag set for 3d?
(defun filltex (p dest temp ra area ri aa x y z)
  (%gl:bind-image-texture 0 temp 0 t 0 :write-only :rg32f)
  (%gl:bind-image-texture 1 dest 0 t 0 :write-only :rg32f)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::out1)
        0)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::kernel)
        1)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::area) area)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::ri) ri)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::ra) ra)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::aa) aa)
  (3bgl-shaders::use-program p)
  (%gl:memory-barrier #.(cffi:foreign-enum-value '%GL:ENUM :texture-fetch-barrier-bit))
  (%gl:dispatch-compute x y z))

(defun scale-tex (p source scales dest x y z &key (scale 1))
  (%gl:bind-image-texture 0 scales 0 t 0 :read-only :rg32f)
  (%gl:bind-image-texture 1 source 0 t 0 :read-only :rg32f)
  (%gl:bind-image-texture 2 dest 0 t 0 :write-only :rg32f)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::tex)
        0)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::kernel)
        1)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::out1)
        2)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::scale) scale)
  (3bgl-shaders::use-program p)
  (%gl:memory-barrier #.(cffi:foreign-enum-value '%GL:ENUM :texture-fetch-barrier-bit))
  (%gl:dispatch-compute x y z))

(defun fill-world (p source dest scale x y z)
  (gl:bind-texture :texture-3d source)
  (%gl:bind-image-texture 1 dest 0 t 0 :write-only :rg32f)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::random-tex)
        0)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::out1)
        1)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::scale) scale)
  (3bgl-shaders::use-program p)
  (%gl:memory-barrier #.(cffi:foreign-enum-value '%GL:ENUM :texture-fetch-barrier-bit))
  (%gl:dispatch-compute x y z)
  (%gl:bind-texture :texture-3d 0))

(defun fft-pass (p source dest x y z)
  ;; assumes twiddle is bound to imageunit 2
  (%gl:bind-image-texture 0 source 0 t 0 :read-only :rg32f)
  (%gl:bind-image-texture 1 dest 0 t 0 :write-only :rg32f)
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
  (%gl:bind-image-texture 0 source 0 t 0 :read-only :rg32f)
  (%gl:bind-image-texture 1 dest 0 t 0 :write-only :rg32f)
  (%gl:bind-image-texture 3 kernel 0 t 0 :read-only :rg32f)
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
  (%gl:bind-image-texture 0 source1 0 t 0 :read-only :rg32f)
  (%gl:bind-image-texture 2 source2 0 t 0 :read-only :rg32f)
  (%gl:bind-image-texture 1 dest 0 t 0 :write-only :rg32f)
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
  (%gl:bind-image-texture 0 source1 0 t 0 :read-only :rg32f)
  (%gl:bind-buffer-base :atomic-counter-buffer 0 counter)
  (setf (3bgl-shaders::uniform p 'smoothlife-shaders::tex)
        0)
  (3bgl-shaders::use-program p)
  (%gl:memory-barrier #.(cffi:foreign-enum-value '%GL:ENUM :texture-fetch-barrier-bit))
  (%gl:dispatch-compute x y z))

(defparameter *step* nil)
(defmethod basecode-draw ((w smoothlife))
  (setf *w* w)
  (incf *frames*)
;  (sleep 0.1)
  (when (programs w)
    (unless (twiddle w)
      (setf (twiddle w) (gen-texture2d :w *wy* :h 2))
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
        (gl:tex-image-2d :texture-2d 0 :rg32f 256 2 0 :rg :float d)))
    (%gl:bind-image-texture 2 (twiddle w) 0 nil 0 :read-only :rg32f)
    (unless (timestamps w)
      (setf (timestamps w) (list (gl:gen-queries 16)
                                 (gl:gen-queries 16)))
      (setf (timestamp-masks w) (list (make-array 16 :initial-element nil)
                                      (make-array 16 :initial-element nil))))
    (unless (times w)
      (setf (times w)
            (loop repeat 16 collect (make-array 512 :initial-element 0))))
    (unless (texture1 w)
      (setf (texture1 w) (gen-texture)))
    (unless (texture2 w)
      (setf (texture2 w) (gen-texture)))
    (unless (texture3 w)
      (setf (texture3 w) (gen-texture)))
    (unless (texture4 w)
      (setf (texture4 w) (gen-texture)))
    (unless (world-init w)
      (setf (world-init w) (car (gl:gen-textures 1))))
    (unless (counter w)
      (setf (counter w) (car (gl:gen-buffers 1)))
      (gl:bind-buffer :atomic-counter-buffer (counter w))
      (%gl:buffer-data :atomic-counter-buffer 4 (cffi:null-pointer)
                       :dynamic-draw )
      (cffi:with-foreign-object (x :uint32)
        (setf (cffi:mem-aref x :uint32) 1)
        (%gl:buffer-sub-data :atomic-counter-buffer 0 4 x))
      (gl:bind-buffer :atomic-counter-buffer 0))
    (flet ((init-world (#++ buf)

             ;; initialize texture1 to random values
             (let* ((wx (floor *wx* 8))
                    (wy (floor *wy* 8))
                    (wz (floor *wz* 8))
                    (r (make-array (* wx wy wz 2)
                                   :element-type 'single-float
                                   :initial-element 1.0)))
               (loop for i below (array-total-size r)
                     do (setf (row-major-aref r i)
                              (if (< (random 1.0) *fill*
                                     )
                                  1.0 0.0)))
               (gl:bind-texture :texture-3d (world-init w))
               (gl:tex-image-3d :texture-3d 0 :r32f wx wy wz 0
                                :rg :float r)
               (gl:tex-parameter :texture-3d :texture-min-filter :linear)
               (gl:tex-parameter :texture-3d :texture-mag-filter :linear)

               (fill-world (getf (programs w) :init-world)
                           (world-init w) (texture1 w) 1
                           (/ *wx* 8) (/ *wy* 8) (/ *wz* 8))
               (gl:bind-texture :texture-3d 0)))
           (qq (n)
             (setf (aref (car (timestamp-masks w)) n) t)
             (%gl:query-counter (nth n (car (timestamps w)))
                                :timestamp)))

      (unless (kernel1 w)
        (setf (kernel1 w) (gen-texture))
        (unless (kernel2 w)
          (setf (kernel2 w) (gen-texture)))
        (assert (and (texture1 w) (texture2 w) (kernel1 w) (kernel2 w)))
        (let* ((ra 22.2)
               (ri (/ ra 2.0800838))
               (aa 2.0))
          (flet ((fft-x (w) (getf (programs w) :fft-x))
                 (fft-y (w) (getf (programs w) :fft-y))
                 (fft-z (w) (getf (programs w) :fft-z)))
            (filltex (getf (programs w) :make-kernel)
                     (texture2 w) (texture3 w)
                     ri 0 0 aa 256/8 256/8 256/8) ;; 4.6 3d
            (scale-tex (getf (programs w) :scale-kernel)
                     (texture2 w) (texture3 w) (kernel1 w)
                     256/8 256/8 256/8 :scale 3000.0)
            (filltex (getf (programs w) :make-kernel)
                     (texture2 w) (texture3 w)
                     ra 0 ri aa 256/8 256/8 256/8)
            (scale-tex (getf (programs w) :scale-kernel)
                       (texture2 w) (texture3 w) (kernel2 w)
                       256/8 256/8 256/8 :scale 22000.0))))

      
      (qq 0)
      (when *step*
        (%gl:bind-image-texture 2 (twiddle w) 0 nil 0 :read-only :rg32f)
        (when (numberp *step*)
          (decf *step*))
        (when (eql *step* 0)
          (setf *step* nil))

        (gl:bind-texture :texture-2d 0)
        ;; fft
        (qq 0)
        (fft-pass (getf (programs w) :fft-x)
                  (kernel1 w) (texture1 w) 256/16 1 256)
        (qq 1)
        (fft-pass (getf (programs w) :fft-y)
                  (texture1 w) (texture2 w) 256/16 1 256)
        (qq 2)
        (fft-pass (getf (programs w) :fft-z)
                  (texture2 w) (texture3 w) 256/16 256 1)
        (qq 3)

#++        (fft-pass (getf (programs w) :fft-x)
                  (texture3 w) (texture2 w) 256/16 1 256)

        ;; ifft x2
        (qq 5)
        #++(fft-pass (getf (programs w) :ifft-x)
                  (texture1 w) (texture4 w) 256/16 1 256)
        (qq 6)
        #++(fft-pass (getf (programs w) :ifft-y)
                  (texture4 w) (texture3 w) 256/16 1 256)
        (qq 7)
        #++(fft-pass (getf (programs w) :ifft-z)
                  (texture3 w) (texture4 w) 256/16 256 1)
        (qq 8))

      (qq 12)


      (gl:bind-texture :texture-2d 0)
      (gl:bind-texture :texture-3d (texture3 w))
      (gl:tex-parameter :texture-3d :texture-mag-filter :linear)
      (gl:use-program 0)
      (gl:with-pushed-matrix* (:modelview)
        (gl:scale 4 2 2)
        (gl:rotate 90 0 0 1)
        (gl:translate 1 0 0)
        (gl:enable :depth-test)
        (gl:color-mask nil nil nil nil)
        ;; write z
        (gl:with-primitives :quads
          (loop for z from 0 below 1 by (/ 1 256.0)
                do
                   (gl:vertex -1 z -1)
                   (gl:vertex -1 z 1)
                   (gl:vertex 1 z 1)
                   (gl:vertex 1 z -1)))
        (gl:color-mask t t t t)
        (gl:color 1 1 1 1)
        (gl:enable :blend :texture-3d)
        #++ (gl:blend-func :src-alpha :one  ;-minus-src-color
                       )
        (gl:blend-func :src-color :one-minus-src-color)
        (gl:disable :depth-test)
        (gl:with-primitives :quads
          (loop for z from 0 below 1 by (/ 1 256.0)
                do
                   ;;(gl:tex-coord 1.5 1.5)
                   ;(gl:color 0.08 1 1 z)
                   (gl:tex-coord 1 1 z)
                   (gl:vertex -1 z -1)

                   ;;(gl:tex-coord 0.5 1.5)
                   (gl:tex-coord 0 1 z)
                   (gl:vertex -1 z 1)

                   ;;(gl:tex-coord 0.5 0.5)
                   (gl:tex-coord 0 0 z)
                   (gl:vertex 1 z 1)

                   ;;(gl:tex-coord 1.5 0.5)
                   (gl:tex-coord 1 0 z)
                   (gl:vertex 1 z -1)))
        (qq 13)
        (gl:disable :texture-3d)
        (gl:enable :depth-test)
        (gl:blend-func :src-alpha :one-minus-src-alpha)
        (rotatef (first (timestamps w)) (second (timestamps w)))
        (rotatef (first (timestamp-masks w)) (second (timestamp-masks w)))
        #++(format t "check queries ~s~%"  (timestamp-masks w))
        (cffi:with-foreign-objects ((done '%gl:int)
                                    (time '%gl:uint64))
          (loop with last = nil
                with x = (setf (aref (car (times w)) 0)
                               (1+ (mod (aref (car (times w)) 0)
                                        (1- (length (car (times w)))))))
                for id in (car (timestamps w))
                for i below 13
                when (aref (car (timestamp-masks w)) i)
                  do #++(format t "check query ~s = ~s~%" i id)
                     (setf (aref (car (timestamp-masks w)) i) nil)
                     (%gl:get-query-object-iv id :query-result-available done)
                     (when (plusp (cffi:mem-ref done '%gl:int))
                       (%gl:get-query-object-ui64v id :query-result time)
                       (when last
                         (setf (aref (nth i (times w)) x)
                               (- (cffi:mem-ref time '%gl:uint64)
                                  last)))
                       (setf last (cffi:mem-ref time '%gl:uint64)))
                ))
        ;(gl:matrix-mode :modelview)
        ;(gl:pop-matrix)

        (basecode::with-pixel-ortho-projection (w :origin :lower-left)
          (gl:disable :texture-2d :texture-3d)
          (gl:with-pushed-matrix* (:modelview)
            (gl:load-identity)
            (loop with d = 100
                  with s = 2
                  for x1 = (aref (car (times w)) 0)
                  for i from 1 below 13
                  for times in (cdr (times w))
                  for color from 1
                  for r = (ldb (byte 1 0) color)
                  for g = (ldb (byte 1 1) color)
                  for b = (ldb (byte 1 2) color)
                  for a = (ldb (byte 1 3) color)
                  do (if (plusp a)
                         (gl:color (* r 0.5) (* g 0.5) (* b 0.5) 1)
                         (gl:color r g b 1))
                     (gl:with-primitives :lines
                       (gl:vertex 0 (* i d))
                       (gl:vertex 512 (* i d))
                       (loop for j below 20 by 5
                             do (gl:vertex 0 (+ (* s j) (* i d)))
                                (gl:vertex 10 (+ (* s j) (* i d)))))
                     (gl:with-primitives :line-strip
                       (loop for j from 1 below 512
                             for y = (aref times j)
                             ;; 10/1000000.0 =  10px/ms
                             do (gl:vertex j (+ (* i d) (* s (/ y 1000000.0))))))
                  finally (gl:with-primitives :lines
                            (gl:vertex x1 0)
                            (gl:vertex x1 (* 13 d))))))))))

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
       (d texture4)
       (d twiddle)))
    ((#\space :space)
     (setf *step* 1))
    ((:r)
     (setf *step* :foo))
    ((:n)
     (setf *step* :new))
    ((:p)
     (print (setf *step* :auto)))
    ((:f)
     (let* ((now (get-internal-real-time))
            (dt (float (/ (- now *frame-time*)
                          internal-time-units-per-second))))
       (format t "~s frames/~s sec = ~s fps~%" *frames* dt (/ *frames* dt))
       (setf *frames* 0
             *frame-time* now)))
    ((:1 :2 :3 :4 :5 :6 :7 :8 :9)
     (setf *step* :new)
     (print (setf *fill* (float  (/ (parse-integer (string k)) 18.0)))))))



(setf 3bgl-shaders::*print-shaders* t)
(setf 3bgl-shaders::*verbose* nil)
; (basecode-run (make-instance 'smoothlife))
