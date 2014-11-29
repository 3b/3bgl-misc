(defpackage #:smoothlife-shaders
  (:use :cl :basecode :3bgl-glsl)
  (:shadowing-import-from :3bgl-glsl :defun :defconstant :defmacro))
(in-package #:smoothlife-shaders)


(uniform flag :int :location 0)
(uniform tex :image-2d :location 1 :layout (:rg32f t))
(uniform out1 :image-2d :location 2
                        :layout (:rg32f t))
(uniform kernel :image-2d :location 3
                          :layout (:rg32f t))

;; twiddle factors for FFT, x = element in FFT, y = pass
;; probably should move to local/constant once everything works?
(uniform twiddle :image-2d :location 3 :layout (:rg32f t))

(uniform rule-in1 :image-2d :location 0 :layout (:rg32f t))
(uniform rule-in2 :image-2d :location 1 :layout (:rg32f t))

(uniform counter :atomic-uint :layout (:binding 0 :offset 0))

(shared scratch (:float 8192))

(defmacro scratch (x y stride offset &optional im)
 (print `(aref scratch (+ ,@(when im `(4096))
                          ,x (* 8 (+ ,y
                                     ,(if (eql stride 1)
                                          offset
                                          `(* ,stride ,offset))))))))

(defmacro scratch2 (x y stride offset)
  `(vec2 (aref scratch (+ ,x (* 8 (+ ,y (* ,stride ,offset)))))
         (aref scratch (+ 4096 ,x (* 8 (+ ,y (* ,stride ,offset)))))))

(defmacro fft8 (in out)
  `(let* ((t00017 0.0)
          (t00016 0.0)
          (t00015 0.0)
          (t00014 0.0)
          (t00013 0.0)
          (t00012 0.0)
          (t00011 0.0)
          (t00010 0.0)
          (t00009 0.0)
          (t00008 0.0)
          (t00007 0.0)
          (t00006 0.0)
          (t00005 0.0)
          (t00004 0.0)
          (t00003 0.0)
          (t00002 0.0)
          (t00001 0.0)
          (t00000 0.0)
          (r001 (vec2 0 0))
          (r000 (vec2 0 0)))
     (setf r000 (,in 0))
     (setf r001 (,in 4))
     (setf t00000 (- (.y r000) (.y r001)))
     (setf t00001 (- (.x r000) (.x r001)))
     (setf t00002 (+ (.y r000) (.y r001)))
     (setf t00003 (+ (.x r000) (.x r001)))
     (setf r001 (,in 2))
     (setf r000 (,in 6))
     (setf t00004 (- (.x r001) (.x r000)))
     (setf t00005 (+ t00000 t00004))
     (setf t00006 (- t00000 t00004))
     (setf t00004 (- (.y r001) (.y r000)))
     (setf t00000 (- t00001 t00004))
     (setf t00007 (+ t00001 t00004))
     (setf t00004 (+ (.y r001) (.y r000)))
     (setf t00001 (- t00002 t00004))
     (setf t00008 (+ t00002 t00004))
     (setf t00004 (+ (.x r001) (.x r000)))
     (setf t00002 (- t00003 t00004))
     (setf t00009 (+ t00003 t00004))
     (setf r000 (,in 1))
     (setf r001 (,in 5))
     (setf t00004 (- (.y r000) (.y r001)))
     (setf t00003 (* 0.70710677 t00004))
     (setf t00010 (- (.x r000) (.x r001)))
     (setf t00011 (fma -0.70710677 t00010 t00003))
     (setf t00003 (* 0.70710677 t00010))
     (setf t00010 (fma 0.70710677 t00004 t00003))
     (setf t00003 (+ (.y r000) (.y r001)))
     (setf t00004 (+ (.x r000) (.x r001)))
     (setf r001 (,in 7))
     (setf r000 (,in 3))
     (setf t00012 (- (.y r001) (.y r000)))
     (setf t00013 (* 0.70710677 t00012))
     (setf t00012 (- (.x r001) (.x r000)))
     (setf t00014 (* 0.70710677 t00012))
     (setf t00012 (+ t00014 t00013))
     (setf t00015 (- t00011 t00012))
     (setf t00016 (- t00000 t00015))
     (setf t00017 (+ t00000 t00015))
     (setf t00015 (+ t00011 t00012))
     (setf t00012 (- t00006 t00015))
     (setf t00011 (+ t00006 t00015))
     (setf t00015 (- t00014 t00013))
     (setf t00013 (- t00010 t00015))
     (setf t00014 (+ t00005 t00013))
     (,out 7 t00016 t00014)
     (setf t00014 (- t00005 t00013))
     (,out 3 t00017 t00014)
     (setf t00014 (+ t00010 t00015))
     (setf t00015 (- t00007 t00014))
     (,out 5 t00015 t00012)
     (setf t00012 (+ t00007 t00014))
     (,out 1 t00012 t00011)
     (setf t00011 (+ (.y r001) (.y r000)))
     (setf t00012 (- t00003 t00011))
     (setf t00014 (- t00002 t00012))
     (setf t00007 (+ t00002 t00012))
     (setf t00012 (+ t00003 t00011))
     (setf t00011 (- t00008 t00012))
     (setf t00003 (+ t00008 t00012))
     (setf t00012 (+ (.x r001) (.x r000)))
     (setf t00008 (- t00004 t00012))
     (setf t00002 (+ t00001 t00008))
     (,out 6 t00014 t00002)
     (setf t00002 (- t00001 t00008))
     (,out 2 t00007 t00002)
     (setf t00002 (+ t00004 t00012))
     (setf t00012 (- t00009 t00002))
     (,out 4 t00012 t00011)
     (setf t00011 (+ t00009 t00002))
     (,out 0 t00011 t00003)))


(defun fft8il (x y lx ly stride)
  (macrolet ((in (i)
               `(.xy (image-load tex (ivec2 x (+ y (* stride ,i))))))
             (out (i re im)
               `(progn
                  (setf (scratch lx ly stride ,i) ,re)
                  (setf (scratch lx ly stride ,i t) ,im))))
    (fft8 in out)))


(defun fft8li (x y lx ly stride)
  (macrolet ((in (i)
               `(vec2 (scratch2 lx ly stride ,i)))
             (out (i re im)
               `(image-store out1 (ivec2 x (+ y (* stride ,i)))
                             (vec4 ,re ,im 0 0))))
    (fft8 in out)))



(defun fft8l (lx ly stride)
  (macrolet ((in (i)
               `(vec2 (scratch2 lx ly stride ,i)))
             (out (i re im)
               `(progn
                  (setf (scratch lx ly stride ,i) ,re)
                  (setf (scratch lx ly stride ,i t) ,im))))
    (fft8 in out)))




(defun ifft8il (x y lx ly stride)
  ;; doesn't actually do IFFT, just swaps re/im as first step in using
  ;; normal FFT to calculate IFFT
  (macrolet ((in (i)
               `(.yx (image-load tex (ivec2 x (+ y (* stride ,i))))))
             (out (i re im)
               `(progn
                  (setf (scratch lx ly stride ,i) ,re)
                  (setf (scratch lx ly stride ,i t) ,im))))
    (fft8 in out)))


(defun loadx (x y lx)
  (let* ((stride 1)
         (stride2 1)
         (r0 (image-load tex (ivec2 (+ (* y 8) 0) (+ x 0))))
         (r1 (image-load tex (ivec2 (+ (* y 8) 1) (+ x 0))))
         (r2 (image-load tex (ivec2 (+ (* y 8) 2) (+ x 0))))
         (r3 (image-load tex (ivec2 (+ (* y 8) 3) (+ x 0))))
         (r4 (image-load tex (ivec2 (+ (* y 8) 4) (+ x 0))))
         (r5 (image-load tex (ivec2 (+ (* y 8) 5) (+ x 0))))
         (r6 (image-load tex (ivec2 (+ (* y 8) 6) (+ x 0))))
         (r7 (image-load tex (ivec2 (+ (* y 8) 7) (+ x 0)))))
    (progn
      ;; re
      (setf (scratch lx (* y 8) 1 0) (.x r0))
      (setf (scratch lx (* y 8) 1 1) (.x r1))
      (setf (scratch lx (* y 8) 1 2) (.x r2))
      (setf (scratch lx (* y 8) 1 3) (.x r3))
      (setf (scratch lx (* y 8) 1 4) (.x r4))
      (setf (scratch lx (* y 8) 1 5) (.x r5))
      (setf (scratch lx (* y 8) 1 6) (.x r6))
      (setf (scratch lx (* y 8) 1 7) (.x r7)))
    (progn
      ;; im
      (setf (scratch lx (* y 8) 1 0 t) (.y r0))
      (setf (scratch lx (* y 8) 1 1 t) (.y r1))
      (setf (scratch lx (* y 8) 1 2 t) (.y r2))
      (setf (scratch lx (* y 8) 1 3 t) (.y r3))
      (setf (scratch lx (* y 8) 1 4 t) (.y r4))
      (setf (scratch lx (* y 8) 1 5 t) (.y r5))
      (setf (scratch lx (* y 8) 1 6 t) (.y r6))
      (setf (scratch lx (* y 8) 1 7 t) (.y r7)))))

(defun loadxt (x y lx)
  ;; swap real/imag components for use in calculating IFFT
  (let* ((stride 1)
         (stride2 1)
         (r0 (image-load tex (ivec2 (+ (* y 8) 0) (+ x 0))))
         (r1 (image-load tex (ivec2 (+ (* y 8) 1) (+ x 0))))
         (r2 (image-load tex (ivec2 (+ (* y 8) 2) (+ x 0))))
         (r3 (image-load tex (ivec2 (+ (* y 8) 3) (+ x 0))))
         (r4 (image-load tex (ivec2 (+ (* y 8) 4) (+ x 0))))
         (r5 (image-load tex (ivec2 (+ (* y 8) 5) (+ x 0))))
         (r6 (image-load tex (ivec2 (+ (* y 8) 6) (+ x 0))))
         (r7 (image-load tex (ivec2 (+ (* y 8) 7) (+ x 0)))))
    (progn
      ;; im->re
      (setf (scratch lx (* y 8) 1 0) (.y r0))
      (setf (scratch lx (* y 8) 1 1) (.y r1))
      (setf (scratch lx (* y 8) 1 2) (.y r2))
      (setf (scratch lx (* y 8) 1 3) (.y r3))
      (setf (scratch lx (* y 8) 1 4) (.y r4))
      (setf (scratch lx (* y 8) 1 5) (.y r5))
      (setf (scratch lx (* y 8) 1 6) (.y r6))
      (setf (scratch lx (* y 8) 1 7) (.y r7)))
    (progn
      ;; re->im
      (setf (scratch lx (* y 8) 1 0 t) (.x r0))
      (setf (scratch lx (* y 8) 1 1 t) (.x r1))
      (setf (scratch lx (* y 8) 1 2 t) (.x r2))
      (setf (scratch lx (* y 8) 1 3 t) (.x r3))
      (setf (scratch lx (* y 8) 1 4 t) (.x r4))
      (setf (scratch lx (* y 8) 1 5 t) (.x r5))
      (setf (scratch lx (* y 8) 1 6 t) (.x r6))
      (setf (scratch lx (* y 8) 1 7 t) (.x r7)))))

(defmacro c*r (a b)
  `(- (* (.x ,a) (.x ,b))
      (* (.y ,a) (.y ,b))))

(defmacro c*i (a b)
  `(+ (* (.y ,a) (.x ,b))
      (* (.x ,a) (.y ,b))))

(defun twiddle-pass (x y1 ty)
  (let* ((y (* y1 8))
         (stride 1)
         (t0 (image-load twiddle (ivec2 (+ y 0) ty)))
         (t1 (image-load twiddle (ivec2 (+ y 1) ty)))
         (t2 (image-load twiddle (ivec2 (+ y 2) ty)))
         (t3 (image-load twiddle (ivec2 (+ y 3) ty)))
         (t4 (image-load twiddle (ivec2 (+ y 4) ty)))
         (t5 (image-load twiddle (ivec2 (+ y 5) ty)))
         (t6 (image-load twiddle (ivec2 (+ y 6) ty)))
         (t7 (image-load twiddle (ivec2 (+ y 7) ty)))
         (r0 (vec2 (scratch x y stride 0) (scratch x y stride 0 t)))
         (r1 (vec2 (scratch x y stride 1) (scratch x y stride 1 t)))
         (r2 (vec2 (scratch x y stride 2) (scratch x y stride 2 t)))
         (r3 (vec2 (scratch x y stride 3) (scratch x y stride 3 t)))
         (r4 (vec2 (scratch x y stride 4) (scratch x y stride 4 t)))
         (r5 (vec2 (scratch x y stride 5) (scratch x y stride 5 t)))
         (r6 (vec2 (scratch x y stride 6) (scratch x y stride 6 t)))
         (r7 (vec2 (scratch x y stride 7) (scratch x y stride 7 t))))

    (setf (scratch x y stride 0) (c*r r0 t0))
    (setf (scratch x y stride 1) (c*r r1 t1))
    (setf (scratch x y stride 2) (c*r r2 t2))
    (setf (scratch x y stride 3) (c*r r3 t3))
    (setf (scratch x y stride 4) (c*r r4 t4))
    (setf (scratch x y stride 5) (c*r r5 t5))
    (setf (scratch x y stride 6) (c*r r6 t6))
    (setf (scratch x y stride 7) (c*r r7 t7))

    (setf (scratch x y stride 0 t) (c*i r0 t0))
    (setf (scratch x y stride 1 t) (c*i r1 t1))
    (setf (scratch x y stride 2 t) (c*i r2 t2))
    (setf (scratch x y stride 3 t) (c*i r3 t3))
    (setf (scratch x y stride 4 t) (c*i r4 t4))
    (setf (scratch x y stride 5 t) (c*i r5 t5))
    (setf (scratch x y stride 6 t) (c*i r6 t6))
    (setf (scratch x y stride 7 t) (c*i r7 t7))))


(defun transposell (lx sy dy sstride dstride)
  (let* ((r0 (scratch2 lx sy sstride 0))
         (r1 (scratch2 lx sy sstride 1))
         (r2 (scratch2 lx sy sstride 2))
         (r3 (scratch2 lx sy sstride 3))
         (r4 (scratch2 lx sy sstride 4))
         (r5 (scratch2 lx sy sstride 5))
         (r6 (scratch2 lx sy sstride 6))
         (r7 (scratch2 lx sy sstride 7)))
    (barrier)
    ;; re
    (setf (scratch lx dy dstride 0) (.x r0))
    (setf (scratch lx dy dstride 1) (.x r1))
    (setf (scratch lx dy dstride 2) (.x r2))
    (setf (scratch lx dy dstride 3) (.x r3))
    (setf (scratch lx dy dstride 4) (.x r4))
    (setf (scratch lx dy dstride 5) (.x r5))
    (setf (scratch lx dy dstride 6) (.x r6))
    (setf (scratch lx dy dstride 7) (.x r7))
    ;; im
    (setf (scratch lx dy dstride 0 t) (.y r0))
    (setf (scratch lx dy dstride 1 t) (.y r1))
    (setf (scratch lx dy dstride 2 t) (.y r2))
    (setf (scratch lx dy dstride 3 t) (.y r3))
    (setf (scratch lx dy dstride 4 t) (.y r4))
    (setf (scratch lx dy dstride 5 t) (.y r5))
    (setf (scratch lx dy dstride 6 t) (.y r6))
    (setf (scratch lx dy dstride 7 t) (.y r7))))

(defun fft-x ()
  (declare (layout (:in nil :local-size-x 8 :local-size-y 64)))
  (let ((x (.x gl-global-invocation-id))
        (y (.y gl-global-invocation-id))
        (lx (.x gl-local-invocation-id))
        (ly (.y gl-local-invocation-id)))
    (loadx x y lx)
    (barrier)
    ;; 512 pt
    ;; = 8pt fft
    (barrier)
    (fft8l lx ly 64)
    (barrier)
    ;;   + twiddle 8x64
    (twiddle-pass lx y 0)
    (barrier)
    ;;   + transpose 8x8
    (transposell lx y (* 8 y) 64 1)
    (barrier)
    ;; + 64 pt fft
    ;;   = 8 pt fft
    (fft8l lx ly 64)
    (barrier)
    ;; + twidddle 8x8 / 8 stride
    (twiddle-pass lx y 1)
    ;; + transpose 8x64
    (barrier)
    (transposell lx y
                 (3bgl-shaders::uint (* 8 (+ (* 8 (mod y 8))
                                             (floor (/ y 8)))))
                 64 1)
    ;;   + 8pt fft
    (barrier)
    (fft8l lx (3bgl-shaders::uint (+ (mod y 8)
                                     (* 64 (floor (/ y 8)))))
           8)
    (barrier)
    ;; copy to dest image
    (dotimes (i 8)
      (let* ((y512 (+ (* y 8) i))
             (xy (ivec2 y512 x))
             (yy (3bgl-shaders::uint    ;y512
                  (+ (* 64 (mod y512 8))
                     (floor (/ y512 8))))))
        ;; fixme: write out in X order instead of y
        (image-store out1 xy
                     (* 1 (vec4 (scratch lx yy 0 0)
                                (scratch lx yy 0 0 t)
                                0 0)))))))

(defun fft-y ()
  (declare (layout (:in nil :local-size-x 8 :local-size-y 64)))
  (let ((x (.x gl-global-invocation-id))
        (y (.y gl-global-invocation-id))
        (lx (.x gl-local-invocation-id))
        (ly (.y gl-local-invocation-id)))
    tex ;; fixme: compiler is missing dependencies somewhere
    ;; 512 pt
    ;; = 8pt fft
    (fft8il x y lx ly 64)
    (barrier)
    ;;   + twiddle 8x64
    (twiddle-pass lx y 0)
    (barrier)
    ;;   + transpose 8x8
    (transposell lx y (* 8 y) 64 1)
    (barrier)
    ;; + 64 pt fft
    ;;   = 8 pt fft
    (fft8l lx ly 64)
    (barrier)
    ;; + twidddle 8x8 / 8 stride
    (twiddle-pass lx y 1)
    ;; + transpose 8x64
    (barrier)
    (transposell lx y
                 (3bgl-shaders::uint (* 8 (+ (* 8 (mod y 8))
                                             (floor (/ y 8)))))
                 64 1)
    ;;   + 8pt fft
    (barrier)
    (fft8l lx (3bgl-shaders::uint (+ (mod y 8)
                                     (* 64 (floor (/ y 8)))))
           8)
    (barrier)
    ;; copy to dest image
    (dotimes (i 8)
      (let* ((y512 (+ (* y 8) i))
             (xy (ivec2 x y512))
             (yy (3bgl-shaders::uint    ;y512
                  (+ (* 64 (mod y512 8))
                     (floor (/ y512 8))))))
        (image-store out1 xy
                     (* 1 (vec4 (scratch lx yy 0 0)
                                (scratch lx yy 0 0 t)
                                0 0)))))))


(defun convolve ()
  (declare (layout (:in nil :local-size-x 32 :local-size-y 32)))
  (let* ((x (.x gl-global-invocation-id))
         (y (.y gl-global-invocation-id))
         (xy (ivec2 x y))
         (a (image-load tex xy))
         (k (image-load kernel xy)))
    (image-store out1 xy
                 (vec4 (c*r a k)
                       (c*i a k)
                       0 0))))



(defun ifft-x ()
  (declare (layout (:in nil :local-size-x 8 :local-size-y 64)))
  ;; calculate IFFT by swapping RE/IM on input and output, and
  ;; dividing result by N
  (let ((x (.x gl-global-invocation-id))
        (y (.y gl-global-invocation-id))
        (lx (.x gl-local-invocation-id))
        (ly (.y gl-local-invocation-id)))
    (loadxt x y lx)
    (barrier)
    ;; 512 pt
    ;; = 8pt fft
    (barrier)
    (fft8l lx ly 64)
    (barrier)
    ;;   + twiddle 8x64
    (twiddle-pass lx y 0)
    (barrier)
    ;;   + transpose 8x8
    (transposell lx y (* 8 y) 64 1)
    (barrier)
    ;; + 64 pt fft
    ;;   = 8 pt fft
    (fft8l lx ly 64)
    (barrier)
    ;; + twidddle 8x8 / 8 stride
    (twiddle-pass lx y 1)
    ;; + transpose 8x64
    (barrier)
    (transposell lx y
                 (3bgl-shaders::uint (* 8 (+ (* 8 (mod y 8))
                                             (floor (/ y 8)))))
                 64 1)
    ;;   + 8pt fft
    (barrier)
    (fft8l lx (3bgl-shaders::uint (+ (mod y 8)
                                     (* 64 (floor (/ y 8)))))
           8)
    (barrier)
    ;; copy to dest image
    (dotimes (i 8)
      (let* ((y512 (+ (* y 8) i))
             (xy (ivec2 y512 x))
             (yy (3bgl-shaders::uint    ;y512
                  (+ (* 64 (mod y512 8))
                     (floor (/ y512 8))))))
        ;; fixme: write out in X order instead of y
        (image-store out1 xy
                     ;; todo: probably should just divide by 512^2 in ifft-y
                     ;; and skip swap on output of -x and input of -y?
                     (* #.(/ 1.0 512)
                        (vec4 (scratch lx yy 0 0 t)
                              (scratch lx yy 0 0)
                              0 0)))))))



(defun ifft-y ()
  (declare (layout (:in nil :local-size-x 8 :local-size-y 64)))
  ;; calculate IFFT by swapping RE/IM on input and output, and
  ;; dividing result by N
  (let ((x (.x gl-global-invocation-id))
        (y (.y gl-global-invocation-id))
        (lx (.x gl-local-invocation-id))
        (ly (.y gl-local-invocation-id)))
    tex ;; fixme: compiler is missing dependencies somewhere
    ;; 512 pt
    ;; = 8pt fft
    (ifft8il x y lx ly 64)
    (barrier)
    ;;   + twiddle 8x64
    (twiddle-pass lx y 0)
    (barrier)
    ;;   + transpose 8x8
    (transposell lx y (* 8 y) 64 1)
    (barrier)
    ;; + 64 pt fft
    ;;   = 8 pt fft
    (fft8l lx ly 64)
    (barrier)
    ;; + twidddle 8x8 / 8 stride
    (twiddle-pass lx y 1)
    ;; + transpose 8x64
    (barrier)
    (transposell lx y
                 (3bgl-shaders::uint (* 8 (+ (* 8 (mod y 8))
                                             (floor (/ y 8)))))
                 64 1)
    ;;   + 8pt fft
    (barrier)
    (fft8l lx (3bgl-shaders::uint (+ (mod y 8)
                                     (* 64 (floor (/ y 8)))))
           8)
    (barrier)
    ;; copy to dest image
    (dotimes (i 8)
      (let* ((y512 (+ (* y 8) i))
             (xy (ivec2 x y512))
             (yy (3bgl-shaders::uint    ;y512
                  (+ (* 64 (mod y512 8))
                     (floor (/ y512 8))))))
        (image-store out1 xy
                     (* #.(/ 1.0 512)
                        (vec4 (scratch lx yy 0 0 t)
                              (scratch lx yy 0 0)
                              0 0)))))))

;;; smoothlife


;; sigmoid curves
(defun ss1m (x a)
  (return (/ 1.0 (+ 1.0 (exp (* #.(/ -4.0 0.147) ;; alpha m = 0.147
                                (- x a)))))))

(defun ss1n (x a)
  (return (/ 1.0 (+ 1.0 (exp (* #.(/ -4.0 0.028) ;; alpha n = 0.028
                                (- x a)))))))

(defun ss2 (x a b)
  (return (* (ss1n x a) (- 1.0 (ss1n x b)))))

(defun ssm (x y m)
  (let ((s (ss1m m 0.5)))
    (return (+ (* x (- 1.0 s))
               (* y s)))))

;; rules for calculating next generation
;; (discrete version)
(defun rules ()
  (declare (layout (:in nil :local-size-x 32 :local-size-y 32)))
  (let* ((x (.x gl-global-invocation-id))
         (y (.y gl-global-invocation-id))
         (xy (ivec2 x y))
         (a (image-load rule-in1 xy))
         (b (image-load rule-in2 xy))
         ;; borth/death intervals
         (b1 0.278) (b2 0.365) (d1 0.267) (d2 0.445)
         ;;(b1 0.278) (b2 0.350) (d1 0.267) (d2 0.415)
         ;;(b1 0.278) (d1 0.237) (b2 0.365) (d2 0.445)
         ;;(b1 0.257) (b2 0.336) (d1 0.365) (d2 0.549)
         ;;(d1 0.1350) (b1 0.136) (b2 0.365) (d2 0.364)
         ;(b1 0.221) (b2 0.325) (d1 0.465) (d2 0.589)
         ;;(b1 0.230) (b2 0.342) (d1 0.3) (d2 0.599)
         ;;(b1 0.230) (b2 0.3265) (d1 0.615) (d2 0.62)
         ;;(b1 0.2530) (b2 0.35) (d1 0.91) (d2 0.92)
         ;;(b1 0.2) (b2 0.2819) (d1 0.99) (d2 0.9999)
         (n (.x b)) ;; n = 'neighborhood' = kernel2
         (m (.x a))) ;; m = inner = kernel1
    ;;(setf n (/ (float x) 512.0))
    ;;(setf m (/ (float y) 512.0))
    (image-store out1 xy
                 (vec4 (* 1 (ss2 n (ssm b1 d1 m) (ssm b2 d2 m)))
                       0 0 0))))


;;; hack to check for empty screen to allow restarting
(defun count-pixels ()
  (declare (layout (:in nil :local-size-x 32 :local-size-y 32)))
   (let* ((x (.x gl-global-invocation-id))
          (y (.y gl-global-invocation-id))
          (xy (ivec2 x y))
          (a (image-load tex xy)))
     (when (> (.x a) 0.001)
       (atomic-counter-increment counter))))
