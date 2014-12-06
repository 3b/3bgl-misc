(defpackage #:smoothlife-shaders
  (:use :cl :basecode :3bgl-glsl)
  (:shadowing-import-from :3bgl-glsl :defun :defconstant :defmacro))
(in-package #:smoothlife-shaders)


(uniform flag :int :location 0)
(uniform tex :image-3d :location 1 :layout (:rg16f t))
(uniform out1 :image-3d :location 2
                        :layout (:rg16f t))
(uniform kernel :image-3d :location 3
                          :layout (:rg16f t))

;; twiddle factors for FFT, x = element in FFT, y = pass
;; probably should move to local/constant once everything works?
(uniform twiddle :image-2d :location 3 :layout (:rg16f t))

(uniform rule-in1 :image-3d :location 0 :layout (:rg16f t))
(uniform rule-in2 :image-3d :location 1 :layout (:rg16f t))

(uniform counter :atomic-uint :layout (:binding 0 :offset 0))

(shared scratch (:float 8192))

(defmacro scratch (x y stride offset &optional im)
  `(aref scratch (+ ,@(when im `(4096))
                    ,x (* 16 (+ ,y
                                ,(if (eql stride 1)
                                     offset
                                     `(* ,stride ,offset)))))))

(defmacro scratch2 (x y stride offset)
  `(vec2 (aref scratch (+ ,x (* 16 (+ ,y (* ,stride ,offset)))))
         (aref scratch (+ 4096 ,x (* 16 (+ ,y (* ,stride ,offset)))))))

(defmacro FFT16 (in out)
  `(LET* ((T00035 0.0)
          (T00034 0.0)
          (T00033 0.0)
          (T00032 0.0)
          (T00031 0.0)
          (T00030 0.0)
          (T00029 0.0)
          (T00028 0.0)
          (T00027 0.0)
          (T00026 0.0)
          (T00025 0.0)
          (T00024 0.0)
          (T00023 0.0)
          (T00022 0.0)
          (T00021 0.0)
          (T00020 0.0)
          (T00019 0.0)
          (T00018 0.0)
          (T00017 0.0)
          (T00016 0.0)
          (T00015 0.0)
          (T00014 0.0)
          (T00013 0.0)
          (T00012 0.0)
          (T00011 0.0)
          (T00010 0.0)
          (T00009 0.0)
          (T00008 0.0)
          (T00007 0.0)
          (T00006 0.0)
          (T00005 0.0)
          (T00004 0.0)
          (T00003 0.0)
          (T00002 0.0)
          (T00001 0.0)
          (T00000 0.0)
          (R001 (vec2 0.0))
          (R000 (vec2 0.0)))
     (SETF R000 (,IN 0))
     (SETF R001 (,IN 8))
     (SETF T00000 (- (.Y R000) (.Y R001)))
     (SETF T00001 (- (.X R000) (.X R001)))
     (SETF T00002 (+ (.Y R000) (.Y R001)))
     (SETF T00003 (+ (.X R000) (.X R001)))
     (SETF R001 (,IN 4))
     (SETF R000 (,IN 12))
     (SETF T00004 (- (.X R001) (.X R000)))
     (SETF T00005 (+ T00000 T00004))
     (SETF T00006 (- T00000 T00004))
     (SETF T00004 (- (.Y R001) (.Y R000)))
     (SETF T00000 (- T00001 T00004))
     (SETF T00007 (+ T00001 T00004))
     (SETF T00004 (+ (.Y R001) (.Y R000)))
     (SETF T00001 (- T00002 T00004))
     (SETF T00008 (+ T00002 T00004))
     (SETF T00004 (+ (.X R001) (.X R000)))
     (SETF T00002 (- T00003 T00004))
     (SETF T00009 (+ T00003 T00004))
     (SETF R000 (,IN 2))
     (SETF R001 (,IN 10))
     (SETF T00004 (- (.Y R000) (.Y R001)))
     (SETF T00003 (* 0.70710677 T00004))
     (SETF T00004 (- (.X R000) (.X R001)))
     (SETF T00010 (* 0.70710677 T00004))
     (SETF T00004 (- T00003 T00010))
     (SETF T00011 (+ T00010 T00003))
     (SETF T00003 (+ (.Y R000) (.Y R001)))
     (SETF T00010 (+ (.X R000) (.X R001)))
     (SETF R001 (,IN 14))
     (SETF R000 (,IN 6))
     (SETF T00012 (- (.Y R001) (.Y R000)))
     (SETF T00013 (* 0.70710677 T00012))
     (SETF T00012 (- (.X R001) (.X R000)))
     (SETF T00014 (* 0.70710677 T00012))
     (SETF T00012 (+ T00014 T00013))
     (SETF T00015 (- T00004 T00012))
     (SETF T00016 (- T00000 T00015))
     (SETF T00017 (+ T00000 T00015))
     (SETF T00015 (+ T00004 T00012))
     (SETF T00012 (- T00006 T00015))
     (SETF T00004 (+ T00006 T00015))
     (SETF T00015 (- T00014 T00013))
     (SETF T00013 (- T00011 T00015))
     (SETF T00014 (+ T00005 T00013))
     (SETF T00006 (- T00005 T00013))
     (SETF T00013 (+ T00011 T00015))
     (SETF T00015 (- T00007 T00013))
     (SETF T00011 (+ T00007 T00013))
     (SETF T00013 (+ (.Y R001) (.Y R000)))
     (SETF T00007 (- T00003 T00013))
     (SETF T00005 (- T00002 T00007))
     (SETF T00000 (+ T00002 T00007))
     (SETF T00007 (+ T00003 T00013))
     (SETF T00013 (- T00008 T00007))
     (SETF T00003 (+ T00008 T00007))
     (SETF T00007 (+ (.X R001) (.X R000)))
     (SETF T00008 (- T00010 T00007))
     (SETF T00002 (+ T00001 T00008))
     (SETF T00018 (- T00001 T00008))
     (SETF T00008 (+ T00010 T00007))
     (SETF T00007 (- T00009 T00008))
     (SETF T00010 (+ T00009 T00008))
     (SETF R000 (,IN 1))
     (SETF R001 (,IN 9))
     (SETF T00008 (- (.Y R000) (.Y R001)))
     (SETF T00009 (- (.X R000) (.X R001)))
     (SETF T00001 (+ (.Y R000) (.Y R001)))
     (SETF T00019 (+ (.X R000) (.X R001)))
     (SETF R001 (,IN 5))
     (SETF R000 (,IN 13))
     (SETF T00020 (- (.X R001) (.X R000)))
     (SETF T00021 (+ T00008 T00020))
     (SETF T00022 (* 0.38268343 T00021))
     (SETF T00023 (* 0.9238795 T00021))
     (SETF T00021 (- T00008 T00020))
     (SETF T00020 (* 0.9238795 T00021))
     (SETF T00008 (* 0.38268343 T00021))
     (SETF T00021 (- (.Y R001) (.Y R000)))
     (SETF T00024 (- T00009 T00021))
     (SETF T00025 (FMA -0.9238795 T00024 T00022))
     (SETF T00022 (FMA 0.38268343 T00024 T00023))
     (SETF T00023 (+ T00009 T00021))
     (SETF T00021 (FMA -0.38268343 T00023 T00020))
     (SETF T00020 (FMA 0.9238795 T00023 T00008))
     (SETF T00008 (+ (.Y R001) (.Y R000)))
     (SETF T00023 (- T00001 T00008))
     (SETF T00009 (* 0.70710677 T00023))
     (SETF T00023 (+ T00001 T00008))
     (SETF T00008 (+ (.X R001) (.X R000)))
     (SETF T00001 (- T00019 T00008))
     (SETF T00024 (* 0.70710677 T00001))
     (SETF T00001 (- T00009 T00024))
     (SETF T00026 (+ T00024 T00009))
     (SETF T00009 (+ T00019 T00008))
     (SETF R000 (,IN 15))
     (SETF R001 (,IN 7))
     (SETF T00008 (- (.Y R000) (.Y R001)))
     (SETF T00019 (- (.X R000) (.X R001)))
     (SETF T00024 (+ (.Y R000) (.Y R001)))
     (SETF T00027 (+ (.X R000) (.X R001)))
     (SETF R001 (,IN 3))
     (SETF R000 (,IN 11))
     (SETF T00028 (- (.X R001) (.X R000)))
     (SETF T00029 (+ T00008 T00028))
     (SETF T00030 (* 0.38268343 T00029))
     (SETF T00031 (- T00008 T00028))
     (SETF T00028 (* 0.9238795 T00031))
     (SETF T00008 (- (.Y R001) (.Y R000)))
     (SETF T00032 (- T00019 T00008))
     (SETF T00033 (FMA 0.9238795 T00032 T00030))
     (SETF T00030 (- T00025 T00033))
     (SETF T00034 (- T00016 T00030))
     (SETF T00035 (+ T00016 T00030))
     (SETF T00030 (+ T00025 T00033))
     (SETF T00033 (- T00006 T00030))
     (SETF T00025 (+ T00006 T00030))
     (SETF T00030 (* 0.38268343 T00032))
     (SETF T00032 (FMA -0.9238795 T00029 T00030))
     (SETF T00030 (- T00022 T00032))
     (SETF T00029 (+ T00014 T00030))
     (,OUT 15 T00034 T00029)
     (SETF T00029 (- T00014 T00030))
     (,OUT 7 T00035 T00029)
     (SETF T00029 (+ T00022 T00032))
     (SETF T00032 (- T00017 T00029))
     (,OUT 11 T00032 T00033)
     (SETF T00033 (+ T00017 T00029))
     (,OUT 3 T00033 T00025)
     (SETF T00025 (+ T00019 T00008))
     (SETF T00008 (FMA 0.38268343 T00025 T00028))
     (SETF T00028 (- T00021 T00008))
     (SETF T00019 (- T00015 T00028))
     (SETF T00033 (+ T00015 T00028))
     (SETF T00028 (+ T00021 T00008))
     (SETF T00008 (- T00004 T00028))
     (SETF T00021 (+ T00004 T00028))
     (SETF T00028 (* 0.9238795 T00025))
     (SETF T00025 (FMA -0.38268343 T00031 T00028))
     (SETF T00028 (- T00020 T00025))
     (SETF T00031 (+ T00012 T00028))
     (,OUT 13 T00019 T00031)
     (SETF T00031 (- T00012 T00028))
     (,OUT 5 T00033 T00031)
     (SETF T00031 (+ T00020 T00025))
     (SETF T00025 (- T00011 T00031))
     (,OUT 9 T00025 T00008)
     (SETF T00008 (+ T00011 T00031))
     (,OUT 1 T00008 T00021)
     (SETF T00021 (+ (.Y R001) (.Y R000)))
     (SETF T00008 (- T00024 T00021))
     (SETF T00031 (* 0.70710677 T00008))
     (SETF T00008 (+ T00024 T00021))
     (SETF T00021 (- T00023 T00008))
     (SETF T00024 (- T00007 T00021))
     (SETF T00011 (+ T00007 T00021))
     (SETF T00021 (+ T00023 T00008))
     (SETF T00008 (- T00003 T00021))
     (SETF T00023 (+ T00003 T00021))
     (SETF T00021 (+ (.X R001) (.X R000)))
     (SETF T00003 (- T00027 T00021))
     (SETF T00007 (* 0.70710677 T00003))
     (SETF T00003 (+ T00007 T00031))
     (SETF T00025 (- T00001 T00003))
     (SETF T00020 (- T00005 T00025))
     (SETF T00033 (+ T00005 T00025))
     (SETF T00025 (+ T00001 T00003))
     (SETF T00003 (- T00018 T00025))
     (SETF T00001 (+ T00018 T00025))
     (SETF T00025 (- T00007 T00031))
     (SETF T00031 (- T00026 T00025))
     (SETF T00007 (+ T00002 T00031))
     (,OUT 14 T00020 T00007)
     (SETF T00007 (- T00002 T00031))
     (,OUT 6 T00033 T00007)
     (SETF T00007 (+ T00026 T00025))
     (SETF T00025 (- T00000 T00007))
     (,OUT 10 T00025 T00003)
     (SETF T00003 (+ T00000 T00007))
     (,OUT 2 T00003 T00001)
     (SETF T00001 (+ T00027 T00021))
     (SETF T00021 (- T00009 T00001))
     (SETF T00027 (+ T00013 T00021))
     (,OUT 12 T00024 T00027)
     (SETF T00027 (- T00013 T00021))
     (,OUT 4 T00011 T00027)
     (SETF T00027 (+ T00009 T00001))
     (SETF T00001 (- T00010 T00027))
     (,OUT 8 T00001 T00008)
     (SETF T00008 (+ T00010 T00027))
     (,OUT 0 T00008 T00023)))

(defun fft16il (x y z lx ly stride)
  (macrolet ((in (i)
               `(.xy (image-load tex (ivec3 x (+ y (* stride ,i)) z))))
             (out (i re im)
               `(progn
                  (setf (scratch lx ly stride ,i) ,re)
                  (setf (scratch lx ly stride ,i t) ,im))))
    (fft16 in out)))

(defun fft16ilz (x y z lx ly stride)
  (macrolet ((in (i)
               `(.xy (image-load tex (ivec3 x y (+ z (* stride ,i))))))
             (out (i re im)
               `(progn
                  (setf (scratch lx ly stride ,i) ,re)
                  (setf (scratch lx ly stride ,i t) ,im))))
    (fft16 in out)))


#++
(defun fft16li (x y lx ly stride)
  (macrolet ((in (i)
               `(vec2 (scratch2 lx ly stride ,i)))
             (out (i re im)
               `(image-store out1 (ivec2 x (+ y (* stride ,i)))
                             (vec4 ,re ,im 0 0))))
    (fft16 in out)))



(defun fft16l (lx ly stride)
  (macrolet ((in (i)
               `(vec2 (scratch2 lx ly stride ,i)))
             (out (i re im)
               `(progn
                  (setf (scratch lx ly stride ,i) ,re)
                  (setf (scratch lx ly stride ,i t) ,im))))
    (fft16 in out)))




(defun ifft16il (x y z lx ly stride)
  ;; doesn't actually do IFFT, just swaps re/im as first step in using
  ;; normal FFT to calculate IFFT
  (macrolet ((in (i)
               `(.yx (image-load tex (ivec3 x (+ y (* stride ,i)) z))))
             (out (i re im)
               `(progn
                  (setf (scratch lx ly stride ,i) ,re)
                  (setf (scratch lx ly stride ,i t) ,im))))
    (fft16 in out)))

(defun ifft16ilz (x y z lx ly stride)
  ;; doesn't actually do IFFT, just swaps re/im as first step in using
  ;; normal FFT to calculate IFFT
  (macrolet ((in (i)
               `(.yx (image-load tex (ivec3 x y (+ z (* stride ,i))))))
             (out (i re im)
               `(progn
                  (setf (scratch lx ly stride ,i) ,re)
                  (setf (scratch lx ly stride ,i t) ,im))))
    (fft16 in out)))


(defmacro with-image-vars ((x y z base-var count) &body body)
  `(let (,@(loop for i below count
                 for s =  (alexandria:format-symbol t
                                                    "~a~d" base-var i)
                 collect (list s `(image-load tex (ivec3 (+ ,y ,i)
                                                         ,x ,z)))))
     ,@body))

(defmacro with-local-vars ((x y base-var count &key stride) &body body)
  `(let (,@(loop for i below count
                 for s = (alexandria:format-symbol t
                                                   "~a~d" base-var i)
                 collect (list s `(scratch2 ,x ,y ,stride ,i))))
     ,@body))

(defmacro with-twiddle-vars ((y ty base-var count) &body body)
  `(let (,@(loop for i below count
                 for s = (alexandria:format-symbol t
                                                   "~a~d" base-var i)
                 collect (list s `(image-load twiddle (ivec2 (+ ,y ,i) ,ty)))))
     ,@body))

(defmacro write-local (y lx base-var count &key base-var2
                                             (r-form `(.x ,base-var))
                                             (i-form `(.y ,base-var))
                                             (stride 1))
  `(progn
     ;; re
     ,@(loop for i below count
             for s = (alexandria:format-symbol t
                                               "~a~d" base-var i)
             for s2 = (when base-var2
                        (alexandria:format-symbol t
                                                  "~a~d" base-var2 i))
             for f = (if s2
                         (subst s base-var (subst s2 base-var2 r-form))
                         (subst s base-var r-form))

             collect
             `(setf (scratch ,lx ,y ,stride ,i) ,f))
     ;; im
     ,@(loop for i below count
             for s = (alexandria:format-symbol t
                                               "~a~d" base-var i)
             for s2 = (when base-var2
                        (alexandria:format-symbol t
                                                  "~a~d" base-var2 i))
             for f = (if s2
                         (subst s base-var (subst s2 base-var2 i-form))
                         (subst s base-var i-form))
             collect
             `(setf (scratch ,lx ,y ,stride ,i t) ,f))))

(defun loadx (x y1 z lx)
  (let ((y (* y1 16)))
    (with-image-vars (x y z r 16)
     (write-local y lx r 16))))


(defun loadxt (x y1 z lx)
  ;; swap real/imag components for use in calculating IFFT
  (let ((y (* y1 16)))
    (with-image-vars (x y z r 16)
      (write-local y lx r 16 :r-form (.y r) :i-form (.x r)))))

(defmacro c*r (a b)
  `(- (* (.x ,a) (.x ,b))
      (* (.y ,a) (.y ,b))))

(defmacro c*i (a b)
  `(+ (* (.y ,a) (.x ,b))
      (* (.x ,a) (.y ,b))))

(defun twiddle-pass (x y1 ty)
  (let* ((y (* y1 16)))
    (with-twiddle-vars (y ty t 16)
      (with-local-vars (x y r 16 :stride 1)
        (write-local y x r 16 :base-var2 t
                     :r-form (c*r r t)
                     :i-form (c*i r t))))))

(defun transposell (lx sy dy sstride dstride)
  (with-local-vars (lx sy r 16 :stride sstride)
    (barrier)
    (write-local dy lx r 16 :stride dstride)))

(defun fft-x ()
  (declare (layout (:in nil :local-size-x 16 :local-size-y 16 :local-size-z 1)))
  (let ((x (.x gl-global-invocation-id))
        (y (.y gl-global-invocation-id))
        (z (.z gl-global-invocation-id))
        (lx (.x gl-local-invocation-id))
        (ly (.y gl-local-invocation-id)))
    (loadx x y z lx)
    (barrier)
    ;; 256 pt
    ;; = 16pt fft
    (barrier)
    (fft16l lx ly 16)
    (barrier)
    ;;   + twiddle 16x16
    (twiddle-pass lx y 0)
    (barrier)
    ;;   + transpose 16x16
    (transposell lx y (* 16 y) 16 1)
    (barrier)
    ;; + 16 pt fft
    (fft16l lx ly 16)
    (barrier)
    ;; copy to dest image
    (dotimes (i 16)
      (let* ((y256 (+ (* y 16) i))
             (xy (ivec3 y256 x z))
             (yy (3bgl-shaders::uint y256)))
        ;; fixme: write out in X order instead of y
        (image-store out1 xy
                     (* 1;#. (/ (sqrt 256.0))
                        (vec4 (scratch lx yy 0 0)
                              (scratch lx yy 0 0 t)
                              0 0)))))))


(defun fft-y ()
  (declare (layout (:in nil :local-size-x 16 :local-size-y 16
                        :local-size-z 1)))
  (let ((x (.x gl-global-invocation-id))
        (y (.y gl-global-invocation-id))
        (z (.z gl-global-invocation-id))
        (lx (.x gl-local-invocation-id))
        (ly (.y gl-local-invocation-id)))
    tex ;; fixme: compiler is missing dependencies somewhere
    ;; 256 pt
    ;; = 16pt fft
    (fft16il x y z lx ly 16)
    (barrier)
    ;;   + twiddle 16x16
    (twiddle-pass lx y 0)
    (barrier)
    ;;   + transpose 16x16
    (transposell lx y (* 16 y) 16 1)
    (barrier)
    ;; + 16 pt fft
    (fft16l lx ly 16)
    (barrier)
    ;; copy to dest image
    (dotimes (i 16)
      (let* ((y256 (+ (* y 16) i))
             (xy (ivec3 x y256 z))
             (yy (3bgl-shaders::uint y256)))
        (image-store out1 xy
                     (* 1;#. (/ (sqrt 512.0))
                        (vec4 (scratch lx yy 0 0)
                              (scratch lx yy 0 0 t)
                              0 0)))))))

(defun fft-z ()
  (declare (layout (:in nil :local-size-x 16 :local-size-y 1
                        :local-size-z 16)))
  (let ((x (.x gl-global-invocation-id))
        (y (.y gl-global-invocation-id))
        (z (.z gl-global-invocation-id))
        (lx (.x gl-local-invocation-id))
        (lz (.z gl-local-invocation-id)))
    tex ;; fixme: compiler is missing dependencies somewhere
    ;; 256 pt
    ;; = 16pt fft
    (fft16ilz x y z lx lz 16)
    (barrier)
    ;;   + twiddle 16x16
    (twiddle-pass lx z 0)
    (barrier)
    ;;   + transpose 16x16
    (transposell lx z (* 16 z) 16 1)
    (barrier)
    ;; + 16 pt fft
    (fft16l lx lz 16)
    (barrier)
    ;; copy to dest image
    (dotimes (i 16)
      (let* ((z256 (+ (* z 16) i))
             (xy (ivec3 x y z256))
             (zz (3bgl-shaders::uint z256)))
        (image-store out1 xy
                     (* 1;#. (/ (sqrt 512.0))
                        (vec4 (scratch lx zz 0 0)
                              (scratch lx zz 0 0 t)
                              0 0)))))))


(defun convolve ()
  (declare (layout (:in nil :local-size-x 8 :local-size-y 8
                    :local-size-z 8)))
  (let* ((x (.x gl-global-invocation-id))
         (y (.y gl-global-invocation-id))
         (z (.z gl-global-invocation-id))
         (xy (ivec3 x y z))
         (a (image-load tex xy))
         (k (image-load kernel xy)))
    (image-store out1 xy
                 (vec4 (c*r a k)
                       (c*i a k)
                       0 0))))

(defun ifft-x ()
  (declare (layout (:in nil :local-size-x 16 :local-size-y 16
                        :local-size-z 1)))
  ;; calculate IFFT by swapping RE/IM on input and output, and
  ;; dividing result by N
  (let ((x (.x gl-global-invocation-id))
        (y (.y gl-global-invocation-id))
        (z (.z gl-global-invocation-id))
        (lx (.x gl-local-invocation-id))
        (ly (.y gl-local-invocation-id)))
    (loadxt x y z lx)
    (barrier)
    ;; 256 pt
    ;; = 16pt fft
    (barrier)
    (fft16l lx ly 16)
    (barrier)
    ;;   + twiddle 16x16
    (twiddle-pass lx y 0)
    (barrier)
    ;;   + transpose 16x16
    (transposell lx y (* 16 y) 16 1)
    (barrier)
    ;; + 16 pt fft
    (fft16l lx ly 16)
    (barrier)
    ;; copy to dest image
    (dotimes (i 16)
      (let* ((y256 (+ (* y 16) i))
             (xy (ivec3 y256 x z))
             (yy (3bgl-shaders::uint y256)))
        (image-store out1 xy
                     (* #. (/ 1.0 256.0)
                        (vec4 (scratch lx yy 0 0 t)
                              (scratch lx yy 0 0)
                              0 0)))))))

(defun ifft-y ()
  (declare (layout (:in nil :local-size-x 16 :local-size-y 16
                        :local-size-z 1)))
  (let ((x (.x gl-global-invocation-id))
        (y (.y gl-global-invocation-id))
        (z (.z gl-global-invocation-id))
        (lx (.x gl-local-invocation-id))
        (ly (.y gl-local-invocation-id)))
    tex ;; fixme: compiler is missing dependencies somewhere
    ;; 256 pt
    ;; = 16pt fft
    (ifft16il x y z lx ly 16)
    (barrier)
    ;;   + twiddle 16x16
    (twiddle-pass lx y 0)
    (barrier)
    ;;   + transpose 16x16
    (transposell lx y (* 16 y) 16 1)
    (barrier)
    ;; + 16 pt fft
    (fft16l lx ly 16)
    (barrier)
    ;; copy to dest image
    (dotimes (i 16)
      (let* ((y256 (+ (* y 16) i))
             (xy (ivec3 x y256 z))
             (yy (3bgl-shaders::uint y256)))
        (image-store out1 xy
                     (* #.(/ 1.0 256.0)
                        (vec4 (scratch lx yy 0 0 t)
                              (scratch lx yy 0 0)
                              0 0)))))))

(defun ifft-z ()
  (declare (layout (:in nil :local-size-x 16 :local-size-y 1
                        :local-size-z 16)))
  (let ((x (.x gl-global-invocation-id))
        (y (.y gl-global-invocation-id))
        (z (.z gl-global-invocation-id))
        (lx (.x gl-local-invocation-id))
        (lz (.z gl-local-invocation-id)))
    tex ;; fixme: compiler is missing dependencies somewhere
    ;; 256 pt
    ;; = 16pt fft
    (ifft16ilz x y z lx lz 16)
    (barrier)
    ;;   + twiddle 16x16
    (twiddle-pass lx z 0)
    (barrier)
    ;;   + transpose 16x16
    (transposell lx z (* 16 z) 16 1)
    (barrier)
    ;; + 16 pt fft
    (fft16l lx lz 16)
    (barrier)
    ;; copy to dest image
    (dotimes (i 16)
      (let* ((z256 (+ (* z 16) i))
             (xy (ivec3 x y z256))
             (zz (3bgl-shaders::uint z256)))
        (image-store out1 xy
                     (* 1 #.(/ 1.0 256.0)
                        (vec4 (scratch lx zz 0 0 t)
                              (scratch lx zz 0 0)
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
  (declare (layout (:in nil :local-size-x 8 :local-size-y 8
                        :local-size-z 8)))
  (let* ((xyz (ivec3 (.xyz gl-global-invocation-id)))
         (a (image-load rule-in1 xyz))
         (b (image-load rule-in2 xyz))
         ;; birth/death intervals
         ;;(b1 0.278) (b2 0.365) (d1 0.267) (d2 0.445) ;; 2d from paper
         ;; (b1 0.214) (b2 0.251) (d1 0.274) (d2 0.437) ;; 3d
         (b1 0.227) (b2 0.251) (d1 0.227) (d2 0.445) ;; 3d

         ;; junk
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
    (image-store out1 xyz
                 (vec4 (* 1 (ss2 n (ssm b1 d1 m) (ssm b2 d2 m)))
                       0 0 0))))


;;; hack to check for empty screen to allow restarting
(defun count-pixels ()
  (declare (layout (:in nil :local-size-x 8 :local-size-y 8
                        :local-size-z 8)))
   (let* ((a (image-load tex (ivec3 (.xyz gl-global-invocation-id)))))
     (when (> (.x a) 0.001)
       (atomic-counter-increment counter))))
