(defpackage #:compute-test-shaders
  (:use :cl :basecode :3bgl-glsl)
  (:shadowing-import-from :3bgl-glsl :defun :defconstant :defmacro))
(in-package #:compute-test-shaders)

(uniform flag :int :location 0)
(uniform tex :image-2d :location 1 :layout (:rg32f t))
(uniform out1 :image-2d :location 2
                        :layout (:rg32f t))

;; twiddle factors for FFT, x = element in FFT, y = pass
;; probably should move to local/constant once everything works?
(uniform twiddle :image-2d :location 3 :layout (:rg32f t))

(shared scratch (:float 8192))

(defmacro scratch (x y stride offset &optional im)
  `(aref scratch (+ ,@(when im `(4096)) ,x (* 8 (+ ,y (* ,stride ,offset))))))

(defmacro scratch2 (x y stride offset)
  `(vec2 (aref scratch (+ ,x (* 8 (+ ,y (* ,stride ,offset)))))
         (aref scratch (+ 4096 ,x (* 8 (+ ,y (* ,stride ,offset)))))))

(defmacro with-fft8 (() &body body)
  `(let* ((T00 (+ (.x R0) (.x R4)))
          (T01 (+ (.x R2) (.x R6)))
          (T02 (+ T00 T01))
          (T03 (+ (.x R1) (.x R5)))
          (T04 (+ (.x R3) (.x R7)))
          (T05 (+ T03 T04))
          (T06 (+ T02 T05))
          (T07 (+ (.y r0) (.y r4)))
          (T08 (+ (.y r2) (.y r6)))
          (T09 (+ T07 T08))
          (T10 (+ (.y r1) (.y r5)))
          (T11 (+ (.y r3) (.y r7)))
          (T12 (+ T10 T11))
          (T13 (+ T09 T12))
          (T14 (- (.x R0) (.x R4)))
          (T15 (- (.y r2) (.y r6)))
          (T16 (+ T14 T15))
          (T17 (- (.x R1) (.x R5)))
          (T18 (- (.y r3) (.y r7)))
          (T19 (+ T17 T18))
          (T20 (* 0.70710677 T19))
          (T21 (- (.y r1) (.y r5)))
          (T22 (- (.x R3) (.x R7)))
          (T23 (- T21 T22))
          (T25 (fma 0.70710677 T23 T20))
          (T26 (+ T16 T25))
          (T27 (- (.y r0) (.y r4)))
          (T28 (- (.x R2) (.x R6)))
          (T29 (- T27 T28))
          (T31 (* 0.70710677 T23))
          (T32 (fma -0.70710677 T19 T31))
          (T33 (+ T29 T32))
          (T34 (- T00 T01))
          (T35 (- T10 T11))
          (T36 (+ T34 T35))
          (T37 (- T07 T08))
          (T38 (- T03 T04))
          (T39 (- T37 T38))
          (T40 (- T14 T15))
          (T41 (- T17 T18))
          (T42 (* -0.70710677 T41))
          (T43 (+ T21 T22))
          (T45 (fma 0.70710677 T43 T42))
          (T46 (+ T40 T45))
          (T47 (+ T27 T28))
          (T48 (fma -0.70710677 T43 T42))
          (T49 (+ T47 T48))
          (T50 (- T02 T05))
          (T51 (- T09 T12))
          (T52 (- T16 T25))
          (T53 (- T29 T32))
          (T54 (- T34 T35))
          (T55 (+ T37 T38))
          (T56 (- T40 T45))
          (T57 (- T47 T48)))
     ,@body))

(defun fft8il (x y lx ly stride stride2)
  (let* ((r0 (image-load tex (ivec2 x (+ y (* stride 0)))))
         (r1 (image-load tex (ivec2 x (+ y (* stride 1)))))
         (r2 (image-load tex (ivec2 x (+ y (* stride 2)))))
         (r3 (image-load tex (ivec2 x (+ y (* stride 3)))))
         (r4 (image-load tex (ivec2 x (+ y (* stride 4)))))
         (r5 (image-load tex (ivec2 x (+ y (* stride 5)))))
         (r6 (image-load tex (ivec2 x (+ y (* stride 6)))))
         (r7 (image-load tex (ivec2 x (+ y (* stride 7))))))
    (with-fft8 ()
      (progn
        ;; re
        (setf (scratch lx ly stride2 0) t06)
        (setf (scratch lx ly stride2 1) t26)
        (setf (scratch lx ly stride2 2) t36)
        (setf (scratch lx ly stride2 3) t46)
        (setf (scratch lx ly stride2 4) t50)
        (setf (scratch lx ly stride2 5) t52)
        (setf (scratch lx ly stride2 6) t54)
        (setf (scratch lx ly stride2 7) t56))
      (progn
        ;; im
        (setf (scratch lx ly stride2 0 t) t13)
        (setf (scratch lx ly stride2 1 t) t33)
        (setf (scratch lx ly stride2 2 t) t39)
        (setf (scratch lx ly stride2 3 t) t49)
        (setf (scratch lx ly stride2 4 t) t51)
        (setf (scratch lx ly stride2 5 t) t53)
        (setf (scratch lx ly stride2 6 t) t55)
        (setf (scratch lx ly stride2 7 t) t57)))))

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



(defun fft8li (x y lx ly stride)
  (let ((r0 (vec2 (scratch lx ly stride 0) (scratch lx ly stride 0 t)))
        (r1 (vec2 (scratch lx ly stride 1) (scratch lx ly stride 1 t)))
        (r2 (vec2 (scratch lx ly stride 2) (scratch lx ly stride 2 t)))
        (r3 (vec2 (scratch lx ly stride 3) (scratch lx ly stride 3 t)))
        (r4 (vec2 (scratch lx ly stride 4) (scratch lx ly stride 4 t)))
        (r5 (vec2 (scratch lx ly stride 5) (scratch lx ly stride 5 t)))
        (r6 (vec2 (scratch lx ly stride 6) (scratch lx ly stride 6 t)))
        (r7 (vec2 (scratch lx ly stride 7) (scratch lx ly stride 7 t))))
    (with-fft8 ()
      (progn
        (image-store out1 (ivec2 x (+ y (* stride 0))) (vec4 t06 t13 0 0))
        (image-store out1 (ivec2 x (+ y (* stride 1))) (vec4 t26 t33 0 0))
        (image-store out1 (ivec2 x (+ y (* stride 2))) (vec4 t36 t39 0 0))
        (image-store out1 (ivec2 x (+ y (* stride 3))) (vec4 t46 t49 0 0))
        (image-store out1 (ivec2 x (+ y (* stride 4))) (vec4 t50 t51 0 0))
        (image-store out1 (ivec2 x (+ y (* stride 5))) (vec4 t52 t53 0 0))
        (image-store out1 (ivec2 x (+ y (* stride 6))) (vec4 t54 t55 0 0))
        (image-store out1 (ivec2 x (+ y (* stride 7))) (vec4 t56 t57 0 0))))))



(defun fft8l (lx ly stride)
  (let ((r0 (vec2 (scratch lx ly stride 0) (scratch lx ly stride 0 t)))
        (r1 (vec2 (scratch lx ly stride 1) (scratch lx ly stride 1 t)))
        (r2 (vec2 (scratch lx ly stride 2) (scratch lx ly stride 2 t)))
        (r3 (vec2 (scratch lx ly stride 3) (scratch lx ly stride 3 t)))
        (r4 (vec2 (scratch lx ly stride 4) (scratch lx ly stride 4 t)))
        (r5 (vec2 (scratch lx ly stride 5) (scratch lx ly stride 5 t)))
        (r6 (vec2 (scratch lx ly stride 6) (scratch lx ly stride 6 t)))
        (r7 (vec2 (scratch lx ly stride 7) (scratch lx ly stride 7 t))))
    (with-fft8 ()
     ;; re
      (setf (scratch lx ly stride 0) t06)
      (setf (scratch lx ly stride 1) t26)
      (setf (scratch lx ly stride 2) t36)
      (setf (scratch lx ly stride 3) t46)
      (setf (scratch lx ly stride 4) t50)
      (setf (scratch lx ly stride 5) t52)
      (setf (scratch lx ly stride 6) t54)
      (setf (scratch lx ly stride 7) t56)
      ;; im
      (setf (scratch lx ly stride 0 t) t13)
      (setf (scratch lx ly stride 1 t) t33)
      (setf (scratch lx ly stride 2 t) t39)
      (setf (scratch lx ly stride 3 t) t49)
      (setf (scratch lx ly stride 4 t) t51)
      (setf (scratch lx ly stride 5 t) t53)
      (setf (scratch lx ly stride 6 t) t55)
      (setf (scratch lx ly stride 7 t) t57))))

(defun twiddle-pass (x y1 ty)
  (let* ((y (* y1 8))
         (stride 1)
         (r0 (vec2 (scratch x y stride 0) (scratch x y stride 0 t)))
         (r1 (vec2 (scratch x y stride 1) (scratch x y stride 1 t)))
         (r2 (vec2 (scratch x y stride 2) (scratch x y stride 2 t)))
         (r3 (vec2 (scratch x y stride 3) (scratch x y stride 3 t)))
         (r4 (vec2 (scratch x y stride 4) (scratch x y stride 4 t)))
         (r5 (vec2 (scratch x y stride 5) (scratch x y stride 5 t)))
         (r6 (vec2 (scratch x y stride 6) (scratch x y stride 6 t)))
         (r7 (vec2 (scratch x y stride 7) (scratch x y stride 7 t)))
         (t0 (image-load twiddle (ivec2 (+ y 0) ty)))
         (t1 (image-load twiddle (ivec2 (+ y 1) ty)))
         (t2 (image-load twiddle (ivec2 (+ y 2) ty)))
         (t3 (image-load twiddle (ivec2 (+ y 3) ty)))
         (t4 (image-load twiddle (ivec2 (+ y 4) ty)))
         (t5 (image-load twiddle (ivec2 (+ y 5) ty)))
         (t6 (image-load twiddle (ivec2 (+ y 6) ty)))
         (t7 (image-load twiddle (ivec2 (+ y 7) ty))))

    (progn

      (setf (scratch x y stride 0) (- (* (.x r0) (.x t0)) (* (.y r0) (.y t0))))
      (setf (scratch x y stride 1) (- (* (.x r1) (.x t1)) (* (.y r1) (.y t1))))
      (setf (scratch x y stride 2) (- (* (.x r2) (.x t2)) (* (.y r2) (.y t2))))
      (setf (scratch x y stride 3) (- (* (.x r3) (.x t3)) (* (.y r3) (.y t3))))
      (setf (scratch x y stride 4) (- (* (.x r4) (.x t4)) (* (.y r4) (.y t4))))
      (setf (scratch x y stride 5) (- (* (.x r5) (.x t5)) (* (.y r5) (.y t5))))
      (setf (scratch x y stride 6) (- (* (.x r6) (.x t6)) (* (.y r6) (.y t6))))
      (setf (scratch x y stride 7) (- (* (.x r7) (.x t7)) (* (.y r7) (.y t7))))

      (setf (scratch x y stride 0 t)(+ (* (.y r0) (.x t0)) (* (.x r0) (.y t0))))
      (setf (scratch x y stride 1 t)(+ (* (.y r1) (.x t1)) (* (.x r1) (.y t1))))
      (setf (scratch x y stride 2 t)(+ (* (.y r2) (.x t2)) (* (.x r2) (.y t2))))
      (setf (scratch x y stride 3 t)(+ (* (.y r3) (.x t3)) (* (.x r3) (.y t3))))
      (setf (scratch x y stride 4 t)(+ (* (.y r4) (.x t4)) (* (.x r4) (.y t4))))
      (setf (scratch x y stride 5 t)(+ (* (.y r5) (.x t5)) (* (.x r5) (.y t5))))
      (setf (scratch x y stride 6 t)(+ (* (.y r6) (.x t6)) (* (.x r6) (.y t6))))
      (setf (scratch x y stride 7 t)(+ (* (.y r7) (.x t7)) (* (.x r7) (.y t7)))))))


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
    (progn
      (setf (scratch lx dy dstride 0) (.x r0))
      (setf (scratch lx dy dstride 1) (.x r1))
      (setf (scratch lx dy dstride 2) (.x r2))
      (setf (scratch lx dy dstride 3) (.x r3))
      (setf (scratch lx dy dstride 4) (.x r4))
      (setf (scratch lx dy dstride 5) (.x r5))
      (setf (scratch lx dy dstride 6) (.x r6))
      (setf (scratch lx dy dstride 7) (.x r7)))
    (progn
      (setf (scratch lx dy dstride 0 t) (.y r0))
      (setf (scratch lx dy dstride 1 t) (.y r1)
            )
      (setf (scratch lx dy dstride 2 t) (.y r2))
      (setf (scratch lx dy dstride 3 t) (.y r3))
      (setf (scratch lx dy dstride 4 t) (.y r4))
      (setf (scratch lx dy dstride 5 t) (.y r5))
      (setf (scratch lx dy dstride 6 t) (.y r6))
      (setf (scratch lx dy dstride 7 t) (.y r7)))))

(defun compute ()
  (declare (layout (:in nil :local-size-x 8 :local-size-y 64)))
  (let ((x (.x gl-global-invocation-id))
        (y (.y gl-global-invocation-id))
        (lx (.x gl-local-invocation-id))
        (ly (.y gl-local-invocation-id)))
    (when (= flag 3)
     (progn
       (dotimes (i 8)
         (setf (scratch lx (* y 8) 1 i ) (/ (float i) 256.0))
         (setf (scratch lx (* y 8) 1 i t) (/ (float (max 0 (- y 0)))
                                             (* 64 8.0 2))))

       (barrier)))
    ;; fft in X

    (when (= flag 3)
      (loadx x y lx)
      (barrier)
      (dotimes (i 1)
        ;; 512 pt
        ;; = 8pt fft
;        (loadx x y lx)
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
        ;; display
        ))

    ;; fft in Y
    (when (= flag 1)
      (dotimes (i 1)
        ;; 512 pt
        ;; = 8pt fft
        (fft8il x y lx ly 64 64)
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
        ;; display
        ))
    ;; fixme: this should be specific for x/y pass
    (when (> flag 0)
      (dotimes (i 8)
        (let* ((y512 (+ (* y 8) i))
               (xy (ivec2 x y512))
               (yy (3bgl-shaders::uint ;y512
                                       (+ (* 64 (mod y512 8))
                                          (floor (/ y512 8))))))
          (image-store out1 xy
                       (* 1(vec4 (scratch lx yy 0 0)
                                 (scratch lx yy 0 0 t)
                                 0 0))))))

    (when (= flag 0)
      ;; draw a centered cone for initialization
      #++
      (dotimes (i 8)
        (let ((xy (ivec2 x (+ i (* y 8)))))
          (image-store out1 xy
                       (max (vec4 0)
                            (vec4 (- 1 (/ (length (- (vec2 xy) (vec2 256 256)))
                                          200))
                                  0 0 0)))))
      ;; cone at corner
      #++
      (dotimes (i 8)
        (let* ((xy (ivec2 x (+ i (* y 8))))
               (cxy (vec2  (mod (+ x 256) 512)
                           (mod (+ (+ i (* y 8)) 256) 512))))
          (image-store out1 xy
                       (max (vec4 0)
                            (vec4 (max 0
                                       (- 1 (/ (length (- cxy (vec2 256 256)))
                                               200)))
                                  0 0 0)))))

      ;; gaussian
      
      (dotimes (i 8)
        (let* ((xy (ivec2 x (+ i (* y 8))))
               (c (vec2 256 256))
               (dxy (- (vec2 (mod (+ xy 256) 512))
                       c)))
          (image-store out1 xy
                       (max (vec4 0)
                            (vec4 (exp (- (/ (float (dot dxy dxy))
                                             (float (* 2 32 32)))))
                                  0 0 0)))))
      #++
      (dotimes (i 8)
        (let* ((xy (ivec2 x (+ i (* y 8))))
               (y (.y xy)))
          (image-store out1 xy
                       (max (vec4 0)
                            (vec4 (exp (- (/ (float (* (- y 0) (- y 0)))
                                             (float (* 2 8 8)))))
                                  0 0 0))))))))



