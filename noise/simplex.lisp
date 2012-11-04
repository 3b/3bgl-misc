(in-package #:3bgl-noise)
;;; perlin 2001 simplex noise
;;; see http://www.cs.umbc.edu/~olano/s2002c36/ch02.pdf
;;; and http://www.itn.liu.se/~stegu/simplexnoise/simplexnoise.pdf

;;; direct translation of the java code from perlin 2001 for reference
(alexandria:define-constant +bit-patterns+
    (make-array 8 :element-type '(unsigned-byte 8)
                :initial-contents '(#x15 #x38 #x32 #x2c #x0d #x13 #x07 #x2a))
  :test 'equalp)

(defparameter *hist* (make-array 64 :initial-element 0))

(defun simplex-3d-perlin-reference (x y z)
  (declare (optimize speed)
           ((single-float #. (* 1.0 most-negative-fixnum)
                          #. (* 1.0 most-positive-fixnum)) x y z))
  (let* ((x (float x 1f0))
         (y (float y 1f0))
         (z (float z 1f0))
         (a (make-array 3 :element-type '(unsigned-byte 32) :initial-element 0))
         (s (* (+ x y z) (/ 3f0)))
         ;;(i (mod (floor (+ x s)) 256))
         ;;(j (mod (floor (+ y s)) 256))
         ;;(k (mod (floor (+ z s)) 256))
         (i (floor (rem (+ x s) (expt 2 31))))
         (j (floor (rem (+ y s) (expt 2 31))))
         (k (floor (rem (+ z s) (expt 2 31))))
         (/6d0 (/ 6f0))
         (s (* (+ i j k) /6d0))
         (u (+ (- x i) s))
         (v (+ (- y j) s))
         (w (+ (- z k) s))
         (hi (if (>= u w) (if (>= u v) 0 1) (if (>= v w) 1 2)))
         (lo (if (< u w) (if (< u v) 0 1) (if (< v w) 1 2))))
    (declare #++(optimize speed)
             (type (signed-byte 32) i j k)
             (dynamic-extent a)
             (single-float x y z s u v w))
    (labels ((b (i j k b)
               (aref +bit-patterns+
                     (dpb (ldb (byte 1 b) i) (byte 1 2)
                          (dpb (ldb (byte 1 b) j) (byte 1 1)
                               (ldb (byte 1 b) k)))))
             (shuffle (i j k)
               (let ((i (mod i 256))
                     (j (mod j 256))
                     (k (mod k 256)))
                 (ldb (byte 6 0)
                      (+ (b i j k 0)
                         (b j k i 1)
                         (b k i j 2)
                         (b i j k 3)
                         (b j k i 4)
                         (b k i j 5)
                         (b i j k 6)
                         (b j k i 7)))))
             (k (ka)
               (let* ((a0 (aref a 0))
                      (a1 (aref a 1))
                      (a2 (aref a 2))
                      (s (* (+ a0 a1 a2) /6d0))
                      (x (+ (- u a0) s))
                      (y (+ (- v a1) s))
                      (z (+ (- w a2) s))
                      (tt (- 0.6f0 (expt x 2) (expt y 2) (expt z 2)))
                      (h 0))
                 (incf (aref a ka))
                 (when (< tt 0)
                   (return-from k 0f0))
                 (setf h (shuffle (+ i a0) (+ j a1) (+ k a2)))
                 (let* ((b5 (logbitp 5 h))
                        (b4 (logbitp 4 h))
                        (b3 (logbitp 3 h))
                        (b2 (logbitp 2 h))
                        (b (ldb (byte 2 0) h))
                        ;; store xyz in array and index by b?
                        (p (if (= b 1) x (if (= b 2) y z)))
                        (q (if (= b 1) y (if (= b 2) z x)))
                        (r (if (= b 1) z (if (= b 2) x y))))
                   (when (eq b5 b3) (setf p (- p)))
                   (when (eq b5 b4) (setf q (- q)))
                   (when (eq b5 (eq b3 b4)) (setf r (- r)))
                   (setf tt (expt tt 2))
                   (* 8 tt tt (+ p (if (zerop b)
                                       (+ q r)
                                       (if b2 r q))))))))
      (+ (k hi) (k (- 3 hi lo)) (k lo) (k 0)))))



;;; 3d simplex noise from 'simplex noise demystified'
;;; like perlin's, except uses permutation vector instead of bit tricks
;;; to hash coordinates
(alexandria:define-constant +permutation-vector+
    ;; permutation vector from 'improved perlin noise' and 'simplex
    ;; noise demystified', any random permutation of 0..255 should
    ;; work though
    ;; repeated twice to avoid needing to wrap indices
    (make-array 512 :element-type '(unsigned-byte 8)
                :initial-contents
                (let ((v '(151 160 137 91 90 15 131 13 201 95 96 53 194
                           233 7 225 140 36 103 30 69 142 8 99 37 240 21
                           10 23 190 6 148 247 120 234 75 0 26 197 62 94
                           252 219 203 117 35 11 32 57 177 33 88 237 149
                           56 87 174 20 125 136 171 168 68 175 74 165 71
                           134 139 48 27 166 77 146 158 231 83 111 229
                           122 60 211 133 230 220 105 92 41 55 46 245 40
                           244 102 143 54 65 25 63 161 1 216 80 73 209
                           76 132 187 208 89 18 169 200 196 135 130 116
                           188 159 86 164 100 109 198 173 186 3 64 52
                           217 226 250 124 123 5 202 38 147 118 126 255
                           82 85 212 207 206 59 227 47 16 58 17 182 189
                           28 42 223 183 170 213 119 248 152 2 44 154
                           163 70 221 153 101 155 167 43 172 9 129 22 39
                           253 19 98 108 110 79 113 224 232 178 185 112
                           104 218 246 97 228 251 34 242 193 238 210 144
                           12 191 179 162 241 81 51 145 235 249 14 239
                           107 49 192 214 31 181 199 106 157 184 84 204
                           176 115 121 50 45 127 4 150 254 138 236 205 93
                           222 114 67 29 24 72 243 141 128 195 78 66 215
                           61 156 180)))
                  (concatenate 'list v v)))
  :test 'equalp)
(alexandria:define-constant +cube-edge-vectors+
    ;; gradient vectors for improved perlin noise, gustavson simplex
    ;; noise, etc: centers of edges of a cube
    (make-array 12 :element-type '(simple-array (signed-byte 8) (3))
                :initial-contents
                (mapcar (lambda (a) (coerce a '(simple-array (signed-byte 8) (3))))
                        '((1 1 0) (-1 1 0) (1 -1 0) (-1 -1 0)
                          (1 0 1) (-1 0 1) (1 0 -1) (-1 0 -1)
                          (0 1 1) (0 -1 1) (0 1 -1) (0 -1 -1))))
  :test 'equalp)
(declaim (type (simple-array (simple-array (signed-byte 8) (3)) (12))
               +cube-edge-vectors+))

(defun simplex-3d-gustavson (x y z)
  (declare (optimize speed))
  (let* ((x32 (rem (coerce x 'double-float) (expt 2 31)))
         (y32 (rem (coerce y 'double-float) (expt 2 31)))
         (z32 (rem (coerce z 'double-float) (expt 2 31)))
         (/2 (/ 2d0))
         (/3 (/ 3d0))
         (/6 (/ 6d0))
         ;; skew coordinates to simplex space, ijk = coordinates of
         ;; simplex cell containing x,y,z
         (s (* (+ x32 y32 z32) /3))
         (i (floor (+ x32 s)))
         (j (floor (+ y32 s)))
         (k (floor (+ z32 s)))
         ;; skew corner of simplex cell back to original coordinate system,
         ;; and find offset of point within cell
         (tt (* (+ i j k) /6))
         (x0 (+ (- x32 i) tt))
         (y0 (+ (- y32 j) tt))
         (z0 (+ (- z32 k) tt)))
    (multiple-value-bind (i1 j1 k1 i2 j2 k2)
        (if (>= x0 y0)
            (if (>= y0 z0)
                (values 1 0 0  1 1 0)
                (if (>= x0 z0)
                    (values 1 0 0  1 0 1)
                    (values 0 0 1  1 0 1)))
            (if (< y0 z0)
                (values 0 0 1  0 1 1)
                (if (< x0 z0)
                    (values 0 1 0  0 1 1)
                    (values 0 1 0  1 1 0))))
      (labels ((perm (i j k)
                 (mod (aref +permutation-vector+
                            (+ i (aref +permutation-vector+
                                       (+ j (aref +permutation-vector+ k)))))
                      12)))
        (let* ((ii (mod i 256))
               (jj (mod j 256))
               (kk (mod k 256)))
          (labels ((dot (v x y z)
                     (+ (* (aref v 0) x)
                        (* (aref v 1) y)
                        (* (aref v 2) z)))
                   (corner (di dj dk g)
                     (let* ((xc (+ (- x0 di) g))
                            (yc (+ (- y0 dj) g))
                            (zc (+ (- z0 dk) g))
                            (tc (- 0.6 (expt xc 2) (expt yc 2) (expt zc 2))))
                       (if (< tc 0)
                           0d0
                           (let ((g (aref +cube-edge-vectors+
                                          (perm (+ ii di)
                                                (+ jj dj)
                                                (+ kk dk))))
                                 (tc (expt tc 2)))
                             (declare (type (simple-array (signed-byte 8) (3))
                                            g))
                             (* tc tc (dot g xc yc zc)))))))
            (coerce
             (* 32 (+ (corner 0 0 0 0d0)
                      (corner i1 j1 k1 /6)
                      (corner i2 j2 k2 /3)
                      (corner 1 1 1 /2)))
             'single-float)))))))

;;; 2d simplex noise from 'simplex noise demystified'
;;; reuses permutation and vectors from 3d
(defun simplex-2d-gustavson (x y)
  (declare (optimize speed))
  (let* ((x32 (rem (coerce x 'double-float) (expt 2 31)))
         (y32 (rem (coerce y 'double-float) (expt 2 31)))
         ;; skew factor to convert to simplex space
         (f2 (* 0.5d0 (1- (sqrt 3d0))))
         ;; and to convert back
         (g2 (/ (- 3d0 (sqrt 3d0)) 6d0))
         ;; skew coordinates to simplex space, ij = coordinates of
         ;; simplex cell containing x,y
         (s (* (+ x32 y32) f2))
         (i (floor (+ x32 s)))
         (j (floor (+ y32 s)))
         ;; skew corner of simplex cell back to original coordinate system,
         ;; and find offset of point within cell
         (tt (* (+ i j) g2))
         (x0 (+ (- x32 i) tt))
         (y0 (+ (- y32 j) tt))
         ;; offset of 2nd corner in simplex space
         (i1 (if (> x0 y0) 1 0))
         (j1 (- 1 i1)))
    (labels ((perm (i j)
               (mod (aref +permutation-vector+
                          (+ i (aref +permutation-vector+ j)))
                    12)))
      (let* ((ii (mod i 256))
             (jj (mod j 256)))
        (labels ((dot (v x y)
                   (+ (* (aref v 0) x)
                      (* (aref v 1) y)))
                 (corner (di dj g)
                   (let* ((xc (+ (- x0 di) g))
                          (yc (+ (- y0 dj) g))
                          (tc (- 0.5 (expt xc 2) (expt yc 2))))
                     (if (< tc 0)
                         0d0
                         (let ((g (aref +cube-edge-vectors+
                                        (perm (+ ii di)
                                              (+ jj dj))))
                               (tc (expt tc 2)))
                           (declare (type (simple-array (signed-byte 8) (3))
                                          g))
                           (* tc tc (dot g xc yc)))))))
          (coerce
           (* 70 (+  (/ (mod ii 2) 2.0)
                     (/ (mod jj 2) 4.0)
                     #++(corner 0 0 0d0)
                    #++(corner i1 j1 g2)
                    #++(corner 1 1 (* 2 g2))))
           'single-float))))))


;;; 4d simplex noise, similar to 'simplex noise demystified'

(alexandria:define-constant +hypercube-edge-vectors+
    ;; 4d version of +cube-edge-vectors+
    (make-array 32 :element-type '(simple-array (signed-byte 8) (4))
                :initial-contents
                (mapcar (lambda (a)
                          (coerce a '(simple-array (signed-byte 8) (4))))
                        '((0 1 1 1) (0 1 1 -1) (0 1 -1 1) (0 1 -1 -1)
                          (0 -1 1 1) (0 -1 1 -1) (0 -1 -1 1) (0 -1 -1 -1)
                          (1 0 1 1) (1 0 1 -1) (1 0 -1 1) (1 0 -1 -1)
                          (-1 0 1 1) (-1 0 1 -1) (-1 0 -1 1) (-1 0 -1 -1)
                          (1 1 0 1) (1 1 0 -1) (1 -1 0 1) (1 -1 0 -1)
                          (-1 1 0 1) (-1 1 0 -1) (-1 -1 0 1) (-1 -1 0 -1)
                          (1 1 1 0) (1 1 -1 0) (1 -1 1 0) (1 -1 -1 0)
                          (-1 1 1 0) (-1 1 -1 0) (-1 -1 1 0) (-1 -1 -1 0))))
  :test 'equalp)
(declaim (type (simple-array (simple-array (signed-byte 8) (4)) (32))
               +hypercube-edge-vectors+))

(defun simplex-4d (x y z w)
  (declare (optimize speed))
  (let* ((x32 (rem (coerce x 'double-float) (expt 2 31)))
         (y32 (rem (coerce y 'double-float) (expt 2 31)))
         (z32 (rem (coerce z 'double-float) (expt 2 31)))
         (w32 (rem (coerce w 'double-float) (expt 2 31)))
         ;; skew factor to convert to simplex space
         (f4 (/ (- (sqrt 5d0) 1d0) 4d0))
         ;; and to convert back
         (g4 (/ (- 5d0 (sqrt 5d0)) 20d0))
         ;; skew coordinates to simplex space, ijk = coordinates of
         ;; simplex cell containing x,y,z
         (s (* (+ x32 y32 z32 w32) f4))
         (i (floor (+ x32 s)))
         (j (floor (+ y32 s)))
         (k (floor (+ z32 s)))
         (l (floor (+ w32 s)))
         ;; skew corner of simplex cell back to original coordinate system,
         ;; and find offset of point within cell
         (tt (* (+ i j k l) g4))
         (xyzw (make-array 4 :element-type 'double-float
                           :initial-contents (list (+ (- x32 i) tt)
                                                   (+ (- y32 j) tt)
                                                   (+ (- z32 k) tt)
                                                   (+ (- w32 l) tt))))
         (edge-order (make-array 4 :element-type '(unsigned-byte 8)
                                 :initial-contents '(0 1 2 3))))
    (declare (dynamic-extent xyzw edge-order))
    ;; sorting network to decide which order to traverse edges of
    ;; simplex cell to define correct simplex... possibly would be
    ;; better to do 6 compares, and use that to build an index into a
    ;; predefined table of orders, similar to the paper?
    (flet ((comp (a b)
             (when (<= (aref xyzw (aref edge-order a))
                       (aref xyzw (aref edge-order b)))
               (rotatef (aref edge-order a) (aref edge-order b)))))
      (comp 0 1)
      (comp 2 3)
      (comp 0 2)
      (comp 1 3)
      (comp 1 2))
    (assert (>= (aref xyzw (aref edge-order 0))
                (aref xyzw (aref edge-order 1))
                (aref xyzw (aref edge-order 2))
                (aref xyzw (aref edge-order 3))
                ))

    (labels ((perm (i j k l)
               (mod (aref +permutation-vector+
                          (+ i (aref +permutation-vector+
                                     (+ j (aref +permutation-vector+
                                                (+ k (aref +permutation-vector+
                                                           l)))))))
                    32)))
      (let* ((ijkl (make-array 4 :element-type '(unsigned-byte 8)
                               :initial-contents (list (mod i 256)
                                                       (mod j 256)
                                                       (mod k 256)
                                                       (mod l 256)))))
        (declare (dynamic-extent ijkl))
        (labels ((dot (v x y z w)
                   (+ (* (aref v 0) x)
                      (* (aref v 1) y)
                      (* (aref v 2) z)
                      (* (aref v 3) w)))
                 ;; calculate contribution of current corner, and
                 ;; advance xyzw to next corner
                 (corner (n)
                   (let* ((xc (aref xyzw 0))
                          (yc (aref xyzw 1))
                          (zc (aref xyzw 2))
                          (wc (aref xyzw 3))
                          (ic (aref ijkl 0))
                          (jc (aref ijkl 1))
                          (kc (aref ijkl 2))
                          (lc (aref ijkl 3))
                          (tc (- 0.6 (expt xc 2) (expt yc 2) (expt zc 2)
                                 (expt wc 2))))
                     (when n
                       (incf (aref xyzw (aref edge-order n)) -1)
                       (setf (aref ijkl (aref edge-order n))
                                (mod (1+ (aref ijkl (aref edge-order n))) 256))
                       (loop for i below 4 do (incf (aref xyzw i) g4)))
                     (if (< tc 0)
                         0d0
                         (let ((g (aref +hypercube-edge-vectors+
                                        (perm ic jc kc lc)))
                               (tc (expt tc 2)))
                           (declare (type (simple-array (signed-byte 8) (4))
                                          g))
                           (* tc tc (dot g xc yc zc wc)))))))
          (coerce
           (* 27 (+ (corner 0) (corner 1) (corner 2) (corner 3) (corner nil)))
           'single-float))))))

