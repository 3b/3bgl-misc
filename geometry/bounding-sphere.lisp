(in-package #:3bgl-geometry)

;;; some bounding-sphere algorithms

;;; todo:
;; try using list for storing point list so we can get O(1)
;;   move-to-front? (might not be worth it, since with pivot version
;;   we usually only do a few small moves)
;; implement EPOS approximation from 'Fast and Tight Bounding Spheres', Larsson
;; see if vector of 3xN floats is faster than vector of sb-cga:vec?

#++
(defparameter *updates* 0)
#++(declaim (type fixnum *updates*))

;; Welzl move-to-front miniball, simplified a bit for 3d only
(defun mtf-mb (points &key (end (length points)) (dim 0) p1 p2 p3)
  #++(when (= dim 0) (setf *updates* 0))
  (multiple-value-bind (r c)
      (%mtf-mb points :end end :dim dim :p1 p1 :p2 p2 :p3 p3)
    (values (sqrt r) c)))

(defun %mtf-mb (points &key (end (length points)) (dim 0) p1 p2 p3)
  (declare (optimize debug speed))
  (declare ((simple-array sb-cga:vec (*)) points)
           (type (integer 0 5) dim)
           (fixnum end))
  (let ((r -1.0)
        (c (sb-cga:vec 0.0 0.0 0.0)))
    (declare (sb-cga:vec c)
             (single-float r))
    (labels ((d^2 (p)
               (declare (sb-cga:vec p))
               (sb-cga:dot-product p p))
             (mb0 (p)
                  (declare (sb-cga:vec p))
                  (setf (aref c 0) (aref p 0)
                        (aref c 1) (aref p 1)
                        (aref c 2) (aref p 2))
                  (setf r 0.0
                        p1 p))
             (mb1 (p)
               (declare (sb-cga:vec p))
               (let ((d (sb-cga:vec- p p1)))
                 (declare (dynamic-extent d))
                 (sb-cga:%vec-lerp c p1 p 0.5)
                 (setf r (/ (d^2 d) 4)
                       p2 p)))
             (mb2 (p)
               (declare (sb-cga:vec p))
               (setf p3 p)
               (let* ((p1-p2 (sb-cga:vec- p1 p2))
                      (p2-p3 (sb-cga:vec- p2 p3))
                      (d (d^2
                          (sb-cga:cross-product p1-p2 p2-p3))))
                 (declare (dynamic-extent p1-p2 p2-p3)
                          (sb-cga:vec p1-p2 p2-p3)
                          (single-float d))
                 (when (< (abs d) 0.00001) ;; fixme: make constant configurable
                 ;; points are too close together, or collinear, not sure
                 ;; what 'correct' way to handle it it, so for now just
                 ;; return an empty disc on the assumption it isn't
                 ;; adding much meaningful info anyway...
                 (return-from mb2 (values -1.0 (sb-cga:vec 0.0 0.0 0.0))))
                 ;; calculate circle through b1,b2,b3
                 (let* ((p2-p1 (sb-cga:vec- p2 p1))
                        (p3-p1 (sb-cga:vec- p3 p1))
                        (lp1-p2 (d^2 p1-p2))
                        (lp2-p3 (d^2 p2-p3))
                        (lp3-p1 (d^2 p3-p1))
                        (p1-p3 (sb-cga:vec- p1 p3))
                        (lp1-p3 (d^2 p1-p3))
                        (p3-p2 (sb-cga:vec- p3 p2))
                        (2d^2 (* 2 d))
                        (pa (/ (* lp2-p3
                                  (sb-cga:dot-product p1-p2 p1-p3))
                               2d^2))
                        (pb (/ (* lp1-p3
                                  (sb-cga:dot-product p2-p1 p2-p3))
                               2d^2))
                        (pc (/ (* lp1-p2
                                  (sb-cga:dot-product p3-p1 p3-p2))
                               2d^2)))
                   (declare (sb-cga:vec p2-p1 p3-p1 p1-p3 p3-p2)
                            (single-float r 2d^2 pa pb pc)
                            (type (single-float 0.0) lp1-p2 lp1-p3 lp2-p3)
                            (dynamic-extent p2-p1 p3-p1 p1-p3 p3-p2))
                   ;; fixme: figure out if sb-cga can avoid consing
                   ;; here, fix if not
                   (sb-cga:%vec+ c
                                 (sb-cga:vec+ (sb-cga:vec* p1 pa)
                                              (sb-cga:vec* p2 pb))
                                 (sb-cga:vec* p3 pc))

                   (setf r (/ (* lp1-p2 lp2-p3 lp3-p1)
                              (* 4 d)))
                   (values r c))))
             (mb3 (p4)
               (declare (sb-cga:vec p4))
               (let* ((l1 (d^2 p1))
                      (l2 (d^2 p2))
                      (l3 (d^2 p3))
                      (l4 (d^2 p4))
                      (x1 (aref p1 0))
                      (y1 (aref p1 1))
                      (z1 (aref p1 2))
                      (x2 (aref p2 0))
                      (y2 (aref p2 1))
                      (z2 (aref p2 2))
                      (x3 (aref p3 0))
                      (y3 (aref p3 1))
                      (z3 (aref p3 2))
                      (x4 (aref p4 0))
                      (y4 (aref p4 1))
                      (z4 (aref p4 2))
                      (xyz1 (sb-cga:matrix-determinant
                             (sb-cga:matrix x1 y1 z1 1.0
                                            x2 y2 z2 1.0
                                            x3 y3 z3 1.0
                                            x4 y4 z4 1.0))))
                 (declare (single-float l1 l2 l3 l4 x1 x2 x3 y1 y2 y3 z1 z2 z3
                                        xyz1))
                 ;; make sure we have reasonable values
                 (when (< (abs xyz1) 0.0001)
                   (return-from mb3 (values -3.0 c)))
                 ;; if so, calculate radius/coords
                 (let* ((lyz1 (sb-cga:matrix-determinant
                               (sb-cga:matrix l1 y1 z1 1.0
                                              l2 y2 z2 1.0
                                              l3 y3 z3 1.0
                                              l4 y4 z4 1.0)))
                        (lxy1 (sb-cga:matrix-determinant
                               (sb-cga:matrix l1 x1 y1 1.0
                                              l2 x2 y2 1.0
                                              l3 x3 y3 1.0
                                              l4 x4 y4 1.0)))
                        (lxz1 (sb-cga:matrix-determinant
                               (sb-cga:matrix l1 x1 z1 1.0
                                              l2 x2 z2 1.0
                                              l3 x3 z3 1.0
                                              l4 x4 z4 1.0)))
                        (lxyz (sb-cga:matrix-determinant
                               (sb-cga:matrix l1 x1 y1 z1
                                              l2 x2 y2 z2
                                              l3 x3 y3 z3
                                              l4 x4 y4 z4)))
                        (x (/ lyz1 xyz1 2.0))
                        (y (- (/ lxz1 xyz1 2.0)))
                        (z (/ lxy1 xyz1 2.0))
                        (r^2 (+ (expt x 2) (expt y 2) (expt z 2)
                                (- (/ lxyz xyz1)))))
                   (declare (single-float lyz1 lxz1 lxy1 lxyz x y z r^2))
                   (when (plusp r^2)
                     (setf r r^2
                           (aref c 0) x
                           (aref c 1) y
                           (aref c 2) z)
                     ;; do we actually need the return values?
                     (values r c)))))
             (dd (a b)
               (d^2 (sb-cga:vec- a b)))
             (mtf (x)
               (declare (fixnum x) (ignorable x))
               #++(incf *updates*)
               (let ((a (aref points x)))
                 (replace points points :start1 1 :start2 0 :end2 x)
                 (setf (aref points 0) a))))
      (declare (inline dd d^2 mb0 mb1 mb2 mb3 mtf))
      (case dim
        (1 (mb0 p1))
        (2 (mb1 p2))
        (3 (mb2 p3)))
      (loop for p across points
            for i below end
            for d = (dd p c)
            when (> d (+ r 0.00005))
              do (case dim
                   (0
                    (multiple-value-bind (r2 c2)
                        (%mtf-mb points :end i :dim (1+ dim) :p1 p)
                      (declare (single-float r2) (sb-cga:vec c2))
                      (mtf i)
                      (setf r r2 c c2)))
                   (1
                    (multiple-value-bind (r2 c2)
                        (%mtf-mb points :end i :dim (1+ dim) :p1 p1 :p2 p)
                      (declare (single-float r2) (sb-cga:vec c2))
                      (mtf i)
                      (setf r r2 c c2)))
                   (2
                    (multiple-value-bind (r2 c2)
                        (%mtf-mb points :end i :dim (1+ dim) :p1 p1 :p2 p2 :p3 p)
                      (declare (single-float r2) (sb-cga:vec c2))
                      (mtf i)
                      (setf r r2 c c2)))
                   (3
                    (mb3 p)
                    (mtf i))))
      (values r c))))

#++
(defparameter *pivots* 0)
(defun %mtf (points x)
  (declare ((simple-array sb-cga:vec (*)) points))
  (declare (fixnum x) (optimize speed))
  #++(incf *updates*)
  (let ((a (aref points x)))
    (replace points points :start1 1 :start2 0 :end2 x)
    (setf (aref points 0) a)))

;; heuristic optimization from
;; 'fast and robust smallest enclosing balls', gaertner 99
;; with additional early exit for faster approximation within
;; specified tolerance
(defun mtf-pivot (points &key (max-error 0.0))
  (declare (optimize speed))
  (declare ((simple-array sb-cga:vec (*)) points))
  #++(setf *pivots* 0
           *updates* 0)
  (let ((tmp (sb-cga:vec 0.0 0.0 0.0))
        (max-error (float max-error 0.0)))
    (declare (dynamic-extent tmp)
             (sb-cga:vec tmp))
    (labels ((d^2 (p)
               (declare (sb-cga:vec p))
               (sb-cga:dot-product p p))
             (dd (a b)
               (d^2 (sb-cga:%vec- tmp a b))))
      (loop with end = (length points)
            for i from 1 below end
            for (r^2 c)
            of-type ((single-float 0.0) sb-cga:vec)
              = (multiple-value-list
                 (%mtf-mb points :end i))
            for (best d-best) of-type (fixnum (single-float 0.0))
              = (loop
                  with d-best = 0.0
                  with best = nil
                  for j from i below end
                  for d = (dd (aref points j) c)
                  when (> d d-best)
                    do (setf best j d-best d)
                  finally (when (or (not best) (<= d-best r^2))
                            (return-from mtf-pivot
                              (values (sqrt r^2) c)))
                          (return (list best d-best)))
            do #++(incf *pivots*)
              (let ((r (sqrt r^2))
                    (d (sqrt d-best)))
                (when (< (- d r) (* d max-error))
                  (return-from mtf-pivot
                    (values d c))))
              ;; since we have vector of points instead of lists, optimize
              ;; move-to-front a bit by swapping with something closer
              ;; to the front of list first if possible
              ;; (usually only have a few swaps, but from arbitrary
              ;;  points, so this way we usually only shift a few
              ;;  entries instead of O(N/2) entries)
              (when (> best (+ i 2))
                (rotatef (aref points (1+ i)) (aref points best))
                (setf best (1+ i)))
              (%mtf points best)))))

