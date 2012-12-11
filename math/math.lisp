(in-package #:3bgl-math)

(declaim (inline deg-to-rad))
(defun deg-to-rad (x)
  (typecase x
    (single-float
     (float (* x (/ pi 180.0)) 1.9))
    (t (* x (/ pi 180)))))


(defun perspective-matrix (fovy-degrees aspect z-near z-far)
  (let ((f (float (/ (tan (/ (deg-to-rad fovy-degrees) 2))) 1.0))
        (dz (- z-near z-far)))
    (matrix (/ f aspect) 0.0 0.0 0.0
            0.0 f 0.0 0.0
            0.0 0.0 (/ (+ z-near z-far) dz) (/ (* 2 z-near z-far) dz)
            0.0 0.0 -1.0 0.0)))

(declaim (inline copy-matrix))
(defun copy-matrix (m)
  (matrix (aref m 0) (aref m 4) (aref m 8) (aref m 12)
          (aref m 1) (aref m 5) (aref m 9) (aref m 13)
          (aref m 2) (aref m 6) (aref m 10) (aref m 14)
          (aref m 3) (aref m 7) (aref m 11) (aref m 15)))



