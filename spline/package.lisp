(defpackage #:3bgl-splines
  (:use :cl)
  (:export #:interpolate-quadratic
           #:evaluate-quadratic
           #:evaluate-quadratic-normal
           #:evaluate-quadratic-tangent
           #:subdivide-quadratic))