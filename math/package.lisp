(defpackage #:3bgl-math
  (:use #:cl #:sb-cga)
  (:export
   #:deg-to-rad
   #:copy-matrix
   #:perspective-matrix
   #:ortho-matrix
   #:look-at))