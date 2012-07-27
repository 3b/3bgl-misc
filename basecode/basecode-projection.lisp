(in-package #:basecode)

;;; todo: adjust projection matrix when parameters are changed...

;; fixed size ortho projection (ORTHO-PROJECTION-SCALE unit square
;; centered at 0,0,0 is scaled to fit in view area)
(defclass ortho-projection-fixed ()
  ((ortho-projection-scale :initform 1.0 :initarg ortho-projection-scale
                           :accessor ortho-projection-scale)))

(defmethod basecode-reshape :before ((w ortho-projection-fixed))
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((right (* (ortho-projection-scale w)
                  (/ (max (aspect w) 1.0) 2)))
	(top (* (ortho-projection-scale w)
                (/ (max (/ (aspect w)) 1.0) 2))))
    (glu:ortho-2d (- right) right (- top) top))
  (gl:matrix-mode :modelview))


;; fixed scale ortho projection (scaled so 1 unit is ORTHO-PROJECTION-SCALE
;; pixels, with 0,0,0 at center of screen)
(defclass ortho-projection-pixel ()
  ((ortho-projection-scale :initform 1.0 :initarg ortho-projection-scale
                           :accessor ortho-projection-scale)
   (ortho-projection-origin :initform :center :initarg :ortho-projection-origin
                            :accessor ortho-projection-origin)))

(defmethod basecode-reshape :before ((w ortho-projection-pixel))
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (case (ortho-projection-origin w)
    (:lower-left
     (glu:ortho-2d 0 (width w) 0 (height w)))
    (:upper-left
     (glu:ortho-2d 0 (width w) (height w) 0))
    ;; not really expecting these to be used, but might as well be there
    ;; in case someone tries...
    (:lower-right
     (glu:ortho-2d (width w) 0 0 (height w)))
    (:upper-right
     (glu:ortho-2d (width w) 0 (height w) 0))
    ;; :center by default
    (t
     (let ((right (/ (width w)
                     (* (ortho-projection-scale w) 2)))
           (top (/ (height w)
                   (* (ortho-projection-scale w) 2))))
       (glu:ortho-2d (- right) right (- top) top))))
  (gl:matrix-mode :modelview))

(defclass perspective-projection ()
  ((projection-near :initform 1.0
                    :accessor projection-near :initarg :projection-near)
   (projection-far :initform 100.0
                   :accessor projection-far :initarg :projection-far)
   (projection-fov :initform 45
                   :reader projection-fov :initarg :projection-fov)))

(defmethod (setf projection-fov) (new (w perspective-projection))
  (setf (slot-value w 'projection-fov) new)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  #++(glu:perspective (projection-fov w) (aspect w)
                  (projection-near w) (projection-far w))
  (glu:perspective (/ (projection-fov w) (aspect w)) (aspect w)
                   (projection-near w) (projection-far w))
  (gl:matrix-mode :modelview)
  new)

(defmethod basecode-reshape :before ((w perspective-projection))
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective (projection-fov w) (aspect w)
                  (projection-near w) (projection-far w))
  (gl:matrix-mode :modelview))
