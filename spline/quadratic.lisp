(in-package #:3bgl-splines)

;;; some quick utilities for working with quadratic splines

(defun evaluate-quadratic (start control end u)
  "evaluate the quadratic spline defined by endpoints START and END with
control point CONTROL at the point U, where U = 0.0 is START and 1.0 = END,
and START, CONTROL, END are sb-cga:vec"
  (sb-cga:vec-lerp (sb-cga:vec-lerp start control u)
                   (sb-cga:vec-lerp control end u)
                   u))

(defun evaluate-quadratic-normal (start control end u)
  "evaluate the quadratic spline defined by endpoints START and END
with control point CONTROL at the point U, where U = 0.0 is START and
1.0 = END, and START, CONTROL, END are sb-cga:vec, and return the
direction perpendicular to the curve at that point, in the same plane as
the curve. (results are undefined if start, control, and end are collinear)"
  ;; lerp between the end points and controls to get endpoints of tangent
  ;; line
  ;; then cross product of that and line perpendicular to the plane of
  ;; the curve gives normal to curve
  ;; -- should this return tangent as second value?
  (sb-cga:normalize
   (sb-cga:cross-product (sb-cga:vec- (sb-cga:vec-lerp start control u)
                                      (sb-cga:vec-lerp control end u))
                         (sb-cga:cross-product (sb-cga:vec- start control)
                                               (sb-cga:vec- end control)))))

(defun evaluate-quadratic-tangent (start control end u)
  "evaluate the quadratic spline defined by endpoints START and END
with control point CONTROL at the point U, where U = 0.0 is START and
1.0 = END, and START, CONTROL, END are sb-cga:vec, and return the
direction tangent to the curve at that point, in the same plane as
the curve."
  (sb-cga:normalize
   (sb-cga:vec- (sb-cga:vec-lerp start control u)
                (sb-cga:vec-lerp control end u))))

(defun deg->rad (d)
  (* d (/ pi 180)))

(defun subdivide-quadratic (start control end
                            &key (angle-tolerance-rad (deg->rad 10))
                            (max-error 0.001)
                            max-depth
                            normals)
  "Subdivide the quadratic curve defined by the SB-CGA:VECs START,
CONTROL, and END, until either a given control point is within
MAX-ERROR of the curve, the angle formed by the control and end points
is less than ANGLE-TOLERANCE-RAD radians from a straight line, or the
subdivision depth reaches MAX-DEPTH. If any of the termination
criteria are NIL, they will not be checked, if all are NIL, the curve
will not be subdivided. Returns a vector of points approximating the
curve.
If NORMALS is true, a vector of normals corresponding to each point
will be returned as the second value, and a vector of tangents as
third value."
  ;; todo: version that optimizes for a fixed segment budget
  ;; implementing this from memory, probably buggy...
  (when (not (or angle-tolerance-rad max-error max-depth))
    (return-from subdivide-quadratic (vector start control end)))
  (let ((angle-tolerance-sin (when angle-tolerance-rad
                               (sin (min (abs angle-tolerance-rad)
                                         (/ pi 2)))))
        (results (make-array 3 :adjustable t :fill-pointer 0))
        (normals (when normals (make-array 3 :adjustable t :fill-pointer 0)))
        (tangents (when normals (make-array 3 :adjustable t :fill-pointer 0))))
    (labels ((check-tolerances (ds c de p d)
               ;; return t when segment is within tolerances (and doesn't
               ;; need subdivided further)
               ;; ds and de are normalized vectors from start->c and c->end
               (or
                (and max-depth (>= d max-depth))
                (and max-error
                     (< (abs (sb-cga:vec-length (sb-cga:vec- p c)))
                        max-error))
                (and angle-tolerance-sin
                     (and (>= (sb-cga:dot-product ds de) 0)
                          (< (sb-cga:vec-length (sb-cga:cross-product ds de))
                             (abs angle-tolerance-sin))))
                ))

             (add-point (p pn pt)
               (vector-push-extend p results)
               (when tangents
                 (vector-push-extend pt tangents))
               (when normals
                 (vector-push-extend pn normals)))
             (subd (s c e depth sn en st et)
               (let* ((a (sb-cga:vec-lerp s c 0.5))
                      (b (sb-cga:vec-lerp c e 0.5))
                      (p (sb-cga:vec-lerp a b 0.5))
                      (ct (when (or tangents normals)
                            (sb-cga:normalize (sb-cga:vec- a b))))
                      (cn (when (or tangents normals)
                            (sb-cga:normalize
                             (sb-cga:cross-product
                              ct
                              (sb-cga:cross-product (sb-cga:vec- s c)
                                                    (sb-cga:vec- e c)))))))
                 (if (check-tolerances (sb-cga:normalize (sb-cga:vec- c s))
                                       c
                                       (sb-cga:normalize (sb-cga:vec- e c))
                                       p
                                       depth)
                     (progn
                       (add-point c cn ct)
                       (add-point e en et))
                     (progn
                       (subd s a p (1+ depth) sn cn st ct )
                       (subd p b e (1+ depth) cn en ct et))))))
      (let ((st (when tangents
                  (evaluate-quadratic-tangent start control end 0.0)))
            (sn (when normals
                  (evaluate-quadratic-normal start control end 0.0)))
            (et (when tangents
                  (evaluate-quadratic-tangent start control end 1.0)))
            (en (when normals
                  (evaluate-quadratic-normal start control end 1.0))))
        (add-point start sn st)
        (subd start control end 0 sn en st et))
      (values results normals tangents))))

