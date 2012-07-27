(in-package #:glu-ttf-extrude)

(defclass extrude-tess (glu:tessellator)
  ;; each triangle is stored as 3 lists of 2 sb-cga:vec, position then normal
  ;; which will be optimized to reuse shared vertices in fill-buffers
  ((triangles :accessor triangles
              :initform (make-array 1 :adjustable t :fill-pointer 0))
   (edge-flag :accessor edge-flag :initform nil)
   (current-tri :accessor current-tri :initform nil)
   (extrude-depth :accessor extrude-depth :initform 1.0 :initarg :extrude-depth)
   (extruded :accessor extruded :initform nil)))

(defmethod glu:begin-data-callback ((tess extrude-tess) type data)
  ;; should always be triangles since we have edge-flag
  ;; callback... ignore for now...
  )

(defmethod glu:end-data-callback ((tess extrude-tess) data)
  )

(defmethod glu:vertex-data-callback ((tess extrude-tess) vert data)
  (labels ((gla (a)
             (list
              (apply 'sb-cga:vec
                     (loop for i below 3
                        collect (float (elt a i) 1.0)))
              (apply 'sb-cga:vec
                     (loop for i from 3 below 6
                        collect (float (elt a i) 1.0)))))
           (tri (p1 p2 p3 n)
             (vector-push-extend (list p1 n) (triangles tess))
             (vector-push-extend (list p3 n) (triangles tess))
             (vector-push-extend (list p2 n) (triangles tess)))
           (tri3 (p1 n1 p2 n2 p3 n3)
             (vector-push-extend (list p1 n1) (triangles tess))
             (vector-push-extend (list p3 n3) (triangles tess))
             (vector-push-extend (list p2 n2) (triangles tess)))
           (edge (p1 n1 p2 n2 p3 p4)
             ;; calculate face normals if we don't have a normal already
             (let* ((need-n1 (< (abs (sb-cga:vec-length n1)) 0.001))
                    (need-n2 (< (abs (sb-cga:vec-length n2)) 0.001))
                    (fn (when (or need-n1 need-n2)
                          (sb-cga:normalize
                           (sb-cga:cross-product (sb-cga:vec- p2 p1)
                                                 (sb-cga:vec 0.0 0.0 1.0))))))
               ;; should this normalize existing normals or rely on
               ;; caller to provide them normalized?
               (when need-n1 (setf n1 fn))
               (when need-n2 (setf n2 fn)))
             ;; add face tris
             (tri3 p2 n2 p1 n1 p3 n1)
             (tri3 p2 n2 p3 n1 p4 n2)))
    (let ((vi (gla vert))
          (offset (sb-cga:vec 0.0 0.0 (float (extrude-depth tess) 1.0))))
      (push (cons (edge-flag tess) vi) (current-tri tess))
      (when (= 3 (length (current-tri tess)))
        (destructuring-bind ((e1 v1 n1) (e2 v2 n2) (e3 v3 n3))
            (nreverse (current-tri tess))
          (let ((v1b (sb-cga:vec+ v1 offset))
                (v2b (sb-cga:vec+ v2 offset))
                (v3b (sb-cga:vec+ v3 offset)))
            ;; add triangle to both caps
            (tri v1 v2 v3 (sb-cga:vec 0.0 0.0 1.0))
            ;; back face gets opposite winding and opposite normal
            (tri v2b v1b v3b (sb-cga:vec 0.0 0.0 -1.0))
            ;; add edge tris
            (when e1
              (edge v1 n1 v2 n2 v1b v2b))
            (when e2
              (edge v2 n2 v3 n3 v2b v3b))
            (when e3
              (edge v3 n3 v1 n1 v3b v1b))))
        (setf (current-tri tess) nil)))))

(defmethod glu:edge-flag-data-callback ((tess extrude-tess) flag data)
  (setf (edge-flag tess) flag))

(defmethod glu:combine-data-callback ((tess extrude-tess)
                                      coords vertex-data weight polygon-data)
  ;; for now assuming intersections should be shaded as sharp corners, so
  ;; return 0 for normal
  (list (gl:glaref coords 0) (gl:glaref coords 1) (gl:glaref coords 2)
        0.0 0.0 0.0))

(defparameter *verts* 0)
(defparameter *glyphs* 0)

(defmethod fill-buffers ((tess extrude-tess) vertex-vbo index-vbo vao)
  (gl:bind-vertex-array vao)

  (gl:bind-buffer :array-buffer vertex-vbo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; todo: reuse shared verts
  (gl:with-gl-array (arr :float :count (* 6 (length (triangles tess))))
    (loop for (v n) across (triangles tess)
       for i from 0 by 6
       do #++(setf (gl:glaref arr (+ i 0)) (aref v 0)
                (gl:glaref arr (+ i 1)) (aref v 1)
                (gl:glaref arr (+ i 2)) (aref v 2)
                (gl:glaref arr (+ i 3)) (aref n 0)
                (gl:glaref arr (+ i 4)) (aref n 1)
                (gl:glaref arr (+ i 5)) (aref n 2))
          (setf (cffi:mem-aref (gl::gl-array-pointer arr) :float (+ i 0)) (aref v 0)
                (cffi:mem-aref (gl::gl-array-pointer arr) :float (+ i 1)) (aref v 1)
                (cffi:mem-aref (gl::gl-array-pointer arr) :float (+ i 2)) (aref v 2)
                (cffi:mem-aref (gl::gl-array-pointer arr) :float (+ i 3)) (aref n 0)
                (cffi:mem-aref (gl::gl-array-pointer arr) :float (+ i 4)) (aref n 1)
                (cffi:mem-aref (gl::gl-array-pointer arr) :float (+ i 5)) (aref n 2))
         (incf *verts*))
    (incf *glyphs*)
    (gl:buffer-data :array-buffer :static-draw arr)
    nil)

  (gl:bind-buffer :element-array-buffer index-vbo)
  (gl:with-gl-array (arr :unsigned-short :count (length (triangles tess)))
    (loop for foo across (triangles tess)
       for i from 0
       do (setf #++(gl:glaref arr i)
                (cffi:mem-aref (gl::gl-array-pointer arr) :unsigned-short i)
                i))
    (gl:buffer-data :element-array-buffer :static-draw arr))

  ;; just set up 'position' for now
  (gl:enable-vertex-attrib-array 0)
  (gl:vertex-attrib-pointer 0 3 :float nil 24 (cffi:null-pointer))
  (gl:enable-vertex-attrib-array 2)
  (gl:vertex-attrib-pointer 2 3 :float nil 24 (cffi:make-pointer 12))

  (gl:bind-vertex-array 0)
  (length (triangles tess)))

(defun control-points-smooth (p c n tolerance)
  (let*  ((pc (complex (x c) (y c)))
          (p1 (- (complex (x p) (y p)) pc))
          (p2 (- (complex (x n) (y n)) pc)))
    (<= 0.0 (abs (- (phase p1) (phase p2))) tolerance)))

(defun call-with-contour-segemnts-x (fun contour tolerance)
  (when (plusp (length contour))
    (loop with i = 0
       with points = (explicit-contour-points contour)
       with length = (length points)
       for prev = (aref points (mod (+ i length -1) length))
       for cur = (aref points i)
       for next1 = (aref points (mod (+ i length 1) length))
       for next2 = (aref points (mod (+ i length 2) length))
       for smooth = t ;(control-points-smooth prev cur next1 tolerance)
       when (not (on-curve-p next1))
       do
         (incf i 2)
         (funcall fun cur next1 next2 smooth (>= i (1- length)))
       else do
         (incf i)
         (funcall fun cur nil next1 smooth (>= i (1- length)))
       until (> i (1- length)))))

(defmacro do-contour-segments-x ((s c e smooth-p last-p
                                    &key (smooth-tolerance-deg 5)) contour
                                 &body body)
  "like do-contour-segments, except it checks to see if the start point is
smooth (within smooth-tolerance-deg of 180 from previous segment/arc)
and indicated when current segment is the last one"
  `(call-with-contour-segemnts-x (lambda (,s ,c ,e ,smooth-p ,last-p)
                                   ,@body)
                                 ,contour
                                 (* ,smooth-tolerance-deg ,(/ pi 180))))

(defun extrude-glyph (glyph loader &key (scale (/ (units/em loader))))
  (let ((tess (make-instance 'extrude-tess :extrude-depth 0.3)))
    (glu:tess-property tess :winding-rule :nonzero)
    (assert scale)
    ;; first triangulate the glyph
    (labels ((v (c n)
               (let ((x (float (* (x c) scale) 1.0))
                     (y (float (* (y c) scale) 1.0))
                     (nx (if n (first n) 0.0))
                     (ny (if n (second n) 0.0)))

                 ;; for smooth shaded vertices (interpolated curve
                 ;; points, or end points of a curve that is tangent
                 ;; to adjacent segment), we pass a normal to the
                 ;; tesselator

                 ;; for sharp edges (corners between straight
                 ;; segments, or curves that are not tangent to
                 ;; adjacent segment as in B or P) we pass 0,0,0 as
                 ;; the normal, and calculate it for each face
                 ;; when extruding

                 (glu:tess-vertex tess (list x y 0.0 1.0 0.0 0.0)
                                  (list x y 0.0 nx ny 0.0))))
             (vv (c n)
               (let ((x (float (* (aref c 0) scale) 1.0))
                     (y (float (* (aref c 1) scale) 1.0))
                     (nx (if n (aref n 0) 0.0))
                     (ny (if n (aref n 1) 0.0)))
                 (glu:tess-vertex tess (list x y 0.0 1.0 0.0 0.0)
                                  (list x y 0.0 nx ny 0.0))))
             (cpvec (cp)
               (sb-cga:vec (float (x cp) 1.0) (float (y cp) 1.0) 0.0))
             (subdivide (s c e smooth)
               (declare (ignorable smooth))
               (let ((invert-normal (plusp (sb-cga:dot-product
                                            (sb-cga:vec 0.0 0.0 1.0)
                                            (sb-cga:cross-product
                                             (sb-cga:vec- (cpvec s) (cpvec c))
                                             (sb-cga:vec- (cpvec e) (cpvec c)))))))
                 (multiple-value-bind (points normals)
                     (3bgl-splines:subdivide-quadratic
                      (cpvec s) (cpvec c) (cpvec e)
                      :normals t
                      :angle-tolerance-rad (* 15 (/ pi 180)))
                   (loop for i from 0 below  (length points)
                      for p = (aref points i)
                      for n = (if invert-normal
                                  (sb-cga:vec* (aref normals i) -1.0)
                                  (aref normals i))
                      do (vv p n))))))
      (glu:with-tess-polygon (tess nil)
        (do-contours (contour glyph)
          (let ((first t))
            (setf (edge-flag tess) nil)
            (glu:with-tess-contour tess
              (do-contour-segments-x (s c e smooth last) contour
                (declare (ignorable last))
                (if c
                    (subdivide s c e smooth)
                    (unless nil (v e (list 0.0 0.0))))
                (setf first nil)))))))
    ;; fixme: just return the geometry, not the deleted tesselator
    (glu:tess-delete tess)
    tess))


