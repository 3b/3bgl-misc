(in-package #:glu-ttf-extrude)

(defclass extrude-tess (glu:tessellator)
  ((vertices :accessor vertices
             :initform (make-array 1 :adjustable t :fill-pointer 0))
   (indices :accessor indices
            :initform (make-array 1 :adjustable t :fill-pointer 0
                                  :element-type '(unsigned-byte 32)))
   (vertex-map :accessor vertex-map :initform (make-hash-table :test #'equal))
   (edges :accessor edges
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
             (loop for i below 3 #++(glu::vertex-data-length tess)
                collect (gl:glaref a i)))
           (vi (v)
             (let* ((vl (gla v)))
               (or (gethash vl (vertex-map tess))
                   (setf (gethash vl (vertex-map tess))
                         (vector-push-extend vl (vertices tess)))))))
    (let ((vi (vi vert)))
      #++(format t "vertex ~s (i ~s) -> ~s~%" vi (length (indices tess))
              (gla vert))
      (vector-push-extend vi (indices tess))
      (push (list vi (edge-flag tess)) (current-tri tess))
      (when (= 3 (length (current-tri tess)))
        (destructuring-bind ((v1 e1) (v2 e2) (v3 e3))
            (nreverse (current-tri tess))
          (when e1
            (vector-push-extend (list v1 v2) (edges tess)))
          (when e2
            (vector-push-extend (list v2 v3) (edges tess)))
          (when e3
            (vector-push-extend (list v3 v1) (edges tess))))
        (setf (current-tri tess) nil)))))

(defmethod glu:edge-flag-data-callback ((tess extrude-tess) flag data)
  (setf (edge-flag tess) flag))

(defmethod glu:combine-data-callback ((tess extrude-tess) coords vertex-data weight polygon-data)
  #++(format t "combine ~s~%"   (loop for i from 0 below 3
                                collect (gl:glaref coords i)))
  (loop for i from 0 below 3
     collect (gl:glaref coords i)))

(defmethod extrude-shape ((tess extrude-tess))
  ;; make sure we don't get called twice
  ;; (possibly should store the final data separately so it only
  ;; wastes time if called again, instead of breaking things?)
  (assert (not (extruded tess)))
  (flet ((fvec (x y z)
           (vec (float x 1.0)
                (float y 1.0)
                (float z 1.0))))
    (let ((last-index (length (indices tess)))
          (last-vertex (length (vertices tess)))
          (last-cap-vertex nil))
      ;; first copy the cap vertices, offset by extrude-depth
      ;; and assign normals for both caps
      (loop for i below last-vertex
         for v = (aref (vertices tess) i)
         do (setf (aref (vertices tess) i)
                  (list (apply #'fvec v)
                        (vec 0.0 0.0 -1.0)))
         do (vector-push-extend (list (fvec (first v) (second v)
                                           (+ (third v)
                                              (extrude-depth tess)))
                                      (vec 0.0 0.0 1.0))
                                (vertices tess)))
      ;; and add the indices for the other cap, but swapping 2 indices in
      ;; each triangle so it faces the other way
      (loop for i below last-index by 3
         for i1 = (+ last-vertex (aref (indices tess) (+ 0 i)))
         for i2 = (+ last-vertex (aref (indices tess) (+ 1 i)))
         for i3 = (+ last-vertex (aref (indices tess) (+ 2 i)))
         do
           (vector-push-extend i2 (indices tess))
           (vector-push-extend i1 (indices tess))
           (vector-push-extend i3 (indices tess)))

      ;; we want a sharp edge between the caps and edges for shading, so we need
      ;; copies of all the edge vertices with different normals
      ;; (for now assuming all vertices are uses for edges, glu tesselator
      ;;  doesn't seem to add interior points)
      (setf last-cap-vertex (length (vertices tess)))
      ;; copy points
      (loop for i below last-cap-vertex
         for vp = (car (aref (vertices tess) i))
         ;; set normal to 0 so we can accumulate normals of edges
         ;; using this vert later
         do (vector-push-extend (list vp (fvec 0 0 0)) (vertices tess)))
      ;; accumulate normals and add triangles along the edges joining the 2 caps
      (loop
         with vertices = (vertices tess)
         for (i1e i2e) across (edges tess)
         for i1 = (+ i1e last-cap-vertex)
         for i2 = (+ i2e last-cap-vertex)
         for i3 = (+ i1 last-vertex)
         for i4 = (+ i2 last-vertex)
         for n = (sb-cga:cross-product (sb-cga:vec- (car (aref vertices i1))
                                                    (car (aref vertices i2)))
                                       (fvec 0 0 1))
         do
           (setf (second (aref vertices i1))
                 (sb-cga:vec+ (second (aref vertices i1)) n))
           (setf (second (aref vertices i2))
                 (sb-cga:vec+ (second (aref vertices i2)) n))
           (setf (second (aref vertices i3))
                 (sb-cga:vec+ (second (aref vertices i3)) n))
           (setf (second (aref vertices i4))
                 (sb-cga:vec+ (second (aref vertices i4)) n))
           (vector-push-extend i2 (indices tess))
           (vector-push-extend i1 (indices tess))
           (vector-push-extend i3 (indices tess))

           (vector-push-extend i2 (indices tess))
           (vector-push-extend i3 (indices tess))
           (vector-push-extend i4 (indices tess)))
      ;; normalize edge normals
      (loop for i from last-cap-vertex below (length (vertices tess))
         do (setf (second (aref (vertices tess) i))
                  (sb-cga:normalize (second (aref (vertices tess) i)))))
      (setf (extruded tess) t))))

(defmethod fill-buffers ((tess extrude-tess) vertex-vbo index-vbo vao)
  #++(format t "fill buffers: ~%")
  (gl:bind-vertex-array vao)

  (gl:bind-buffer :array-buffer vertex-vbo)
  (gl:with-gl-array (arr :float :count (* 6 (length (vertices tess))))
    (loop for (v n) across (vertices tess)
       for i from 0 by 6
       #+do (format t "~s:  ~s ~s ~s~%" (/ i 3)
                  (float x 1.0)
                  (float y 1.0)
                  (float z 1.0))
       do (setf (gl:glaref arr (+ i 0)) (aref v 0)
                (gl:glaref arr (+ i 1)) (aref v 1)
                (gl:glaref arr (+ i 2)) (aref v 2)
                (gl:glaref arr (+ i 3)) (aref n 0)
                (gl:glaref arr (+ i 4)) (aref n 1)
                (gl:glaref arr (+ i 5)) (aref n 2)
                ))
    (gl:buffer-data :array-buffer :static-draw arr))

  (gl:bind-buffer :element-array-buffer index-vbo)
  (gl:with-gl-array (arr :unsigned-short :count (length (indices tess)))
    (loop for index across (indices tess)
       for i from 0
       #+do (format t "~s:  ~s~%" i index)

       do (setf (gl:glaref arr i) index))
    (gl:buffer-data :element-array-buffer :static-draw arr))

  ;; just set up 'position' for now
  (gl:enable-vertex-attrib-array 0)
  (gl:vertex-attrib-pointer 0 3 :float nil 24 (cffi:null-pointer))
  (gl:enable-vertex-attrib-array 2)
  (gl:vertex-attrib-pointer 2 3 :float nil 24 (cffi:make-pointer 12))

  (gl:bind-vertex-array 0)
  #++(format t "length = ~s~%"   (length (indices tess)))
  (length (indices tess)))


(defun extrude-glyph (glyph loader &key (scale (/ (units/em loader))))
  (let ((tess (make-instance 'extrude-tess :extrude-depth 0.3)))
    #++(format t "triangulate glyph ~s / ~s (~s)~%" i
            (font-index glyph)
            (code-char (code-point glyph)))
    ;; first triangulate the glyph
    (labels ((v (c)
               (let ((x (float (* (x c) scale) 1.0))
                     (y (float (* (y c) scale) 1.0)))
                 #++(format t " -> ~s ~s~%" x y)
                 (glu:tess-vertex tess
                                  (list x y 0.0) (list x y 0.0))))
             (subdivide (s c e)
                       ;;; todo: subdivide the curve to some
                       ;;; angle/length tolerance
               (declare (ignorable s))
               (v c)
               (v e)))
      (glu:with-tess-polygon (tess nil)
        (do-contours (contour glyph)
          #++(format t "~&---~%")
          (let ((first t))
            (setf (edge-flag tess) nil)
            (glu:with-tess-contour tess
              (do-contour-segments (s c e) contour
                #++(format t "~s ~s ~s~%" s c e)
                #++(when first
                     (v s))
                (if c
                    (subdivide s c e)
                    (v e))
                (setf first nil)))))))
    ;; once it is triangulated, extrude it
    (extrude-shape tess)
    tess))


