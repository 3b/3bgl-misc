(in-package #:3bgl-mesh)

;; convenience util for building a mesh
;; locally binds functions for adding vertex components, returns a MESH object

(defmacro with-mesh-builder ((vertex-count indices &key calculate-tangents)
                             &body body)
  ;; todo: make vertex-count optional
  (alexandria:with-gensyms (verts uvs normals tangents
                                  bone-weights bone-indices
                                  index sv-indices)
    (alexandria:once-only (indices)
      `(with-static-vectors ((,verts (* ,vertex-count 3))
                             (,uvs (* ,vertex-count 3))
                             (,normals (* ,vertex-count 3))
                             (,tangents (* ,vertex-count 3))
                             (,bone-weights (* ,vertex-count 4))
                             (,bone-indices (* ,vertex-count 4)
                                            :unsigned-byte))
         (let ((,index 0))
           (labels ((normal (x y z)
                      (assert (< ,index ,vertex-count))
                      (setf (aref ,normals (+ 0 (* 3 ,index))) (float x 1.0)
                            (aref ,normals (+ 1 (* 3 ,index))) (float y 1.0)
                            (aref ,normals (+ 2 (* 3 ,index))) (float z 1.0)))
                    (tex-coord (u v &optional (s 0.0))
                      (assert (< ,index ,vertex-count))
                      (setf (aref ,uvs (+ 0 (* 3 ,index))) (float u 1.0)
                            (aref ,uvs (+ 1 (* 3 ,index))) (float v 1.0)
                            (aref ,uvs (+ 2 (* 3 ,index))) (float s 1.0)))
                    (tangent (x y z)
                      (assert (< ,index ,vertex-count))
                      (setf (aref ,tangents (+ 0 (* 3 ,index))) (float x 1.0)
                            (aref ,tangents (+ 1 (* 3 ,index))) (float y 1.0)
                            (aref ,tangents (+ 2 (* 3 ,index))) (float z 1.0)))
                    (bones (a b c d)
                      (assert (< ,index ,vertex-count))
                      (setf (aref ,bone-indices (+ 0 (* 4 ,index))) a
                            (aref ,bone-indices (+ 1 (* 4 ,index))) b
                            (aref ,bone-indices (+ 2 (* 4 ,index))) c
                            (aref ,bone-indices (+ 3 (* 4 ,index))) d))
                    (bone-weights (a b c d)
                      (assert (< ,index ,vertex-count))
                      (setf (aref ,bone-weights (+ 0 (* 4 ,index))) (float a 1.0)
                            (aref ,bone-weights (+ 1 (* 4 ,index))) (float b 1.0)
                            (aref ,bone-weights (+ 2 (* 4 ,index))) (float c 1.0)
                            (aref ,bone-weights (+ 3 (* 4 ,index))) (float d 1.0)))
                    (vertex (x y z)
                      (assert (< ,index ,vertex-count))
                      (setf (aref ,verts (+ 0 (* 3 ,index))) (float x 1.0)
                            (aref ,verts (+ 1 (* 3 ,index))) (float y 1.0)
                            (aref ,verts (+ 2 (* 3 ,index))) (float z 1.0))
                      (incf ,index)
                      ;; set defaults for other attributes of next vertex
                      ;; fixme: do this more efficiently
                      (when (< ,index ,vertex-count)
                        (normal 0 1 0)
                        (tex-coord 0 0)
                        (tangent 1 0 0)
                        (bones 0 0 0 0)
                        (bone-weights 1 0 0 0))))
             (declare (ignorable #'bone-weights #'bones #'normal #'tangent
                                 #'tex-coord))
             (progn
               ,@body)
             #++(loop for n from 0
                   for i below ,index
                   do (format t "~s/~s ~s~%" n ,vertex-count
                              (subseq ,verts (* i 3) (+ (* i 3) 3))
                              ))
             (static-vectors:with-static-vector (,sv-indices
                                                 (length ,indices)
                                                 :element-type
                                                 '(unsigned-byte 16))
               #++(loop for i across (coerce ,indices 'vector)
                     for n from 0
                     do (setf (aref ,sv-indices n) i))
               (replace ,sv-indices ,indices)
               ,@(when calculate-tangents
                 `((%calculate-tangent-space ,indices ,verts ,uvs ,normals
                                             ,tangents nil)))
               (%make-mesh ,sv-indices
                           ,verts ,uvs ,normals ,tangents
                           ,bone-weights ,bone-indices
                           :index-type :unsigned-short :index-size 2))))))))
