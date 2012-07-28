(in-package #:3bgl-model-viewer)

(defun v4 (x &optional (y 0.0) (z 0.0) (w 1.0))
  (make-array 4
              :element-type 'single-float
              :initial-contents (list (float x 1.0) (float y 1.0)
                                      (float z 1.0) (float w 1.0))))

(defun v4v (v &optional (defaults #(1.0 0.0 0.0 1.0)))
  (when (or v defaults)
    (map-into
     (make-array 4 :element-type 'single-float :initial-contents defaults)
     #'identity v)))
