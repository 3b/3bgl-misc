(in-package 3bgl-sg2)

(defclass ai-material (material)
  ;; no slots for now, just for dispatch
  ())

(3bgl-ssbo::define-ssbo-writer %write-per-object/ai (layout pointer size draws)
  (let* ((count (length draws)))
    (multiple-value-bind (total-size ok partial partial-count partial-size)
        (3bgl-ssbo::check-size count :errorp nil)
      (unless ok
        (if partial
            (break "can't draw all objects, drawing ~s of ~s objects (~s / ~s bytes) ~%" partial-count count partial-size total-size)
            (progn (break "can't draw any objects~%")
                   (return-from %write-per-object/ai (values 0 0)))))
      (3bgl-ssbo::set-slot 3bgl-ai-shaders::object-count partial-count)
      (3bgl-ssbo::with-array@slot 3bgl-ai-shaders::objects
        (loop for i below partial-count
              for draw in draws
              for (material-data nil nil matrix) = draw
              do (3bgl-ssbo::with-struct@index i
                   (3bgl-ssbo::set-slot 3bgl-ai-shaders::material-id
                                        material-data)
                   (3bgl-ssbo::set-slot 3bgl-ai-shaders::m
                                        matrix))))
      (values partial-size partial-count))))

(defmethod write-per-object ((mat ai-material) draws)
  (let* ((rm *resource-manager*)
         (pop (per-object-packing mat)))
    (if (3bgl-ssbo::packing pop)
        (3bgl-ssbo::with-current-region (p) (streaming-ssbo rm)
          (multiple-value-bind (size count)
              (%write-per-object/ai pop p (3bgl-ssbo::remaining) draws)
            (3bgl-ssbo::use-bytes size)
            (3bgl-ssbo::bind-range :shader-storage-buffer +per-object-binding+)
            (values size count)))
        (values 0 0))))
