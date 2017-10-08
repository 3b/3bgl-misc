(in-package 3bgl-sg2)

(defclass graph-material (material)
  ())
(defclass graph-material-lines (graph-material)
  ())
(3bgl-ssbo::define-ssbo-writer %write-per-object/graph (layout pointer size draws)
  (let* ((count (length draws)))
    (multiple-value-bind (total-size ok partial partial-count partial-size)
        (3bgl-ssbo::check-size count :errorp nil)
      (unless ok
        (if partial
            (break "can't draw all objects, drawing ~s of ~s objects (~s / ~s bytes) ~%" partial-count count partial-size total-size)
            (progn (break "can't draw any objects~%")
                   (return-from %write-per-object/graph (values 0 0)))))
      (3bgl-ssbo::set-slot 3bgl-sg2-graph-shaders::object-count partial-count)
      (3bgl-ssbo::with-array@slot 3bgl-sg2-graph-shaders::objects
        (loop for i below partial-count
              for draw in draws
              for (material-data nil nil matrix) = draw
              do (3bgl-ssbo::with-struct@index i
                   (3bgl-ssbo::set-slot 3bgl-sg2-graph-shaders::color
                                        material-data)
                   (3bgl-ssbo::set-slot 3bgl-sg2-graph-shaders::m
                                         matrix))))
      (values partial-size partial-count))))

(defmethod write-per-object ((mat graph-material) draws)
  (let* ((rm *resource-manager*)
         (pop (per-object-packing mat)))
    (if (3bgl-ssbo::packing pop)
        (3bgl-ssbo::with-current-region (p) (streaming-ssbo rm)
          (multiple-value-bind (size count)
              (%write-per-object/graph pop p (3bgl-ssbo::remaining) draws)
            (3bgl-ssbo::use-bytes size)
            (3bgl-ssbo::bind-range :shader-storage-buffer +per-object-binding+)
            (values size count)))
        (values 0 0))))

(defmethod primitive ((m graph-material))
  :line-strip)

(defmethod primitive ((m graph-material-lines))
  :lines)

(defclass graph-line-node (transform)
  ((color :initform #(1 1 1 1) :initarg :color :reader color)
   (next :initform 0 :accessor next)

   (buffer :initform nil :reader buffer))
  (:default-initargs :size (1+ (* 60 30))))

(defmethod initialize-instance :after ((o graph-line-node) &key size)
  (setf (slot-value o 'buffer)
        (make-array size :element-type 'single-float :initial-element 0.0)))

(defun graph-line-material ()
  (or (get-material '(graph-material))
      (make-material '(graph-material)
                     (scenegraph::make-state
                      `(:vertex-format ((0 0 1 :float nil 0))
                        :sample-alpha-to-coverage t
                        :depth-test nil
                        :cull-face nil
                        :program
                        ,(get-program
                          :vertex '3bgl-sg2-graph-shaders::vertex
                          :fragment '3bgl-sg2-graph-shaders::fragment))))))

(defmethod draw-node ((n graph-line-node) &key mv)
  (call-next-method)
  (when *once*
   (format t "draw node ~s: ~% ~s~% ~s~%" (name n) mv (matrix n)))
  (add-draw (graph-line-material) (color n) nil
            (list (next n) (buffer n))
            (sb-cga:matrix* mv (matrix n))))

(defun get-graph-line-buffer-set ()
  (get-buffer-set (buffer-builder::vertex-format-for-layout
                    '((:h :vec2 :location 0)))))

(defmethod submit-material-draws ((material graph-material) bs draws rm cs)
  (let* ((*print-length* 32)
         (bs (get-graph-line-buffer-set))
         (vao (vao bs))
         (ranges ()))
  (when *once*
    (format t "draws = ~s~%" draws))
    ;; upload vertex data
    (3bgl-ssbo::with-current-region (p) cs
      (loop for (nil nil .draw nil) in draws
            for (bstart buf) of-type (fixnum (simple-array single-float (*)))
              = .draw
            for rstart = 1 then (+ rstart l)
            for l of-type (unsigned-byte 32) = (length buf)
            do (when *once*
                 (format t "add draw ~s ~s~%" bstart buf))
               (unless (< (* 4 2 l) (3bgl-ssbo::remaining))
                    (cerror "continue" "not enough space for graph line data ~s / ~s"
                            l (floor (3bgl-ssbo::remaining) 8))
                    (loop-finish))
               (push (list rstart (1- l)) ranges)
               (when *once*
                 (format t " =  ~s ~s~%" rstart l))
               (loop for n fixnum below l
                     for 2n fixnum = (* n 2)
                     for i = (mod (+ n bstart) l)
                     do (setf (cffi:mem-aref p :float (+ 2n 0)) (float n 1.0)
                              (cffi:mem-aref p :float (+ 2n 1)) (aref buf i)))
               (3bgl-ssbo::use-bytes (* 4 2 l)))
      (let ((offset (3bgl-ssbo::bind nil)))
        (%gl:vertex-array-vertex-buffer vao 0 (3bgl-ssbo::buffer cs) offset 8))
      (gl:bind-vertex-array vao)
      ;; upload per-object data and build commands
      (multiple-value-bind (size count)
          (write-per-object material draws)
        (declare (ignorable size))
        (unless count
          (break "count = ~s, size=~s?" count size))
        (when count
          (3bgl-ssbo::with-current-region (p) cs
            ;; fixme: factor out indirect buffer writers
            (let* ((max (floor (3bgl-ssbo::remaining) (* 4 4))))
              (when (< max count)
                (cerror "continue"
                        "not enough space for draw commands. ~s / ~s~%"
                        max count)
                (setf count max)))
            (when *once*
              (format t "ranges = ~s ~s~%" count ranges))
            (macrolet ((add (offset value)
                         `(setf (cffi:mem-aref p :unsigned-int (+ i ,offset))
                                ,value)))
              (loop for (rstart rcount) in (reverse ranges)
                    for index below count
                    for base-instance = index
                    for i = (* index 4)
                    do (let ((n rcount)) ;; # verices
                         (add 0 n)
                         (if (typep material 'graph-material-lines)
                             (incf *no* (floor n 2))
                             (incf *no* (1- n))))
                       (add 1 1) ;; instance count
                       (add 2 rstart)
                       (add 3 base-instance)
                       (incf *objects*))
              (3bgl-ssbo::use-bytes (* 4 4 count)))
            (let ((offset (3bgl-ssbo::bind :draw-indirect-buffer)))
              (incf *draws*)
              (%gl:multi-draw-arrays-indirect (primitive material)
                                              offset
                                              count
                                              0))))))))
