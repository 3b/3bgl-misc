(in-package 3bgl-sg2)

;; mixin to track timing

;; mixin for handling simple timing graphs
(defclass timing-helper ()
  ((max-queries :reader max-queries :initform 32 :initarg :max-timer-queries)
   (query-index :accessor query-index :initform 0)
   (timestamps :accessor timestamps :initform nil)
   (cpu-timestampe :accessor cpu-timestamps :initform nil)
   (timestamp-masks :accessor timestamp-masks :initform nil)
   (timestamp-ids :accessor timestamp-ids :initform nil)

   (current-times/masks :accessor current-times/masks :initform nil)
   (current-times/gpu :accessor current-times/gpu :initform nil)
   (current-times/cpu :accessor current-times/cpu :initform nil)
   (current-times/ids :accessor current-times/ids :initform nil)))

(defmethod basecode::run-main-loop :before ((w timing-helper))
  ;; todo: adjust max-queries incrementally?
  (let ((mq (max-queries w)))
    (setf (timestamps w) (list (gl:gen-queries mq)
                               (gl:gen-queries mq)))
    (setf (cpu-timestamps w)
          (list (make-array mq :element-type 'double-float
                               :initial-element (basecode::now))
                (make-array mq :element-type 'double-float
                               :initial-element (basecode::now))))
    (setf (current-times/masks w)
          (make-array mq :initial-element nil)
          (current-times/ids w)
          (make-array mq :initial-element nil)
          (current-times/cpu w)
          (make-array mq :element-type 'double-float
                         :initial-element (basecode::now))
          (current-times/gpu w)
          (make-array mq :element-type 'double-float
                         :initial-element (basecode::now)))
    (setf (timestamp-masks w) (list (make-array mq :initial-element nil)
                                    (make-array mq :initial-element nil)))
    (setf (timestamp-ids w) (list (make-hash-table :test 'equal)
                                  (make-hash-table :test 'equal)))))

(defmethod mark (w &key id)
  (declare (ignore w id))
  ;; ignore if we don't have the mixin so it can be easily disabled
  ;; without modifying all the code
  nil)

(defmethod mark ((w timing-helper) &key id)
  (let ((n (query-index w)))
    (when (>= n (max-queries w))
      (setf (query-index w) 0)
      (error "too many timer queries? n=~s max=~s" n (max-queries w)))
    (%gl:query-counter (nth n (car (timestamps w))) :timestamp)
    (setf (aref (car (cpu-timestamps w)) n) (basecode::now))
    (setf (aref (car (timestamp-masks w)) n) t)
    (setf (gethash (or id n) (car (timestamp-ids w))) n)
    (incf (query-index w))))

(defmethod update-times (w)
  ;; do nothing if not timing
  nil)

(defmethod update-times ((w timing-helper))
  ;; look at previous frame's data
  (rotatef (first (timestamps w)) (second (timestamps w)))
  (rotatef (first (timestamp-masks w)) (second (timestamp-masks w)))
  (rotatef (first (timestamp-ids w)) (second (timestamp-ids w)))
  ;; get queries
  (replace (current-times/masks w) (car (timestamp-masks w)))
  (replace (current-times/cpu w) (car (cpu-timestamps w)))
  (loop for k being the hash-keys of (car (timestamp-ids w))
          using (hash-value v)
        do (setf (aref (current-times/ids w) v) k))
  (cffi:with-foreign-objects ((done '%gl:int)
                              (time '%gl:uint64))
    (let ((queries (make-array (max-queries w) :element-type 'double-float
                                               :initial-element 0d0)))
      ;;(declare (dynamic-extent queries))
      (loop for i below (max-queries w)
            for id in (car (timestamps w))
            when (aref (car (timestamp-masks w)) i)
              do (setf (aref (car (timestamp-masks w)) i) nil)
                 (%gl:get-query-object-iv id :query-result-available done)
                 (when (plusp (cffi:mem-ref done '%gl:int))
                   (%gl:get-query-object-ui64v id :query-result time)
                   (setf (aref queries i)
                         (/ (cffi:mem-ref time '%gl:uint64)
                            1000000000d0))))

      (replace (current-times/gpu w) queries)))

  ;; reset timing for next frame
  (setf (query-index w) 0)
  (fill (car (timestamp-masks w)) nil)
  (clrhash (car (timestamp-ids w))))

(defclass fps-graph-node (transform)
  ((timing :initarg :timing :reader timing)
   (scale :initform 1.0 :accessor scale :initarg :scale)))

(defun ensure-lines (fgn)
  (loop for i below (- (max-queries (timing fgn))
                       (length (children fgn)))
        for v = (sb-cga:vec*
                 (sb-cga:normalize
                  (sb-cga:vec (random 1.0) (random 1.0) (random 1.0)))
                 (+ 0.5 (random 0.5)))
        do (add-node (sg fgn) 'graph-line-node (list fgn i) fgn
                     :color (list (aref v 0) (aref v 1) (aref v 2) 1))))

(defmethod draw-node ((n fps-graph-node) &key mv)
  (declare (ignorable mv))
  (ensure-lines n)
  (add-draw (graph-line-material) '(1 0 0 1) nil
            (list 0 #.(coerce (loop repeat 1920 collect 0.0)
                              '(simple-array single-float (*))))
            (sb-cga:matrix* mv (matrix n)))

  (add-draw (graph-line-material) '(1 0 0 1) nil
            (list 0 (coerce (loop repeat 1920 collect (* (scale n) 16.666))
                              '(simple-array single-float (*))))
            (sb-cga:matrix* mv (matrix n)))
  (add-draw (graph-line-material) '(1 0 0 1) nil
            (list 0 (coerce (loop repeat 1920 collect (* (scale n) 33.3333))
                              '(simple-array single-float (*))))
            (sb-cga:matrix* mv (matrix n)))
  (loop with gpu = (current-times/gpu (timing n))
        for t0 = (aref gpu 0)
        for c in (children n)
        for v across gpu
        for m across (current-times/masks (timing n))
        for id across (current-times/ids (timing n))
        do (setf (label c) id)
           (if m
               (add-graph-sample c (* (scale n)
                                      (* 1000 (float (- v t0) 1.0))))
               (add-graph-sample c (* (scale n) 0.0))))
  (call-next-method)
)


(defmethod basecode::basecode-draw :around ((w timing-helper))
  (call-next-method)
  #++(mark w :id :end)
  #++(update-times w))
#++
(compute-applicable-methods #'basecode::basecode-draw
                            (list scene2test::*w*))
