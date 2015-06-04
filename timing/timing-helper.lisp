(defpackage #:basecode-timing-helper
  (:use #:cl #:basecode))
(in-package #:basecode-timing-helper)

;; mixin for handling simple timing graphs
(defclass basecode-timing-helper ()
  ((max-queries :reader max-queries :initform 16 :initarg :max-timer-queries)
   (query-index :accessor query-index :initform 0)
   (timestamps :accessor timestamps :initform nil)
   (timestamp-masks :accessor timestamp-masks :initform nil)
   (timestamp-mode :accessor timestamp-mode :initform nil)
   (times :accessor times :initform nil)
   (times-x :accessor times-x :initform 0)))

(defmethod run-main-loop :before ((w basecode-timing-helper))
  ;; todo: adjust max-queries incrementally?
  (let ((mq (max-queries w)))
    (setf (timestamps w) (list (gl:gen-queries mq)
                               (gl:gen-queries mq)))
    (setf (timestamp-masks w) (list (make-array mq :initial-element nil)
                                    (make-array mq :initial-element nil)))
    (setf (timestamp-mode w) (list (make-array mq :initial-element nil)
                                   (make-array mq :initial-element nil)))
    (setf (times w)
          (coerce (loop repeat mq collect (make-array 512 :initial-element 0))
                  'vector))))

(defmethod mark (w &key mode)
  (declare (ignore w mode))
  ;; ignore if we don't have the mixin so it can be easily disabled
  ;; without modifying all the code
  nil)

(defmethod mark ((w basecode-timing-helper) &key (mode :delta))
  (let ((n (query-index w)))
    (assert (< n (max-queries w)))
    (%gl:query-counter (nth n (car (timestamps w))) :timestamp)
    (setf (aref (car (timestamp-masks w)) n) t)
    (setf (aref (car (timestamp-mode w)) n)
          mode)
    (incf (query-index w))))

(defmethod basecode-draw :around ((w basecode-timing-helper))
  (setf (query-index w) 0)
  (mark w :mode :start)
  (loop for a in (timestamp-mode w) do (fill a :unused))

  (call-next-method)
  (gl:disable :depth-test)
  (rotatef (first (timestamps w)) (second (timestamps w)))
  (rotatef (first (timestamp-masks w)) (second (timestamp-masks w)))
  ;; update TIMES
  (cffi:with-foreign-objects ((done '%gl:int)
                              (time '%gl:uint64))
    (let ((queries (make-array (max-queries w) :initial-element 0)))
                                        ;(declare (dynamic-extent queries))
      (loop for i below (max-queries w)
            for id in (car (timestamps w))
            when (aref (car (timestamp-masks w)) i)
              do (setf (aref (car (timestamp-masks w)) i) nil)
                 (%gl:get-query-object-iv id :query-result-available done)
                 (when (plusp (cffi:mem-ref done '%gl:int))
                   (%gl:get-query-object-ui64v id :query-result time)
                   (setf (aref queries i) (cffi:mem-ref time '%gl:uint64))))
      (loop with x = (setf (times-x w)
                           (mod (1+ (times-x w))
                                (length (aref (times w) 0))))
            for i from 0
            for mode across (car (timestamp-mode w))
            do (case mode
                 (:delta
                  (assert (plusp i))
                  (setf (aref (aref (times w) i) x)
                        (- (aref queries i)
                           (aref queries (1- i)))))
                 (:total
                  (setf (aref (aref (times w) i) x)
                        (- (aref queries i)
                           (aref queries 0))))
                 (t
                  (setf (aref (aref (times w) i) x) 0))))))

  ;; draw graphs
  ;; fixme: convert this stuff to VBOs or something instead of immediate mode
  (basecode::with-pixel-ortho-projection (w :origin :lower-left)
    (gl:disable :texture-2d :texture-3d)
    (gl:with-pushed-matrix* (:modelview)
      (gl:load-identity)
      (loop with d = 100
            with s = 2
            with i = 0
            for x1 = (times-x w)
            for mode across (car (timestamp-mode w))
            for times across (times w)
            for color = (1+ i)
            for r = (ldb (byte 1 0) color)
            for g = (ldb (byte 1 1) color)
            for b = (ldb (byte 1 2) color)
            for a = (ldb (byte 1 3) color)
            when (member mode '(:delta :total))
              do (when (= color 4)
                   ;; pure blue is hard to see, lighten it a bit
                   (setf g 0.4 r 0.4))
                 (if (plusp a)
                        (gl:color (* r 0.5) (* g 0.5) (* b 0.5) 1)
                        (gl:color r g b 1))
                 (gl:with-primitives :lines
                   (gl:vertex 0 (* i d))
                   (gl:vertex 512 (* i d))
                   (loop for j below 20 by 5
                         do (gl:vertex 0 (+ (* s j) (* i d)))
                            (gl:vertex 10 (+ (* s j) (* i d)))))
                 (gl:with-primitives :line-strip
                   (loop for j from 1 below 512
                         for y = (aref times j)
                         ;; 10/1000000.0 =  10px/ms
                         do (gl:vertex j (+ (* i d) (* s (/ y 1000000.0))))))
                 (incf i)
            finally (gl:with-primitives :lines
                      (gl:color 1 1 1 1)
                      (gl:vertex x1 0)
                      (gl:vertex x1 (* 13 d)))))))

