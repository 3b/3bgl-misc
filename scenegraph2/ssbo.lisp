(in-package #:3bgl-sg2)

(defun get-buffer-for-binding (program index)
  (car
   (find-if (lambda (a)
              (eql (getf (getf (cddr a) :layout) :binding)
                   index))
            (alexandria:hash-table-values (3bgl-shaders::ssbos program)))))

(defun calculate-packing (p &key (index 0))
  (let* ((buffer-name (get-buffer-for-binding p index)))
    (when buffer-name
      (let ((pack ()))
        (loop for (name nil . rest) in (alexandria:hash-table-values
                                        (3bgl-shaders::structs p))
              do (push `(,name
                         (:struct
                          ()
                          ,@ (expand-glsl-types (getf rest :components))))
                       pack))
        (loop for (name nil . rest) in (alexandria:hash-table-values
                                        (3bgl-shaders::ssbos p))
              when (getf (getf rest :layout) :std430)
                do (push '(:packing :std430) pack)
              do (push `(,name
                         (:block
                             ()
                           ,@(expand-glsl-types (getf rest :components))))
                       pack))
        (let* ((packed (glsl-packing:pack-structs
                        (nreverse pack) :roots (list buffer-name)))
               (ssbo (assoc buffer-name packed :key 'car))
               ;; we assume the material ssbo ends with variable-size
               ;; member, so return size of initial part of block and
               ;; stride of elements in the last member
               (type (second ssbo))
               (last (car (last (getf type :members))))
               (struct (assoc (cadar (getf last :type)) packed :key 'car)))
          (list :packing type
                :struct (car struct)
                :types (alexandria:plist-hash-table
                        (loop for ((n) p) in packed
                              collect n collect (list* :struct p)))
                :base (getf type :size)
                :stride (getf last :stride)))))))

(defparameter *inv-base-types*
  (alexandria:plist-hash-table
   (reverse (alexandria:hash-table-plist glsl-packing:*base-types*))
   :test 'equalp))

(defparameter *foreign-type-map*
  (alexandria:plist-hash-table
   '((:float 32) :float
     (:float 64) :double
     ;; (:float 16) ??
     (:int 8) :int8
     (:int 16) :int16
     (:int 32) :int32
     (:int 64) :int64
     (:uint 8) :uint8
     (:uint 16) :uint16
     (:uint 32) :uint32
     (:uint 64) :uint64
     (:bool) :int32)
   :test 'equalp))

(defvar *writer-structs*)
(defvar *writer-defaults*)
(defvar *writer-functions*) ;; hash of type -> (write-form local-function-def)

(defun make-value-writer (type pointer value &key index slot)
  (unless (gethash (gethash type *writer-structs* type)
                   *writer-functions*)
    (let* ((orig-type type)
           (type (if (consp type)
                     type
                     (gethash type *writer-structs* type)))
           (ctype (gethash type *foreign-type-map*)))
      (if ctype
          (let ((fn (gensym (format nil "~@:(~a.~)" ctype))))
            (setf (gethash type *writer-functions*)
                  (list fn
                        `(,fn (pointer value &optional (index 0))
                              (setf (cffi:mem-aref pointer ,ctype index)
                                    ,(ecase (car type)
                                       (:bool
                                        `(if value 1 0))
                                       (:float
                                        (if (= (second type) 64)
                                            '(coerce value 'double-float)
                                            '(coerce value 'single-float)))
                                       (:int
                                        '(coerce value 'integer))))))))
          (ecase (car type)
            (:vec
             (let ((fn (gensym (format nil "~@:(vec~a/~a.~)"
                                       (third type)
                                       (gethash (second type)
                                                *foreign-type-map*
                                                (second type))))))
               (setf (gethash type *writer-functions*)
                     (list fn
                           `(,fn (pointer value)
                                 ,@(loop for i below (third type)
                                         append (make-value-writer
                                                 (second type)
                                                 'pointer
                                                 `(elt value ,i)
                                                 :index i)))))))
            (:mat
             ;; accept either a 4x4 or MxN matrix
             (let ((fn (gensym (format nil "~@:(mat~ax~a/~a.~)"
                                       (third type)
                                       (fourth type)
                                       (gethash (second type)
                                                *foreign-type-map*
                                                (second type)))))
                   (element-count (* (third type) (fourth type))))
               (destructuring-bind (&key matrix-stride major
                                    &allow-other-keys)
                   slot
                 (declare (ignorable major matrix-stride)))
               (setf (gethash type *writer-functions*)
                     (list fn
                           `(,fn (pointer value)
                                 (cond
                                   ((= (array-total-size value)
                                       ,element-count)
                                    ;; fixme: handle matrix-stride
                                    (loop for i below ,element-count
                                          do ,@(make-value-writer
                                                (second type)
                                                'pointer
                                                `(elt value i)
                                                :index 'i)))
                                   ((= (array-total-size value) 16)
                                    ;; todo: copy from upper-left of
                                    ;; column-major 4x4 array
                                    )

                                   ((= (array-rank value) 2)
                                    ;; todo: copy from upper-left of 2d array,
                                    ;; fill extra with identity
                                    )))))))
            (:array
             (let ((fn (gensym (format nil "~@:(array~a/~a.~)"
                                       (third type)
                                       (gethash (second type)
                                                *foreign-type-map*
                                                (second type))))))
               (setf (gethash type *writer-functions*)
                     ;; fixme: handle :* here instead of special case
                     ;; (needs to make sure it is last slot, and check
                     ;; pointer bounds)
                     (if (member (third type) '(:* *))
                         (list nil nil)
                         (list
                          fn
                          `(,fn (pointer value)
                                (--- array)))))))
            (:struct
             (let ((fn (gensym (format nil "~@:(struct/~a.~)" orig-type))))
               (setf (gethash orig-type *writer-functions*)
                     (list fn
                           `(,fn (pointer value)
                                 ,@(make-slot-writers (getf (cdr type) :members)
                                                      'value 'pointer
                                                      *writer-defaults*))))))))))
  (let ((fn (car (gethash type *writer-functions*))))
    (when fn
      (list (list* fn pointer value (when index (list index)))))))

(defun default-for-type (slot)
  (let ((type (car (getf slot :type))))
    (or (gethash (getf slot :name) *writer-defaults*)
        (typecase type
          ((cons (eql :bool))
           nil)
          ((cons (eql :int))
           0)
          ((cons (eql :float))
           0.0)
          ((cons (eql :vec) (cons (cons (eql :int))))
           (subseq #(0 0 0 1) 0 (third type)))
          ((cons (eql :vec) (cons (cons (eql :float))))
           (subseq #(0.0 0.0 0.0 1.0) 0 (third type)))
          ((cons (eql :mat))
           #(1 0 0 0
             0 1 0 0
             0 0 1 0
             0 0 0 1))))))

(defun make-slot-writer (slot pointer value)
  (destructuring-bind (&key type offset &allow-other-keys) slot
    (make-value-writer (car type)
                       (if (plusp offset)
                           `(cffi:inc-pointer ,pointer ,offset)
                           pointer)
                       value
                       :slot slot)))

(defun make-slot-writers (slots value-hash pointer defaults-hash)
  (declare (ignore defaults-hash))
  (loop for slot in slots
        for name = (getf slot :name)
        when (make-slot-writer slot pointer
                               `(gethash ',name ,value-hash
                                         ,(default-for-type slot)))
          append it))

(defun make-writer-for-layout (layout count-slot)
  (let* ((top-slots (getf (getf layout :packing) :members))
         (struct-type (getf layout :struct))
         (base (getf layout :base))
         (stride (getf layout :stride))
         (*writer-structs* (getf layout :types))
         (*writer-functions* (make-hash-table :test 'equalp))
         (count-slot (and count-slot
                          (find count-slot top-slots
                                :key (lambda (a) (getf a :name))))))
    (let ((*package* (or (find-package '3bgl-sg2) *package*))
          ;; calculate body first so we can add local functions around it
          (body `((assert (<= (+ ,base
                                 ,@ (when stride
                                      `((* ,stride (length entries)))))
                              size))
                  ,@(when count-slot
                      `((setf (gethash ',(getf count-slot :name) globals)
                              (length entries))))
                  ,@(make-slot-writers top-slots
                                       'globals 'pointer nil)
                  ,@(when (and struct-type (plusp stride))
                      `((loop for p2 = (cffi:inc-pointer pointer ,base)
                                then (cffi:inc-pointer p2 ,stride)
                              for mat across entries
                              for i from 0
                              do (progn
                                   ,@(make-value-writer (car struct-type)
                                                        'p2 'mat))))))))
      (if layout
          (let ((l `(lambda (globals pointer size &key entries)
                      (declare (ignorable entries))
                      (labels (,@ (remove nil
                                    (mapcar 'second
                                            (alexandria:hash-table-values
                                             *writer-functions*))))
                        ,@body))))
            (with-simple-restart (continue "disable material")
              (compile nil l)))
          (lambda (&rest r) (declare (ignore r)))))))
