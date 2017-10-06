(in-package #:3bgl-sg2)

(defclass material ()
  ((state :initarg :state :reader state)
   (dirty :initform t :accessor dirty)
   (material-ssbo :initform nil :accessor material-ssbo)
   (ssbo-size :initform 0 :accessor ssbo-size)
   (block-size :initform 0 :accessor block-size)
   (packing :initform nil :accessor packing)
   (material-writer :initform nil :accessor material-writer)
   ;; lisp-side vector of material data, vector of hash tables?
   (materials :initform (make-array 16 :adjustable t :fill-pointer 0) :reader materials)
   (material-index :initform (make-hash-table :test 'equalp) :reader material-index)
   ;; if set, names slot in material ssbo to store # of valid materials
   (count-var :initform nil :initarg :count-var :accessor count-var)
   ;; should be hash table of default values for material params
   ;; currently evaluated when writer is compiled, so need to manually
   ;; mark shader dirty when mofifying
;;; todo: either mark dirty automatically or check at runtime
   (defaults :initform (make-hash-table) :initarg :defaults :reader defaults)
   ;; hash of values for top-level slots of material ssbo
   (globals :initform (make-hash-table) :initarg :globals :reader globals)
   ;; packing for per-object buffer (not handled directly here, but
   ;; layout is per material)
   (per-object-packing :initform nil :accessor per-object-packing)))

;; todo: fallback shader in case something uses undefined material
(defmethod program ((m material))
  (scenegraph::get-state (state m) :program))

(defmethod get-material (name)
  (if (typep name 'material)
      name
      (gethash name (materials *resource-manager*))))

(defun intern-material (name value)
  (let ((m (get-material name)))
    (or (gethash value (material-index m))
        (setf (dirty m) t
              (gethash value (material-index m))
              (vector-push-extend value (materials m))))))

(defparameter *repack* (make-hash-table :test 'equal))

(defun update-material (name &key repack)
  (when (and *repack* (not (gethash name *repack*)))
    (setf repack t)
    (setf (gethash name *repack*) t))
  (let ((m (get-material name)))
    (when m
      (restart-case
          (progn
            (3bgl-shaders::ensure-compiled (program m))
            (when (or repack
                      (not (packing m)))
              (let* ((packing (3bgl-ssbo:calculate-layout
                               (alexandria:hash-table-values
                                (3bgl-shaders::ssbos (program m)))
                               (alexandria:hash-table-values
                                (3bgl-shaders::structs (program m)))
                               :index +materials-binding+))
                     (3bgl-ssbo::*writer-defaults* (make-hash-table)))
                (unless (equal packing (packing m))
                  (setf (dirty m) t))
                (setf (packing m) packing)
                (setf (material-writer m)
                      (3bgl-ssbo::make-writer-for-layout packing
                                                         (count-var m)))))
            (unless (typep (per-object-packing m)
                           '3bgl-ssbo::ssbo-layout/static)
              (setf (per-object-packing m)
                    (make-instance '3bgl-ssbo::ssbo-layout/static)))
            (when (or repack
                      (not (3bgl-ssbo::packing
                            (per-object-packing m))))
              (let* ((packing (3bgl-ssbo:calculate-layout
                               (alexandria:hash-table-values
                                (3bgl-shaders::ssbos (program m)))
                               (alexandria:hash-table-values
                                (3bgl-shaders::structs (program m)))
                               :index +per-object-binding+))
                     (3bgl-ssbo::*writer-defaults* (make-hash-table)))
                (setf (3bgl-ssbo::packing
                       (per-object-packing m))
                      packing)))

            (when (and (dirty m) (packing m))
              ;; todo: don't recreate whole buffer every time. at minimum
              ;; probably shouldn't reallocate if size is same or
              ;; smaller. either allocate larger to start with or switch to
              ;; mutable API and use buffer-data instead of buffer-storage
              (when (material-ssbo m)
                (gl:delete-buffers (list (shiftf (material-ssbo m) nil))))
              (setf (material-ssbo m) (gl:create-buffer))

              (let* ((count (fill-pointer (materials m)))
                     (base (getf (packing m) :base))
                     (stride (getf (packing m) :stride))
                     (buffer-size (+ base (* count stride))))
                (when (material-writer m)
                  (cffi:with-foreign-object (p :char buffer-size)
                    (loop for i below buffer-size
                          do (setf (cffi:mem-aref p :unsigned-char i) 0))
                    (funcall (material-writer m) (globals m) p buffer-size
                             :entries (materials m))
                    (gl:named-buffer-storage (material-ssbo m) p ()
                                             :end buffer-size))))
              (setf (dirty m) nil)))
        (reset-material ()
          (reset-material m))))))

(defun reset-material (name)
  (let ((m (get-material name)))
    (when m
      (setf (fill-pointer (materials m)) 0)
      (clrhash (material-index m))
      (setf (dirty m) t)
      (setf (ssbo-size m) 0)
      (when (material-ssbo m)
        (gl:delete-buffers (list (shiftf (material-ssbo m) nil)))))))

(defun delete-material (name)
  (let ((m (get-material name)))
    (when m
      (when (material-ssbo m)
        (gl:delete-buffers (list (shiftf (material-ssbo m) nil))))
      (when (program m)
        (3bgl-shaders::reset-program (program m))))))

(defun make-material (name state &key (defaults (make-hash-table))
                                   count-var)
  (when (get-material name)
    (delete-material (gethash name (materials *resource-manager*))))
  (setf (gethash name (materials *resource-manager*))
        (make-instance 'material :state state :defaults defaults
                                 :count-var count-var)))

(defun bind-material (name)
  (let ((m (get-material name)))
    ;; not sure if missing material should be an error here or not?
    ;; possibly should load some debug material instead, with option
    ;; to error?
    #++(assert m)

    (when (and m (program m))
      (when (dirty m)
        (update-material m))
      (when (material-ssbo m)
        (%gl:bind-buffer-base :shader-storage-buffer +materials-binding+
                              (material-ssbo m)))
      (scenegraph::apply-state (state m)
                               (when (previous-material *resource-manager*)
                                 (state
                                  (previous-material *resource-manager*))))
      (setf (previous-material *resource-manager*) m))))
