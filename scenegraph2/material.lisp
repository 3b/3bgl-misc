(in-package #:3bgl-sg2)

(defclass material (3bgl-ssbo::ssbo)
  ((state :initarg :state :reader state)
   (material-index :initform (make-hash-table :test 'equalp) :reader material-index)
   ;; should be hash table of default values for material params
   ;; currently evaluated when writer is compiled, so need to manually
   ;; mark shader dirty when mofifying
;;; todo: either mark dirty automatically or check at runtime
   (defaults :initform (make-hash-table) :initarg :defaults :reader defaults)
   ;; packing for per-object buffer (not handled directly here, but
   ;; layout is per material)
   (per-object-packing :initform (make-instance '3bgl-ssbo::ssbo-layout/static)
                       :accessor per-object-packing))
  (:default-initargs
   :variable-size-data (make-array 16 :adjustable t :fill-pointer 0) ))

(defmethod ssbo-size ((m material))
  (3bgl-ssbo::ssbo-size m))

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
        (setf (3bgl-ssbo::dirty m) t
              (gethash value (material-index m))
              (vector-push-extend value (3bgl-ssbo::variable-size-data m))))))

(defun calculate-layout-for-program (program index)
  (3bgl-ssbo:calculate-layout
   (alexandria:hash-table-values (3bgl-shaders::ssbos program))
   (alexandria:hash-table-values (3bgl-shaders::structs program))
   :index index))

(defun update-material (name &key repack)
  (flet ((needs-repack (x)
           (or repack
               (not (3bgl-ssbo::packing x)))))
    (let ((m (get-material name)))
      (when m
        (restart-case
            (progn
              (3bgl-shaders::ensure-compiled (program m))

              (when (needs-repack m)
                (let* ((packing (calculate-layout-for-program
                                 (program m) +materials-binding+)))
                  (3bgl-ssbo::update-ssbo-layout m packing
                                                 :defaults (defaults m))))

              (when (needs-repack (per-object-packing m))
                (setf (3bgl-ssbo::packing (per-object-packing m))
                      (calculate-layout-for-program
                       (program m) +per-object-binding+))))
          (reset-material ()
            (reset-material m)))))))

(defun reset-material (name)
  (let ((m (get-material name)))
    (when m
      (setf (fill-pointer (3bgl-ssbo::variable-size-data m)) 0)
      (clrhash (material-index m))
      (3bgl-ssbo::destroy m))))

(defun delete-material (name)
  (let ((m (get-material name)))
    (when m
      (3bgl-ssbo::destroy m)
      (when (program m)
        (3bgl-shaders::reset-program (program m))))))

(defun make-material (name state &key (defaults (make-hash-table)) count-var
                                   (class (if (and (consp name)
                                                   (find-class (car name)))
                                              (car name)
                                              'material)))
  (when (get-material name)
    (delete-material (gethash name (materials *resource-manager*))))
  (setf (gethash name (materials *resource-manager*))
        (make-instance class :state state :defaults defaults
                             :count-slot count-var)))

(defun ensure-material (name state &rest r
                        &key defaults count-var class
                        &allow-other-keys)
  (declare (ignore defaults count-var class))
  (or
   (get-material name)
   (apply 'make-material name state r)))

(defun bind-material (name)
  (let ((m (get-material name)))
    ;; not sure if missing material should be an error here or not?
    ;; possibly should load some debug material instead, with option
    ;; to error?
    #++(assert m)

    (when (and m (program m))
      (when (3bgl-ssbo::dirty m)
        (update-material m))
      (3bgl-ssbo::bind-ssbo m +materials-binding+)
      (scenegraph::apply-state (state m)
                               (when (previous-material *resource-manager*)
                                 (state
                                  (previous-material *resource-manager*))))
      (setf (previous-material *resource-manager*) m))))
