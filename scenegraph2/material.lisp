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
   ;; if set, names slot in material ssbo to store # of valid materials
   (count-var :initform nil :initarg :count-var :accessor count-var)
   ;; should be hash table of default values for material params
   ;; currently evaluated when writer is compiled, so need to manually
   ;; mark shader dirty when mofifying
;;; todo: either mark dirty automatically or check at runtime
   (defaults :initform (make-hash-table) :initarg :defaults :reader defaults)
   ;; hash of values for top-level slots of material ssbo
   (globals :initform (make-hash-table) :initarg :globals :reader globals)))

;; todo: fallback shader in case something uses undefined material
(defmethod program ((m material))
  (scenegraph::get-state (state m) :program))

(defmethod get-material (name)
  (if (typep name 'material)
      name
      (gethash name (materials *resource-manager*))))

(defun update-material (name &key repack)
  (let ((m (get-material name)))
    (when m
      (when (or repack
                (not (packing m)))
        (let* ((packing (calculate-packing (program m)
                                           :index +materials-binding+))
               (*writer-defaults* (defaults m)))
          (setf (packing m) packing)
          (setf (material-writer m)
                (make-writer-for-layout packing (count-var m)))))

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
              (funcall (material-writer m) (globals m) p buffer-size
                       :entries (materials m))
              (gl:named-buffer-storage (material-ssbo m) p () :end buffer-size))))
        (setf (dirty m) nil)))))


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

    ;;(assert m)
    (when (and m (program m))
      (3bgl-shaders::use-program (program m))
      (when (dirty m)
        (update-material m))
      (when (material-ssbo m)
        (%gl:bind-buffer-base :shader-storage-buffer +materials-binding+
                              (material-ssbo m)))
      (scenegraph::apply-state (state m)
                               (state (previous-material *resource-manager*)))

      (setf (previous-material *resource-manager*) m))))
