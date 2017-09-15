(in-package 3bgl-sg2)

(defparameter +globals-binding+ 0)
(defparameter +materials-binding+ 1)
(defparameter +per-object-binding+ 2)

(defclass resource-manager ()
  ;; buffers is indexed by a 'vertex format' as created by
  ;; buffer-builder::vertex-format-for-layout, values are buffer-set
  ;; objects corresponding to layout.
  ((buffers :initform (make-hash-table :test 'equalp) :reader buffers)
   (index-buffer :initform (make-instance 'index-buffer) :reader index-buffer)
   (objects :initform (make-hash-table :test 'equalp) :reader objects)
   (materials :initform (make-hash-table :test 'equalp) :reader materials)
   (previous-material :initform nil :accessor previous-material)))

(defvar *resource-manager* nil)
(defvar *foo* nil)

(defmacro with-resource-manager (() &body body)
  `(let ((*resource-manager* (make-instance 'resource-manager)))
     (setf *foo* *resource-manager*)
     ,@body))

(defclass vbo ()
  ((vbo :initform nil :accessor vbo)
   (stride :initarg :stride :reader stride)
   ;; next available element
   (next :initform 0 :accessor next)
   ;; size in units of STRIDE
   (size :initform 0 :accessor size)))

(defclass index-buffer (vbo)
  ((index-type :initarg :index-type :reader index-type))
  ;; todo: calculate stride from type
  (:default-initargs :stride 2 :index-type :unsigned-short))

(defun index-type-size (type)
  (ecase type (:unsigned-byte 1) (:unsigned-short 2) (:unsigned-int 4)))

(defun calc-min-size (count &key (alloc-granularity 1024))
;;; todo: more efficient growth (and/or preallocate)
  (* alloc-granularity
     (ceiling count alloc-granularity)))

(defmethod grow-buffer (buffer new-size
                        &key (stride (stride buffer))
                          (size (size buffer)))
  (if (> new-size size)
      (let ((vbo (gl:create-buffer))
            (octets (* new-size stride)))
        (format t "growing buffer from ~s to ~s elements = ~a -> ~a bytes (stride ~s)~%"
                size new-size (* size stride) octets stride)
        (assert vbo)
        (gl:named-buffer-storage vbo (cffi:null-pointer) '(:dynamic-storage)
                                 :end octets)
        (when (vbo buffer)
          (%gl:copy-named-buffer-sub-data (vbo buffer) vbo 0 0
                                          (* stride (next buffer)))
          (gl:delete-buffers (list (shiftf (vbo buffer) nil))))
        (setf (vbo buffer) vbo)
        new-size)
      size))

(defun reset-buffer (buffer)
  (setf (next buffer) 0
        (size buffer) 0)
  (let ((vbo (shiftf (vbo buffer) nil)))
    (when vbo (gl:delete-buffers (list vbo)))))
(defun reset-buffer-set (bs)
  (setf (next bs) 0
        (size bs) 0)
  (let ((bindings (shiftf (bindings bs) nil)))
    (when bindings (gl:delete-buffers (mapcar 'vbo bindings)))))

(defun upload-index-data (buffer pointer count type)
  (assert (eq type (index-type buffer)))
  ;; make sure buffer has enough space (grow+copy if needed)
  (let ((start (next buffer))
        (new-size (calc-min-size (+ count (next buffer))
                                 ;; grow by 1Mi elements
                                 ;; (~2MB)
                                 :alloc-granularity (expt 2 20))))
    (assert (>= new-size (size buffer)))
    (assert (>= new-size (+ count (next buffer))))
    (setf (size buffer) (grow-buffer buffer new-size))
    (%gl:named-buffer-sub-data (vbo buffer)
                               (* (stride buffer) (next buffer))
                               (* (stride buffer) count)
                               pointer)
    (incf (next buffer) count)
    (list start count)))

(defclass buffer-binding ()
  ;; parameters to bind-vertex-buffer
  ((vbo :initform nil :accessor vbo)
   (stride :initarg :stride :reader stride)
   (index :initarg :index :initform 0 :reader index)
   (offset :initarg :offset :initform 0 :reader offset)
   ;; link back to parent so we can get size/next from it
   (parent :initarg :parent :reader parent)))

(defmethod next ((b buffer-binding))
  (next (parent b)))
(defmethod size ((b buffer-binding))
  (size (parent b)))

(defclass buffer-set ()
  ;; 'vertex format' and correponding VAO of this buffer (should be
  ;; shared with all others of same format, eventually may want to
  ;; move to higher level object, but this way can point directly to
  ;; this from mesh object and have all needed info)
  ((vertex-format :initarg :vertex-format :reader vertex-format)
   (vao :initarg :vao :reader vao)
   ;; list of buffer-binding objects
   (bindings :initarg :bindings :accessor bindings)
   ;; next available index and total size of buffer, in vertices
   (next :initform 0 :accessor next)
   (size :initform 0 :accessor size)))

(defun get-buffer-set (format)
  (or (gethash format (buffers *resource-manager*))
      (let* ((stride (getf (nthcdr 6 (first format)) :stride))
             (bs (make-instance 'buffer-set
                                :vertex-format format
                                :vao (caadr
                                      (scenegraph::canonicalize-state
                                       :vertex-format format)))))
        (setf (gethash format (buffers *resource-manager*)) bs)
        (setf (bindings bs)
              (list (make-instance 'buffer-binding :stride stride
                                                   :index 0 :offset 0
                                                   :parent bs)))
        bs)))


(defun buffer-geometry (buffer-set count &rest pointers)
  (let* ((start (next buffer-set))
         (new-size (calc-min-size (+ count (next buffer-set))
                                  ;; allocate space for 64k vertices at
                                  ;; a time (probably ~1-2MB). Probably
                                  ;; can be larger but not sure how
                                  ;; many different formats will be
                                  ;; used at once in practice
                                  :alloc-granularity (expt 2 16))))
    (assert (>= new-size (size buffer-set)))
    (assert (= (length pointers) (length (bindings buffer-set))))
    (loop for binding in (bindings buffer-set)
          for pointer in pointers
          do (assert (>= new-size (+ count start)))
             (grow-buffer binding new-size)
             (%gl:named-buffer-sub-data (vbo binding)
                                        (* (stride binding) start)
                                        (* (stride binding) count)
                                        pointer))
    (setf (size buffer-set) new-size)
    (incf (next buffer-set) count)
    (list start count)))



(defclass mesh ()
  ;; arguments to DrawElementsBaseVertex, (assuming :unsigned-short
  ;; indices and :triangles primitive type)
  ((index-count :initarg :count :reader index-count)
   ;; index into global index buffer of first index for this mesh
   (first-index :initarg :first :reader first-index)
   ;; offset added to index values to get actual vertex index in buffer-set
   (base-vertex :initarg :base :reader base-offset)
   ;; material data = ?
   (material :initarg :material :reader material)
   ;; ref to buffer set storing the mesh vertex data
   (buffer-set :initarg :buffer-set :reader buffer-set)))

(defclass object ()
  ;; group of meshes which are 'the same object' in some sense (shared
  ;; skeleton in particular, shared transform, possibly shared
  ;; geometry data).
  ;;;
  ;; for now, assuming all culled as group
  ((parts :initarg :parts :reader parts)))

(defun reset-object (object)
  ;; no foreign state to clean up, so just make sure we don't keep any
  ;; refs to things that do have state alive
  (setf (slot-value object 'parts) nil))

#++(defun get-object (loader name)
  (or (gethash (list loader name) (meshes *resource-manager*))
      ()))









(defun reset-manager (manager)
  (let ((a (alexandria:hash-table-values (buffers manager))))
    (clrhash (buffers manager))
    (map nil 'reset-buffer-set a))
  (reset-buffer (index-buffer manager))
  (let ((a (alexandria:hash-table-values (objects manager))))
    (clrhash (objects manager))
    (map nil 'reset-object a)))
