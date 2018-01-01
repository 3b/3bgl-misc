(in-package 3bgl-sg2)

(defparameter +globals-binding+ 0)
(defparameter +materials-binding+ 1)
(defparameter +per-object-binding+ 2)
(defvar *globals-program* '3bgl-sg2-shaders-common::common-vertex)

(defparameter *no* 0)
(defparameter *draws* 0)
(defparameter *objects* 0)
(defparameter *once* t)

(defvar *timing-helper* nil)
(defclass resource-manager ()
  ;; buffers is indexed by a 'vertex format' as created by
  ;; buffer-builder::vertex-format-for-layout, values are buffer-set
  ;; objects corresponding to layout.
  ((buffers :initform (make-hash-table :test 'equalp) :reader buffers)
   (index-buffer :initform (make-instance 'index-buffer) :reader index-buffer)
   (objects :initform (make-hash-table :test 'equalp) :reader objects)
   (textures :initform (make-hash-table :test 'equalp) :reader textures)
   (handles :initform (make-hash-table :test 'equalp) :reader handles)
   (samplers :initform (make-hash-table :test 'equalp) :reader samplers)
   (materials :initform (make-hash-table) :reader materials)
   (material-names :initform (make-hash-table :test 'equalp) :reader material-names)
   (previous-material :initform nil :accessor previous-material)
   (programs :initform (make-hash-table :test 'equalp) :reader programs)
   (globals :initform (make-hash-table) :reader %globals)
   (globals-layout :initform (make-instance '3bgl-ssbo::ssbo-layout/static)
                   :reader globals-layout)
   (modified-functions :initform (make-hash-table) :reader modified-functions)
   ;; (interned) material -> list (vector?) of things to draw
   (draw-lists :initform (make-hash-table) :reader draw-lists)
   ;; globals + per-object data
   (streaming-ssbo :initform 0 :accessor streaming-ssbo)
   ;; multi-draw-indirect command lists
   (command-ssbo :initform 0 :accessor command-ssbo)))


(defvar *resource-manager* nil)
(defvar *foo* nil)

(defmethod initialize-instance :after ((m resource-manager) &key)
  ;; set some defaulta for globals
  (loop for mat in '(3bgl-sg2-shaders-common::mvp
                     3bgl-sg2-shaders-common::vp
                     3bgl-sg2-shaders-common::v
                     3bgl-sg2-shaders-common::p
                     3bgl-sg2-shaders-common::ui-matrix)
        do (setf (gethash mat (%globals m)) (sb-cga:identity-matrix)))
  (setf (gethash '3bgl-sg2-shaders-common::ui-scale (%globals m)) 1.0)
  (setf (streaming-ssbo m)
        (3bgl-ssbo::make-persistent-mapped-buffer
         ;; 16MB x triple-buffered. 16M is enough for 41 floats each
         ;; for 100k objects, so probably overkill. possibly should
         ;; reduce it once code is smart enough to handle running out
         ;; of space reasonably (and/or using space more efficiently)
         ;; (16MB is size of 2kx2kxRGBA texture without mipmaps
         ;; though, so possibly not worth worrying about optimizing)
         (expt 2 24) :regions 3))
  (setf (command-ssbo m)
        (3bgl-ssbo::make-persistent-mapped-buffer
         ;; 4MB x triple-buffered. enough for ~209k draws. should
         ;; reduce to match streaming-ssbo once there is a better idea
         ;; of size of per-object data. One command is (* 4 5) bytes
         (expt 2 22) :regions 3)))

(defun reset-resource-manager (manager)
  (macrolet ((reset (slot fun)
               `(let ((v (alexandria:hash-table-values (,slot manager))))
                  (clrhash (,slot manager))
                  (map nil ',fun v))))
    (when manager
      (3bgl-ssbo::reset-persistent-mapped-buffer (streaming-ssbo manager))
      (3bgl-ssbo::reset-persistent-mapped-buffer (command-ssbo manager))
      (reset buffers reset-buffer-set)
      (reset objects reset-object)
      ;; reset handles before textures and samplers
      (reset handles reset-handle)
      (reset textures reset-texture)
      (reset samplers reset-sampler)
      (reset materials reset-material)
      (setf (previous-material manager) nil)
      (reset-buffer (index-buffer manager))
      (reset programs 3bgl-shaders::reset-program))))

(defmethod ensure-buffers ((m resource-manager))
  (3bgl-ssbo::ensure-buffers (streaming-ssbo m))
  (3bgl-ssbo::ensure-buffers (command-ssbo m)))

(defmethod next-region ((m resource-manager))
  (3bgl-ssbo::next-region (streaming-ssbo m))
  (3bgl-ssbo::next-region (command-ssbo m)))

(defparameter *live-managers* (make-hash-table))


(defun notice-modified-shaders (functions)
  (format t "notice-modified-shaders ~s~%" functions)
  ;; fixme: locks or something, though probably not recompiling things
  ;; while starting/exiting the program very often
  (loop for rm in (alexandria:hash-table-keys *live-managers*)
        do (loop for f in functions
                 do (setf (gethash f (modified-functions rm)) t))))

(pushnew 'notice-modified-shaders 3bgl-shaders::*modified-function-hook*)

(defmacro with-resource-manager ((&key timing) &body body)
  `(let* ((*resource-manager* (make-instance 'resource-manager))
          (*timing-helper* ,timing))
     (setf *foo* *resource-manager*)
     (setf (gethash *resource-manager* *live-managers*) *resource-manager*)
     (setf (gethash *globals-program* (modified-functions *resource-manager*))
           t)
     (unwind-protect
          (progn
            ,@body)
       (remhash *resource-manager* *live-managers*)
       (reset-resource-manager *resource-manager*))))

(defclass strided-buffer (3bgl-ssbo::buffer)
  ((stride :initarg :stride :reader stride))
  (:default-initargs :flags '(:dynamic-storage)))

(defmethod vbo ((b strided-buffer))
  (3bgl-ssbo::name b))

(defclass vbo (strided-buffer)
  (;; next available element
   (next :initform 0 :accessor next)))

(defmethod size ((vbo vbo))
  ;; size in units of STRIDE
  (/ (3bgl-ssbo::size vbo) (stride vbo)))

(defmethod (setf size) (new-size (vbo vbo))
  ;; size in units of STRIDE
  (assert (= new-size
             (/ (3bgl-ssbo::size vbo) (stride vbo))))
  new-size)

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

(defmethod grow-buffer (buffer new-size &key)
  (let ((stride (stride buffer))
        (size (size buffer)))
    (if (> new-size size)
        (progn
          (format t "growing buffer from ~s to ~s elements = ~a -> ~a bytes (stride ~s)~%"
                  size new-size (* size stride) (* new-size stride) stride)
          (3bgl-ssbo::resize buffer (* new-size stride)
                             :copy-octets (* stride (next buffer)))
          (size buffer))
        size)))

(defun reset-buffer (buffer)
  (3bgl-ssbo::destroy buffer)
  (setf (next buffer) 0
        (size buffer) 0))

(defun reset-buffer-set (bs)
  (setf (next bs) 0
        (size bs) 0)
  (let ((bindings (shiftf (bindings bs) nil)))
    (map 'nil '3bgl-ssbo::destroy bindings)))

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


(defclass buffer-binding (strided-buffer)
  ;; parameters to bind-vertex-buffer
  ((index :initarg :index :initform 0 :reader index)
   (offset :initarg :offset :initform 0 :reader offset)
   ;; link back to parent so we can get size/next from it
   (parent :initarg :parent :reader parent)))

(defmethod next ((b buffer-binding))
  (next (parent b)))
(defmethod size ((b buffer-binding))
  (size (parent b)))


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

  ;; for now, assuming all culled as group
  ((parts :initarg :parts :reader parts)))

(defun reset-object (object)
  ;; no foreign state to clean up, so just make sure we don't keep any
  ;; refs to things that do have state alive
  (setf (slot-value object 'parts) nil))

#++(defun get-object (loader name)
     (or (gethash (list loader name) (meshes *resource-manager*))
         ()))

(defun get-program (&rest components)
  ;; sort shaders by name of stage
  (setf components (alexandria:alist-plist
                    (sort (alexandria:plist-alist components)
                          'string< :key 'car)))
  (or (gethash components (programs *resource-manager*))
      (setf (gethash components (programs *resource-manager*))
            (apply '3bgl-shaders::shader-program components))))

(defun update-materials-for-recompiled-shaders (rm)
  (let ((mf (modified-functions rm))
        (mm (make-hash-table)))
    (when (gethash *globals-program* mf)
      (format t "rebuild globals layout~%")
      (setf (3bgl-ssbo::packing (globals-layout rm))
            (multiple-value-bind (a b c blocks structs)
                (3bgl-shaders::generate-stage :vertex *globals-program*
                                              :expand-uniforms t)
              (declare (ignore a b c))
              (let ((pack (3bgl-ssbo::calculate-layout blocks structs :index 0)))
                pack))))
    ;; clear previous material so shaders get reloaded
    (setf (previous-material rm) nil)
    ;; see if any materials have modified programs
    (loop for m in (alexandria:hash-table-values (materials *resource-manager*))
          for sp = (program m)
          for stages = (alexandria:hash-table-values (3bgl-shaders::stages sp))
          do (loop for s in stages
                   when (gethash s (modified-functions rm))
                     do (setf (gethash m mm) t)
                     and return nil))
    ;; update materials with modified programs
    (loop for m in (alexandria:hash-table-keys mm)
          do (update-material m :repack t)))
  (clrhash (modified-functions rm)))

(defun add-draw (material material-data index vertex matrix)
  ;; material-data is material-id
  ;; index is (count offset)
  ;; vertex is (buffer-set start count)
  (let ((h (gethash material (draw-lists *resource-manager*))))
    ;;draw-lists is hash table of (material -> hash table of
    ;;(buffer-set -> draw))
    (unless h
      (setf h (make-hash-table))
      (setf (gethash material (draw-lists *resource-manager*)) h))
    (push (list material-data index vertex matrix) (gethash (first vertex) h))))

(3bgl-ssbo::define-ssbo-writer write-rm-globals (layout pointer size rm)
  (let* ((size (3bgl-ssbo::check-size 0))
         (g (%globals rm)))
    (macrolet ((s (slot &optional (d sb-cga:+identity-matrix+))
                 `(3bgl-ssbo::set-slot ,slot (or (gethash ',slot g) ,d))))
      (s 3bgl-sg2-shaders-common::mvp)
      (s 3bgl-sg2-shaders-common::vp)
      (s 3bgl-sg2-shaders-common::v)
      (s 3bgl-sg2-shaders-common::p)
      (s 3bgl-sg2-shaders-common::ui-scale)
      (s 3bgl-sg2-shaders-common::ui-matrix))
    size))

(defun write-globals ()
  (let* ((rm *resource-manager*)
         (gl (globals-layout rm)))
    (when (3bgl-ssbo::packing gl)
      (3bgl-ssbo::with-current-region (p)
                                      (streaming-ssbo rm)
        (3bgl-ssbo::use-bytes (write-rm-globals gl p (3bgl-ssbo::remaining) rm))
        (3bgl-ssbo::bind-range :shader-storage-buffer +globals-binding+)))))

(defun set-global (var value)
  (setf (gethash var (%globals *resource-manager*)) value))
(defun clear-globals ()
  (clrhash (%globals *resource-manager*)))


(defmethod write-per-object (mat draws)
  (break "no per-object writer for material ~s?" mat)
  ;; not sure if this should have some default behavior or not...
  (values 0 0))

(defmethod primitive (mat)
  :triangles)

(defmethod submit-material-draws (material bs draws rm cs)
  (let ((vao (vao bs)))
    (%gl:vertex-array-element-buffer
     vao (vbo (index-buffer rm)))
    (loop for b in (bindings bs)
          ;; todo: use %gl:vertex-array-vertex-buffers?
          do (%gl:vertex-array-vertex-buffer
              vao (index b) (vbo b)
              (offset b) (stride b)))
    (gl:bind-vertex-array VAO))
  (multiple-value-bind (size count)
      (write-per-object material draws)
    (declare (ignorable size))
    (unless count
      (break "count = ~s, size=~s?" count size))
    (when count
      (3bgl-ssbo::with-current-region (p) cs
        (let* ((max (floor (3bgl-ssbo::remaining) (* 5 4))))
          (when (< max count)
            (cerror "continue"
                    "not enough space for draw commands. ~s / ~s~%"
                    max count)
            (setf count max)))
        ;;(setf count (min 25000 count))
        (macrolet ((add (offset value)
                     `(setf (cffi:mem-aref p :unsigned-int (+ i ,offset))
                            ,value)))
          (loop for draw in draws
                for index below count
                for (nil ;; material dataa
                     (index-offset index-count) ;; index
                     (bs base-vertex count)     ;;vertex
                     nil)                       ;; matrix
                  = draw
                for i = (* index 5)
                for base-instance = index
                do (let ((n index-count #++(min index-count 42)))
                     (add 0 n)
                     (incf *no* (floor n 3)))
                   (add 1 1)
                   (add 2 index-offset)
                   (add 3 base-vertex)
                   (add 4 base-instance)
                   (incf *objects*))
          (3bgl-ssbo::use-bytes (* 5 4 count)))
        (let ((offset (3bgl-ssbo::bind :draw-indirect-buffer)))
          (incf *draws*)
          (%gl:multi-draw-elements-indirect (primitive material)
                                            :unsigned-short
                                            offset
                                            count
                                            0))))))

(defun submit-draws ()
  (mark *timing-helper* :id :submit-draw-start)
  (setf *no* 0)
  (setf *draws* 0)
  (setf *objects* 0)
  (update-materials-for-recompiled-shaders *resource-manager*)
  (mark *timing-helper* :id :updated-materials)
  (ensure-buffers *resource-manager*)
  (mark *timing-helper* :id :ensured-buffers)
  (write-globals)
  (mark *timing-helper* :id :wrote-globals)
  (loop
    with rm = *resource-manager*
    with cs = (command-ssbo rm)
    for i from 0
    for mat-name being the hash-keys of (draw-lists *resource-manager*)
      using (hash-value buffer-sets)
    for material = (if (typep mat-name 'material)
                       mat-name
                       (gethash mat-name (materials *resource-manager*)))
    do (bind-material material)
       (loop
         for bs being the hash-keys of buffer-sets
           using (hash-value draws)
         do (submit-material-draws material bs draws rm cs)))
  (mark *timing-helper* :id :submitted-draws)
  (mark *timing-helper* :id (list :done-draw-loop
                                  (hash-table-count
                                   (draw-lists *resource-manager*))))
  ;; possibly should clear individual per-bs hashes in each entry to
  ;; avoid reallocation every frame?
  (clrhash (draw-lists *resource-manager*))
  ;; do this at end of draw to minimize the amount of things the sync
  ;; needs to wait for
  (next-region *resource-manager*)
  (mark *timing-helper* :id :next-region)
  (setf *once* nil))

