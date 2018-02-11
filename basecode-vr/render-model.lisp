(in-package #:basecode-vr)


(defparameter *bprintf* t)
(defmacro dprintf (format &rest r)
  `(when *bprintf* (format t ,format ,@r)))

(defclass render-model ()
  ((vert-buffer :accessor vert-buffer :initform 0)
   (index-buffer :accessor index-buffer :initform 0)
   (vert-array :accessor vert-array :initform 0)
   (texture :accessor texture :initform 0)
   (vertex-count :accessor vertex-count :initform 0)
   (model-name :accessor model-name :initform nil :initarg :name)))

(defclass render-model-manager ()
  ((render-models :accessor render-models
                  :initform (make-hash-table :test 'equal))
   (tracked-device-to-render-model :reader %tracked-device-to-render-model
                                   :initform (make-array
                                              vr::+max-tracked-device-count+))))

(defmethod run-main-loop :after ((w render-model-manager))
  (map nil 'cleanup (alexandria:hash-table-values (render-models w)))
  (clrhash (render-models w)))

(defmethod driver (o)
  (vr::get-tracked-device-property vr::+tracked-device-index-hmd+
                                   :tracking-system-name-string))

;;; finds a render model we've already loaded or loads a new one
(defmethod find-or-load-render-model ((o render-model-manager) name)
  (when (gethash name (render-models o))
    (return-from find-or-load-render-model (gethash name (render-models o))))

  ;; load the model if we didn't find one
  (let* ((model (vr::load-render-model-async name))
         (texture (when model (vr::load-texture-async
                               (getf model 'vr::diffuse-texture-id))))
         (render-model (when (and model texture)
                         (make-instance 'render-model
                                        :name name
                                        :model model :texture texture))))
    (vr::free-render-model model)
    (vr::free-texture texture)
    (setf (gethash name (render-models o)) render-model)))

;;; Create/destroy GL a Render Model for a single tracked device
(defmethod setup-render-model-for-tracked-device ((o render-model-manager) tracked-device-index)
  (when (> tracked-device-index vr::+max-tracked-device-count+)
    (return-from setup-render-model-for-tracked-device))

  ;; try to find a model we've already set up
  (let* ((render-model-name (vr::get-tracked-device-property
                             tracked-device-index :render-model-name-string))
         (render-model (find-or-load-render-model o render-model-name)))
    ;; todo: catch errors and set to :failed
    (if render-model
        (setf (aref (%tracked-device-to-render-model o) tracked-device-index)
              render-model)
        (progn
          (setf (aref (%tracked-device-to-render-model o) tracked-device-index)
                :loading)
          ;; return NIL to indicate it isn't loaded yet
          nil))))

;;; Create/destroy GL Render Models
(defmethod setup-render-models ((o render-model-manager))
  (fill (%tracked-device-to-render-model o) nil)
  (unless vr::*system*
    (return-from setup-render-models nil))

  (loop for tracked-device below vr::+max-tracked-device-count+
        when (vr::is-tracked-device-connected tracked-device)
          do (setup-render-model-for-tracked-device o tracked-device)))

(defun tracked-device-to-render-model (w i)
  (when (> i vr::+max-tracked-device-count+)
    (return-from tracked-device-to-render-model))
  (let ((d (aref (%tracked-device-to-render-model w) i)))
    (typecase d
      (render-model d)
      ((eql :loading)
       (setup-render-model-for-tracked-device w i)))))

;; allocates and populates the GL resources for a render model
(defmethod initialize-instance :after ((m render-model) &key model texture)
  ;; create and bind a VAO to hold state for this model
  (setf (vert-array m) (gl:gen-vertex-array))
  (gl:bind-vertex-array (vert-array m))

  (let ((size (cffi:foreign-type-size '(:struct vr::render-model-vertex-t))))

    ;; populate a vertex buffer
    (setf (vert-buffer m) (gl:gen-buffer))
    (gl:bind-buffer :array-buffer (vert-buffer m))
    (%gl:buffer-data :array-buffer
                     (* size (getf model 'vr::vertex-count))
                     (getf model 'vr::vertex-data)
                     :static-draw)

    ;; identify the components in the vertex buffer
    (flet ((offset (slot)
             (cffi:foreign-slot-offset
              '(:struct vr::render-model-vertex-t) slot)))
      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 3 :float nil size (offset 'vr::position))
      (gl:enable-vertex-attrib-array 1)
      (gl:vertex-attrib-pointer 1 2 :float nil size (offset 'vr::texture-coord))
      (gl:enable-vertex-attrib-array 2)
      (gl:vertex-attrib-pointer 2 3 :float nil size (offset 'vr::normal))))

  ;; create and populate the index buffer
  (setf (index-buffer m) (gl:gen-buffer))
  (gl:bind-buffer :element-array-buffer (index-buffer m))
  (%gl:buffer-data :element-array-buffer
                   (* 2 (getf model 'vr::triangle-count) 3)
                   (getf model 'vr::index-data)
                   :static-draw)

  (gl:bind-vertex-array 0)

  ;; create and populate the texture
  (setf (texture m) (gl:gen-texture))
  (gl:bind-texture :texture-2d (texture m))

  (when texture
    (gl:tex-image-2d :texture-2d 0 :rgba
                     (getf texture 'vr::width) (getf texture 'vr::height)
                     0 :rgba :unsigned-byte (getf texture 'vr::texture-map-data)))

  (gl:generate-mipmap :texture-2d)

  (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
  (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)

  (gl:tex-parameter :texture-2d :texture-max-anisotropy-ext
                    (gl:get* :max-texture-max-anisotropy-ext))

  (gl:bind-texture :texture-2d 0)

  (setf (vertex-count m) (* 3 (getf model 'vr::triangle-count))))

(defmethod cleanup ((m null)))

(defmethod cleanup ((m render-model))
  (gl:delete-buffers (list (shiftf (index-buffer m) 0)))
  (gl:delete-vertex-arrays (list (shiftf (vert-array m) 0)))
  (gl:delete-buffers (list (shiftf (vert-buffer m) 0))))

(defmethod draw ((m render-model))
  (gl:bind-vertex-array (vert-array m))

  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d (texture m))

  (%gl:draw-elements :triangles (vertex-count m) :unsigned-short 0)

  (gl:bind-vertex-array 0))
