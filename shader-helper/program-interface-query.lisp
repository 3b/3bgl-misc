(defpackage #:basecode-piq
  (:use #:cl)
  (:export :dump-resources
           :get-program-interface
           :get-program-resource-index
           :get-program-resource-name
           :get-program-resource))
(in-package #:basecode-piq)

;;; some wrappers for (core versions of) ARB_program_interface_query
;;; should eventually probably get cleaned up and moved to cl-opengl if useful

(defun get-program-interface (program interface)
  (cffi:with-foreign-object (p :int)
    (loop for i in `(:active-resources
                     ,@(unless (eq interface :atomic-counter-buffer)
                          '(:max-name-length))
                     ,@(when (member interface '(:uniform-block
                                                 :atomic-counter-buffer
                                                 :shader-storage-buffer))
                         '(:max-num-active-variables))
                     ,@(when (member interface
                                     '(:vertex-subroutine-uniform
                                       :tess-control-subroutine-uniform
                                       :tess-evaluation-subroutine-uniform
                                       :geometry-subroutine-uniform
                                       :fragment-subroutine-uniform
                                       :compute-subroutine-uniform))
                         '(:max-num-compatible-subroutines)))
          collect i
          do (setf (cffi:mem-aref p :int) -1)
             (%gl:get-program-interface-iv program interface i p)
          collect (cffi:mem-aref p :int))))

(defparameter *resource-props*
  '((:atomic-counter-buffer :buffer-binding :buffer-data-size :num-active-variables :active-variables :referenced-by-vertex-shader :referenced-by-tess-control-shader :referenced-by-tess-evaluation-shader :referenced-by-geometry-shader :referenced-by-fragment-shader :referenced-by-compute-shader)
    (:buffer-variable :name-length :type :array-size :offset :block-index :array-stride :matrix-stride :is-row-major :referenced-by-vertex-shader :referenced-by-tess-control-shader :referenced-by-tess-evaluation-shader :referenced-by-geometry-shader :referenced-by-fragment-shader :referenced-by-compute-shader :top-level-array-size :top-level-array-stride)
    (:compute-subroutine :name-length)
    (:compute-subroutine-uniform :name-length :array-size :num-compatible-subroutines :compatible-subroutines :location)
    (:fragment-subroutine :name-length)
    (:fragment-subroutine-uniform :name-length :array-size :num-compatible-subroutines :compatible-subroutines :location)
    (:geometry-subroutine :name-length)
    (:geometry-subroutine-uniform :name-length :array-size :num-compatible-subroutines :compatible-subroutines :location)
    (:program-input :name-length :type :array-size :referenced-by-vertex-shader :referenced-by-tess-control-shader :referenced-by-tess-evaluation-shader :referenced-by-geometry-shader :referenced-by-fragment-shader :referenced-by-compute-shader :location :is-per-patch)
    (:program-output :name-length :type :array-size :referenced-by-vertex-shader :referenced-by-tess-control-shader :referenced-by-tess-evaluation-shader :referenced-by-geometry-shader :referenced-by-fragment-shader :referenced-by-compute-shader :location :location-index :is-per-patch)
    (:shader-storage-block :name-length :buffer-binding :buffer-data-size :num-active-variables :active-variables :referenced-by-vertex-shader :referenced-by-tess-control-shader :referenced-by-tess-evaluation-shader :referenced-by-geometry-shader :referenced-by-fragment-shader :referenced-by-compute-shader)
    (:tess-control-subroutine :name-length)
    (:tess-control-subroutine-uniform :name-length :array-size :num-compatible-subroutines :compatible-subroutines :location)
    (:tess-evaluation-subroutine :name-length)
    (:tess-evaluation-subroutine-uniform :name-length :array-size :num-compatible-subroutines :compatible-subroutines :location)
    (:transform-feedback-varying :name-length :type :array-size)
    (:uniform :name-length :type :array-size :offset :block-index :array-stride :matrix-stride :is-row-major :atomic-counter-buffer-index :referenced-by-vertex-shader :referenced-by-tess-control-shader :referenced-by-tess-evaluation-shader :referenced-by-geometry-shader :referenced-by-fragment-shader :referenced-by-compute-shader)
    (:uniform-block :name-length :buffer-binding :buffer-data-size :num-active-variables :active-variables :referenced-by-vertex-shader :referenced-by-tess-control-shader :referenced-by-tess-evaluation-shader :referenced-by-geometry-shader :referenced-by-fragment-shader :referenced-by-compute-shader #++ :location)
    (:vertex-subroutine :name-length)
    (:vertex-subroutine-uniform :name-length :array-size :num-compatible-subroutines :compatible-subroutines :location)))


(defun get-program-resource-name (program interface index &optional max-length)
  (if max-length
      (cffi:with-foreign-pointer-as-string (p max-length)
        (%gl:get-program-resource-name program interface index max-length
                                       (cffi:null-pointer) p))
      (cffi:with-foreign-object (p :int)
        (setf (cffi:mem-aref p :int) 0)
        ;; todo: call get-program-resource-iv with :name-length instead
        (%gl:get-program-interface-iv program interface :max-name-length p)
        (get-program-resource-name program interface index
                                   (cffi:mem-aref p :int)))))

(defun get-program-resource-index (program interface name)
  (cffi:with-foreign-string (s name)
    (let ((i (%gl:get-program-resource-index program interface s)))
      (if (eql i (cffi:foreign-enum-value '%gl:enum :invalid-index))
          nil
          i))))

(defparameter *variable-types*
  ;; mapping of enums used for types to preferred name (no extension suffix etc)
  (alexandria:alist-hash-table
   ;; table 2.16 in gl 4.2 compat spec
   ;; table 7.3 in gl 4.5 core spec
   ;; falls back to cffi:foreign-enum-value if not found, so doesn't need
   ;; to be perfect
   (loop for k in '(:float :float-vec2 :float-vec3 :float-vec4
                    :double :double-vec2 :double-vec3 :double-vec4
                    :int :int-vec2 :int-vec3 :int-vec4
                    :unsigned-int :unsigned-int-vec2
                    :unsigned-int-vec3 :unsigned-int-vec4
                    :bool :bool-vec2 :bool-vec3 :bool-vec4
                    :float-mat2 :float-mat3 :float-mat4
                    :float-mat2x3 :float-mat2x4
                    :float-mat3x2 :float-mat3x4
                    :float-mat4x2 :float-mat4x3
                    :double-mat2 :double-mat3 :double-mat4
                    :double-mat2x3 :double-mat2x4
                    :double-mat3x2 :double-mat3x4
                    :double-mat4x2 :double-mat4x3
                    :sampler-1d :sampler-2d :sampler-3d :sampler-cube
                    :sampler-1d-shadow :sampler-2d-shadow
                    :sampler-1d-array :sampler-2d-array
                    :sampler-cube-map-array
                    :sampler-1d-array-shadow :sampler-2d-array-shadow
                    :sampler-2d-multisample :sampler-2d-multisample-array
                    :sampler-cube-shadow :sampler-cube-map-array-shadow
                    :sampler-buffer :sampler-2d-rect :sampler-2d-rect-shadow
                    :int-sampler-1d :int-sampler-2d :int-sampler-3d
                    :int-sampler-cube
                    :int-sampler-1d-array :int-sampler-2d-array
                    :int-sampler-cube-map-array :int-sampler-2d-multisample
                    :int-sampler-2d-multisample-array
                    :int-sampler-buffer :int-sampler-2d-rect
                    :unsigned-int-sampler-1d :unsigned-int-sampler-2d
                    :unsigned-int-sampler-3d :unsigned-int-sampler-cube
                    :unsigned-int-sampler-1d-array
                    :unsigned-int-sampler-2d-array
                    :unsigned-int-sampler-cube-map-array
                    :unsigned-int-sampler-2d-multisample
                    :unsigned-int-sampler-2d-multisample-array
                    :unsigned-int-sampler-buffer
                    :unsigned-int-sampler-2d-rect
                    :image-1d :image-2d :image-3d :image-2d-rect
                    :image-cube :image-buffer
                    :image-1d-array :image-2d-array :image-cube-map-array
                    :image-2d-multisample :image-2d-multisample-array
                    :int-image-1d :int-image-2d :int-image-3d
                    :int-image-2d-rect :int-image-cube :int-image-buffer
                    :int-image-1d-array :int-image-2d-array
                    :int-image-cube-map-array :int-image-2d-multisample
                    :int-image-2d-multisample-array
                    :unsigned-int-image-1d :unsigned-int-image-2D
                    :unsigned-int-image-3d :unsigned-int-image-2d-rect
                    :unsigned-int-image-cube :unsigned-int-image-buffer
                    :unsigned-int-image-1d-array :unsigned-int-image-2d-array
                    :unsigned-int-image-cube-map-array
                    :unsigned-int-image-2d-multisample
                    :unsigned-int-image-2d-multisample-array
                    :unsigned-int-atomic-counter)
         for v = (cffi:foreign-enum-value '%gl:enum k)
         collect (cons v k))))


(defparameter *property-translators*
  (flet ((opt-bool (k v)
           (unless (zerop v)
             (list k (if (eql v 1) t v))))
         (enum (k v)
           (list k
                 (or (gethash v *variable-types*)
                     (cffi:foreign-enum-keyword '%gl:enum v)))))
    
    (list
      :REFERENCED-BY-VERTEX-SHADER #'opt-bool
      :REFERENCED-BY-TESS-CONTROL-SHADER #'opt-bool
      :REFERENCED-BY-TESS-EVALUATION-SHADER #'opt-bool
      :REFERENCED-BY-GEOMETRY-SHADER #'opt-bool
      :REFERENCED-BY-FRAGMENT-SHADER #'opt-bool
      :REFERENCED-BY-COMPUTE-SHADER #'opt-bool
      :TYPE #'enum)))

(defun get-program-resource (program interface index)
  ;; allow using name instead of numeric index
  (unless (numberp index)
    (setf index (get-program-resource-index program interface index))
    (unless index
      (return-from get-program-resource nil)))

  (let* ((full-props (cdr (assoc interface *resource-props*)))
         (props (remove :active-variables full-props))
         (result nil)
         (count (length props)))
    (when props
     (cffi:with-foreign-objects ((in :int count)
                                 (out :int count)
                                 (len :int))
       (loop for prop in props
             for i from 0
             do (setf (cffi:mem-aref in :int i)
                      (cffi:foreign-enum-value '%gl:enum prop)))
       (%gl:get-program-resource-iv program interface index count
                                    in count len out)
       (let ((len (cffi:mem-aref len :int)))
         (unless (= len count)
           (warn "didn't get expected number of props? got ~s expected ~s"
                 len count))
         (setf result
               (loop for i from 0
                     for prop in props
                     for val = (cffi:mem-aref out :int i)
                     for translate = (getf *property-translators* prop)
                     when translate
                       append (funcall translate prop val)
                     else collect prop
                          and collect val))))
     (let ((vars (getf result :num-active-variables)))
       (when (and vars (plusp vars))
         (cffi:with-foreign-objects ((in :int 1)
                                     (out :int vars))
           (setf (cffi:mem-aref in :int)
                 (cffi:foreign-enum-value '%gl:enum :active-variables))
           (%gl:get-program-resource-iv program interface index 1
                                        in vars (cffi:null-pointer) out)
           (setf (getf result :active-variables)
                 (loop for i below vars
                       collect (cffi:mem-aref out :int i))))))
     result)))



(defun dump-resources (program)
  (loop for i in '(:uniform :uniform-block :atomic-counter-buffer
                   :program-input :program-output
                   :vertex-subroutine :tess-control-subroutine
                   :tess-evaluation-subroutine :geometry-subroutine
                   :fragment-subroutine :compute-subroutine
                   :vertex-subroutine-uniform :tess-control-subroutine-uniform
                   :tess-evaluation-subroutine-uniform
                   :geometry-subroutine-uniform
                   :fragment-subroutine-uniform :compute-subroutine-uniform
                   :transform-feedback-varying :buffer-variable
                   :shader-storage-block)
        for pif = (get-program-interface program i)
        for active = (getf pif :active-resources)
        for max-length = (getf pif :max-name-length)
        when (plusp active)
        do (format t "~&~s:~%~{  ~s ~s~%~}" i pif)
           (loop for x below active
                 do (format t "   ~s = ~s~%"
                            x (get-program-resource-name program i x
                                                         max-length))
                 do (format t "~{      ~s ~s~%~}"
                            (get-program-resource program i x)))

))
