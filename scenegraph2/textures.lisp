(in-package 3bgl-sg2)

(defclass sampler ()
  ((sampler :reader sampler :initarg :sampler)
   (spec :reader spec :initarg :spec)))

(defclass texture ()
  ((texture :reader texture :initarg :texture)
   (source :reader source :initarg :source)
   (target :reader target :initarg :target :initform :texture-2d)))

(defclass handle ()
  ((texture :reader texture :initarg :texture)
   (sampler :reader sampler :initarg :sampler)
   (handle :reader handle :initarg :handle)
   (resident :accessor resident :initform nil)))

(defun get-handle (texture sampler &key resident)
  (let* ((tx (get-texture texture))
         (s (get-sampler sampler))
         (h (or (gethash (list tx s) (handles *resource-manager*))
                (setf (gethash (list tx s) (handles *resource-manager*))
                      (make-instance
                       'handle
                       :texture tx :sampler s
                       :handle
                       (if s
                           (%gl:get-texture-sampler-handle-arb (texture tx)
                                                               (sampler s))
                           (%gl:get-texture-handle-arb (texture tx))))))))
    (when (and resident (not (resident h)))
      (%gl:make-texture-handle-resident-arb (handle h))
      (setf (resident h) t))
    h))

(defun get-sampler (name
                    &key
                      (min-filter :linear-mipmap-linear)
                      (mag-filter :linear)
                      (max-anisotropy 0.0)
                      (min-lod -1000)
                      (max-lod 1000)
                      ;;(swizzle-r :red)
                      ;;(swizzle-g :green)
                      ;;(swizzle-b :blue)
                      ;;(swizzle-a :alpha)
                      (wrap-s :repeat)
                      (wrap-t :repeat)
                      (wrap-r :repeat)
                      (border-color #(0.0 0.0 0.0 0.0))
                      (compare-mode :none)
                      (compare-func :lequal))
  (when (typep name '(or sampler null))
    (return-from get-sampler name))
  (when (gethash name (samplers *resource-manager*))
    (return-from get-sampler (gethash name (samplers *resource-manager*))))
  (macrolet ((floats (&rest vars)
               `(progn
                  ,@(loop for v in vars
                          collect `(setf ,v (coerce ,v 'single-float))))))
    (setf border-color (map 'vector (lambda (x) (coerce x 'single-float))
                            border-color))
    (floats max-anisotropy min-lod max-lod)
    (assert (member border-color '(#(0 0 0 0) #(0 0 0 1)
                                   #(1 1 1 0) #(1 1 1 1))
                    :test 'equalp))
    (let ((spec (list :min-filter min-filter
                      :mag-filter mag-filter
                      :max-anisotropy max-anisotropy
                      :min-lod min-lod
                      :max-lod max-lod
                      ;;:swizzle-r swizzle-r
                      ;;:swizzle-g swizzle-g
                      ;;:swizzle-b swizzle-b
                      ;;:swizzle-a swizzle-a
                      :wrap-s wrap-s
                      :wrap-t wrap-t
                      :wrap-r wrap-r
                      :border-color border-color
                      :compare-mode compare-mode
                      :compare-func compare-func)))
      (flet ((make-sampler ()
               (let ((s (gl:create-sampler)))
                 (gl:sampler-parameter s :texture-min-filter min-filter)
                 (gl:sampler-parameter s :texture-mag-filter mag-filter)
                 (gl:sampler-parameter s :texture-max-anisotropy-ext
                                       max-anisotropy)
                 (gl:sampler-parameter s :texture-min-lod min-lod)
                 (gl:sampler-parameter s :texture-max-lod max-lod)
                 ;;(gl:sampler-parameter s :texture-swizzle-r swizzle-r)
                 ;;(gl:sampler-parameter s :texture-swizzle-g swizzle-g)
                 ;;(gl:sampler-parameter s :texture-swizzle-b swizzle-b)
                 ;;(gl:sampler-parameter s :texture-swizzle-a swizzle-a)
                 (gl:sampler-parameter s :texture-wrap-s wrap-s)
                 (gl:sampler-parameter s :texture-wrap-t wrap-t)
                 (gl:sampler-parameter s :texture-wrap-r wrap-r)
                 (gl:sampler-parameter s :texture-border-color border-color)
                 (gl:sampler-parameter s :texture-compare-mode compare-mode)
                 (gl:sampler-parameter s :texture-compare-func compare-func)
                 (make-instance 'sampler :sampler s :spec spec))))
        (setf (gethash name (samplers *resource-manager*))
              (make-sampler))))))

(defmethod load-texture ((type (eql :builtin)) name &key)
  ;; todo: default textures
  (cerror "continue" "builtin textures not done yet"))

(defparameter *compress-loaded-textures* nil)
;; fixme: SRGB as a global parameter like this isn't quite right, need
;; to include it in the identifiert used for reusing previously loaded
;; textures
(defparameter *load-textures-as-srgb* nil)
(defun get-internal-format (channels bytes)
  (cond
    ((= bytes 2) ;; no compression or srgb for 16-bit
     (ecase channels (1 :r16) (2 :rg16) (3 :rgb16) (4 :rgb16)))
    ((and *compress-loaded-textures* *load-textures-as-srgb*)
     (ecase channels
       (1 :compressed-red) ;; no srgb for r,rg
       (2 :compressed-rg)
       (3 :compressed-srgb)
       (4 :compressed-srgb-alpha)))
    (*load-textures-as-srgb*
     (ecase channels
       (1 :red) (2 :rg) (3 :srgb) (4 :srgb-alpha)))
    (t (ecase channels (1 :r8) (2 :rg8) (3 :rgb8) (4 :rgb8)))))

(defparameter *internal-formats*
  ;; todo: support signed/un-normalized formats as well?
  #2a((nil nil nil)
      (nil :r8 :r16)   ;; technically grey in png files
      (nil :rg8 :rg16) ;; grey+alpha
      (nil :rgb8 :rgb16)
      (nil :rgba8 :rgba16)))

(defparameter *pngload-colortypes*
  '(:greyscale 1 :grayscale 1
    :greyscale-alpha 2 :grayscale-alpha 2
    :truecolor 3 :truecolour 3
    :truecolor-alpha 4 :truecolour-alpha 4))

(defun load-texture-pngload (name)
  (pngload:with-png-in-static-vector (png name :flip-y t)
    (when png
      (let* ((tex (gl:create-texture :texture-2d))
             (w (pngload:width png))
             (h (pngload:height png))
             (channels (/ (array-total-size (pngload:data png))
                          w h))
             (bytes (ceiling (/ (pngload:bit-depth png) 8)))
             (internal-format (aref *internal-formats* channels bytes)))
        (declare (ignorable internal-format))
        #++(progn
             (%gl:texture-storage-2d tex
                                     (floor (max (log w 2) (log h 2)))
                                     internal-format
                                     w h)
             (%gl:texture-sub-image-2d tex 0 0 0 w h
                                       (ecase channels
                                         (1 :red) (2 :rg) (3 :rgb) (4 :rgba))
                                       (ecase bytes
                                         (1 :unsigned-byte)
                                         (2 :unsigned-short))
                                       (static-vectors:static-vector-pointer
                                        (pngload:data png))))
        (progn
          (gl:bind-texture :texture-2d tex)
          (%gl:tex-image-2d :texture-2d 0
                            (cffi:foreign-enum-value
                             '%gl:enum
                             (get-internal-format channels 1))
                            w h
                            0
                            (ecase channels
                              (1 :red) (2 :rg) (3 :rgb) (4 :rgba))
                            (ecase bytes
                              (1 :unsigned-byte)
                              (2 :unsigned-short))
                            (static-vectors:static-vector-pointer
                             (pngload:data png)))
          (gl:bind-texture :texture-2d 0))
        (gl:generate-texture-mipmap tex)
        tex))))


(defun flip (image)
  (check-type image (simple-array (unsigned-byte 8) 3))
  (locally (declare (type (simple-array (unsigned-byte 8) 3) image)
                    (optimize speed))
    (opticl:with-image-bounds (wy wx c) image
      (assert (< wx 65536))
      (assert (< wy 65536))
      (assert (<= c 4))
      (locally (declare (type (unsigned-byte 16) wx wy)
                        (type (unsigned-byte 3) c))
        (loop with stride = (* wx (or c 1))
              for y below (floor wy 2)
              for y1 = (* y stride)
              for y2 = (* (- wy y 1) stride)
              do (loop for i below stride
                       do (rotatef (row-major-aref image (+ y1 i))
                                   (row-major-aref image (+ y2 i)))))))))

(defun load-texture-opticl (name)
  (let ((img (opticl:read-image-file name)))
    (when img
      (let* ((tex (gl:create-texture :texture-2d))
             (w (array-dimension img 1))
             (h (array-dimension img 0))
             (channels (array-dimension img 2))
             (bytes 1) ;; todo: 16bit images?
             (internal-format (get-internal-format channels bytes))
             (ats (array-total-size img)))
        (%gl:texture-storage-2d tex
                                (floor (max (log w 2) (log h 2)))
                                internal-format
                                w h)
        (static-vectors:with-static-vector (s ats)
          (check-type img (simple-array (unsigned-byte 8) 3))
          (locally (declare (type (simple-array (unsigned-byte 8) 3) img)
                            (optimize speed))
            (flip img)
            (loop for i below ats
                  do (setf (aref s i) (row-major-aref img i))))

          (%gl:texture-sub-image-2d tex 0 0 0 w h
                                    (ecase channels
                                      (1 :red) (2 :rg) (3 :rgb) (4 :rgba))
                                    (ecase bytes
                                      (1 :unsigned-byte)
                                      (2 :unsigned-short))
                                    (static-vectors:static-vector-pointer s)))
        (gl:generate-texture-mipmap tex)
        tex))))

(defun load-texture-file (name &key &allow-other-keys)
  ;; todo: support more than :texture-2d
  (let* ((f (probe-file (merge-pathnames name))))
    (when f
      (if (alexandria:ends-with-subseq ".png" name :test 'char-equal)
          (load-texture-pngload f)
          (load-texture-opticl f)))))

(defmethod load-texture ((type (eql :file)) name  &key target)
  (load-texture-file name :target target))

#++ ;; todo
(defmethod load-texture ((type (eql :stream)) name  &key)
  (load-texture-file name))

#++
(defparameter *texture-load-queue*
  #+sbcl (sb-concurrency:make-queue :name "texture-load-queue")
  #-sbcl nil)
#++
(defun enqueue-texture-load (target type name)
  #+sbcl (sb-concurrency:enqueue (list :texture target type name) *texture-load-queue*)
  #-sbcl (push (list :texture target type name) *texture-load-queue*))



(defun get-texture (name &key (type :file) (target :texture-2d))
  (when (typep name '(or texture null))
    (return-from get-texture name))
  ;; todo: load files on another thread, return a debug texture until
  ;; actually loaded
  (let ((s (list type name target)))
    (or (gethash s (textures *resource-manager*))
        (let ((tx (load-texture type name :target target)))
          (when tx
            (setf (gethash s (textures *resource-manager*))
                  (make-instance 'texture
                                 :texture tx
                                 :target target
                                 :source s)))))))


(defun reset-texture (tex)
  (when (texture tex)
    (gl:delete-texture (shiftf (slot-value tex 'texture) nil))))

(defun reset-sampler (sampler)
  (when (sampler sampler)
    (gl:delete-sampler (shiftf (slot-value sampler 'sampler) nil))))

(defun reset-handle (handle)
  (when (and (resident handle) (handle handle))
    (when (%gl:is-texture-handle-resident-arb (handle handle))
      (%gl:make-texture-handle-non-resident-arb
       (shiftf (slot-value handle 'handle) nil)))))
