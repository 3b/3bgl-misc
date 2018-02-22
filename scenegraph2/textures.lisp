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
                      (max-anisotropy 1.0)
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
                 (gl:sampler-parameter s :texture-cube-map-seamless t)
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


(defparameter *layouts*
  (alexandria:plist-hash-table
   ;; index by layer # (= +x -x +y -y +z -z), = column,row
   '(:horizontal #((2 1) (0 1) (1 0) (1 2) (1 3) (1 1))
     :vertical #((2 1) (0 1) (1 0) (1 2) (1 1) (1 3)))))

(defun flip-region-x (d x1 y1 w h stride)
  (declare (type (simple-array (unsigned-byte 8) (*)) d))
  (loop for y from y1 below (+ y1 h)
        for row = (* y stride)
        do (loop with w/2 = (/ w 2)
                 with end = (+ x1 row w)
                 with start = (+ x1 row)
                 for x below w/2
                 do (rotatef (aref d (+ x start )) (aref d (- end x 1))))))

(defun flip-region-y (d x1 y1 w h stride)
  (declare (type (simple-array (unsigned-byte 8) (*)) d))
  (loop with h/2 = (/ h 2)
        for y below h/2
        for r1 = (+ x1 (* (+ y1 y) stride))
        for r2 = (+ x1 (* (+ y1 (- h y 1)) stride))
        do (rotatef (subseq d r1 (+ r1 w))
                    (subseq d r2 (+ r2 w)))))

(defvar *cube-faces* '(:texture-cube-map-positive-x
                       :texture-cube-map-negative-x
                       :texture-cube-map-positive-y
                       :texture-cube-map-negative-y
                       :texture-cube-map-positive-z
                       :texture-cube-map-negative-z))

(defun map-cube-faces (data fun width height pixel-bytes
                       &key (layout :guess) data-is-static)
  (when (eq layout :guess)
    (unless (or (= (* width 3/4) height)
                (= (* width 4/3) height))
      (cerror "continue"
              "failed to guess orientation of cube map, size = ~s x ~s?"
              width height))
    (if (>= height width)
        (setf layout :vertical)
        (setf layout :horizontal)))
  (flet ((body (p)
           (loop
             with cw = (floor
                        (if (eq layout :vertical) (/ width 3) (/ width 4)))
             with ch = (floor
                        (if (eq layout :vertical) (/ height 4) (/ height 3)))
             ;; hack to make it load something reasonable if it gets default
             ;; (square) texture
             with wh = (min cw ch)
             with stride-bytes = (* pixel-bytes width)
             for layer in *cube-faces*
             for (i j) across (gethash layout *layouts*)
             for x = (* i cw)
             for y = (* j ch)
             when (eq layer :texture-cube-map-negative-z)
               do (flip-region-y p x y wh wh stride-bytes)
                  (flip-region-x p x y wh wh stride-bytes)
             do (funcall fun layer
                         (cffi:inc-pointer
                          (static-vectors:static-vector-pointer p)
                          (+ (* pixel-bytes x) (* stride-bytes y)))
                         wh wh width))))
    (if data-is-static
        (body data)
        ;; copy to foreign memory once so we don't copy whole thing for
        ;; each face in tex-image-2d
        (static-vectors:with-static-vector (p (length data)
                                              :element-type
                                              '(unsigned-byte 8)
                                              :initial-contents data)
          (body p)))))

(defun load-texture-pngload (name &key cube)
  (pngload:with-png-in-static-vector (png name :flip-y t)
    (when png
      (let* ((tex (gl:create-texture (if cube :texture-cube-map :texture-2d)))
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
        (if cube
            (progn
              (gl:bind-texture :texture-cube-map tex)
              (map-cube-faces
               (pngload:data png)
               (lambda (face pointer w h pixel-stride)
                 (gl:pixel-store :unpack-row-length pixel-stride)
                 (%gl:tex-image-2d face 0
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
                                   pointer))
               w h (* channels bytes)
               :data-is-static t)
              (gl:pixel-store :unpack-row-length 0)
              (gl:bind-texture :texture-cube-map 0))
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
              (gl:bind-texture :texture-2d 0)))
        (gl:enable :texture-cube-map-seamless)
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

(defun equirectangular-to-cube (src-tex dest-tex format w)
  (let ((program (get-program
                  :compute '3bgl-sg2-shaders-common::equirectangular-to-cube)))
    (gl:bind-texture :texture-2d src-tex)
    (%gl:bind-image-texture 1 dest-tex 0 t 0 :read-write format)
    (setf (3bgl-shaders::uniform program "e2c-in") 0)
    (setf (3bgl-shaders::uniform program "e2c-out") 1)
    (3bgl-shaders::use-program program)
    (%gl:dispatch-compute (floor w 8) (floor w 8) 6))
  (%gl:bind-image-texture 1 0 0 t 0 :read-write format))


(defun load-texture-hdr (name &key cube)
  (let ((hdr (3bgl-radiance-hdr::read-hdr-file name)))
    (when hdr
      (let* ((tex (gl:create-texture (if cube :texture-cube-map :texture-2d)))
             (w (3bgl-radiance-hdr::width hdr))
             (h (3bgl-radiance-hdr::height hdr)))
        ;; fixme: figure out how to do this with immutable textures...
        (cond
          ;; assume 3/4 or 4/3 aspect ratio is cube cross
          ((and cube (or (= w (* 4/3 h))
                         (= h (* 4/3 w))))
           (gl:bind-texture :texture-cube-map tex)
           (3bgl-radiance-hdr::map-cube-faces
            hdr
            (lambda (face pointer w h pixel-stride)
              (gl:pixel-store :unpack-row-length pixel-stride)
              (gl:tex-image-2d face 0
                               :rgb9-e5
                               w h
                               0
                               :rgb
                               :unsigned-int-5-9-9-9-rev
                               pointer)))
           (gl:pixel-store :unpack-row-length 0)
           (gl:bind-texture :texture-cube-map 0))
          (cube
           ;; for any other aspect ratio load it as equirectangular and
           ;; convert to cube manually
           (let ((temp-tex (gl:create-texture :texture-2d))
                 ;; fixme: decide how big cube map should be?
                 ;; (for now, just using width / 4)
                 (cw (/ w 4)))
             ;; make cube faces a multiple of 8 so we can use 8x8
             ;; compute shader more easily
             (setf cw (* 8 (ceiling cw 8)))
             (unwind-protect
                  (progn
                    (gl:bind-texture :texture-2d temp-tex)
                    (gl:tex-image-2d :texture-2d 0
                                     :rgb9-e5
                                     w h
                                     0
                                     :rgb
                                     :unsigned-int-5-9-9-9-rev
                                     (3bgl-radiance-hdr::data hdr))
                    (gl:generate-texture-mipmap temp-tex)
                    (%gl:texture-storage-2d tex
                                            5
                                            :rgba16f
                                            cw cw)
                    (equirectangular-to-cube temp-tex tex :rgba16f cw))
               (gl:delete-texture temp-tex))
             (gl:bind-texture :texture-2d 0)))
          (t
           (gl:bind-texture :texture-2d tex)
           (gl:tex-image-2d :texture-2d 0
                            :rgb9-e5
                            w h
                            0
                            :rgb
                            :unsigned-int-5-9-9-9-rev
                            (3bgl-radiance-hdr::data hdr))
           (gl:bind-texture :texture-2d 0)))
        (gl:enable :texture-cube-map-seamless)
        (gl:generate-texture-mipmap tex)
        tex))))

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
      (cond
        ((alexandria:ends-with-subseq ".png" (namestring f) :test 'char-equal)
         (load-texture-pngload f))
        ((alexandria:ends-with-subseq ".hdr" (namestring f) :test 'char-equal)
         (load-texture-hdr f))
        (t
         (load-texture-opticl f))))))

(defun load-cube-texture-file (name &key &allow-other-keys)
  (let* ((f (probe-file (merge-pathnames name)))
         (ns (namestring f)))
    (when f
      (cond
        ((alexandria:ends-with-subseq ".png" ns :test 'char-equal)
         (load-texture-pngload f :cube t))
        ((alexandria:ends-with-subseq ".hdr" ns :test 'char-equal)
         (load-texture-hdr f :cube t))
        (t
         (error "cubemap loading not implemented for opticl yet~%(loading ~s)"
                name))))))

(defmethod load-texture ((type (eql :file)) name  &key target)
  (if (eql target :texture-cube-map)
      (load-cube-texture-file name :target target)
      (load-texture-file name :target target)))



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

(defmethod normalize-texture-name-for-loader (type name)
  name)
(defmethod normalize-texture-name-for-loader ((type (eql :file)) name)
  (probe-file name))

(defun get-texture (name &key (type :file) (target :texture-2d))
  (when (typep name '(or texture null))
    (return-from get-texture name))
  (setf name (normalize-texture-name-for-loader type name))
  ;; todo: load files on another thread, return a debug texture until
  ;; actually loaded
  (let ((s (list type name target)))
    (or (gethash s (textures *resource-manager*))
        (let ((tx (load-texture type name :target target)))
          (when tx
            (setf (gethash s (textures *resource-manager*))
                  (make-instance 'texture
                                 :texture (if (typep tx 'texture)
                                              (texture tx)
                                              tx)
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
