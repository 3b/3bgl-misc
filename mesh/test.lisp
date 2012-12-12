(in-package #:basecode)

(defclass mesh-test (basecode-glop perspective-projection basecode-clear
                                   fps-graph  basecode-draw-ground-plane
                                   freelook-camera basecode-exit-on-esc
                                   )
  ((mesh :initform nil :accessor mesh)
   (program :initform (list nil nil) :accessor program)
   (tex :initform nil :accessor tex))
  (:default-initargs :look-at-eye '(3 2 15)))

(defparameter *foo* nil)
(Defparameter *lightpos* (list 4.0 6.0 4.0 1.0))

(defmethod basecode-draw ((w mesh-test))
  (declare (optimize debug))
  (sleep 0.03)
  #++(when (tex w)
    (gl:enable :texture-2d)
    (gl:disable :depth-test)
    (basecode::with-unit-ortho-projection (w :scale 2 :origin :lower-left)
      (loop for (tex) on (tex w) ;by #'cddr
         for i from 0
         for x = (floor i 2)
         for y = (mod i 2)
         do
         (gl:bind-texture :texture-2d tex)
         (gl:with-pushed-matrix* (:modelview)
           (gl:color 1.0 1.0 1.0 1.0)
           (gl:load-identity)
           (gl:translate x y 0)
           (gl:scale 0.95 0.85 1.0)
           (gl:with-primitives :quads
             (gl:tex-coord 0.0 0.0)
             (gl:vertex 0.0 0.0)

             (gl:tex-coord 0.0 1.0)
             (gl:vertex 0.0 1.0)

             (gl:tex-coord 1.0 1.0)
             (gl:vertex 1.0 1.0)

             (gl:tex-coord 1.0 0.0)
             (gl:vertex 1.0 0.0))))))
  (gl:enable :cull-face :depth-test)
  (gl:disable :texture-2d)
  (gl:with-primitives :points
    (gl:color 1.0 1.0 1.0 1.0)
    (apply #'gl:vertex *lightpos*))
  (gl:disable :cull-face)
  (destructuring-bind (&optional p1 p2) (program w)
    (when p1
      (gl:use-program p1)
      (3bgl-shaders::uniform-matrix
       p1 "mvp" (sb-cga:matrix*
                 (projection-matrix w)
                 (freelook-camera-modelview w)
                 (sb-cga:translate* 0.0 0.0 0.0)))
      (3bgl-shaders::uniform-matrix
       p1 "normalMatrix"
       (sb-cga:transpose-matrix (freelook-camera-orientation w)))
      (3bgl-shaders::uniform-matrix
       p1 "m" (sb-cga:identity-matrix))
      (3bgl-shaders::uniform-matrix
       p1 "v"
       (freelook-camera-modelview w))
      (apply #'3bgl-shaders::uniformf p1 "lightPos" *lightpos*)
      (3bgl-shaders::uniformi p1 "diffuseTex" 0)
      (3bgl-shaders::uniformi p1 "specularTex" 1)
      (3bgl-shaders::uniformi p1 "normalTex" 2)
      (gl:active-texture :texture0)
      (gl:bind-texture :texture-2d (or (first (tex w)) 0))
      (gl:active-texture :texture1)
      (gl:bind-texture :texture-2d (or (second (tex w)) 0))
      (gl:active-texture :texture2)
      (gl:bind-texture :texture-2d (or (third (tex w)) 0))
      (3bgl-mesh::draw (mesh w))
      (loop for i in '(:texture0 :texture1 :texture2)
            do (gl:active-texture i)
               (gl:bind-texture :texture-2d 0))
      (gl:active-texture :texture0))
    (when p2
      (gl:use-program p2)
      (3bgl-shaders::uniform-matrix
       p2 "mvp" (sb-cga:matrix*
                (projection-matrix w)
                (freelook-camera-modelview w)
                (sb-cga:translate* 0.0 0.0 0.0)))
      (3bgl-shaders::uniform-matrix
       p2 "normalMatrix"
       (sb-cga:transpose-matrix (freelook-camera-orientation w)))
      (3bgl-shaders::uniformf p2 "tsdLength" 1.03)
      (gl:line-width 2.8)
      #++(ignore-errors (3bgl-mesh::draw (mesh w) :mode :points))
      ))

  (setf *foo* (mesh w))

  (gl:use-program 0))


(defun png (fn tn)
  (let ((image (time (opticl:read-image-file fn))))
    (gl:bind-texture :texture-2d tn)
    (3bgl-opticl:tex-image-2d :texture-2d 0 :rgba image)
    (gl:generate-mipmap :texture-2d)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)))

(defun dds (fn tn)
  (gl:bind-texture :texture-2d tn)
  (3bgl-dds:tex-image-2d fn)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear))

(defmethod key-down ((w mesh-test) k)
  (with-simple-restart
      (continue "continue")
    (case k
      ((:r)
       (setf (first (program w))
             (3bgl-shaders::reload-program (first (program w))
                                           '3bgl-mesh-shaders::vertex
                                           '3bgl-mesh-shaders::fragment
                                           )))

      ((:n)
       (setf (second (program w))
             (3bgl-shaders::reload-program (second (program w))
                                           '3bgl-mesh-shaders::vertex
                                           '3bgl-mesh-shaders::tsd-fragment
                                           :geometry
                                           '3bgl-mesh-shaders::tsd-geometry
                                           )))
      ((:t)
       (when (tex w)
         (gl:delete-textures (tex w))
         (setf (tex w) nil))
       (let ((tex (gl:gen-textures 3)))
         (dds "/tmp/diffuse.dds" (first tex))
         (dds "/tmp/specular.dds" (second tex))
         (png "/tmp/normal.png" (third tex))
         (setf (tex w) tex)))
      ((:+ :kp-add)
       (when (mesh w)
         (3bgl-mesh::free-mesh (mesh w)))
       (setf (mesh w)
             (3bgl-mesh::cube-mesh)))
      ((:- :kp-subtract)
       (when (mesh w)
         (3bgl-mesh::free-mesh (mesh w)))
       (setf (mesh w)
             (3bgl-mesh::cylinder-mesh :divisions 32 :segments 32 :length 10 :radius 3)))
      ((:equal)
       (when (tex w)
         (gl:bind-texture :texture-2d (car (tex w)))
         (loop for i in '(:depth-stencil-texture-mode
                         :texture-mag-filter :texture-min-filter
                         :texture-swizzle-r :texture-swizzle-g
                         :texture-swizzle-b :texture-swizzle-a
                         :texture-wrap-s :texture-wrap-t :texture-wrap-r
                         :texture-compare-mode :texture-compare-func
                         :texture-immutable-format
                         :texture-swizzle-rgba
                         :texture-base-level :texture-max-level
                         :texture-view-min-level :texture-view-num-levels
                         :texture-view-min-layer :texture-view-num-layers
                         :texture-immutable-levels
                         :texture-min-lod :texture-max-lod
                         :texture-border-color)
              do (format t "~s = ~s~%" i (gl::get-tex-parameter :texture-2d i)))
         (loop for i in '(:texture-internal-format
                          :texture-red-type :texture-green-type
                          :texture-blue-type
                          :texture-alpha-type :texture-depth-type
                          :texture-compressed
                          :texture-width :texture-height :texture-depth
                          :texture-red-size :texture-green-size :texture-blue-size
                          :texture-alpha-size :texture-depth-size
                          :texture-buffer-offset :texture-buffer-size)
               do (format t "~s = ~s~%" i (gl::get-tex-level-parameter :texture-2d 0 i)))
         (when (gl::get-tex-level-parameter :texture-2d 0
                                            :texture-compressed)
           (format t "~s = ~s~%" :texture-compressed-image-size
                   (gl::get-tex-level-parameter :texture-2d 0 :texture-compressed-image-size)))))
      (:l2
       (reset-freelook-camera w)
       (setf (look-at-eye w) (list 4 3 3)
             (look-at-up w) (list 0 1 0))))))

;; (basecode-run (make-instance 'mesh-test))




