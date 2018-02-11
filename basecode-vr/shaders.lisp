(cl:defpackage basecode-vr-shaders
  (:use :3bgl-glsl/cl #++ :3bgl-material-lib-shaders)
  )
(in-package basecode-vr-shaders)
(uniform matrix :mat4)

(input position :vec4 :location 0)
(input uv-coords-in :vec2 :location 1) ;; render-model, comanion-window
(input color-in :vec4 :stage :vertex :location 1) ;; controller
(input color-in :vec4 :stage :fragment)           ;; controller
(input normal-in :vec3 :location 2)               ;; render-model

(output uv-coords :vec2 :stage :vertex)  ;; render-model
(input uv-coords :vec2 :stage :fragment) ;; render-model

(uniform diffuse :sampler-2d)
(output output-color :vec4)

(defun controller-transform-vertex ()
  (setf output-color (vec4 (.xyz color-in) 1))
  (setf gl-position (* matrix position)))
(defun controller-transform-fragment ()
  (setf output-color color-in))

(defun render-model-vertex ()
  (setf uv-coords uv-coords-in
        gl-position (* matrix (vec4 (.xyz position) 1))))

(defun render-model-fragment ()
  (setf output-color
        (texture diffuse uv-coords)))


(output uv :vec2 :stage :vertex :qualifiers (:noperspective))
(input uv :vec2 :stage :fragment :qualifiers (:noperspective))

(defun mirror-window-vertex ()
  (setf uv uv-coords-in
        gl-position (vec4 (.xy position) 0 1)))
(defun mirror-window-fragment ()
  (setf output-color (texture diffuse uv)))
