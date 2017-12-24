(cl:defpackage 3bgl-sg2-graph-shaders
  (:use :3bgl-glsl/cl :3bgl-sg2-shaders-common))

(cl:in-package 3bgl-sg2-graph-shaders)

(input h :vec3 :location 0)

(defstruct -per-object
  (m :mat4)
  (color :vec4))

(interface -objects (:buffer t :layout (:binding 2 :std430 t))
  (object-count :int)
  (objects (-per-object :*)))

;; not sure :flat qualifier is required/valid on vs output?
(output color :vec4 :stage :vertex); :qualifiers (:flat)
(input color :vec4 :stage :fragment :qualifiers (:flat))

(defun vertex ()
  (let* ((pos (vec4 (* ui-scale h) 1))
         (draw-id gl-draw-id)
         (po (aref objects draw-id))
         (m (@ po m)))
    (setf gl-position (* ui-matrix m pos))
    #++(setf gl-position (vec4 (* (vec2 (/ 1 2800.0) 0.0005) (.xy pos)) 0 1)
          #++(+ (vec4 (* 0.001 gl-vertex-id) 0 0 1)
             (* 0.001 pos)))
    (setf color (@ po color))
    ;;(setf color (vec4 0 0 ui-scale 1))
    ;;(setf (.x color) (/ gl-vertex-id 1800.0))
    ))

(output out-color :vec4 :stage :fragment)

(defun fragment ()
  (setf out-color (* 1 color))
  (setf (.a out-color) 1))

