(cl:defpackage #:scene2test-shaders
  (:use :3bgl-glsl/cl))
(cl:in-package scene2test-shaders)


(input position :vec4 :location 0)
(input normal :vec3 :location 1)
(input uv :vec2 :location 2)
(input color :vec4 :location 3)

;; final output
(output out-color :vec4 :stage :fragment)

(uniform mv :mat4)
(uniform mvp :mat4)

(interface varyings (:out (:vertex outs)
                     :in (:fragment ins))
  (uv :vec2)
  (color :vec4)
  (normal :vec3))


(defun vertex ()
  (let ((p position))
    (incf (.x p) 1.0)
    (setf (.w p) 1.0)
   (setf gl-position (* mvp p )))
  #++(setf (@ outs color) (vec4 normal 1.0))
  (setf (@ outs color) color)
  (setf (@ outs uv) uv)
  (setf (@ outs normal) normal))


(defun fragment ()
  (let ((a (.a (@ ins color))))
    #++(setf out-color (vec4 (* a (.xyz (@ ins color))) a))
    #++(setf out-color (vec4 (.xy (@ ins uv)) 1 1))
    (setf out-color (vec4 (abs (@ ins normal)) 1))
    ))
