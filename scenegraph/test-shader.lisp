(cl:defpackage #:scenegraph-test-shaders
  (:use :3bgl-glsl/cl))
(cl:in-package #:scenegraph-test-shaders)


(input position :vec4 :location 0)
(input normal :vec3 :location 1)
(input color :vec4 :location 2)

;; final output
(output out-color :vec4 :stage :fragment)

(uniform mv :mat4)
(uniform mvp :mat4)

(interface varyings (:out (:vertex outs)
                     :in (:fragment ins))
  (color :vec4))


(defun vertex ()
  (setf gl-position (* mvp position))
  #++(setf (@ outs color) (vec4 normal 1.0))
  (setf (@ outs color) color))


(defun fragment ()
  (let ((a (.a (@ ins color))))
   (setf out-color (vec4 (* a (.xyz (@ ins color))) a))))
