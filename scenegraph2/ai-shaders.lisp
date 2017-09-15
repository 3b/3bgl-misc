#++(delete-package '3bgl-ai-shaders)
(cl:defpackage 3bgl-ai-shaders
  (:use :3bgl-glsl/cl #++ :3bgl-material-lib-shaders))
(cl:in-package 3bgl-ai-shaders)

(input position :vec4 :location 0)
(input normal :vec3 :location 1)
(input uv :vec2 :location 2)
(input color :vec4 :location 3)

(output out-color :vec4 :stage :fragment)

(interface globals (:buffer t :layout (:binding 0 :std430 t))
  (foo2 :int)
  (mvp :mat4)
  (v :mat4)
  (p :mat4))

(defstruct -material
  (foo :int)
  (color :vec4)
  (mata :mat3x2)
  (matb :mat2x3))

(interface -materials (:buffer t :layout (:binding 1 :std430 t))
  (z :bool)
  (a :float)
  (count :int)
  (b :vec3)
  (c :vec4)
  (materials (-material :*))
)

(interface per-object (:buffer t :layout (:binding 2 :std430 t))
  (material-id :int)
  (m :mat4)
  (gmv :mat4)
  (gmvp :mat4))

(uniform mv :mat4)
(uniform mvp :mat4)

(interface varyings (:out (:vertex outs)
                     :in (:fragment ins))
  (uv :vec2)
  (color :vec4)
  (normal :vec3))


(defconstant +flat-shading+ 1 :int)
(defconstant +gouraud-shading+ 2 :int)
(defconstant +phong-shading+ 3 :int)
(defconstant +blinn-shading+ 4 :int)
(defconstant +toon-shading+ 5 :int)
(defconstant +oren-nayar-shading+ 6 :int)
(defconstant +minnaert-shading+ 7 :int)
(defconstant +cook-torrance-shading+ 8 :int)
(defconstant +no-shading+ 9 :int)
(defconstant +fresnel-shading+ 10 :int)



(defconstant +no-texture+ 0 :int)
(defconstant +diffuse-texture+ 1 :int)
(defconstant +specular-texture+ 2 :int)
(defconstant +ambient-texture+ 3 :int)
(defconstant +emissive-texture+ 4 :int)
(defconstant +height-texture+ 5 :int)
(defconstant +normals-texture+ 6 :int)
(defconstant +shininess-texture+ 7 :int)
(defconstant +opacity-texture+ 8 :int)
(defconstant +displacement-texture+ 9 :int)
(defconstant +lightmap-texture+ 10 :int)
(defconstant +reflection-texture+ 11 :int)
(defconstant +unknown-texture+ 12 :int)




(defun vertex ()
  (let ((pos position))
    (setf (.w pos) 1.0)
    ;(setf gl-position (* p v m pos))
    (setf gl-position (* mvp pos)))
  #++(setf (@ outs color) (vec4 normal 1.0))
  (setf (@ outs color) color)
  (setf (@ outs uv) uv)
  (setf (@ outs normal) normal))


(defun fragment ()
  (let* ((mat (aref materials (clamp material-id 0 count)))
         (a (.a (@ ins color))))
    #++(setf out-color (vec4 (* a (.xyz (@ ins color))) a))
    #++(setf out-color (vec4 (.xy (@ ins uv)) 1 1))
    (setf out-color (* (@ mat color)
                       (vec4 (abs (@ ins normal)) 1)))
    ;(setf out-color (vec4 1 0 0 1))
    ))
































#++
(defun vertex-static ()
  ;(transform)
  ;(outs color uv normal tangent bitangent)
  )
#++
(defun vertex-skinned ()
  #++(with-skinned-position ()
   ; (transform)
   ; (outs color uv normal tangent bitangent)
    ))
#++
(defun fragment-untextured ()

)
#++
(defun fragment-ambient-diffuse ())
#++
(defun fragment-ambient-diffuse-height ())
#++
(defun fragment-ambient-diffuse-opacity ())
#++
(defun fragment-ambient-diffuse-height-opacity ())






#++
(defun vertex ()
  (let ((p position))
    (incf (.x p) 1.0)
    (setf (.w p) 1.0)
   (setf gl-position (* mvp p )))
  #++(setf (@ outs color) (vec4 normal 1.0))
  (setf (@ outs color) color)
  (setf (@ outs uv) uv)
  (setf (@ outs normal) normal))

#++
(defun fragment ()
  (let ((a (.a (@ ins color))))
    #++(setf out-color (vec4 (* a (.xyz (@ ins color))) a))
    #++(setf out-color (vec4 (.xy (@ ins uv)) 1 1))
    (setf out-color (vec4 (abs (@ ins normal)) 1))
    ))
