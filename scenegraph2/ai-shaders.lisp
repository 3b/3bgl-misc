#++(delete-package '3bgl-ai-shaders)
(cl:defpackage 3bgl-ai-shaders
  (:use :3bgl-glsl/cl #++ :3bgl-material-lib-shaders)
  ;; define material property names so we can set them from host code
  ;; even if shader doesn't use it
  (:intern
   #:mat-two-sided
   #:mat-shading-model
   #:mat-wireframe
   #:mat-blend
   #:mat-opacity
   #:mat-bump-scaling
   #:mat-shininess
   #:mat-reflectivity
   #:mat-shininess-strength
   #:mat-index-of-refraction
   #:clr-diffuse
   #:clr-ambient
   #:clr-specular
   #:clr-emissive
   #:clr-transparent
   #:clr-reflective
   #:tex-none
   #:tex-diffuse
   #:tex-specular
   #:tex-ambient
   #:tex-emissive
   #:tex-height
   #:tex-normals
   #:tex-shininess
   #:tex-opacity
   #:tex-displacement
   #:tex-lightmap
   #:tex-reflection
   #:tex-unknown
   #:tex-present
   ))

(cl:in-package 3bgl-ai-shaders)

(input position :vec4 :location 0)
(input normal :vec3 :location 1)
(input tangent :vec3 :location 2)
(input bitangent :vec3 :location 3)
(input color :vec4 :location 4)
(input uv :vec2 :location 5)
(input bones :vec4 :location 6)
(input weights :vec4 :location 7)

(output out-color :vec4 :stage :fragment)

(interface globals (:buffer t :layout (:binding 0 :std430 t))
  (foo2 :int)
  (mvp :mat4)
  (v :mat4)
  (p :mat4))

(defstruct -material
  (color :vec4)
  (mat-two-sided :int)
  (mat-shading-model :int)
  (mat-wireframe :int)
  (mat-blend :float) ;; 0=normal alpha, 1=additive?
  (mat-opacity :float)
  (mat-bump-scaling :float)
  (mat-shininess :float)
  (mat-reflectivity :float)
  (mat-shininess-strength :float)
  (mat-index-of-refraction :float)
  (clr-diffuse :vec3)
  (clr-ambient :vec3)
  (clr-specular :vec3)
  (clr-emissive :vec3)
  (clr-transparent :int)
  (clr-reflective :int)
  (tex-ambient :sampler-2d)
  (tex-diffuse :sampler-2d)
  (tex-height :sampler-2d)
  (tex-opacity :sampler-2d)
  (tex-specular :sampler-2d)
  (tex-present :int)
  )

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

(defstruct texture-samples
  (diffuse :vec4)
  (ambient :vec4)
  (height :vec4)
  (opacity :vec4)
  (specular :vec4))

(defmacro texture-mask (&rest textures)
  (let ((tx '(:none 0
              :diffuse 1
              :specular 2
              :ambient 3
              :emissive 4
              :height 5
              :normals 6
              :shininess 7
              :opacity 8
              :displacement 9
              :lightmap 10
              :reflection 11
              :unknown 12)))
   (loop for i in textures
         sum (ash 1 (getf tx i)))))

(defun sample-textures (mat uv)
  (declare (-material mat))
  (macrolet ((sample (tex &optional (default '(vec4 0 0 0 1)))
               (let ((tx '(tex-none 0
                           tex-diffuse 1
                           tex-specular 2
                           tex-ambient 3
                           tex-emissive 4
                           tex-height 5
                           tex-normals 6
                           tex-shininess 7
                           tex-opacity 8
                           tex-displacement 9
                           tex-lightmap 10
                           tex-reflection 11
                           tex-unknown 12)))
                 `(if (zerop (logand mask ,(ash 1 (getf tx tex))))
                      ,default
                      (texture (@ mat ,tex) uv)))))
    (let ((mask (@ mat tex-present))
          (samples))
      (declare (type texture-samples samples))
      (setf (@ samples diffuse) (sample tex-diffuse (vec4 0.58 0.58 0.58 1)))
      (setf (@ samples ambient) (sample tex-ambient (vec4 0.58 0.58 0.58 1)))
      (setf (@ samples opacity) (sample tex-opacity (vec4 1 0 0 1)))
      (setf (@ samples height) (sample tex-height (vec4 0.5 0 0 1)))
      (setf (@ samples specular) (sample tex-specular (vec4 0 0 0 1)))
      (return samples))))

(defun fragment ()
  (let* ((mat (aref materials (clamp material-id 0 (1- count))))
         (a (.a (@ ins color)))
         (samples (sample-textures mat (@ ins uv))))
    (when (zerop (.x (@ samples opacity)))
      (discard))
    (setf out-color (vec4 1 0.1 0.5 1))
    #++(setf out-color (vec4 (* a (.xyz (@ ins color))) a))
    #++(setf out-color (vec4 (.xy (@ ins uv)) 1 1))
    #++(setf out-color (* (vec4 1)
                          (@ mat color)
                          (vec4 (abs (@ ins normal)) 1)))
                                        ;(setf out-color (vec4 (@ mat clr-diffuse) 1))
    #++(if (zerop (logand 2 (@ mat tex-present)))
           (setf out-color
                 (vec4 (@ ins uv) 0 1))
           (setf out-color (texture (@ mat tex-diffuse) (@ ins uv)))
           )
    #++(setf out-color
             (vec4 (@ ins uv) 0 1))

    ;;(setf (.x out-color) (/ material-id (float count)))
    (setf out-color (@ samples diffuse))
    (setf (.xyz out-color) (* (.xyz out-color)
                              (- 0.95 (expt (.z gl-frag-coord) 32))))
    (setf (.a out-color) 1)
    #++(when (> material-id 10)
         (setf out-color (vec4 0.5)))
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
