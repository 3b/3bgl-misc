#++(delete-package '3bgl-ai-shaders)
(cl:defpackage 3bgl-ai-shaders
  (:use :3bgl-glsl/cl #++ :3bgl-material-lib-shaders
        :3bgl-sg2-shaders-common)
)

(cl:in-package 3bgl-ai-shaders)

(input position :vec4 :location 0)
(input normal :vec3 :location 1)
(input tangent :vec3 :location 2)
(input bitangent :vec3 :location 3)
(input color :vec4 :location 4)
(input uv1 :vec2 :location 5)
(input bones :vec4 :location 6)
(input weights :vec4 :location 7)

(output out-color :vec4 :stage :fragment)


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
  (tex-normals :sampler-2d)
  (tex-emissive :sampler-2d)
  (tex-present :int))

(interface -materials (:buffer t :layout (:binding 1 :std430 t))
  (z :bool)
  (a :float)
  (count :int)
  (b :vec3)
  (c :vec4)
  (materials (-material :*)))

(defstruct -per-object
  (material-id :int)
  (m :mat4))

(interface -objects (:buffer t :layout (:binding 2 :std430 t))
  (object-count :int) ;; not sure if this will be used or not
  (objects (-per-object :*)))

(interface varyings (:out (:vertex outs)
                     :in (:fragment ins))
  (uv :vec2)
  (color :vec4)
  (normal :vec3)
  (tbn :mat3)
  (world-pos :vec3))

;; not sure :flat qualifier is required/valid on vs output?
(output material-id :int :stage :vertex) ; :qualifiers (:flat)
(input material-id :int :stage :fragment :qualifiers (:flat))


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
  (let* ((pos position)
         (draw-id gl-draw-id)
         (po (aref objects draw-id))
         (m (@ po m)))
    (setf (.w pos) 1.0)
    (setf material-id (@ po material-id))
    (setf (@ outs uv) uv1)
    (setf (@ outs color) color)
    (setf (@ outs tbn) (mat3 tangent bitangent normal))
    (setf (@ outs world-pos) (.xyz (* m pos)))
    (setf gl-position (* vp m pos))))

(defstruct texture-samples
  (diffuse :vec4)
  (ambient :vec4)
  (height :vec4)
  (opacity :vec4)
  (specular :vec4)
  (normals :vec4)
  (emissive :vec4))

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
      (setf (@ samples diffuse) (sample tex-diffuse (vec4 1 1 1 1)
                                        #++(vec4 0.58 0.58 0.58 1)))
      (setf (@ samples ambient) (sample tex-ambient (vec4 0.58 0.58 0.58 1)))
      (setf (@ samples opacity) (sample tex-opacity (vec4 1 0 0 1)))
      (setf (@ samples height) (sample tex-height (vec4 0.5 0 0 1)))
      (setf (@ samples normals) (sample tex-normals (vec4 0 1 0 1)))
      (setf (@ samples specular) (sample tex-specular (vec4 0 0 0 1)))
      (setf (@ samples emissive) (sample tex-emissive (vec4 0 0 0 0)))
      (return samples))))


(defun fragment ()
  (let* ((mat (aref materials (clamp (if (= count 6)
                                         0
                                         material-id)
                                     0 (1- count))))
         (samples (sample-textures mat (@ ins uv)))
         (a (* (@ mat mat-opacity)
               (.a (@ samples diffuse))
               (.x (@ samples opacity))
               1
                                        ;(.a (@ ins color))
               ))
         (normal (normalize
                  (* (@ ins tbn)
                     (- (* 2 (.xyz (@ samples normals))) 1))))
         ;; based on
         ;; http://blog.selfshadow.com/publications/s2013-shading-course/karis/s2013_pbs_epic_notes_v2.pdf
         ;; https://learnopengl.com/PBR/Lighting
         (view-dir (normalize (- eye-pos (@ ins world-pos))))
         (light-dir (normalize (- (vec3 0 2 0) (@ ins world-pos))))
         (half-dir (normalize (+ light-dir view-dir)))
         (v-dot-h (max (dot view-dir half-dir) 0))
         (n-dot-l (max (dot normal light-dir) 0))
         (n-dot-h (max (dot normal half-dir) 0))
         (n-dot-v (max (dot normal view-dir) 0))
         (roughness (.g (@ samples specular)))
         (roughness2 (expt roughness 2))
         (metal (.b (@ samples specular)))
         (base-color (.xyz (@ samples diffuse)))
         (f0-default 0.04)
         (f0 (mix (vec3 f0-default)
                  (.rgb base-color)
                  metal))
         (diffuse (/ base-color pi))
         (a2 (* roughness2 roughness2))
         (specular-d (/ a2
                        (* pi (expt (1+ (* (expt n-dot-h 2)
                                           (- a2 1)))
                                    2))))
         (specular-g-k (/ (expt (+ 1 roughness2) 2) 8))
         (one-minus-specular-g-k (- 1 specular-g-k))
         (specular-g (* (/ n-dot-l
                           (+ specular-g-k
                              (* n-dot-l one-minus-specular-g-k)))
                        (/ n-dot-v
                           (+ specular-g-k
                              (* n-dot-v one-minus-specular-g-k)))))

         (specular-f (+ f0
                        (* (- 1 f0)
                           (expt (- 1 v-dot-h) 5)
                           )))
         (specular (/ (* specular-d specular-f specular-g)
                      (max (* 4 n-dot-v n-dot-l) 0.001)) )
         (c (* n-dot-l
               (+ (* (- 1 specular-f)
                     (vec3 (- 1 metal))
                     diffuse)
                  specular))))
    (declare (type :vec3 c))
    (when (zerop a)
      (discard))

    (setf (.xyz c)
          (+ (.xyz c)
             (.xyz (@ samples emissive))))

    (setf out-color (vec4 c 1))))
