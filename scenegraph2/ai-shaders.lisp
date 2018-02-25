#++(delete-package '3bgl-ai-shaders)
(cl:defpackage 3bgl-ai-shaders
  (:use :3bgl-glsl/cl #++ :3bgl-material-lib-shaders
        :3bgl-sg2-shaders-common))

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
(defconstant +skybox-shading+ -1 :int)



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

(defun vertex-depth-only ()
  (let* ((pos position)
         (draw-id gl-draw-id)
         (po (aref objects draw-id))
         (m (@ po m)))
    (setf (.w pos) 1.0)
    (setf gl-position (* vp m pos))))

(defun fragment-depth-only ()
  (setf out-color (vec4 1 0 0 1)))

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

(defmacro with-texture-samples ((mat uv &optional (lod 0)) &body body)
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
              tex-unknown 12))
        (defaults '(tex-diffuse (vec4 1 1 1 1)
                    tex-ambient (vec4 0.58 0.58 0.58 1)
                    tex-opacity (vec4 1 1 1 1)
                    tex-height (vec4 0.5 0 0 1)
                    tex-normals (vec4 0.5 0.5 1 1)
                    tex-specular (vec4 0 0 0 1)
                    tex-emissive (vec4 0 0 0 0))))
    (alexandria:once-only (mat uv)
      `(let ((tex-present (@ ,mat tex-present)))
         (macrolet ((sample (tex &optional (tlod ,lod))
                      (unless (getf ',tx tex)
                        (error "unknown texture ~s?" tex))
                      #++`(texture-lod (@ ,',mat ,tex)
                                       ,',uv
                                       (max ,tlod
                                            (.y (texture-query-lod  (@ ,',mat ,tex)
                                                                    ,',uv))))
                      #++`(texture (@ ,',mat ,tex)
                                   ,',uv
                                   ,tlod)
                      `(if (zerop (logand tex-present ,(ash 1 (getf ',tx tex)))
                                  )
                           ,(or (getf ',defaults tex) '(vec4 0 0 0 1))
                           (texture (@ ,',mat ,tex) ,',uv ,tlod))))
           ,@body)))))

(defconstant +env-map-rot+ 324000.0 :float)

(defun sample-ibl-diffuse (dir)
  (return (texture diffuse-cube-map dir))
  #++
  (let* ((c (vec4 1 0 1 1))
         (a (/ (float now) +env-map-rot+))
         (ss (sin a))
         (cc (cos a))
         (r (mat3 cc 0 ss
                  0 -1 0
                  (- ss) 0 cc))
         (dir (normalize (* r  dir))))
    (return (texture diffuse-cube-map dir))))

(defun sample-ibl-specular (roughness f r n-dot-v)
  (let* ( (color (.rgb (texture-lod specular-cube-map r
                                    (* (expt roughness 2)
                                       prefiltered-specular-max-lod))))
          (brdf (.xy (texture prefiltered-specular-lut (vec2 n-dot-v roughness)))))
    (return (* color (+ (* f (.x brdf)) (.y brdf))))))

(defun normal-shading (mat)
  (declare (type -material mat))
  (with-texture-samples (mat (@ ins uv) 1)
    (let* ((tn (@ mat tex-normals))
           (l (.y (texture-query-lod (@ mat tex-diffuse) (@ ins uv))))
           (tex-diffuse (sample tex-diffuse))
           #++(tex-opacity (sample tex-opacity))
           (a (* (@ mat mat-opacity)
                 (.a tex-diffuse)
                                        ;   (.x tex-opacity)
                 1
                 #++(.a (@ ins color))))
           ;; based on
           ;; http://blog.selfshadow.com/publications/s2013-shading-course/karis/s2013_pbs_epic_notes_v2.pdf
           ;; https://learnopengl.com/PBR/Lighting
           (tex-normal (sample tex-normals))
           (view-dir (normalize (- eye-pos (@ ins world-pos))))
           (light-dir (normalize (- (vec3 0 2 0) (@ ins world-pos))))
           (half-dir (normalize (+ light-dir view-dir)))
           (v-dot-h (max (dot view-dir half-dir) 0))

           (normal (normalize
                    (* (@ ins tbn)
                       (- (* 2 (.xyz tex-normal)) 1))))
           (n-dot-l (max (dot normal light-dir) 0))
           (n-dot-h (max (dot normal half-dir) 0))
           (n-dot-v (max (dot normal view-dir) 0))
           (tex-specular (sample tex-specular))
           (roughness (.g tex-specular))
           (roughness2 (expt roughness 2))
           (metal (.b tex-specular) )
           (base-color (.xyz tex-diffuse))
           (f0-default 0.04)
           (f0 (mix (vec3 f0-default)
                    (.rgb base-color)
                    metal))
           (ks (+ f0
                  (* (- 1 f0)
                     (expt (- 1 n-dot-v) 5))))
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
                             (expt (- 1 v-dot-h) 5))))
           (ispec (sample-ibl-specular roughness
                                       specular-f (reflect view-dir normal)
                                       n-dot-v))
           (specular (/ (* specular-d specular-f specular-g)
                        (max (* 4 n-dot-v n-dot-l) 0.001)))
           (irradiance (sample-ibl-diffuse normal))
           (ambient (* (- 1 ks) (.xyz irradiance) base-color))

           (c                           ;normal
                                        ;(vec3 (@ ins uv) 0)
             (+ ambient
                0
                ispec
                #++(* n-dot-l
                      (+ (* (- 1 specular-f)
                            (vec3 (- 1 metal))
                            diffuse)
                         specular)))))
      (declare (type :vec3 c))
      #++(when (zerop a)
           (discard))

      #++(setf (.xyz c)
               #++(.xyz tex-diffuse)
               (+ (.xyz c)
                  (.xyz (sample tex-emissive))))

      (return (vec4 c 1)))))

(defun skybox-shader (mat)
  (declare (type -material mat)
           (ignore mat))
  (let* ((c (vec4 1 0 1 1))
         (a (/ (float now) +env-map-rot+))
         (ss (sin a))
         (cc (cos a))
         (r (mat3 cc 0 ss
                  0 -1 0
                  (- ss) 0 cc))
         (dir (normalize (- eye-pos (@ ins world-pos))
                         #++(* r #++ (@ ins world-pos)
                                 (- (@ ins world-pos) eye-pos)))))
    (return (texture-lod
             specular-cube-map
             dir
             0 #++(expt (/ (float (mod now 4000)) 1000)2))))
  #++
  (let* ((c (vec4 1 0 1 1))
         (a (/ (float now) +env-map-rot+))
         (ss (sin a))
         (cc (cos a))
         (r (mat3 cc 0 ss
                  0 -1 0
                  (- ss) 0 cc))
         (dir (normalize (* r #++(@ ins world-pos)
                              (- (@ ins world-pos) eye-pos)))))
    (cond
      ((= env-map-mode +env-map-mode-none+)
       (discard))
      ((= env-map-mode +env-map-mode-cube+)
       (setf c (texture specular-cube-map dir)))
      ((= env-map-mode +env-map-mode-equirectangular+)
       (let ((tc (vec2 (atan (.z dir) (.x dir))
                       (asin (.y dir)))))
         (setf tc (+ 0.5 (* tc +env-map-scale+)))
         ;; try to avoid problems at poles
         (when (>= (abs (.y dir)) 0.9999)
           (setf tc (vec2 0.5
                          (* (- 1 (/ (.y (texture-size specular-env-map 0))))
                             (sign (.y dir))))))
         ;; just using mip 0 for skybox for now, since mipmapping
         ;; breaks when angle wraps
         (setf c (texture-lod specular-env-map tc 0))))
      ((= env-map-mode 3) ;; temp debug
       (setf c (vec4 dir 1))))
    (return c)))

(defun fragment ()
  (declare (layout (:in nil :early-fragment-tests :early-fragment-tests)))
  (let ((mat (aref materials (clamp material-id 0 (1- count)))))
    (if (= (@ mat mat-shading-model) -1)
        (setf out-color (skybox-shader mat))
        (setf out-color (normal-shading mat)))))
