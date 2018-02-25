;; probably should be moved to shader lib once it exists
(cl:defpackage 3bgl-sg2-shaders-common
  (:use :3bgl-glsl/cl)
  (:export
   #:m #:v #:p #:vp #:mvp #:globals #:ui-matrix #:ui-scale
   #:eye-pos
   ;; ai-style material properties. define material property
   ;; names so we can set them from host code even if shader
   ;; doesn't use it
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
   #:env-map-mode
   #:specular-env-map
   #:specular-cube-map
   #:diffuse-env-map
   #:diffuse-cube-map
   #:+env-map-mode-equirectangular+
   #:+env-map-mode-cube+
   #:+env-map-mode-none+
   #:now
   #:+env-map-scale+
   #:prefiltered-specular-lut
   #:prefiltered-specular-max-lod))

(in-package #:3bgl-sg2-shaders-common)

(defconstant +env-map-mode-none+ 0 :int)
(defconstant +env-map-mode-cube+ 1 :int)
(defconstant +env-map-mode-equirectangular+ 2 :int)
;; constants for equirectangular map lookups
(defconstant +env-map-scale+ (vec2 (/ 0.5 pi) (/ pi)) :vec2)
(interface globals (:buffer t :layout (:binding 0 :std430 t))
  (mvp :mat4)
  (vp :mat4)
  (v :mat4)
  (p :mat4)
  (eye-pos :vec3)
  (ui-matrix :mat4)
  (ui-scale :float)
  (now :uint)
  ;; 0 = none, 1 = use *-cube-map, 2 = use *-env-map as equirectangular
  (env-map-mode :int)
  ;; todo: separate skybox map?
  (specular-env-map :sampler-2d)
  (specular-cube-map :sampler-cube)
  (prefiltered-specular-max-lod :uint) ;; # of mipmaps of specular-cube-map
  (prefiltered-specular-lut :sampler-2d)
  (diffuse-env-map :sampler-2d)
  (diffuse-cube-map :sampler-cube))

(defun foo () (mat4 1))
(defun common-vertex ()
  ;; not really intended for use, but compiling a sshader is currently
  ;; easiest way to get layout of globals
  (setf gl-position (* mvp (foo) position)))



(defun sample-equirectangular (sampler dir)
  (let ((tc (vec2 (atan (.z dir) (.x dir))
                  (asin (- (.y dir))))))
    (setf tc (+ 0.5 (* tc +env-map-scale+)))
    ;; try to avoid problems at poles
    (when (>= (abs (.y dir)) 0.9999)
      (setf tc (vec2 0.5
                     (* (/ (.y (texture-size sampler 0)))
                        (sign (.y dir))))))
    (return (texture-lod sampler tc 0))))
;;
;;(input e2c-uv :vec3 :location 1)
;;(output e2c-out :vec4 :stage :fragment)
;;(uniform e2c-sampler :sampler-2d)
;;(interface varyings (:out (:vertex e2c-outs)
;;                     :in (:fragment e2c-ins))
;;  (uv :vec3))
;;
;;(defun equirectangular-to-cube/v ()
;;  (setf (@ e2c-outs uv) e2c-uv)
;;  (setf gl-position position))
;;
;;(defun equirectangular-to-cube/f ()
;;  (setf e2c-out (sample-equirectangular e2c-sampler (@ e2c-ins uv))))
;;

(uniform e2c-in :sampler-2d :location 0)
(uniform e2c-out :image-cube
         :location 1
         :layout (:binding 1)
         :qualifiers (:restrict :writeonly))

(defun e2c-cube-map-dir (pos)
  (let ((uv (normalize
             (vec3 (- (/ (vec2 (.xy pos))
                         (.xy (* gl-num-work-groups gl-work-group-size)))
                      0.5)
                   -0.5))))
    (cond
      ((= 0 (.z pos)) (setf uv (* (mat3 0 0 1 0 1 0 -1 0 0) uv)))
      ((= 1 (.z pos)) (setf uv (* (mat3 0 0 -1 0 1 0 1 0 0) uv)))
      ((= 3 (.z pos)) (setf uv (* (mat3 1 0 0 0 0 1 0 -1 0) uv)))
      ((= 2 (.z pos)) (setf uv (* (mat3 1 0 0 0 0 -1 0 1 0) uv)))
      ((= 5 (.z pos)) (setf uv (* (mat3 -1 0 0 0 1 0 0 0 -1) uv))))
    (return uv)))

(defun equirectangular-to-cube ()
  (declare (layout (:in nil :local-size-x 8 :local-size-y 8 :local-size-z 1)))
  (let* ((pos gl-global-invocation-id)
         (uv (e2c-cube-map-dir pos)))
    (image-store e2c-out (ivec3 pos)
                 (sample-equirectangular e2c-in uv))))


(defun hammersly2d (i n)
  (declare (type :uint i))
  (return (vec2 (/ (float i) n)
                (* (float (bitfield-reverse i))
                   2.3283064365386963e-10))))

(defun distribution-ggx (n-dot-h r)
  (let* ((a (expt r 2))
         (a2 (expt a 2))
         (d2 (expt n-dot-h 2)))
    (return (/ a2
               (* pi (expt (1+ (* d2 (- a2 1))) 2))))))

(defun importance-sample-ggx (epsilon tangent-space roughness)
  (let* ((a (expt roughness 2))
         ;; from https://agraphicsguy.wordpress.com/2015/11/01/sampling-microfacet-brdf/
         (a2 (expt a 2))
         ;; fewer ops, but probably slower/less accurate due to trig
         ;; functions, will try both... (paper says atan version
         ;; should use A but A2 looks closer to other version?)
         #++(theta (atan (sqrt (* a2 (/ (.y epsilon)
                                        (- 1 (.y epsilon)))))))
         #++(cos-theta (cos theta))
         #++(sin-theta (sin theta))
         (cos2 (/ (- 1 (.y epsilon))
                  (+ 1 (* (.y epsilon) (- a2 1)))))
         (cos-theta (sqrt cos2))
         (sin-theta (sqrt (- 1 cos2)))
         (phi (* 2 pi (.x epsilon)))
         (v (vec3 (* sin-theta (cos phi))
                  (* sin-theta (sin phi))
                  cos-theta)))
    (return (normalize (* tangent-space v)))))


;;; calculate specular env map and LUT for split-sum approx from
;;; https://cdn2.unrealengine.com/Resources/files/2013SiggraphPresentationsNotes-26915738.pdf
(uniform pfs-in :sampler-cube :location 0)
(uniform pfs-out :image-cube
         :location 1
         :layout (:binding 1)
         :qualifiers (:restrict :writeonly))
(uniform pfs-lut-out :image-2d
         :location 1
         :layout (:binding 1)
         :qualifiers (:restrict :writeonly))
(uniform pfs-sample-count :uint :stage :compute :default 1024 :location 2)
(uniform pfs-roughness :float :stage :compute :location 3)

(defun pre-filter-specular ()
  (declare (layout (:in nil :local-size-x 8 :local-size-y 8 :local-size-z 1)))
  (let* ((pos gl-global-invocation-id)
         (n (* (vec3 1 -1 1)
               (e2c-cube-map-dir pos)))
         (up (if (> (abs (.z n)) 0.999)
                 (vec3 1 0 0)
                 (vec3 0 0 1)))
         (tan (normalize (cross up n)))
         (tangent-space (mat3 tan
                              (cross n tan)
                              n))
         (w 0.0)
         (c (vec3 0)))
    (dotimes (i pfs-sample-count)
      (let* ((e (hammersly2d i pfs-sample-count))
             (h (importance-sample-ggx e tangent-space  pfs-roughness))
             (l (normalize (- (* 2 (dot n h) h) n)))
             (n-dot-l (max 0 (dot n l)))
             (lod 0.0))
        (when (> n-dot-l 0)
          (unless (= 0 pfs-roughness)
            (let* ((n-dot-h (dot n h))
                   (h-dot-v (dot h n))
                   (d (float (distribution-ggx n-dot-h pfs-roughness)))
                   (inv-pdf (/ (* 4 h-dot-v)
                               (+ 0.0001 (* d n-dot-h))))
                   (res 512 #++(.x (texture-size pfs-in 0)))
                   (tex (/ (* 4 pi)
                           (* 6 res res))))
              (setf lod (* 0.5 (log2 (/ inv-pdf tex))))))
          (incf c (* n-dot-l
                     (.rgb (texture-lod pfs-in l lod))))
          (incf w n-dot-l))))
    (image-store pfs-out (ivec3 pos)
                 (vec4 (/ c w) 1))))

(defun g-smith (r n-dot-v n-dot-l)
  (let* ((k (* r r 0.5)
            #++(/ (expt (1+ r) 2) 8))
         (one-minus-k (- 1 k)))
    (return  (* (/ n-dot-l
                   (+ k
                      (* n-dot-l one-minus-k)))
                (/ n-dot-v
                   (+ k
                      (* n-dot-v one-minus-k)))))))

(defun calculate-pfs-lut ()
  (declare (layout (:in nil :local-size-x 8 :local-size-y 8 :local-size-z 1)))
  (let* ((pos gl-global-invocation-id)
         (npos (/ (vec3 pos)
                  (* gl-num-work-groups gl-work-group-size)))
         ;; x = cos theta v, y = roughness
         (n-dot-v (.x npos))
         (roughness (.y npos))
         (v (vec3 (sqrt (- 1 (expt n-dot-v 2)))
                  0
                  n-dot-v))
         (n (vec3 0 0 1))
         (up (if (> (abs (.z n)) 0.999)
                 (vec3 1 0 0)
                 (vec3 0 0 1)))
         (tan (normalize (cross up n)))
         (tangent-space (mat3 tan
                              (cross n tan)
                              n))
         (sum (vec2 0.0)))
    (dotimes (i pfs-sample-count)
      (let* ((e (hammersly2d i pfs-sample-count))
             (h (importance-sample-ggx e tangent-space roughness))
             (l (- (* 2 (dot v h) h) v))
             (n-dot-l (clamp (.z l) 0 1)))
        (when (> n-dot-l 0)
          (let* ((n-dot-h (clamp (.z h) 0 1))
                 (v-dot-h (clamp (dot v h) 0 1))
                 (g (g-smith roughness n-dot-v n-dot-l))
                 (g-vis (/ (* g v-dot-h)
                           (* n-dot-h n-dot-v)))
                 (fc (expt (- 1 v-dot-h) 5)))
            (incf sum (* g-vis (vec2 (- 1 fc) fc)))))))
    (image-store pfs-lut-out (ivec2 pos)
                 (vec4 (/ sum pfs-sample-count) 0 1))))
