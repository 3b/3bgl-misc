;; probably should be moved to shader lib once it exists
(cl:defpackage 3bgl-sg2-shaders-common
  (:use :3bgl-glsl/cl)
  ;(:intern #:m #:v #:p #:vp #:mvp)
  (:export #:m #:v #:p #:vp #:mvp #:globals #:ui-matrix #:ui-scale
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
           #:tex-present))

(in-package #:3bgl-sg2-shaders-common)

(interface globals (:buffer t :layout (:binding 0 :std430 t))
  (mvp :mat4)
  (vp :mat4)
  (v :mat4)
  (p :mat4)
  (eye-pos :vec3)
  (ui-matrix :mat4)
  (ui-scale :float))

(defun foo () (mat4 1))
(defun common-vertex ()
  ;; not really intended for use, but compiling a sshader is currently
  ;; easiest way to get layout of globals
  (setf gl-position (* mvp (foo) position)))
