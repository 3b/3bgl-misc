;; probably should be moved to shader lib once it exists
(cl:defpackage 3bgl-sg2-shaders-common
  (:use :3bgl-glsl/cl)
  ;(:intern #:m #:v #:p #:vp #:mvp)
  (:export #:m #:v #:p #:vp #:mvp #:globals #:ui-matrix #:ui-scale))

(in-package #:3bgl-sg2-shaders-common)


(interface globals (:buffer t :layout (:binding 0 :std430 t))
  (mvp :mat4)
  (vp :mat4)
  (v :mat4)
  (p :mat4)
  (ui-matrix :mat4)
  (ui-scale :float))

(defun foo () (mat4 1))
(defun common-vertex ()
  ;; not really intended for use, but compiling a sshader is currently
  ;; easiest way to get layout of globals
  (setf gl-position (* mvp (foo) position)))

