(defpackage #:screencast-shaders
  (:use #:cl #:3bgl-glsl)
  (:shadowing-import-from #:3bgl-glsl #:defun #:defconstant)
  (:export #:vertex #:fragment)
  (:import-from #:3bgl-shader-example-shaders
                #:time #:mv #:mvp #:normal-matrix))
(in-package #:screencast-shaders)

(input position :vec4 :location 0)
(output color :vec4 :stage :fragment)
(uniform mvp :mat4) ;; model-view-projection matrix
(uniform time :float)

(defun pulse (x)
  (return (* (abs (sin time)) x))) ;; need to use RETURN in 3bgl-shader...

(defun vertex ()
  (setf gl-position (* mvp (* (vec4 (pulse (vec3 1 1 1)) 1) ;; pulse works with float and vec3, and works in both shaders...
                              position))))

(defun fragment ()
  (setf color (vec4 (pulse 1) 0 0 1)))
