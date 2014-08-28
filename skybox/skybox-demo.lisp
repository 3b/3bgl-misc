(defpackage #:skybox-demo
  (:use :cl :basecode))
(in-package #:skybox-demo)

(defclass skybox-demo (basecode-glop perspective-projection basecode-clear
                 fps-graph basecode-draw-ground-plane
                 freelook-camera
                 basecode-exit-on-esc)
  ((program :accessor program :initform nil))
  (:default-initargs :look-at-eye '(3 2 15)))

#++
(defmethod run-main-loop :around ((w skybox-demo))
  (setf (program w) (3bgl-shaders::reload-program
                     (program w)
                     'skybox-shader::vertex
                     'skybox-shader::fragment)
        ;(fbo w) (make-instance 'skybox-fbo)
        )
  (call-next-method))

(defparameter *w* nil)

;; fixme: don't duplicate constant between this and shader
(defconstant +surface-height+ 6360e3) ;; should match constant in shaders.lisp

(defmethod basecode-draw ((w skybox-demo))
;  (gl:flush)
;  (gl:finish)
;  (sleep 0.03)
  (setf *w* w)
  (gl:color 1 1 1 1)
  (gl:disable :cull-face :depth-test)

  (gl:with-pushed-matrix* (:modelview)
    #++(let ((x (sb-cga:vec* (basecode::freelook-camera-position w)
                         -0.99)))
     (gl:translate (aref x 0) (aref x 1) (aref x 2))
     )
    ;(gl:load-matrix)
    #++(gl:with-primitives :quads
      (gl:vertex -1 1 -1)
      (gl:vertex 1 1 -1)
      (gl:vertex 1 -1 -1)
      (gl:vertex -1 -1 -1))
    (when (program w)
      (let ((p1 (program w))
            (light-dir #++(sb-cga:normalize (sb-cga:vec 0.1 0.15 -0.9))
                       #++(sb-cga:normalize
                        (sb-cga:vec 0.1
                                    (float (sin (/ (get-internal-real-time)
                                                   4000.0))
                                           1.0)
                                    (float (cos (/ (get-internal-real-time)
                                                   4000.0))
                                           1.0))
                        )
                       (sb-cga:normalize
                        (sb-cga:vec (* 0.2
                                       (float (sin (/ (get-internal-real-time)
                                                      4000.0))
                                              1.0))
                                    (+ 0.17
                                       (* 0.2
                                        (float (cos (/ (get-internal-real-time)
                                                       4000.0))
                                               1.0)))
                                    -1.0)

                        ))
            (eye-pos (basecode::freelook-camera-position w)))
        (gl:use-program p1)
        (3bgl-shaders::uniformfv p1 "lightDir" light-dir)
        (3bgl-shaders::uniformfv p1 "eyePos" eye-pos)
        ;; translate eye and light into coordinate space where
        ;; sun is on +z axis, and eye is on xz plane on surface of planet
        (let* ((cos (min 1.0 (sb-cga:dot-product light-dir
                                                 (sb-cga:vec 0.0 0.0 1.0))))
               (sin (sin (acos cos)))
               (eye-pos-planet (sb-cga:vec (* sin +surface-height+)
                                           0.0
                                           (* cos +surface-height+))))
         (3bgl-shaders::uniformfv p1 "eyePosPlanet" eye-pos-planet))
        (3bgl-shaders::uniform-matrix p1 "mv"
                                      (basecode::freelook-camera-modelview w))
        (3bgl-shaders::uniform-matrix p1 "mvp"
                                      (sb-cga:matrix*
                                       (basecode::projection-matrix w)
                                       (basecode::freelook-camera-modelview w)))))

    (3bgl-shaders::uniformf (program w) "foo" 1)
    (flet ((v (x y z)
             (let ((uv (sb-cga:transform-point
                        (sb-cga:vec (float
                                     (* x
                                        (tan
                                         (3bgl-math:deg-to-rad
                                          (/ (basecode::projection-fov w)
                                             2))))
                                     1.0)
                                    (float
                                     (*  y
                                         (tan (3bgl-math:deg-to-rad
                                               (/ (basecode::projection-fov w)
                                                  (basecode::aspect w)
                                                  2))))
                                     1.0)
                                    -1.0)
                        (basecode::freelook-camera-orientation w))))
               (gl:vertex-attrib 1 (aref uv 0) (aref uv 1) (aref uv 2))
               (gl:vertex x y z))))
      (gl:with-primitives :quads
        (v -1 1 0)
        (v 1 1 0)
        (v 1 -1 0)
        (v -1 -1 0)))

    #++
    (3bgl-shaders::uniformf (program w) "foo" -1)
    #++
    (flet ((v (x y z)
             (gl:vertex-attrib 1 x y z)
             (gl:vertex x y z)))
      (gl:with-primitives :quads
        (v -20 20 -1)
        (v 20 20 -1)
        (v 20 -20 -1)
        (v -20 -20 -1)

        (v -10 10 -1)
        (v 10 10 -1)
        (v 10 -10 -1)
        (v -10 -10 -1)))
    (gl:use-program 0)))

(defmethod key-down :after ((w skybox-demo) k)
  (case k
    ((#\r :r)
     (time
      (setf (program w)
            (3bgl-shaders::reload-program (program w)
                                          'skybox-shaders::vertex
                                          'skybox-shaders::fragment))))
    (:n
     #++(setf (basecode::projection-fov w) 70)
     (print (basecode::freelook-camera-offset w))
     (setf (basecode::freelook-camera-offset w) 0.0)
     )
    (:m
     (setf (basecode::projection-fov w) 45)
     )
    (:l
     (setf (basecode::projection-fov w) 70)
     )
    (:p
     (setf (basecode::projection-fov w) 170)
     )))



; (basecode-run (make-instance 'skybox-demo))
