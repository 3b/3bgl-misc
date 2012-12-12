(cl:defpackage #:3bgl-mesh-shaders
  (:use :cl :glsl)
  (:shadowing-import-from #:glsl
                          #:defun #:defconstant))

(cl:in-package #:3bgl-mesh-shaders)

;; vertex attributes
(input position :vec4 :location 0)
(input uv :vec4 :location 1)
(input normal :vec3 :location 2)
(input tangent :vec3 :location 3)
(input bone-weights :vec4 :location 4)
(input bone-indices :ivec4 :location 5)

(input color :vec4 :location 10)


;;; uniforms
;; matrices
(uniform m :mat4 :stage :vertex)
(uniform v :mat4 :stage :vertex)
(uniform mvp :mat4 :stage :vertex)
(uniform normal-matrix :mat4 :stage :vertex)
;; lights
(uniform light-pos :vec4 :stage t)
;; textures
(uniform diffuse-tex :sampler-2d :stage :fragment)
(uniform specular-tex :sampler-2d :stage :fragment)
(uniform normal-tex :sampler-2d :stage :fragment)


;; varyings
(interface varyings (:out (:vertex outs)
                     :in (:fragment ins :geometry (ins "ins" :*) ))
  (position :vec4)
  (normal :vec3)
  (tangent :vec3)
  (bitangent :vec3)
  (color :vec4)
  (uv :vec4)
  (eye-direction :vec3)
  (light-direction :vec3))


;; fragment outputs
(output color :vec4 :stage :fragment)


;; vertex shader
(defun vertex ()
  (declare (values))
  (setf gl-position (* mvp position))
  (let* ((xn (normalize (* (mat3 normal-matrix) normal)))
         (xt (normalize (* (mat3 normal-matrix) tangent)))
         (xb (normalize (* (cross xn xt) (vec3 (.z uv))))))
    (declare (:vec3 xn xt xb))
    (setf (@ outs color) color
          (@ outs uv) (vec4 (.xy uv) 0.0 0.0)
          (@ outs normal) xn
          (@ outs tangent) xt
          (@ outs bitangent) xb
          (@ outs light-direction) (vec3 (- (* v light-pos) (* v m position)))
          (@ outs eye-direction) (- (vec3 0.0 0.0 0.0)
                                    (vec3 (* v m position))))))

;; fragment shader
(defun fragment ()
  (declare (values))
  (let* ((uv (vec2 (@ ins uv)))
         (dtex (texture-2d diffuse-tex uv))
         (stex (texture-2d specular-tex uv))
         (ntex (normalize (- (* 2.0 (.rgb (texture-2d normal-tex uv)))
                             1.0)))
         (distance (length (@ ins light-direction)))
         (one-over-d-squared (/ (* distance distance)))
         (tangent-space
           (transpose
            (mat3 (normalize (@ ins tangent))
                  (normalize (@ ins bitangent))
                  (normalize (@ ins normal)))))
         (n (normalize ntex))
         (l (normalize (* tangent-space (@ ins light-direction))))
         (e (normalize (* tangent-space (@ ins eye-direction))))
         (r (reflect (- l) n))
         (cos-theta (clamp (dot n l)0.0 1.0))
         (cos-alpha (clamp (dot e r) 0.0 1.0))
         ;; attenuate specular reflections from fragments facing away
         ;; from light in case normal map points back towards light
         (spec-back-falloff (smooth-step -0.3 0.0
                                          (dot (vec3 0.0 0.0 1.0)
                                               l))))
    (declare (:vec2 uv) (:vec3 n l e r ntex) (:vec4 dtex stex)
             (:mat3 tangent-space)
             (:float cos-theta cos-alpha distance
                     one-over-d-squared spec-back-falloff))
    (setf color
          (vec4
           (+ (* (.xyz dtex) cos-theta
                 (vec3 1.0 1.0 1.0) ;; fixme: uniforms for light colors...
                 50.0 one-over-d-squared)
              (* (.xyz dtex) 0.1)
              (* spec-back-falloff
                 (* (.xyz stex)
                    (vec3 0.8 0.3 0.8)
                    (pow cos-alpha (* 64.0 (.a stex))) 50.0 one-over-d-squared))
              )
           1.0))))




;;; tangent-space debug shaders
(uniform tsd-length :float :stage :geometry)
(interface varyings (:out (:geometry outs) :in (:fragment tsd-ins))
  (color :vec4))

(defun tsd-geometry ()
  (declare (values)
           (layout (:in :points) (:out :line-strip :max-vertices 6)))
  (let ((position (@ (aref gl-in 0) gl-Position))
        (normal (* (vec4 tsd-length)
                   (vec4 (@ (aref ins 0) normal) 0.0)))
        (tangent (* (vec4 tsd-length)
                    (vec4 (@ (aref ins 0) tangent) 0.0)))
        (bitangent (* (vec4 tsd-length)
                      (vec4 (@ (aref ins 0) bitangent) 0.0))))
    (declare (:vec4 position normal tangent bitangent))
    (setf gl-position position
          (@ outs color) (vec4 1.0 0.0 0.0 1.0))
    (emit-vertex)
    (setf gl-position (+ position normal))
    (emit-vertex)
    (end-primitive)

    (setf gl-position position
          (@ outs color) (vec4 0.0 1.0 0.0 1.0))
    (emit-vertex)
    (setf gl-position (+ position tangent))
    (emit-vertex)
    (end-primitive)

    (setf gl-position position
          (@ outs color) (vec4 0.0 0.0 1.0 1.0))
    (emit-vertex)
    (setf gl-position (+ position bitangent))
    (emit-vertex)
    (end-primitive)))



(defun tsd-fragment ()
  (declare (values))
  (setf color (@ tsd-ins color))
)
