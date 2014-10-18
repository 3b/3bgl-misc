(cl:defpackage #:pixel-cube-shader
  (:use :cl :3bgl-glsl)
  (:shadowing-import-from #:3bgl-glsl
                          #:defun #:defconstant #:defmacro)
  (:export
   #:vertex
   #:geometry
   #:fragment))
(cl:in-package #:pixel-cube-shader)


;; vertex attributes
(input position :vec4 :location 0)
(input uv :vec3 :location 1)

;; final output
(output out-color :vec4 :stage :fragment)


(uniform utexture :sampler-2d)

(uniform m :mat4)
(uniform v :mat4)
(uniform p :mat4)
(uniform mv :mat4)
(uniform mvp :mat4)
(uniform normal-matrix :mat4)

(uniform center :vec2)
(uniform p1 :float)
(uniform p2 :float)
(uniform p3 :float)
(uniform p4 :float)

(uniform width :float)
(uniform height :float)

(interface varyings (:out (:vertex outs :geometry outs)
                     :in (:fragment ins :geometry (ins "ins" :*)))
  (uv :vec2)
  (xy :vec2)
  (normal :vec3)
  (color :vec4))

(defun vertex ()
  ;; just direct passthrough of input vertex coords
  (setf gl-position position))

(defun vertex-instanced ()
  ;; just direct passthrough of input vertex coords
  (let ((ofs (vec4 (floor (/ gl-instance-id height))
                   (mod gl-instance-id height)
                   0.0
                   0.0)))
    (setf gl-position (+ position ofs))))

(defun vertex-instanced-fallback ()
  ;; just direct passthrough of input vertex coords
  (let* ((ofs (vec4 (floor (/ gl-instance-id height))
                    (mod gl-instance-id height)
                    0.0
                    0.0))
         (pos (+ position ofs))
         (color (texel-fetch utexture (ivec2 (.xy pos)) 0)))
    mv ;; work around bug in .xyz
    (setf gl-point-size (max 1.5
                             (* (/ 40 (length (.xyz (* mv pos))))
                                (length (.xyz color)))))
    (setf gl-position (* mvp pos))
    (setf (@ outs color) color
          (@ outs normal) (vec3 1 1 1))))

(defun geometry ()
  (declare (layout (:in :points) (:out :triangle-strip :max-vertices 16)))
  ;; inputs
  ;;    x,y,(z=0?) model-space coords of a point
  ;;    (model space = u/v pixel coords)
  ;;    should go 1 past lower and right edges of texture, since we don't
  ;;      output tris for lower or right face
  ;; uniforms
  ;;    modelview/projection matrix, normal matrix?
  ;;    not sure if there should be any separate scaling or not?
  ;;      possibly normalize width to some fixed value?
  ;;    texture
  ;; outputs
  ;;    transformed triangles, for front,back,upper,and left faces
  ;;     -> position, color, normal?, (extra data for outlines in shader?)
  (let* ((c (@ (aref gl-in 0) gl-position))
         (uv (ivec2 (.xy c)))
         (p (texel-fetch utexture uv 0))
         (lp (length (.xyz p)))
         (u (texel-fetch utexture (+ uv (ivec2 0 1)) 0))
         (lu (length (.xyz u)))
         (l (texel-fetch utexture (+ uv (ivec2 -1 0)) 0))
         (ll (length (.xyz l))))
    (when (> lp 0.1)
      ;; draw back
      (setf (@ outs color) p)
      (setf (@ outs normal) (* (mat3 normal-matrix) (vec3 0.0 0.0 -1.0)))
      (setf gl-position (* mvp (+ c (vec4 -0.5  0.5 -0.5 0.0))))
      (emit-vertex)
      (setf gl-position (* mvp (+ c (vec4 -0.5 -0.5 -0.5 0.0))))
      (emit-vertex)
      (setf gl-position (* mvp (+ c (vec4  0.5  0.5 -0.5 0.0))))
      (emit-vertex)
      (setf gl-position (* mvp (+ c (vec4  0.5 -0.5 -0.5 0.0))))
      (emit-vertex)
      (end-primitive)
      ;; draw front
      ;(setf (@ outs color) (vec4 0 1 0 1))
      (setf (@ outs normal) (* (mat3 normal-matrix) (vec3 0.0 0.0 1.0)))
      (setf gl-position (* mvp (+ c (vec4 -0.5 -0.5 0.5 0.0))))
      (emit-vertex)
      (setf gl-position (* mvp (+ c (vec4 -0.5  0.5 0.5 0.0))))
      (emit-vertex)
      (setf gl-position (* mvp (+ c (vec4  0.5 -0.5 0.5 0.0))))
      (emit-vertex)
      (setf gl-position (* mvp (+ c (vec4  0.5  0.5 0.5 0.0))))
      (emit-vertex)


      ;;(setf gl-position (* mvp (+ c (vec4 -0.5 -0.5 0.5 0.0))))
      ;;(emit-vertex)
      ;;(setf gl-position (* mvp (+ c (vec4 -0.5  0.5 0.5 0.0))))
      ;;(emit-vertex)
      (end-primitive))

    (when (not (^^ (= lp 0.0) (> lu 0.0)))
      ;; draw top
      (if (> lu 0.0)
          (setf (@ outs color) u)
          (setf (@ outs color) p))
;      (setf (@ outs color) (vec4 0 1 1 1))
      (setf (@ outs normal) (* (mat3 normal-matrix) (vec3 0.0 1.0 0.0)))
      (setf gl-position (* mvp (+ c (vec4 -0.5  0.5 -0.5 0.0))))
      (emit-vertex)
      (setf gl-position (* mvp (+ c (vec4  0.5  0.5 -0.5 0.0))))
      (emit-vertex)
      (setf gl-position (* mvp (+ c (vec4 -0.5  0.5  0.5 0.0))))
      (emit-vertex)
      (setf gl-position (* mvp (+ c (vec4  0.5  0.5  0.5 0.0))))
      (emit-vertex)
      (end-primitive))

    (when (not (^^ (= lp 0.0)  (> ll 0.0)))
      ;; draw side
      (if (> ll 0.0)
          (setf (@ outs color) l)
          (setf (@ outs color) p))
      #++(setf (@ outs color) l)
;      (setf (@ outs color) (vec4 1 1 0 1))
      (setf (@ outs normal) (* (mat3 normal-matrix) (vec3 -1.0 0.0 0.0)))
      (setf gl-position (* mvp (+ c (vec4 -0.5 -0.5 -0.5 0.0))))
      (emit-vertex)
      (setf gl-position (* mvp (+ c (vec4 -0.5 -0.5  0.5 0.0))))
      (emit-vertex)
      (setf gl-position (* mvp (+ c (vec4 -0.5  0.5 -0.5 0.0))))
      (emit-vertex)
      (setf gl-position (* mvp (+ c (vec4 -0.5  0.5  0.5 0.0))))
      (emit-vertex)
      (end-primitive))))

(defun fragment ()
  (setf out-color (* (@ ins color)
                     2
                     (clamp
                      (dot (@ ins normal)
                           (normalize (vec3 1 1.5 -2)))
                      0.2 1.0)))
  (setf (.a out-color) 1)
)



