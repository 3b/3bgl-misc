(cl:defpackage #:hdr-test-shaders
  (:use :3bgl-glsl/cl))
(cl:in-package #:hdr-test-shaders)

;; vertex attributes
(input position :vec4 :location 0)
(input uv :vec2 :location 1)

;; final output
(output out-color :vec4 :stage :fragment)

(uniform texture1 :sampler-cube :stage :fragment)
(uniform exposure :float :stage :fragment)

;;(uniform m :mat4)
;;(uniform v :mat4)
;;(uniform p :mat4)
(uniform mv :mat4)
(uniform mvp :mat4)


(interface varyings (:out (:vertex outs)
                     :in (:fragment ins))
  (pos :vec3)
  (uv :vec2))

(defun vertex ()
  (declare (values))
  (setf (@ outs uv) uv
        (@ outs pos) (.xyz  position)
        gl-position (* mvp position)))

(defun filmic-tonemap1 (x)
  (declare (values :vec3) (:vec3 x))
  (let* ((a 0.22)
         (b 0.3)
         #++(c 0.1)
         #++(d 0.2)
         #++(e 0.01)
         #++(f 0.3)
         (bc #++(* b c) 0.03)
         (de #++ (* d e) 0.002)
         (df #++ (* d f) 0.06)
         (e_f #++ (/ e f) 0.03333333)
         (ax2 (* a x x))
         #++(bx (* b x))
         #++(bcx (* bx c)))
    (declare (:float a b
                     ;c d e f
                     bc de df e_f)
             (:vec3  ax2 ;bx bcx
                     ))
    (return
      #++(- (/ (+ ax2 bcx de) (+ ax2 bx df)) e_f)
      (- (/ (+ ax2 (* bc x) de) (+ ax2 (* b x) df)) e_f))))

(defun filmic-tonemap (x)
  ;; from "Uncharted 2: HDR Lighting", John Hable, gdc 2010
  ;; http://filmicgames.com/Downloads/GDC_2010/Uncharted2-Hdr-Lighting.pptx
  (declare (values :vec3) (:vec3 x))
  (let* ((a 0.22)
         (b 0.3)
         #++(c 0.1)
         #++(d 0.2)
         #++(e 0.01)
         #++(f 0.3)
         (bc #++(* b c) 0.03)
         (de #++ (* d e) 0.002)
         (df #++ (* d f) 0.06)
         (e_f #++ (/ e f) 0.03333333))
    (declare (:float a b
                     ;c d e f
                     bc de df e_f))
    (return
      ;; ax^2 not factored out, since this should be 1 fewer op with MAD
      (- (/ (+ (* x (+ (* a x) bc)) de)
            (+ (* x (+ (* a x) b)) df))
         e_f))))

(defun reinhard-tonemap (x)
  (declare (values :vec3) (:vec3 x))
  (return (/ x (+ 1 x))))

(defun fragment ()
  (declare (values))
  (let ((c (vec3 (texture texture1 (@ ins pos)))))
    (declare (:vec3 c))
    (setf c (* c exposure))
    (setf (.xyz out-color)
          (filmic-tonemap c)
          ;(reinhard-tonemap c)
          ))
  (setf (.a out-color) 1.0))
