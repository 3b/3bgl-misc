#++ (asdf:load-systems '3bgl-misc)
(defpackage #:scene2test
  (:use :cl :basecode)
  (:local-nicknames (:s #:scene2test-shaders)
                    (:b #:buffer-builder)
                    (:sg #:3bgl-sg2)))
(in-package #:scene2test)

(defclass scene2test (basecode-glop
                      freelook-camera
                      basecode-exit-on-esc
                      basecode-shader-helper::basecode-shader-helper
                      scenegraph::scenegraph-state-helper
                      perspective-projection)
  ((program :accessor program :initform nil)
   (buffer :accessor buffer :initform nil)
   (tex :accessor tex :initform nil))
  (:default-initargs :look-at-eye '(3 2 15)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; eval at compile time as well so vbo-builder macro can see it
  (defparameter *vnc-layout* '((vertex :vec4)
                               (normal :vec3)
                               (uv :vec2)
                               (color :vec4u8))))

(defparameter *vnc-bindings* (multiple-value-list
                              (b::calc-vbo-layout *vnc-layout*)))

(defparameter *w* nil)

(defparameter *scene-graph* nil)
(defparameter *state* nil)

(declaim (inline v3))
(defun v3 (x y z)
  (sb-cga:vec (coerce x 'single-float)
              (coerce y 'single-float)
              (coerce z 'single-float)))

(defmethod run-main-loop :around ((w scene2test))
  (sg::with-resource-manager ()
    #++(setf (program w)
          (3bgl-shaders::shader-program :vertex 's::vertex
                                        :fragment 's::fragment))
    (format t "rml:a st~%")
    (call-next-method)))
(defmethod run-main-loop :before ((w scene2test))
  (format t "rml:b st~%")
  (setf *state* nil)
  (setf (program w)
        (3bgl-shaders::shader-program :vertex 's::vertex
                                      :fragment 's::fragment))
  ;;(setf (buffer w) (gl:create-buffer))
  #++(cube (buffer w)))

(defparameter *no* 0)
#++(sg::bindings (car (getf (car (second (buffer *w*))) :vertex)))
(defmethod basecode-draw ((w scene2test))
  (gl:clear-color (* 0.2 (abs (sin (/ (get-internal-real-time) 1000.0)))) 0.2 0.3 1)
  (gl:clear :color-buffer :depth-buffer)
  (setf *w* w)
  (gl:color 1 0 1 1)
  (gl:point-size 3)
  (gl:with-primitive :points
    (loop repeat 10 do (gl:vertex (- (random 1.0) 0.5)
                                  (- (random 1.0) 0.5)
                                  (- (random 1.0) 0.5))))
  #++(when  (buffer w)
    (let ((meshes (second (buffer w))))
      (loop for mesh in meshes
            for index = (getf mesh :index)
            for (bs start count) in (getf mesh :vertex)
            for vao = (sg::vao bs)
            do #++(gl:bind-vertex-array vao)
               (%gl:vertex-array-element-buffer
                vao (sg::vbo (sg::index-buffer sg::*resource-manager*)))
               (loop for b in (sg::bindings bs)
                     do (%gl:vertex-array-vertex-buffer
                         vao (sg::index b) (sg::vbo b)
                         (sg::offset b) (sg::stride b)))
               (gl:bind-vertex-array VAO)
               (incf *no* (second index))
               (%gl:draw-elements-base-vertex :triangles (second index)
                                              :unsigned-short
                                              (* 2 (first index)) start)
               (gl:bind-vertex-array 0)
)))
  (when *state*
    (scenegraph::apply-state *state* *state* :force t
                                             #+:bindings (list (buffer w))))
  (let ((p (program w)))
  (gl:point-size 3)
    (when p
      (setf (3bgl-shaders::uniform p 's::mvp)
            (sb-cga:matrix*
             #++(kit.math:frustum -0.1 0.1 -0.1 0.1 0.5 100)
             ;;(kit.math:frustum -10.1 10.1 -0.1 0.1 0.5 100)
             #++(kit.math:look-at (v3 3 2 15) (v3 0 0 0) (v3 0 1 0))
             (basecode::projection-matrix w)
             (basecode::freelook-camera-modelview w)))
      (when  (buffer w)
        (let ((meshes (second (buffer w))))

          (loop for mesh in meshes
            for index = (getf mesh :index)
            for (bs start count) = (getf mesh :vertex)
            for vao = (sg::vao bs)
            do #++(gl:bind-vertex-array vao)
               #++(Format t "meshes ~s~%" mesh)
               (%gl:vertex-array-element-buffer
                vao (sg::vbo (sg::index-buffer sg::*resource-manager*)))
               (loop for b in (sg::bindings bs)
                     do (%gl:vertex-array-vertex-buffer
                         vao (sg::index b) (sg::vbo b)
                         (sg::offset b) (sg::stride b)))
               (gl:bind-vertex-array VAO)
               (incf *no* (second index))
                (%gl:draw-elements-base-vertex :triangles (second index)
                                              :unsigned-short
                                              (* 2 (first index))
                                              start)
                #++(%gl:draw-arrays :points 0 1000)
               (gl:bind-vertex-array 0)))))
    ;(gl:disable :cull-face)
    ;(gl:disable :depth-test)
    #++(%gl:draw-arrays :triangles 0 36))
  (%gl:use-program 0))

;; *w* scenegraph::*v*
;; (setf *state* nil)
(defmethod key-down :after ((w scene2test) k)
  (case k
    (:c
     (sg::load-object :default :cube))
    (:r (setf (buffer w) nil) (sg::reset-manager sg::*resource-manager*))
    (:i (swank:inspect-in-emacs
         (setf (buffer w)
               (sg::load-object :file (asdf:system-relative-pathname
                                       '3bgl-misc
                                       "data/teapot.obj")))))
    (:p
     (unless (program w)
       (setf (program w)
             (3bgl-shaders::shader-program :vertex 's::vertex
                                           :fragment 's::fragment)))
     (setf *state*
           (scenegraph::make-state*
            :program (program w)
            :vertex-format (b::vertex-format-for-layout *vnc-layout*)
            :blend-func '(:one :one-minus-src-alpha)
            :blend nil
            :depth-test t
            :cull-face :back)))
    ((:backspace :tab)
     (basecode::reset-freelook-camera w))
    (:space
     #++(setf (tex w)
           (list
            (sg::load-texture
             (asdf:system-relative-pathname
              '3bgl-misc "data/debug-texture.png"))
            (sg::load-texture
             (asdf:system-relative-pathname
              '3bgl-misc "data/debug-bump-texture.png"))))
     ())))

(setf 3bgl-shaders::*print-shaders* t)


; (basecode-run (make-instance 'scene2test :x 0))
; (basecode-run (make-instance 'scene2test :x 1924))

#++
(
 geometry = variable sized records? (or batch up into fixed size blocks?)
 static geometry = fixed lifetime: load whole level->free whole level = slab GC
 streamed geometry = variable lifetime: load some, free others = compacting GC?
 dynamic geometry = generate per-frame, use once = ring-buffer

 material = ~fixed size records? (max size of used types if known?)
 material data = static/streamed

 bone/anim data

 per-scene uniforms: slab/compacting?
 per-frame uniforms: camera, global material params(sun, ambient) = ring-buffer
 per-object: object material params (which material, colors, etc) = mapped ring

)
