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
   (sg :accessor sg :initform nil)
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

#++(sg::bindings (car (getf (car (second (buffer *w*))) :vertex)))

(defun draw-object (o)
  (let ((meshes o))
    (loop for mesh in meshes
          for index = (getf mesh :index)
          for (bs start count) = (getf mesh :vertex)
          for vao = (sg::vao bs)
          do (%gl:vertex-array-element-buffer
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
             (gl:bind-vertex-array 0))))


(defmethod draw-node ((n sg::transform) &key mv p u)
  (when (sg::children n)
    (let* ((mv (sb-cga:matrix* mv (sg::matrix n))))
      (loop for c in (sg::children n)
            do (draw-node c :mv mv :p p :u u)))))

(defmethod draw-node ((n sg::instance) &key mv p u)
  ;;(break "o"(sg::object n))
  (3bgl-shaders::uniform-matrix-4fv u (sb-cga:matrix* p mv))
  (draw-object (sg::object n))
  (when (sg::children n)
    (loop for c in (sg::children n)
          do (draw-node c :mv mv :p p :u u))))

(defun draw-sg (sg mv p &key u)
  (let ((r (gethash "teapot.obj" (sg::index sg)))
        (b (gethash "Base" (sg::index sg)))
        (top (gethash "Top" (sg::index sg))))
    (when b
      (setf (sg::matrix b)
            (sb-cga:matrix* (sg::matrix b)
                            (sb-cga:rotate-around (sb-cga:normalize
                                                   (sb-cga:vec 0.0 1.0 0.0))
                                                  0.05))))
    (when r
      (setf (sg::matrix r)
            (sb-cga:translate* 0.0
                              (* 3 (sin (/ (get-internal-real-time) 700.0)))
                              0.0)))
    (when top
      (setf (sg::matrix top)

            (sb-cga:translate* 0.0
                               (abs (* 2 (sin (/ (get-internal-real-time) 1200.0))))
                               0.0))))
  (draw-node (sg::root sg) :mv mv :p p :u u))

(defparameter *no* 0)
(defparameter *nn* 0)
(defmethod basecode-draw ((w scene2test))
  (gl:clear-color (* 0.2 (abs (sin (/ (get-internal-real-time) 1000.0)))) 0.2 0.3 1)
  (gl:clear :color-buffer :depth-buffer)
  (setf *w* w)
  (gl:color 1 0 1 1)
  (gl:point-size 3)
  (setf *no* 0)
  (gl:with-primitive :points
    (loop repeat 10 do (gl:vertex (- (random 1.0) 0.5)
                                  (- (random 1.0) 0.5)
                                  (- (random 1.0) 0.5))))

  (when *state*
    (scenegraph::apply-state *state* *state* :force t))
  (let ((p (program w)))
  (gl:point-size 3)
    (when p
      (setf (3bgl-shaders::uniform p 's::mvp)
            (sb-cga:matrix*
             (basecode::projection-matrix w)
             (basecode::freelook-camera-modelview w)))
      (when (sg w)
        (draw-sg (sg w) (basecode::freelook-camera-modelview w)
                 (basecode::projection-matrix w)
                 :u (getf (gethash 's::mvp (3bgl-shaders::uniforms p))
                          :index)))))
  (%gl:use-program 0)
  (let ((o *nn*))
    (setf *nn* (round (+ (* 9 *nn*) (* 1 *no*)) 10))
    (when (= *nn* o)
      (setf *nn* *no*))))

;; *w* scenegraph::*v*
;; (setf *state* nil)
(defmethod key-down :after ((w scene2test) k)
  (case k
    (:c
     (sg::load-object :default :cube))
    (:r
     (setf (buffer w) nil
           (sg w) nil)
     (sg::reset-manager sg::*resource-manager*))
    (:i
     (let* ((sg  (sg::load-object :file (asdf:system-relative-pathname
                                         '3bgl-misc
                                         "data/teapot/teapot.obj")))
            (r (sg::root sg)))
       (sg::add-node sg 'sg::transform :root nil
                                       :matrix
                                       (sb-cga:scale* 0.1 0.1 0.1)
                                       #++(sb-cga:identity-matrix)
                                       #++(sb-cga:translate* 1.0 0.0 0.0))
       (sg::add-node* sg r (sg::root sg))
       (setf (sg w) sg)))
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
    (:l
     #++(setf (tex w)
           (list
            (sg::load-texture
             (asdf:system-relative-pathname
              '3bgl-misc "data/debug-texture.png"))
            (sg::load-texture
             (asdf:system-relative-pathname
              '3bgl-misc "data/debug-bump-texture.png")))))
    (:space
     )))

(setf 3bgl-shaders::*print-shaders* t)


; (basecode-run (make-instance 'scene2test :x 0))
; (basecode-run (make-instance 'scene2test :x 1924))


