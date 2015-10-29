#++ (asdf:load-systems '3bgl-misc)
(defpackage #:scenegraph-test
  (:use :cl :basecode)
  (:local-nicknames (:s #:scenegraph-test-shaders)
                    (:b #:buffer-builder)))
(in-package #:scenegraph-test)

(defclass scenegraph-test (basecode-glop
                           freelook-camera
                           basecode-exit-on-esc
                           basecode-shader-helper::basecode-shader-helper
                           scenegraph::scenegraph-state-helper)
  ((program :accessor program :initform nil)
   (buffer :accessor buffer :initform nil))
  (:default-initargs :look-at-eye '(3 2 15)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; eval at compile time as well so vbo-builder macro can see it
  (defparameter *vnc-layout* '((vertex :vec4)
                               (normal :vec3)
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

(defun cube (vbo &key (size 1))
  (b::with-vbo-builder (b 36 buffer-size
                        ;;:adjustable t
                        ;;:layout-var layout
                        :layout *vnc-layout*)
    (gl:with-primitives :triangles
      (labels ((v (i)
                 (color (ldb (byte 1 0) i)
                        (ldb (byte 1 1) i)
                        (ldb (byte 1 2) i))
                 (vertex (* size (+ -0.5 (ldb (byte 1 0) i)))
                         (* size (+ -0.5 (ldb (byte 1 1) i)))
                         (* size (+ -0.5 (ldb (byte 1 2) i))))
                 (b::emit-vertex)))
        (color 1 0 0)
        (normal 0 0 1)
        (v 0) (v 1) (v 2) (v 1) (v 2) (v 3)
        (loop for i below 4
              for a in '(0 0 2 3)
              for b in '(1 2 3 1)
              for n in '(#(0 -1 0) #(-1 0 0) #(0 1 0) #(1 0 0))
              for c from 2
              do (color (ldb (byte 1 0) c)
                        (ldb (byte 1 1) c)
                        (ldb (byte 1 2) c))
                 (normal n)
                 (v a) (v b) (v (logior a 4))
                 (v b) (v (logior a 4)) (v (logior b 4)))
        (color 0 1 0)
        (normal 0 0 -1)
        (v 4) (v 5) (v 6) (v 5) (v 6) (v 7)))
    (gl:named-buffer-storage vbo b '() :end buffer-size)))

(defmethod run-main-loop :before((w scenegraph-test))
  (setf (buffer w) (gl:create-buffer))
  (cube (buffer w))
  (setf (program w)
        (3bgl-shaders::shader-program :vertex 's::vertex
                                      :fragment 's::fragment)))

(defmethod basecode-draw ((w scenegraph-test))
  (gl:clear-color 0.1 0.2 0.3 1)
  (gl:clear :color-buffer :depth-buffer)
  (setf *w* w)
  (when *state*
    (scenegraph::apply-state *state* *state* :force t
                             :bindings (list (buffer w))))
  (let ((p (program w)))
    (when p
      (setf (3bgl-shaders::uniform p 's::mvp)
            (sb-cga:matrix*
             (kit.math:frustum -0.1 0.1 -0.1 0.1 0.5 100)
             (kit.math:look-at (v3 3 2 15) (v3 0 0 0) (v3 0 1 0))
             #++(basecode::projection-matrix w)
             #++(basecode::freelook-camera-modelview w)))
      (3bgl-shaders::use-program p))

    (%gl:draw-arrays :triangles 0 36)
    #++(gl:with-primitives :triangles
      (labels ((color (r g b &optional (a 1))
                 (gl:vertex-attrib 1 r g b a))
               (v (i)
                 (color (ldb (byte 1 0) i)
                        (ldb (byte 1 1) i)
                        (ldb (byte 1 2) i)
                        )
                 (gl:vertex-attrib 0
                                   (* 2 (+ -0.5 (ldb (byte 1 0) i)))
                                   (* 2 (+ -0.5 (ldb (byte 1 1) i)))
                                   (* 2 (+ -0.5 (ldb (byte 1 2) i))))))
        (color 1 0 0)
        (v 0) (v 1) (v 2) (v 1) (v 2) (v 3)
        (loop for i below 4
              for a in '(0 0 2 3)
              for b in '(1 2 3 1)
              for c from 2
              do (color (ldb (byte 1 0) c)
                        (ldb (byte 1 1) c)
                        (ldb (byte 1 2) c))
                 (v a) (v b) (v (logior a 4))
                 (v b) (v (logior a 4)) (v (logior b 4)))
        (color 0 1 0)
        (v 4) (v 5) (v 6) (v 5) (v 6) (v 7))))
  (%gl:use-program 0))

(multiple-value-list (b::calc-vbo-layout *vnc-layout*))
(b::vertex-format-for-layout *vnc-layout*)

(b::vertex-format-for-layout '((a :vec1u8)
                               (a :vec1u8)))

;; *w* scenegraph::*v*
;; (setf *state* nil)
(defmethod key-down :after ((w scenegraph-test) k)
  (case k
    (:space
     (setf *state*
           (scenegraph::make-state*
            :program (program w)
            :vertex-format (b::vertex-format-for-layout *vnc-layout*)
            :blend t
            :blend-func '(:src-alpha :one-minus-src-alpha)
            )))))


(setf 3bgl-shaders::*print-shaders* t)

; (basecode-run (make-instance 'scenegraph-test :x 1920))

