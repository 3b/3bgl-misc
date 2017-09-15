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
                        (ldb (byte 1 2) i)
                        (/ i 8))
                 (vertex (* size (+ -0.5 (ldb (byte 1 0) i)))
                         (* size (+ -0.5 (ldb (byte 1 1) i)))
                         (* size (+ -0.5 (ldb (byte 1 2) i))))
                 (b::emit-vertex))
               (v* (a b c &optional f)
                 (cond
                   (f (v a) (v b) (v c))
                   (t (v a) (v c) (v b)))))
        (color 1 0 0)
        (normal 0 0 1)
        (v* 0 1 2) (v* 1 3 2)
        (loop for i below 4
              for a in '(0 0 2 3)
              for b in '(1 2 3 1)
              for n in '(#(0 -1 0) #(-1 0 0) #(0 1 0) #(1 0 0))
              for c from 2
              do (color (ldb (byte 1 0) c)
                        (ldb (byte 1 1) c)
                        (ldb (byte 1 2) c)
                        (/ c 8))
                 (normal n)
                 (v* b a (logior a 4) (plusp i))
                 (v* b (logior a 4) (logior b 4) (plusp i)))
        (color 1 1 1)
        (normal 0 0 -1)
        (v* 4 6 5) (v* 5 6 7)))
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
             #++(kit.math:look-at (v3 3 2 15) (v3 0 0 0) (v3 0 1 0))
             #++(basecode::projection-matrix w)
             (basecode::freelook-camera-modelview w))))
    ;(gl:disable :cull-face)
    ;(gl:disable :depth-test)
    (%gl:draw-arrays :triangles 0 36))
  (%gl:use-program 0))

(multiple-value-list (b::calc-vbo-layout *vnc-layout*))
(b::vertex-format-for-layout *vnc-layout*)

(b::vertex-format-for-layout '((a :vec1u8)
                               (a :vec1u8)))

;; *w* scenegraph::*v*
;; (setf *state* nil)
(defmethod key-down :after ((w scenegraph-test) k)
  (case k
    (:c
     (gl:delete-buffers (list (buffer w)))
     (setf (buffer w) (gl:create-buffer))
     (cube (buffer w)))
    (:space
     (setf *state*
           (scenegraph::make-state*
            :program (program w)
            :vertex-format (b::vertex-format-for-layout *vnc-layout*)
            :blend t
           ; :blend-func '(:one :one-minus-src-alpha)
            :blend t
            :blend-func '(:one :zero)
            :depth-test t
            :cull-face :back
            )))))


(setf 3bgl-shaders::*print-shaders* t)

; (basecode-run (make-instance 'scenegraph-test :x 1924))

