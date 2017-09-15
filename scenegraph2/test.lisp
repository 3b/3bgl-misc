#++ (asdf:load-systems '3bgl-misc)
(defpackage #:scene2test
  (:use :cl :basecode)
  (:local-nicknames (:s #:3bgl-ai-shaders)
                    (:b #:buffer-builder)
                    (:sg #:3bgl-sg2)))
(in-package #:scene2test)

(defclass scene2test (basecode-glop
                      freelook-camera
                      basecode-exit-on-esc
                      basecode-shader-helper::basecode-shader-helper
                      scenegraph::scenegraph-state-helper
                      perspective-projection
                      basecode::fps)
  ((sg :accessor sg :initform nil)
   (buffer :accessor buffer :initform nil)
   (hh :initform (make-hash-table :test 'equal) :accessor hh)
   (tex :accessor tex :initform nil)
   (material :accessor material :initform nil)
   (shader-globals :reader shader-globals :initform (make-hash-table))
   (globals-ssbo :initform nil :accessor globals-ssbo))
  (:default-initargs :look-at-eye '(-97 14 -16)
                     :look-at-target '(30 28 5)
                     :projection-far 1000.0))

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

(declaim (inline v3))
(defun v3 (x y z)
  (sb-cga:vec (coerce x 'single-float)
              (coerce y 'single-float)
              (coerce z 'single-float)))

(defmethod run-main-loop :around ((w scene2test))
  (sg::with-resource-manager ()
    (call-next-method)))

(defmethod run-main-loop :before ((w scene2test))
  (setf (globals-ssbo w) (3bgl-ssbo:make-ssbo :data (shader-globals w))))

(defmethod basecode-shader-helper::shader-recompiled :after ((w scene2test) s)
  (format t "shader recompiled ~s~%" s)
  (let ((p (sg::program (sg::get-material (material w)))))
    (when p
      (sg::update-material (material w) :repack t)
      (3bgl-ssbo:update-ssbo-layout (globals-ssbo w)
                                    (3bgl-ssbo:calculate-layout
                                     (alexandria:hash-table-values
                                      (3bgl-shaders::ssbos p))
                                     (alexandria:hash-table-values
                                      (3bgl-shaders::structs p))
                                     :index 0)))))

(defparameter *no* 0)
(defparameter *nn* 0)
(defparameter *objects* 0)

(defun draw-object (o)
  (incf *objects*)
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

(defparameter *once* t)
(defmethod draw-node ((n sg::transform) &key mv p u)
  (when *once* (format t "draw xform ~s~%" (sg::name n)))
  (when (sg::children n)
    (let* ((mv (sb-cga:matrix* mv (sg::matrix n))))
      (loop for c in (sg::children n)
            do (draw-node c :mv mv :p p :u u)))))

(defmethod draw-node ((n sg::instance) &key mv p u)
  (when *once* (format t "draw instance ~s~%" (sg::name n)))
  (call-next-method)
  (when u (3bgl-shaders::uniform-matrix-4fv u (sb-cga:matrix* p mv)))
  (draw-object (sg::object n)))

(defun draw-sg (sg mv p &key u)
  (draw-node (sg::root sg) :mv mv :p p :u u))

(defmethod bind-globals ((w scene2test))
  (3bgl-ssbo:bind-ssbo (globals-ssbo w) sg::+globals-binding+))

(defmethod basecode-draw ((w scene2test))
  (gl:clear-color (* 0.2 (abs (sin (/ (get-internal-real-time) 1000.0)))) 0.2 0.3 1)
  (gl:clear :color-buffer :depth-buffer)
  (setf *w* w)
  (setf *no* 0)
  (setf *objects* 0)

  (progn                                ;time
    (progn
      (when (material w)
        (sg::bind-material (material w)))
      (setf (gethash 's::mvp (shader-globals w))
            (sb-cga:matrix*
             (basecode::projection-matrix w)
             (basecode::freelook-camera-modelview w)
             (sb-cga:scale* 0.1 0.1 0.1)))
      (bind-globals w)
      (when (sg w)
        (draw-sg (sg w) (basecode::freelook-camera-modelview w)
                 (basecode::projection-matrix w)))
      (%gl:use-program 0)))
  (let ((o *nn*))
    (setf *nn* (round (+ (* 9 *nn*) (* 1 *no*)) 10))
    (when (= *nn* o)
      (setf *nn* *no*)))
  (setf *once* nil))

;; *w* scenegraph::*v*
;; (setf *state* nil)
(defmethod key-down :after ((w scene2test) k)
  (case k
    #++(:c
     (sg::load-object :default :cube))
    (:r
     (setf (buffer w) nil
           (sg w) nil)
     (sg::reset-manager sg::*resource-manager*))
    (:i
     (let* ((sg (sg::load-object :file "d:/tmp/t/sponza.obj"
                                 #++(asdf:system-relative-pathname
                                     '3bgl-misc "data/teapot/teapot.obj")))
            (r (sg::root sg)))
       (sg::add-node sg 'sg::transform :root nil
                                       :matrix
                                       (sb-cga:scale* 0.1 0.1 0.1)
                                       #++(sb-cga:identity-matrix)
                                       #++(sb-cga:translate* 1.0 0.0 0.0))
       (sg::add-node* sg r (sg::root sg))
       ;;(sg::dump-scenegraph (sg::root sg))
       (setf (sg w) sg)))
    (:p
     (when (material w)
       (let ((p (sg::program (sg::get-material (material w)))))
         (basecode-shader-helper::forget-program p)
         (sg::delete-material (shiftf (material w) nil))))

     (let* ((p (3bgl-shaders::shader-program :vertex 's::vertex
                                             :fragment 's::fragment))
            (state (scenegraph::make-state*
                    :program p
                    :vertex-format (b::vertex-format-for-layout *vnc-layout*)
                    :blend-func '(:one :one-minus-src-alpha)
                    :blend nil
                    :depth-test t
                    :cull-face :back))
            (mat (sg::make-material 'ai-shaders state
                                    :count-var 's::count)))
       (setf (material w) 'ai-shaders)
       (sg::update-material 'ai-shaders)
       (setf (gethash 's::c (sg::globals mat)) #(1 2 3 4))
       (setf (gethash 's::color (sg::defaults mat)) #(1 0.5 0.2 1))
       (let ((h (make-hash-table)))
         (setf (gethash 's::color h) '(0.2 0.5 1 1))
         (vector-push-extend h (sg::materials mat)))
       (sg::update-material 'ai-shaders)))
    ((:backspace :tab)
     (setf (basecode::projection-far w) 1000.0)
     (basecode::update-projection w)
     (basecode::reset-freelook-camera w))
    (:c
     (let ((mat (sg::get-material 'ai-shaders)))
      (setf (gethash 's::color (aref (sg::materials mat) 0))
            (print (list (random 1.0) (random 1.0) (random 1.0) 1.0)))
       (setf (sg::dirty mat) t)))
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
     (let ((fps (basecode::average-fps w)))
       (format t "~&~,,',,3:d objects, ~,,',,3:d tris @ fps: ~s = ~sms~%"
               *objects* *no* (float fps 1.0)
               (when (> fps 0.001) (float (/ fps) 1.0)))))))

(setf 3bgl-shaders::*print-shaders* t)
#++
(sg::dump-scenegraph (sg::root (sg *w*)) :id t)
; (basecode-run (make-instance 'scene2test :x 0))
; (basecode-run (make-instance 'scene2test :x 1924 :y 15))
