#++(asdf:load-systems '3bgl-misc)
(defpackage #:embed-test
  (:use :cl :basecode))
(in-package #:embed-test)

(defparameter *command* "xterm -fn 10x20 -b 0 -into ~d &")

(defclass embed-test (basecode-glop
                      perspective-projection
                      basecode-clear
                      fps-graph basecode-draw-ground-plane
                      freelook-camera
                      basecode-exit-on-esc
                      3bgl-embed::basecode-embed-helper)
  ((embed :accessor embed :initform nil)
   (cube-shader :accessor cube-shader :initform nil))
  (:default-initargs :look-at-eye '(3 2 15)))

(defparameter *w* nil)

(defmethod run-main-loop :before ((w embed-test))
  (format t "run-main-loop :before embed-test~%")
  (setf (embed w)
        (glop:open-window
         (make-instance '3bgl-embed::glop-embedded :command *command*)
         "" 256 256
          :double-buffer nil
          :parent (glop:x11-window-id (basecode::%glop-window w))))
  (glop:show-window (embed w)))

(defmethod basecode-draw ((w embed-test))
  (when (embed w)
    (glop:dispatch-events (embed w) :blocking nil :on-foo nil))
  (setf *w* w)
  (gl:enable :depth-test)
  (gl:enable :texture-2d)
  (3bgl-embed::bind-texture (embed w))
  (gl:tex-parameter :texture-2d :texture-min-filter :linear)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  (case (3bgl-embed::child-state (embed w))
    ((nil)
     (gl:color 0.2 0.2 0.8 1))
    (:mapped
     (gl:color 1 1 1 1))
    (:unmapped
     (gl:color 0.5 0.5 0.5 1))
    (t
     (gl:color 0.5 0.1 0.1 1)))
  (gl:with-pushed-matrix* (:modelview)
    (gl:scale 1 1 1)
    (gl:rotate 90 1 0 0)
    (gl:translate 0 -10 -10)
    (gl:with-primitives :quads
      (gl:tex-coord 0 0)
      (gl:vertex -10 1 0)
      (gl:tex-coord 1 0)
      (gl:vertex 10 1 0)
      (gl:tex-coord 1 1)
      (gl:vertex 10 1 10)
      (gl:tex-coord 0 1)
      (gl:vertex -10 1 10)))
  (3bgl-embed::release-texture (embed w)))

(defmethod key-up :after ((w embed-test) k)
  (case k
    (:l2
     (format t "changing focus to child~%")
     (3bgl-embed::focus-embedded (embed w)))))

(defmethod mouse-down ((w embed-test) b x y)
  ;; move input focus back to main window on mouse click for now
  (3bgl-embed::unfocus-embedded (embed w) w))

(defmethod key-down :after ((w embed-test) k)
  (print k)
  (case k
    (:r
     (setf (cube-shader w)
           (3bgl-shaders::reload-program (cube-shader w)
                                         'pixel-cube-shader::vertex
                                         'pixel-cube-shader::fragment
                                         :geometry
                                         'pixel-cube-shader::geometry)))
    ((#\m :m)
     )
    (:f7
     )
    (:f10
     (3bgl-embed::resize-embedded (embed w) 80 25)
     )
    (:l1
     (3bgl-embed::resize-embedded (embed w) 80 24)
     #++(glop-xlib:x-set-geometry (glop::x11-window-display
                                (basecode::%glop-window w))
                               (win w)
                               0 0
                               (* 80 6) (* 160 13))

     )
))

#++
(basecode-run (make-instance 'embed-test))

