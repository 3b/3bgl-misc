(in-package #:3bgl-livecode)

(defparameter *command* "xterm -fn 10x20 -b 0 -into ~d -e emacsclient -nw -c &")

(defclass livecode-main (basecode-glop
                         perspective-projection
                         basecode-clear
                         fps-graph
                         basecode-draw-ground-plane
                         freelook-camera
                         basecode-exit-on-esc
                         3bgl-embed::basecode-embed-helper
                         basecode-shader-helper::basecode-shader-helper
                         )
  ((embed :accessor embed :initform nil)
   (cube-shader :accessor cube-shader :initform nil)
   (fallback-shader :accessor fallback-shader :initform nil)
   (point-vbo :accessor point-vbo :initform nil))
  (:default-initargs :look-at-eye '(3 2 15)))

(defparameter *w* nil)

(defmethod run-main-loop :before ((w livecode-main))
  (setf (embed w)
        (glop:open-window
         (make-instance '3bgl-embed::glop-embedded :command *command*
                        :parent w)
         "" 256 256
          :double-buffer nil
          :parent (glop:x11-window-id (basecode::%glop-window w))))
  (glop:show-window (embed w)))
(defmethod 3bgl-embed::embedded-window-mapped ((w livecode-main) c)
  (3bgl-embed::resize-embedded c 81 25))
(defmethod 3bgl-embed::embedded-window-unmapped ((w livecode-main) c)
  (format t "child unmapped, unfocus~%")
  (3bgl-embed::unfocus-embedded c w))


(defun setup-draw-embed (w p)
  (glop:dispatch-events (embed w) :blocking nil :on-foo nil)
  (gl:enable :texture-2d)
  (3bgl-embed::bind-texture (embed w))
  (%gl:tex-parameter-i :texture-2d :texture-min-filter
                       #. (cffi:foreign-enum-value '%gl:enum :linear))
  (%gl:tex-parameter-i :texture-2d :texture-mag-filter
                       #. (cffi:foreign-enum-value '%gl:enum :linear))
  (flet ((c (r g b)
           (gl:color r g b 1)
           (when p
             (setf (3bgl-shaders::uniform p 'pixel-cube-shader::color)
                   (vector (float r 1.0) (float g 1.0) (float b 1.0) 1.0)))))
    (case (3bgl-embed::child-state (embed w))
      ((nil)
       (c 0.2 0.2 0.8))
      (:mapped
       (c 1 1 1))
      (:unmapped
       (c 0.5 0.5 0.5))
      (t
       (c 0.5 0.1 0.1))))
  ;; we use instancing to draw multiple points from single-point VBO
  (unless (point-vbo w)
    (unless (point-vbo w)
      (setf (point-vbo w) (car (gl:gen-buffers 1))))
    (gl:bind-buffer :array-buffer (point-vbo w))
    (static-vectors:with-static-vector (b 4 :element-type 'single-float)
      (setf (aref b 0) 0.0)
      (setf (aref b 1) 0.0)
      (setf (aref b 2) 0.0)
      (setf (aref b 3) 1.0)
      (gl:enable-vertex-attrib-array 0)
      (%gl:buffer-data :array-buffer (* 4 4)
                       (static-vectors:static-vector-pointer b)
                       :static-draw)
      (gl:vertex-attrib-pointer 0 4 :float nil 0 (cffi:null-pointer))))

  ;; set up some shader uniforms
  (setf (3bgl-shaders::uniform p 'pixel-cube-shader::width)
        (1+ (glop:window-width (embed w))))
  (setf (3bgl-shaders::uniform p 'pixel-cube-shader::height)
        (1+ (glop:window-height (embed w))))
  (let* ((mv (sb-cga:matrix*
              (basecode::freelook-camera-modelview w)
              (sb-cga:rotate-around (sb-cga:vec 1.0 0.0 0.0)
                                    (float (kit.math:deg-to-rad 180) 1.0))
              (sb-cga:translate* 0.0 -10.0 0.0)
              (sb-cga:scale* 0.02 0.02 0.08)))
         (mvp (sb-cga:matrix* (basecode::projection-matrix w) mv)))
    (setf (3bgl-shaders::uniform p 'pixel-cube-shader::mv)
          mv)
    (setf (3bgl-shaders::uniform p 'pixel-cube-shader::mvp)
          mvp))
  (setf (3bgl-shaders::uniform p 'pixel-cube-shader::normal-matrix)
        (sb-cga:identity-matrix))
  (setf (3bgl-shaders::uniform p 'pixel-cube-shader::utexture) 0))

(defun fallback-draw-embed (w)
  (unless (fallback-shader w)
    (setf (fallback-shader w)
          (3bgl-shaders::shader-program
           :vertex 'pixel-cube-shader::vertex-instanced-fallback
           :fragment 'pixel-cube-shader::fragment)))
  (when (and (embed w) (fallback-shader w))
    (setup-draw-embed w (fallback-shader w))
      (when (and (embed w) (fallback-shader w))
    (setup-draw-embed w (fallback-shader w))
    (gl:enable :point-smooth :blend :vertex-program-point-size)
    (gl:point-size 10)
    (3bgl-shaders::use-program (fallback-shader w))
    (let ((wx (1+ (glop:window-width (embed w))))
          (wy (1+ (glop:window-height (embed w)))))
      (gl:bind-buffer :array-buffer (point-vbo w))
      (%gl:draw-arrays-instanced :points 0 1 (* wx wy))
      (gl:bind-buffer :array-buffer 0))
    (gl:use-program 0)
    )))

(defparameter *in-embed-draw* nil)
(defparameter *once* t)
(defun draw-embed (w)
  (unless (cube-shader w)
    (setf (cube-shader w)
          (3bgl-shaders::shader-program
           :vertex 'pixel-cube-shader::vertex-instanced
           :fragment 'pixel-cube-shader::fragment
           :geometry 'pixel-cube-shader::geometry)))
  (when (and (embed w) (cube-shader w))
    (setup-draw-embed w (cube-shader w))
    (let ((*in-embed-draw* t))
      (3bgl-shaders::use-program (cube-shader w))
      (let ((wx (1+ (glop:window-width (embed w))))
            (wy (1+ (glop:window-height (embed w)))))
        (gl:bind-buffer :array-buffer (point-vbo w))
        (%gl:draw-arrays-instanced :points 0 1 (* wx wy))
        (gl:bind-buffer :array-buffer 0))
      (gl:use-program 0))))

(defparameter *in-main-draw* nil)
(defun main-draw (w)
  (declare (ignorable w))
  (let ((*in-main-draw* t))
    (when *once*
      (setf *once* nil)
      (error "foo!"))
    ))

(cffi:defcfun ("glXGetCurrentContext" glx-get-current-context) glop-glx::glx-context)
(defparameter *in-dispatch-events* nil)

(defparameter *in-nested-debugger* nil)
(defun make-debugger-hook (old-hook w)
  (lambda (condition hook)
    (format t "debugger hook!")
    (if *in-nested-debugger*
        (progn
          (format t "nested error!~%")
          (funcall old-hook condition hook))
        (let* ((*in-nested-debugger* t)
               (mutex (cons (bt:make-lock) nil))
               (wd (glop:x11-window-display (basecode::%glop-window w)))
               (wid (glop:x11-window-id (basecode::%glop-window w)))
               (context (glop::glx-context-ctx (glop::window-gl-context
                                                (basecode::%glop-window w))))
               (thread))
          (format t "release context~%")
          (glop-glx:glx-release-context wd)
          (format t "start thread~%")
          (setf thread (bt:make-thread
                        (lambda ()
                          (let ((*in-nested-debugger* t))
                            (format t "make current... ~s ~s ~s~%"
                                    wd wid context)
                            (glop-glx:glx-make-current wd wid context)
                            (format t "start nested loop...~%")
                            (basecode::run-nested-loop w mutex)
                            (glop-glx:glx-release-context wd)
                            (format t "returned from nested loop~%")))
                        :initial-bindings
                        (list (cons '*in-nested-debugger* *in-nested-debugger*)
                              (cons '*in-main-draw* *in-main-draw*)
                              (cons '*in-embed-draw* *in-embed-draw*)
                              (cons '*in-dispatch-events* *in-dispatch-events*))
                        :name "nested main loop"))
          (unwind-protect
               (funcall old-hook condition hook)
            (format t "shutting down nested loop...~%")
            (bt:with-lock-held ((car mutex))
              (setf (cdr mutex) t))
            (bt:join-thread thread)
            (format t "restore context~%")
            (glop-glx:glx-make-current wd wid context)
            )))))


(defmethod basecode-draw ((w livecode-main))
  (let ((*debugger-hook* (make-debugger-hook *debugger-hook* w)))
    (gl:enable :depth-test)
    (unless *in-dispatch-events*
      (when (embed w)
        (let ((*in-dispatch-events* t))
          (glop:dispatch-events (embed w) :blocking nil :on-foo nil))))
    (setf *w* w)
    (3bgl-embed::bind-texture (embed w))
    (if *in-embed-draw*
        (fallback-draw-embed w)
        (draw-embed w))
    (3bgl-embed::release-texture (embed w))
    (main-draw w)))


(defmethod key-up :after ((w livecode-main) k)
  (case k
    (:l2
     (format t "changing focus to child~%")
     (3bgl-embed::focus-embedded (embed w)))))

(defmethod mouse-down ((w livecode-main) b x y)
  ;; move input focus back to main window on mouse click for now
  (3bgl-embed::unfocus-embedded (embed w) w))

(defmethod key-down :after ((w livecode-main) k)
  (case k
    ((#\m :m)
     )
    (:f7
     )
    (:f10
     (3bgl-embed::resize-embedded (embed w) 81 25)
     )
    (:l1
     (3bgl-embed::resize-embedded (embed w) 81 24)
     #++(glop-xlib:x-set-geometry (glop::x11-window-display
                                (basecode::%glop-window w))
                               (win w)
                               0 0
                               (* 80 6) (* 160 13))

     )
    (:backspace
     (basecode::reset-freelook-camera w)
     )
))
;;(basecode-run (make-instance 'livecode-main))
