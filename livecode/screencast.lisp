(in-package #:3bgl-livecode)

#++
(defparameter *command* "xterm -fn 10x20 -b 0 -into ~d -e emacsclient -nw -c &")

(defparameter *command* "xterm -fn 8x16 -b 0 -into ~d -e emacsclient -nw -c &")
(defparameter *scale* (/ 8 10.0))

(defclass screencast (basecode-glop
                      perspective-projection
                      basecode-clear
                      fps-graph
                      basecode-draw-ground-plane
                      freelook-camera
                      basecode-exit-on-esc
                      3bgl-embed::basecode-embed-helper
                      basecode-shader-helper::basecode-shader-helper)
  ((embed :accessor embed :initform nil)
   (embed-mv :accessor embed-mv :initform (sb-cga:identity-matrix))
   (embed2 :accessor embed2 :initform nil)
   (embed2-mv :accessor embed2-mv :initform (sb-cga:identity-matrix))
   (cube-shader :accessor cube-shader :initform nil)
   (fallback-shader :accessor fallback-shader :initform nil)
   (teapot-shaders :accessor teapot-shaders :initform (make-array 10 :initial-element nil))
   (teapot-shader-index :accessor teapot-shader-index :initform nil)
   (point-vbo :accessor point-vbo :initform nil))
  (:default-initargs :look-at-eye '(3 2 15)))

(defparameter *w* nil)

(defmethod run-main-loop :before ((w screencast))
  ;; set up shaders
  (setf (fallback-shader w)
          (3bgl-shaders::shader-program
           :vertex 'pixel-cube-shader::vertex-instanced-fallback
           :fragment 'pixel-cube-shader::fragment))
  (setf (cube-shader w)
        (3bgl-shaders::shader-program
         :vertex 'pixel-cube-shader::vertex-instanced
         :fragment 'pixel-cube-shader::fragment
         :geometry 'pixel-cube-shader::geometry))
  ;; make some teapot shaders
  (loop for (v f) in (list* (list 'screencast-shaders:vertex
                                  'screencast-shaders:fragment)
                            3bgl-shader-example::*programs*)
        for i from 0
        do (setf (aref (teapot-shaders w) i)
                 (3bgl-shaders::shader-program
                  :vertex v :fragment f)))

  ;; set up VBO for drawing embedded windows
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


  ;; transforms for embedded windows
  (setf (embed-mv w)
        (sb-cga:matrix*
         (sb-cga:translate* -8.0 -0.0 -7.0)
         (sb-cga:rotate-around (sb-cga:vec 1.0 0.0 0.0)
                               (float (kit.math:deg-to-rad 180) 1.0))
         (sb-cga:translate* 0.0 -10.0 0.0)
         (sb-cga:scale* (/ 0.02 *scale*) (/ 0.02 *scale*) (/ 0.08 *scale*))))
  (setf (embed2-mv w)
        (sb-cga:matrix*
         (sb-cga:translate* 12.0 0.0 -8.0)
         (sb-cga:rotate-around (sb-cga:vec 0.0 1.0 0.0)
                               (float (kit.math:deg-to-rad -90) 1.0))
         (sb-cga:rotate-around (sb-cga:vec 1.0 0.0 0.0)
                               (float (kit.math:deg-to-rad 180) 1.0))
         (sb-cga:translate* 0.0 -10.0 0.0)
         (sb-cga:scale* (/ 0.02 *scale*) (/ 0.02 *scale*) (/ 0.08 *scale*))))
  ;; create embedded windows
  (setf (embed w)
        (glop:open-window
         (make-instance '3bgl-embed::glop-embedded :command *command*
                        :parent w)
         "" 256 256
          :double-buffer nil
          :parent (glop:x11-window-id (basecode::%glop-window w))))
  (glop:show-window (embed w))
  (setf (embed2 w)
        (glop:open-window
         (make-instance '3bgl-embed::glop-embedded :command *command*
                        :parent w)
         "" 256 256
          :double-buffer nil
          :parent (glop:x11-window-id (basecode::%glop-window w))))
  (glop:show-window (embed2 w)))

(defmethod 3bgl-embed::embedded-window-mapped ((w screencast) c)
  (3bgl-embed::resize-embedded c 81 25))
(defmethod 3bgl-embed::embedded-window-unmapped ((w screencast) c)
  (format t "child unmapped, unfocus~%")
  (3bgl-embed::unfocus-embedded c w))


(defun setup-draw-embed (w p embed mv)
  (gl:enable :texture-2d)
  (glop:dispatch-events embed :blocking nil :on-foo nil)
  (3bgl-embed::bind-texture embed)
  (%gl:tex-parameter-i :texture-2d :texture-min-filter
                       #. (cffi:foreign-enum-value '%gl:enum :linear))
  (%gl:tex-parameter-i :texture-2d :texture-mag-filter
                       #. (cffi:foreign-enum-value '%gl:enum :linear))
  (flet ((c (r g b)
           (gl:color r g b 1)
           (when p
             (setf (3bgl-shaders::uniform p 'pixel-cube-shader::color)
                   (vector (float r 1.0) (float g 1.0) (float b 1.0) 1.0)))))
    (case (3bgl-embed::child-state embed)
      ((nil)
       (c 0.2 0.2 0.8))
      (:mapped
       (c 1 1 1))
      (:unmapped
       (c 0.5 0.5 0.5))
      (t
       (c 0.5 0.1 0.1))))

  ;; set up some shader uniforms
  (setf (3bgl-shaders::uniform p 'pixel-cube-shader::width)
        (1+ (glop:window-width embed)))
  (setf (3bgl-shaders::uniform p 'pixel-cube-shader::height)
        (1+ (glop:window-height embed)))
  (let* ((mv (sb-cga:matrix*
              (basecode::freelook-camera-modelview w)
              mv))
         (mvp (sb-cga:matrix* (basecode::projection-matrix w) mv)))
    (setf (3bgl-shaders::uniform p 'pixel-cube-shader::mv)
          mv)
    (setf (3bgl-shaders::uniform p 'pixel-cube-shader::mvp)
          mvp))
  (setf (3bgl-shaders::uniform p 'pixel-cube-shader::normal-matrix)
        (sb-cga:identity-matrix))
  (setf (3bgl-shaders::uniform p 'pixel-cube-shader::utexture) 0))

(defmacro with-embed ((var w shader embed embed-mv) &body body)
  `(let ((,var ,embed))
     (when ,var
       (unwind-protect
            (progn
              (3bgl-embed::bind-texture ,var)
              (setup-draw-embed ,w ,shader
                                ,embed ,embed-mv)
              ,@body)
         (3bgl-embed::release-texture ,var)))))

(defparameter *in-embed-draw* nil)
(defparameter *once* t)

(defun draw-embed (w shader)
  (when (and shader (or (embed w) (embed2 w)))
    (gl:enable :point-smooth :blend :vertex-program-point-size)
    (gl:bind-buffer :array-buffer (point-vbo w))
    (loop for e in (list (embed w) (embed2 w))
          for mv in (list (embed-mv w) (embed2-mv w))
          when e
            do (with-embed (embed w shader e mv)
                 (3bgl-shaders::use-program shader)
                 (let ((wx (1+ (glop:window-width embed)))
                       (wy (1+ (glop:window-height embed))))
                   (%gl:draw-arrays-instanced :points 0 1 (* wx wy)))))
    (gl:bind-buffer :array-buffer 0)
    (gl:use-program 0)))


(defparameter *in-main-draw* nil)

(defparameter *teapot-rotation-x* 0.0)
(defparameter *teapot-rotation-y* 0.0)
(defparameter *teapot-rotation-z* 0.0)

(defun draw-teapot ()
  (gl:disable :blend :texture-2d)

  (gl:color-material :front :ambient-and-diffuse)
  (gl:light :light0 :position '(0 1 1 0))
  (gl:light :light0 :diffuse '(0.2 0.4 0.6 0))

  (gl:color 1 1 1)
  (glut:solid-teapot 1.3)

  (incf *teapot-rotation-x* 0.01)
  (incf *teapot-rotation-y* 0.05)
  (incf *teapot-rotation-z* 0.03))

(defun main-draw (w)
  (declare (optimize debug))
  ;; quick example of running debugger inside scene being debugged...
  (when (teapot-shader-index w)
      (let ((p1 (aref (teapot-shaders w) (teapot-shader-index w))))
        (flet ((radians (x) (coerce (/ (* pi x) 180) 'single-float))
               (v (x y z) (sb-cga:vec (float x 1.0)
                                      (float y 1.0)
                                      (float z 1.0))))
          (let* ((m (sb-cga:matrix*
                     ;; some versions of glut transform the teapot
                     ;; with gl matrix, so duplicate that here
                     ;; (might not look right on newest freeglut though)
                     (sb-cga:rotate-around (v 1 0 0)
                                           (radians 270))
                     ;; lets make the teapot bigger...
                     (sb-cga:scale* (* 1.3 1.75) (* 1.3 1.75) (* 1.3 1.75)) ;; fixed
                     (sb-cga:translate (v 0 0 -1.5))))
                 (v (sb-cga:matrix*
                     (sb-cga:translate (v 4 2 4))
                     (sb-cga:rotate-around (v 1 0 0)
                                           (radians *teapot-rotation-x*))
                     (sb-cga:rotate-around (v 0 1 0)
                                           (radians *teapot-rotation-y*))
                     (sb-cga:rotate-around (v 0 0 1)
                                           (radians *teapot-rotation-z*))))
                 (mv (sb-cga:matrix* (basecode::freelook-camera-modelview w)
                                     v m))
                 (mvp (sb-cga:matrix* (basecode::projection-matrix w)
                                      mv)))
            (when p1
              (setf (3bgl-shaders::uniform p1 'screencast-shaders::time)
                    (float
                     (/ (get-internal-real-time)
                        internal-time-units-per-second)))
              (setf (3bgl-shaders::uniform p1 'screencast-shaders::mv) mv)
              (setf (3bgl-shaders::uniform p1 'screencast-shaders::mvp) mvp)
              (setf (3bgl-shaders::uniform p1 'screencast-shaders::normal-Matrix)
                    mv)
              (3bgl-shaders::use-program p1))))

        (draw-teapot)
        (gl:use-program 0))))

(cffi:defcfun ("glXGetCurrentContext" glx-get-current-context) glop-glx::glx-context)
(defparameter *in-dispatch-events* nil)

(defparameter *in-nested-debugger* nil)
(defun make-debugger-hook (old-hook w)
  (lambda (condition hook)
    #++(format t "debugger hook!")
    (if *in-nested-debugger*
        (progn
          #++(format t "nested error!~%")
          (funcall old-hook condition hook))
        (let* ((*in-nested-debugger* t)
               (mutex (cons (bt:make-lock) nil))
               (wd (glop:x11-window-display (basecode::%glop-window w)))
               (wid (glop:x11-window-id (basecode::%glop-window w)))
               (context (glop::glx-context-ctx (glop::window-gl-context
                                                (basecode::%glop-window w))))
               (thread))
          #++(format t "release context~%")
          (glop-glx:glx-release-context wd)
          #++(format t "start thread~%")
          (setf thread (bt:make-thread
                        (lambda ()
                          (let ((*in-nested-debugger* t))
                            #++(format t "make current... ~s ~s ~s~%"
                                    wd wid context)
                            (glop-glx:glx-make-current wd wid context)
                            #++(format t "start nested loop...~%")
                            (basecode::run-nested-loop w mutex)
                            (glop-glx:glx-release-context wd)
                            #++(format t "returned from nested loop~%")))
                        :initial-bindings
                        (list (cons '*in-nested-debugger* *in-nested-debugger*)
                              (cons '*in-main-draw* *in-main-draw*)
                              (cons '*in-embed-draw* *in-embed-draw*)
                              (cons '*in-dispatch-events* *in-dispatch-events*))
                        :name "nested main loop"))
          (unwind-protect
               (funcall old-hook condition hook)
            #++(format t "shutting down nested loop...~%")
            (bt:with-lock-held ((car mutex))
              (setf (cdr mutex) t))
            (bt:join-thread thread)
            #++(format t "restore context~%")
            (glop-glx:glx-make-current wd wid context)
            )))))


(defmethod basecode-draw ((w screencast))
  (let ((*debugger-hook* (make-debugger-hook *debugger-hook* w)))
    (gl:enable :depth-test)
    (setf *w* w)

    (if *in-embed-draw*
        (draw-embed w (fallback-shader w))
        (let ((*in-embed-draw* t))
          (draw-embed w (cube-shader w))))
    (unless *in-main-draw*
      (let ((*in-main-draw* t))
        (main-draw w)))))

(defparameter *focus* nil)
(defmethod key-up :after ((w screencast) k)
  (case k
    (:l2
     (setf *focus* (embed w))
     (3bgl-embed::focus-embedded (embed w)))
    (:l1
     (setf *focus* (embed2 w))
     (3bgl-embed::focus-embedded (embed2 w)))))

(defmethod mouse-down ((w screencast) b x y)
  ;; move input focus back to main window on mouse click for now
  (when *focus*
    (3bgl-embed::unfocus-embedded *focus* w)
    (setf *focus* nil)))

(defmethod key-down :after ((w screencast) k)
  (case k
    ((#\m :m)
     )
    ((:1 :2 :3 :4 :5 :6 :7 :8 :9 :0)
     (setf (teapot-shader-index w)
           (position k #(:0 :1 :2 :3 :4 :5 :6 :7 :8 :9))))
    (:f7
     )
    (:backspace
     (basecode::reset-freelook-camera w)
     )
))

#++
(setf (embed-mv *w*)
        (sb-cga:matrix*
         (sb-cga:translate* -8.0 -0.0 -7.0)
         (sb-cga:rotate-around (sb-cga:vec 1.0 0.0 0.0)
                               (float (kit.math:deg-to-rad 180) 1.0))
         (sb-cga:translate* 0.0 -10.0 0.0)
         (sb-cga:scale* (/ 0.02 *scale*) (/ 0.02 *scale*)
                        (/ 0.08 *scale*))))

#++
(setf (embed2-mv *w*)
        (sb-cga:matrix*
         (sb-cga:translate* 12.0 0.0 -8.0)
         (sb-cga:rotate-around (sb-cga:vec 0.0 1.0 0.0)
                               (float (kit.math:deg-to-rad -90) 1.0))
         (sb-cga:rotate-around (sb-cga:vec 1.0 0.0 0.0)
                               (float (kit.math:deg-to-rad 180) 1.0))
         (sb-cga:translate* 0.0 -10.0 0.0)
         (sb-cga:scale* (/ 0.02 *scale*) (/ 0.02 *scale*)
                        (/ 0.08 *scale*))))

;(basecode::freelook-camera-position *w*)

;#(13.263169 -5.9924803 -23.041634)


#++
(basecode-run (make-instance 'screencast :width 1920 :height 1080))
