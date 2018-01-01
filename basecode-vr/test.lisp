#++ (asdf:load-systems '3bgl-misc)
(defpackage #:3bgl-vr-test
  (:use :cl :basecode)
  (:local-nicknames (:s #:3bgl-ai-shaders)
                    (:b #:buffer-builder)
                    (:sg #:3bgl-sg2)))
(in-package #:3bgl-vr-test)

(defclass vrtest (basecode-vr::basecode-vr
                  sg::timing-helper
                  basecode-exit-on-esc
                  scenegraph::scenegraph-state-helper
                  basecode::fps)
  ((sg :accessor sg :initform nil))
  (:default-initargs :far-clip 1000.0
                     :max-timer-queries 32))

(defparameter *w* nil)

(defparameter *scene-graph* nil)

(declaim (inline v3))
(defun v3 (x y z)
  (sb-cga:vec (coerce x 'single-float)
              (coerce y 'single-float)
              (coerce z 'single-float)))

(defmethod run-main-loop :around ((w vrtest))
  (sg::with-resource-manager (:timing w)
    (let ((3bgl-shaders::*default-extensions* '(:arb-bindless-texture)))
      (call-next-method))))
(defparameter *once* t)

(defparameter *dt* (cons 0.0 0))
(defparameter *gc* nil)
(defparameter *gc-time* 0)

(defmethod basecode-vr::render-frame :around ((w vrtest))
  (let ((*once* (shiftf *once* nil)))
    (call-next-method)
    (sg::mark w :id :end)
    (sg::update-times w)
    (sg::mark w :id :start)))

(defmethod basecode-vr::render-scene ((w vrtest) eye)
  (unless (eq *gc* sb-kernel::*gc-epoch*)
    (format t "~&gc! ~s after ~s:~6,3,,,'0f~%" sb-ext:*gc-run-time*
            (floor (- (get-internal-real-time) *gc-time*)
                   (* 60 internal-time-units-per-second))
            (mod (/ (- (get-internal-real-time) *gc-time*)
                    internal-time-units-per-second)
                 60.0))
    (format t "~s ~s~%" *gc-time* (get-internal-real-time))
    (setf *gc-time* (get-internal-real-time))
    (setf *gc* sb-kernel::*gc-epoch*))
  (unwind-protect
       (progn
         #++(when (sg w)
              (update-graphs w))
         (gl:enable :multisample)
         (let ((start (basecode::now)))
           (gl:clear-color (* 0.2 (abs (sin (/ (get-internal-real-time) 1000.0))))
                           (if (eql eye :left) 0.12 0.1)
                           (if (eql eye :left) 0.1 0.24)
                           1)
           (gl:clear :color-buffer :depth-buffer)
           (gl:clear-color 1 0 1 1)
           (setf *w* w)
           (sg::mark w :id :clear)
           (progn ;;time
             (let* ((mv (basecode-vr::eye-modelview w eye))
                    (p (basecode-vr::eye-projection w eye))
                    (vp (sb-cga:matrix* p mv)))
               (when *once*
                 (format t "~s ~a~%~A~%" eye mv p))
               (sg::set-global 's::v mv)
               (sg::set-global 's::p p)
               (sg::set-global 's::vp vp)
               (sg::set-global 's::mvp vp)
               (sg::set-global '3bgl-sg2-shaders-common:ui-matrix
                               sb-cga:+identity-matrix+)

               (sg::mark w :id :globals)

               (when (sg w)
                 (sg::draw-sg (sg w) (sb-cga:identity-matrix)))

               #++
               (let ((*random-state* (sb-ext:seed-random-state 1)))
                 ;; (gl:clear :color-buffer)


                 (gl:use-program 0)
                 (gl:disable :depth-test :cull-face :scissor-test :texture-2d)
                 (gl:enable :multisample :blend :line-smooth :depth-test)
                 (gl:point-size 1)
                 (gl:line-width 2.1)
                 (gl:matrix-mode :modelview)
                 (gl:load-matrix mv)
                 (gl:matrix-mode :projection)
                 (gl:load-matrix p)
                 (gl:bind-vertex-array 0)
                 ;;           (gl:bind-framebuffer :framebuffer 0)

                 (gl:with-primitives :lines
                   (loop repeat 1000
                         do (gl:color (random 1.0) (random 1.0) (random 1.0) 1)
                            (gl:vertex (- (random 100.0) 50)
                                       (- (random 100.0) 50)
                                       (- (random 100.0) 50))))
                 (gl:finish)
                 (gl:flush))))
           (let ((dt (- (basecode::now) start)))
             (incf (car *dt*) dt)
             (incf (cdr *dt*)))))))

(defun ensure-sg (w)
  (let* ((sg (sg w)))
    (unless sg
      (setf (sg w) (make-instance 'sg::scenegraph)
            sg (sg w)))
    (unless (sg::find-node sg :root)
      (sg::add-node sg 'sg::transform :root nil))
    (unless (sg::find-node sg :ui-root)
      (sg::add-node sg 'sg::transform
                    :ui-root :root
                    :matrix (sb-cga:matrix*
                             (sb-cga:scale* (/ 2 1920.0) (/ 2 1080.0) 1.0)
                             (sb-cga:translate* (/ 1920 -2.0) (/ 1080 -2.0)
                                                0.0))))
    sg))

(defun add-object (w name path &key (transform (sb-cga:identity-matrix)))
  (let ((sg (ensure-sg w)))
    (when (sg::find-node sg name)
      (sg::remove-node (sg::find-node sg name)))
    (sg::add-node sg 'sg::transform name :root :matrix transform)
    (sg::load-object :file path :sg sg :parent-node name
                     :optimize-graph nil)))

(defun add-ui-node (w name class &rest initargs)
  (let ((sg (ensure-sg w)))
    (when (sg::find-node sg name)
      (sg::remove-node (sg::find-node sg name)))
    (apply #'sg::add-node sg class name :ui-root initargs)))

;; *w* scenegraph::*v*
;; (setf *state* nil)

(defun degrees (x) (float (* x (/ pi 180)) 1.0))

(defparameter *vsync* nil)
(defmethod key-down :after ((w vrtest) k)
  (case k
    (:r
     (setf (sg w) nil)
     (sg::reset-resource-manager sg::*resource-manager*))
    (:i
     (let ((mat (sg::get-material 'sg::ai-shaders)))
       (sg::reset-material mat))
     (let* ((*default-pathname-defaults* #p"d:/tmp/t/")
            (sg (sg::load-object :file "sponza.obj"))
            (r (sg::root sg)))
       (sg::add-node sg 'sg::transform :root nil
                                       :matrix
                                       (sb-cga:scale* 0.1 0.1 0.1))
       (sg::add-node* sg r (sg::root sg))
       (setf (sg w) sg)))
    ((:backspace :tab)
     (setf (basecode::projection-far w) 1000.0)
     (basecode::update-projection w)
     (basecode::reset-freelook-camera w))
    (:l
     (let* ((*default-pathname-defaults* #p"d:/tmp/t/bunny/"))
       (add-object w :bunny "bunny.obj")
       (setf (sg::matrix (sg::find-node (sg w) :bunny))
             (sb-cga:matrix* (sb-cga:scale* 40.0 40.0 40.0)
                             (sb-cga:translate* 0.0 1.0 0.0)))))
    (:space
     (setf (sg::previous-material sg::*resource-manager*) nil)
     (let ((fps (basecode::average-fps w)))
       (format t "~&~,,',,3:d objects, ~,,',,3:d primitives @ fps: ~s = ~sms~%"
               sg::*objects* sg::*no* (float fps 1.0)
               (when (> fps 0.001) (float (/ 1000.0 fps) 1.0)))
       (format t "~s draws~%" sg::*draws*)
       (format t "~,,,3:d objects/sec ~,,,3:d primitives/sec~%"
               (floor (* fps sg::*objects*)) (floor (* fps sg::*no*)))
       (loop for c0 = (aref (sg::current-times/cpu w) 0)
             for g0 = (aref (sg::current-times/gpu w) 0)
             for c across (sg::current-times/cpu w)
             for g across (sg::current-times/gpu w)
             for id across (sg::current-times/ids w)
             for m across (sg::current-times/masks w)
             when m do (format t "~s: @ ~6f / ~6f (~6f)~%"
                               id (* 1000 (- c c0))
                               (* 1000 (- g g0))
                               (* 1000 (- g c0))))))
    (:v
     (loop with sg = (sg w)
           with r = 300.0
           for i below 1000
           for pos = (sb-cga:vec (- (random r) (/ r 2))
                                 (- (random r) (/ r 2))
                                 (- (random r) (/ r 2)))
           for n = (sg::add-node sg 'sg::transform
                                 (format nil "~s.~s" (get-universal-time) i)
                                 :root :matrix (sb-cga:translate pos))
           do (sg::add-node* sg (sg::copy-node (sg::find-node sg :bunny))
                             n)))
    (:1
     (let* ((*default-pathname-defaults* #p"d:/tmp/t/erato/"))
       (add-object w :erato "erato-1.obj")
       (setf (sg::matrix (sg::find-node (sg w) :erato))
             (sb-cga:matrix* (sb-cga:scale* 5.0 5.0 5.0)
                             (sb-cga:translate* 30.0 0.0 -15.0)
                             (sb-cga:rotate* 0.0
                                             (float (/ pi -1.5) 1.0)
                                             0.0)
                             (sb-cga:rotate* (float (/ pi 2) 1.0)
                                             0.0
                                             0.0)
                             (sb-cga:translate* 11439.0 13000.0 12729.0)))))
    (:2
     (let* ((*default-pathname-defaults* #p"d:/tmp/t/lpshead/"))
       (add-object w :head "head.OBJ")
       (setf (sg::matrix (sg::find-node (sg w) :head))
             (sb-cga:matrix* (sb-cga:scale* 200.0 200.0 200.0)
                             (sb-cga:rotate* 0.0 (degrees -90) 0.0)
                             (sb-cga:translate* -0.5 0.250 0.50)))))
    (:3
     (let* ((*default-pathname-defaults* #p"d:/tmp/t/sportsCar/"))
       (add-object w :sportscar "sportsCar.obj")
       (setf (sg::matrix (sg::find-node (sg w) :sportscar))
             (sb-cga:matrix* (sb-cga:scale* 100.0 100.0 100.0)
                             (sb-cga:rotate* 0.0 (degrees -90) 0.0)
                             (sb-cga:translate* -0.5 0.250 0.50)))))
    (:9
     (time
      (let* ((*default-pathname-defaults* #p"d:/tmp/t/Bistro/Bistro/"))
        (add-object w :bistro "Bistro_Research_Interior.fbx")
        #++(setf (sg::matrix (sg::find-node (sg w) :bistro))
                 (sb-cga:matrix* (sb-cga:scale* 0.1 0.1 0.1))))))
    (:0
     (time
      (let* ((*default-pathname-defaults* #p"d:/tmp/t/Bistro/Bistro/"))
        (add-object w :bistrox "Bistro_Research_Exterior.fbx"
                    #++(setf (sg::matrix (sg::find-node (sg w) :bistrox))
                             (sb-cga:matrix* (sb-cga:scale* 0.1 0.1 0.1)))))))
    (:5
     (setf *vsync* (not *vsync*))
     (glop::%swap-interval (basecode::%glop-window w) 0)
     (glop:swap-interval (basecode::%glop-window w) (if *vsync* 2 0)))
    (:4 (setf (sg::matrix (sg::find-node (sg w) :bistro))
              (sb-cga:matrix* (sb-cga:scale* 0.1 0.1 0.1)
                              (sb-cga:rotate* (degrees -90) 0.0 0.0))))
    #++(:g
     (let ((n (add-ui-node w :graph 'sg::fps-graph-node :timing w)))
       (setf (sg::matrix n)
             (sb-cga:scale* 1.0 16.0 1.0))))
    (:h
     (let ((n (sg::find-node (sg w) :bistro)))
       (setf (sg::matrix n)
             (sb-cga:matrix*
              (sb-cga:scale*  100.0 100.0 100.0)
              (sb-cga:translate* 0.0 -0.5 0.0)))))))

(setf 3bgl-shaders::*print-shaders* t)
(setf 3bgl-shaders::*default-version* 460)
#++
(sg::dump-scenegraph (sg::root (sg *w*)) :id t)
; (basecode-run (make-instance 'vrtest :x 0))
; (basecode-run (setf *w* (make-instance 'vrtest :x 1924 :y 15)))
