#++ (asdf:load-systems '3bgl-misc)
(defpackage #:scene2test
  (:use :cl :basecode)
  (:local-nicknames (:s #:3bgl-ai-shaders)
                    (:b #:buffer-builder)
                    (:sg #:3bgl-sg2)))
(in-package #:scene2test)

(defclass scene2test (basecode-glop
                      sg::timing-helper
                      freelook-camera
                      basecode-exit-on-esc
                      basecode-shader-helper::basecode-shader-helper
                      scenegraph::scenegraph-state-helper
                      perspective-projection
                      basecode::fps)
  ((sg :accessor sg :initform nil))
  (:default-initargs :look-at-eye '(-97 14 -16)
                     :look-at-target '(30 28 5)
                     :projection-far 1000.0
                     :swap-buffers nil
                     :max-timer-queries 32))

(defparameter *w* nil)

(defparameter *scene-graph* nil)

(declaim (inline v3))
(defun v3 (x y z)
  (sb-cga:vec (coerce x 'single-float)
              (coerce y 'single-float)
              (coerce z 'single-float)))

(defmethod run-main-loop :around ((w scene2test))
  (sg::with-resource-manager (:timing w)
    (let ((3bgl-shaders::*default-extensions* '(:arb-bindless-texture)))
      (call-next-method))))
(defparameter *once* t)

(defparameter *dt* (cons 0.0 0))
(defparameter *gc* nil)
(defparameter *gc-time* 0)

(defun update-graphs (w)
  (let* ((sg (sg w))
         (r (sg::find-node sg :ui-root)))
    (when r
      (loop for c in (sg::children r)
            when (typep c 'sg::graph-line-node)
              do (let* ((i (sg::next c))
                        (v (aref (sg::buffer c) i)))
                   (when (or (< v -100) (> v 1200))
                     (setf v 540.0))
                   (sg::add-graph-sample c (+ v (random 4.0) -2))
)))))

(defmethod basecode::basecode-draw :around ((w scene2test))
  (call-next-method)
  (sg::mark w :id :swap)
  (glop:swap-buffers (basecode::%glop-window w))
  (sg::mark w :id :end)
  (sg::update-times w)
  (sg::mark w :id :start)
  )

(defmethod basecode-draw ((w scene2test))
  (unless (eq *gc* sb-kernel::*gc-epoch*)
    (format t "~&gc! ~s after ~s:~6,3,,,'0f~%" sb-ext:*gc-run-time*
            (floor (- (get-internal-real-time) *gc-time*)
                   (* 60 internal-time-units-per-second))
            (mod (/ (- (get-internal-real-time) *gc-time*)
                      internal-time-units-per-second)
                   60.0)
            )
    (format t "~s ~s~%" *gc-time* (get-internal-real-time))
    (setf *gc-time* (get-internal-real-time))
    (setf *gc* sb-kernel::*gc-epoch*))
  (unwind-protect
       (progn
         #++(when (sg w)
           (update-graphs w))
         (gl:enable :multisample)
         (let ((start (basecode::now)))
           (gl:clear-color (* 0.2 (abs (sin (/ (get-internal-real-time) 1000.0)))) 0.2 0.3 1)
           (gl:clear :color-buffer :depth-buffer)
           (setf *w* w)
           (sg::mark w :id :clear)
           (progn ;;time
             (progn
               (sg::set-global 's::v
                               (basecode::freelook-camera-modelview w))
               (sg::set-global 's::p
                               (basecode::projection-matrix w))
               (let ((vp (sb-cga:matrix*
                          (basecode::projection-matrix w)
                          (basecode::freelook-camera-modelview w))))
                 (sg::set-global 's::vp vp)
                 (sg::set-global 's::mvp vp)
                 (sg::set-global '3bgl-sg2-shaders-common:ui-matrix
                                 (sb-cga:scale* 1.0 1.0 1.0)
                                 ;sb-cga:+identity-matrix+; vp
                                 ))

               (sg::mark w :id :globals)
               (when (sg w)
                 (sg::draw-sg (sg w) (sb-cga:identity-matrix)))))
           (let ((dt (- (basecode::now) start)))
             (incf (car *dt*) dt)
             (incf (cdr *dt*)))))
    (setf *once* nil)))

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
    (sg::load-object :file path :sg sg :parent-node name)))

(defun add-ui-node (w name class &rest initargs)
  (let ((sg (ensure-sg w)))
    (when (sg::find-node sg name)
      (sg::remove-node (sg::find-node sg name)))
    (apply #'sg::add-node sg class name :ui-root initargs)))

;; *w* scenegraph::*v*
;; (setf *state* nil)

(defun degrees (x) (float (* x (/ pi 180)) 1.0))

(Defparameter *vsync* nil)
(defmethod key-down :after ((w scene2test) k)
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
                               (* 1000 (- g c0)))
             )
))
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
                             n)
           ))
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
     (glop:swap-interval (basecode::%glop-window w) (if *vsync* 2 0))
     )
    (:4 (setf (sg::matrix (sg::find-node (sg w) :bistro))
              (sb-cga:matrix* (sb-cga:scale* 0.1 0.1 0.1)
                              (sb-cga:rotate* (degrees -90) 0.0 0.0))))
    #++(:g
     (let ((n (add-ui-node w :graph-line 'sg::graph-line-node)))
         (loop with b = (sg::buffer n)
               for i below (length b)
               do (setf (aref b i) (abs (* 50 (sin (/ i 60.0)))))))
     (let ((n (add-ui-node w :graph-line2 'sg::graph-line-node
                           :color '(1 0 1 1)
                           :matrix (sb-cga:translate* 0.0 0.0 0.0))))
       (loop with b = (sg::buffer n)
             for i below (length b)
             do (setf (aref b i) (abs (* 70 (sin (/ i 80.0)))))))
     (loop
       for i below 100
       for n = (intern (format nil "G~a" i) :keyword)
       do (let ((n (add-ui-node w n 'sg::graph-line-node
                                :color (list (random 1.0)
                                             (random 1.0)
                                             (random 1.0)
                                             1)
                                :matrix (sb-cga:translate* 0.0 0.0 0.0))))
            (loop with b = (sg::buffer n)
                  with r = 10.0
                  for y = (random 1000.0) then (+ y (- (random r) (/ r 2)))
                  for i below (length b)
                  do (setf (aref b i) y)))))
    (:g
     (let ((n (add-ui-node w :graph 'sg::fps-graph-node :timing w)))
       (setf (sg::matrix n)
              (sb-cga:scale* 1.0 16.0 1.0))))
    (:h
     (let ((n (sg::find-node (sg w) :graph)))
      (setf (sg::matrix n)
            (sb-cga:matrix*
             (sb-cga:scale* 1.0 16.0 1.0)
             (sb-cga:translate* 0.0 2.0 0.0)
             )))
     #++(let ((n (sg::find-node (sg w) :ui-root)))
       (setf (sg::matrix n)
             (sb-cga:matrix*
              ;(sb-cga:rotate-around (sb-cga:vec 1.0 0.0 1.0) 0.45)
              (sb-cga:scale* (/ 2 1920.0) (/ 2 1080.0) 1.0)
              (sb-cga:translate* (/ 1920 -2.0) (/ 1080 -2.0)
                                    0.0)))))
))

#++
(loop for (name . node) in (alexandria:hash-table-alist (sg::index (sg *w*)))
      when (and (typep node 'sg::transform)
                (not (sb-cga:matrix~ (sg::matrix node)
                                     sb-cga:+identity-matrix+)
                     ))
        do (setf  (sg::matrix node) (sb-cga:transpose-matrix (sg::matrix node)))
        and collect (list name node))
#++
(let ((root (sg::find-node (sg *w*) "2.Bistro_Interior")))
  (labels ((r (n)
             (format t "transpose ~s~%" (sg::base-name n))
             (setf (sg::matrix n) (sb-cga:transpose-matrix (sg::matrix n)))
             (mapcar #'r (sg::children n))
             ))
    (r root)))

(setf 3bgl-shaders::*print-shaders* t)
(setf 3bgl-shaders::*default-version* 460)
#++
(sg::dump-scenegraph (sg::root (sg *w*)) :id t)
; (basecode-run (make-instance 'scene2test :x 0))
; (basecode-run (make-instance 'scene2test :x 1924 :y 15))


