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
  ((sg :accessor sg :initform nil))
  (:default-initargs :look-at-eye '(-97 14 -16)
                     :look-at-target '(30 28 5)
                     :projection-far 1000.0))

(defparameter *w* nil)

(defparameter *scene-graph* nil)

(declaim (inline v3))
(defun v3 (x y z)
  (sb-cga:vec (coerce x 'single-float)
              (coerce y 'single-float)
              (coerce z 'single-float)))

(defmethod run-main-loop :around ((w scene2test))
  (sg::with-resource-manager ()
    (let ((3bgl-shaders::*default-extensions* '(:arb-bindless-texture)))
      (call-next-method))))
(defparameter *once* t)

(defparameter *dt* (cons 0.0 0))

(defmethod basecode-draw ((w scene2test))
  (unwind-protect
       (progn
         (gl:enable :multisample)
         (let ((start (basecode::now)))
           (gl:clear-color (* 0.2 (abs (sin (/ (get-internal-real-time) 1000.0)))) 0.2 0.3 1)
           (gl:clear :color-buffer :depth-buffer)
           (setf *w* w)

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
                 (sg::set-global 's::mvp vp))

               (when (sg w)
                 (sg::draw-sg (sg w) (sb-cga:identity-matrix)))))
           (let ((dt (- (basecode::now) start)))
             (incf (car *dt*) dt)
             (incf (cdr *dt*)))))
    (setf *once* nil)))

(defun add-object (w name path &key (transform (sb-cga:identity-matrix)))
  (let* ((sg (sg w)))
    (unless sg
      (setf (sg w) (make-instance 'sg::scenegraph)
            sg (sg w))
      (sg::add-node sg 'sg::transform :root nil :matrix transform))
    (when (sg::find-node sg name)
      (sg::remove-node (sg::find-node sg name)))
    (sg::add-node sg 'sg::transform name :root)
    (sg::load-object :file path :sg sg :parent-node name)))

;; *w* scenegraph::*v*
;; (setf *state* nil)

(defun degrees (x) (float (* x (/ pi 180)) 1.0))

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
       (format t "~&~,,',,3:d objects, ~,,',,3:d tris @ fps: ~s = ~sms~%"
               sg::*objects* sg::*no* (float fps 1.0)
               (when (> fps 0.001) (float (/ fps) 1.0)))
       (format t "~,,,3:d objects/sec ~,,,3:d tris/sec~%"
               (floor (* fps sg::*objects*)) (floor (* fps sg::*no*)))))
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
        (setf (sg::matrix (sg::find-node (sg w) :bistro))
              (sb-cga:matrix* (sb-cga:scale* 0.1 0.1 0.1))))))
    (:0
     (time
      (let* ((*default-pathname-defaults* #p"d:/tmp/t/Bistro/Bistro/"))
        (add-object w :bistrox "Bistro_Research_Exterior.fbx"
        #++(setf (sg::matrix (sg::find-node (sg w) :bistrox))
                 (sb-cga:matrix* (sb-cga:scale* 0.1 0.1 0.1)))))))
    (:5
     (glop:swap-interval (basecode::%glop-window w) 0)
     (glop::%swap-interval (basecode::%glop-window w) 0))
    (:4 (setf (sg::matrix (sg::find-node (sg w) :bistro))
              (sb-cga:matrix* (sb-cga:scale* 0.1 0.1 0.1)
                              (sb-cga:rotate* 0.0 0.0 (degrees 90)))))))

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


