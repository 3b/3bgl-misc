#++ (asdf:load-systems '3bgl-misc '3bgl-ssbo 'lparallel)
(defpackage #:basecode-vr-lighting-test
  (:use :cl :basecode)
  (:local-nicknames (:s #:3bgl-ai-shaders)
                    (:b #:buffer-builder)
                    (:sg #:3bgl-sg2)))
(in-package #:basecode-vr-lighting-test)

(defparameter *envmap-dir* #p"/tmp/model/textures/hdr/")
(defparameter *model-dir* #p"/tmp/model/")


(defclass vrtest (basecode-vr::basecode-vr
                  basecode-vr::locomotion
                  sg::timing-helper
                  basecode-exit-on-esc
                  scenegraph::scenegraph-state-helper
                  basecode::fps)
  ((sg :accessor sg :initform nil)
   (channel :accessor channel :initform nil))
  (:default-initargs :far-clip 1000.0
                     :supersample-scale 1.3 :frame-size-autoscale (/ 0.5 1.3)
                     :frame-size-initial-scale (/ 1.3)
                     :max-timer-queries 64))

(defparameter *light-pos* (sb-cga:vec 0.0 2.0 0.0))
(defparameter *w* nil)

(defparameter *scene-graph* nil)

(declaim (inline v3))
(defun v3 (x y z)
  (sb-cga:vec (coerce x 'single-float)
              (coerce y 'single-float)
              (coerce z 'single-float)))

(defun debug-texture-overrides ()
  (let ((*default-pathname-defaults*
          (asdf:system-relative-pathname '3bgl-misc "data/")))
    `((:AI-TEXTURE-TYPE-NORMALS
       0 ,(namestring (merge-pathnames "debug-normal-texture.png")))
      (:AI-TEXTURE-TYPE-HEIGHT
       0 ,(namestring (merge-pathnames "debug-height-texture.png")))
      (:AI-TEXTURE-TYPE-SPECULAR
       0 ,(namestring (merge-pathnames "debug-metal-texture.png")))
      (:AI-TEXTURE-TYPE-EMISSIVE
       0 ,(namestring (merge-pathnames "debug-emissive-texture.png")))
      (:AI-TEXTURE-TYPE-DIFFUSE
       0 ,(namestring (merge-pathnames "debug-texture.png"))))))

(defun prefilter-specular (textures target format)
  (assert (eql target :texture-cube-map))
  (assert (= (length textures) 1))
  (let* ((src-tex (sg::texture (car textures)))
         ;; too lazy to fix calculations with all texture sizes for
         ;; now, just hard-coding 512...
         (w 512 #++(min 512
                        (gl:get-texture-level-parameter src-tex 0
                                                        :texture-width)))
         (tex (when src-tex
                (gl:create-texture target)))
         (program (sg::get-program
                   :compute '3bgl-sg2-shaders-common::pre-filter-specular))
         ;; only calculate down to 8x8 mip
         (max-level  (floor (log (floor w 8) 2)))
         ok)
    (time
     (unwind-protect
          (when (and src-tex tex)
            (%gl:texture-storage-2d tex
                                    (floor (log w 2))
                                    format
                                    w w)

            (%gl:bind-texture-unit 0 src-tex)
            (gl:generate-texture-mipmap src-tex)
            (gl:texture-parameter src-tex :texture-min-filter
                                  :linear-mipmap-linear)
            (gl:texture-parameter src-tex :texture-mag-filter :linear)
            (3bgl-shaders::with-program (program)
              (unless 3bgl-shaders::*bound-program*
                (break "no program?" program 3bgl-shaders::*bound-program*))
              (setf (3bgl-shaders::uniform program "pfsRoughness") 1.0)
              (loop
                for i from 0 upto max-level
                for lw = w then (/ lw 2)
                unless (and (>= lw 8)
                            (zerop (mod lw 8)))
                  do (break "calculated max level (~s) wrong? w=~s, lw=~s, i=~s"
                            max-level w lw i)
                do (setf (3bgl-shaders::uniform program "pfsRoughness")
                         (expt (float (/ i max-level) ) 1))
                   (%gl:bind-image-texture 1 tex i t 0 :write-only format)
                   (%gl:dispatch-compute (floor lw 8) (floor lw 8) 6)))
            (gl:texture-parameter tex :texture-max-level max-level)
            (setf ok t)
            (%gl:bind-image-texture 1 0 0 t 0 :write-only format))
       (when (and tex (not ok))
         (break "pfs not ok?")
         (gl:delete-texture (shiftf tex nil)))))
    tex))

(defun prefilter-specular-lut (textures target format)
  (assert (eql target :texture-2d))
  (assert (not textures))
  (unless (eq format :rg16)
    (format t "ignoring format ~s for pfs lut, using :rg16f instead.~%" format)
    (setf format :rg16))
  (let* ((w 256)
         (tex (gl:create-texture target))
         (program (sg::get-program
                   :compute '3bgl-sg2-shaders-common::calculate-pfs-lut))
         ok)
    (time
     (unwind-protect
          (3bgl-shaders::with-program (program)
            (unless 3bgl-shaders::*bound-program*
              (break "no program?" program 3bgl-shaders::*bound-program*))
            (%gl:texture-storage-2d tex 7 format w w)
            (%gl:bind-image-texture 1 tex 0 nil 0 :write-only format)
            (%gl:dispatch-compute (floor w 8) (floor w 8) 1)
            (loop for i below 7 for lw = w then (/ lw 2)
                  do (%gl:bind-image-texture 1 tex i nil 0 :write-only format)
                     (%gl:dispatch-compute (floor lw 8) (floor lw 8) 1))
            (gl:texture-parameter tex :texture-max-level 6)
            (gl:generate-texture-mipmap tex)
            (setf ok t))
       (%gl:bind-image-texture 1 0 0 t 0 :write-only format)
       (when (and tex (not ok))
         (break "pfs lut not ok?")
         (gl:delete-texture (shiftf tex nil)))))
    tex))

(defun ensure-sg (w)
  (let* ((sg (sg w)))
    (unless sg
      (setf (sg w) (make-instance 'sg::scenegraph)
            sg (sg w)))
    (unless (sg::find-node sg :root)
      (sg::add-node sg 'sg::transform :root nil))
    (unless (sg::find-node sg :floor)
      (let ((sg::*compress-loaded-textures* nil)
            (sg::*load-textures-as-srgb* t))
        (sg::load-object :default :floor
                         :parent-node (sg::add-node sg 'sg::transform
                                                    :floor :root)
                         :sg sg)
        (let ((sg::*ai-sampler-defaults*
                (list* :min-lod 0 :max-anisotropy 16
                       nil)))
          (sg::load-object :default :light
                           :parent-node (sg::add-node sg 'sg::transform
                                                      :light :root
                                                      :matrix (sb-cga:translate
                                                               *light-pos*))
                           :sg sg)
          (sg::load-object :default :box
                           :parent-node (sg::add-node sg 'sg::transform
                                                      :box0 :root
                                                      :matrix (sb-cga:translate*
                                                               0.0 0.75 0.0))
                           :sg sg)
          (sg::load-object :default :box
                           :parent-node (sg::add-node sg 'sg::transform
                                                      :box1 :root
                                                      :matrix (sb-cga:translate*
                                                               -2.0 0.5 -3.0))
                           :sg sg)

          (sg::load-object :default :box
                           :parent-node (sg::add-node sg 'sg::transform
                                                      :box2 :root
                                                      :matrix (sb-cga:translate*
                                                               12.0 0.5 5.0))
                           :sg sg)

          #++
          (let* ((b (sg::find-node sg :box2))
                 (n1 (sg::add-node sg 'sg::transform
                                   :mori b)))

            (let ((sg::*mat-overrides* (make-hash-table :test 'equal)))
              (setf (gethash "$tex.file" sg::*mat-overrides*)
                    (debug-texture-overrides))
              (sg::load-object :file
                               (merge-pathnames "mori_knob/testObj.obj"
                                                *model-dir*)
                               :parent-node n1 :sg sg))
            (sg::remove-node (sg::find-node sg "2.BackGroundMat"))
            (sg::remove-node (sg::find-node sg "4.LTELogo"))
            (sg::remove-node (sg::find-node sg "5.Material"))
            (loop for ij below 16
                  for i = (floor ij 4)
                  for j = (mod ij 4)
                  for n = (if (zerop ij)
                              n1
                              (sg::add-node* sg (sg::copy-node n1) b))
                  do (setf (sg::matrix n)
                           (sb-cga:matrix*
                            (sb-cga:translate*
                             (- (/ i 3.0) 0.5)
                             0.5
                             (- (/ j 3.0) 0.5))
                            (sb-cga:scale* 0.1 0.1 0.1)
                            (sb-cga:translate* 0.0 0.5 0.0)))))

          (let* ((b (sg::find-node sg :root))
                 (n1 (sg::add-node sg 'sg::transform
                                   :mori b)))

            (let ((sg::*mat-overrides* (make-hash-table :test 'equal)))
              (setf (gethash "$tex.file" sg::*mat-overrides*)
                    (debug-texture-overrides))
              (sg::load-object :file
                               (merge-pathnames "mori_knob/testObj.obj"
                                                *model-dir*)
                               :parent-node n1 :sg sg))
            (sg::remove-node (sg::find-node sg "2.BackGroundMat"))
            (sg::remove-node (sg::find-node sg "4.LTELogo"))
            (sg::remove-node (sg::find-node sg "5.Material"))
            (loop for ij below 16
                  for i = (floor ij 4)
                  for j = (mod ij 4)
                  for n = (if (zerop ij)
                              n1
                              (sg::add-node* sg (sg::copy-node n1) b))
                  do (setf (sg::matrix n)
                           (sb-cga:matrix*
                            (sb-cga:translate*
                             (- (* i 16.0) 24)
                             0.5
                             (- (* j 16.0) 24)))))))
        (sg::load-object :default :skybox
                         :parent-node :root
                         :sg sg)))
    (unless (sg::find-node sg :ui-root)
      (sg::add-node sg 'sg::transform
                    :ui-root :root
                    :matrix (sb-cga:matrix*
                             (sb-cga:scale* (/ 2 1920.0) (/ 2 1080.0) 1.0)
                             (sb-cga:translate* (/ 1920 -2.0) (/ 1080 -2.0)
                                                0.0))))
    sg))


(defmethod run-main-loop :around ((w vrtest))
  ;; not sure if we should use global lparallel:*kernel* or bind one here?
  (let ((lparallel:*kernel* (lparallel:make-kernel 6)))
    (setf (channel w)
          (lparallel:make-channel))
    (sg::with-resource-manager (:timing w)
      (let ((3bgl-shaders::*default-extensions* '(:arb-bindless-texture)))
        (call-next-method)))))

(defmethod run-main-loop :before ((w vrtest))
  (ensure-sg w))

(defparameter *once* t)

(defparameter *dt* (cons 0.0 0))
(defparameter *gc* nil)
(defparameter *gc-time* 0)

(defmethod handle-work* ((w vrtest) tag &rest r &key &allow-other-keys)
  (let ((*print-length* 32))
    (format t "got unknown work ~s ~s?~%" tag r)))

(defmethod handle-work* ((w vrtest) (tag (eql :print)) &key string)
  (format t "~&:print job returned ~s~%" string))


#++
(defmethod handle-work* ((w vrtest) (tag (eql :object-loaded))
                         &key loaded-object)
  ())

(defmethod handle-work ((w vrtest) work)
  (apply #'handle-work* w work))

(defmethod basecode-vr::render-frame :around ((w vrtest))
  (multiple-value-bind (r got-result)
      (lparallel:try-receive-result (channel w))
    (when got-result
      (handle-work w r)))
  (let ((*once* (shiftf *once* nil)))
    (call-next-method)
    (sg::mark w :id :end)
    (sg::update-times w)
    (sg::mark w :id :start)))

(defvar *fff*)
#++(sb-cga:transform-point (sb-cga:vec 0.0 0.0 0.0)
                        (sb-cga:inverse-matrix *fff*))


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
           (let ((c (sb-cga:vec (if (eql eye :left) 0.1 0.0)
                                (float (* 0.08 (abs (sin (/ (basecode::now)))))
                                       1.0)
                                (if (eql eye :left) 0.0 0.1))))
             (setf c (sb-cga:transform-direction
                      c (basecode-vr::eye-modelview w eye)))
             (gl:clear-color (aref c 0) (aref c 1) (aref c 2) 1.0))
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
               (setf *fff* mv)
               (sg::set-global 's::v mv)
               (sg::set-global 's::p p)
               (sg::set-global 's::vp vp)
               (sg::set-global 's::mvp vp)
               (sg::set-global 's::eye-pos (sb-cga:transform-point
                                            (sb-cga:vec 0.0 0.0 0.0)
                                            (sb-cga:inverse-matrix mv)))
               (sg::set-global '3bgl-sg2-shaders-common:ui-matrix
                               sb-cga:+identity-matrix+)

               (sg::mark w :id :globals)

               (when (sg w)
                 (gl:depth-func :never)
                 (sg::draw-sg (sg w) (sb-cga:identity-matrix)
                              :depth-pass t))))
           (let ((dt (- (basecode::now) start)))
             (incf (car *dt*) dt)
             (incf (cdr *dt*)))))))


;; *w* scenegraph::*v*
;; (setf *state* nil)

(defun degrees (x) (float (* x (/ pi 180)) 1.0))

(defparameter *vsync* nil)

(defun load-env (specular diffuse)
  (let* ((*default-pathname-defaults* *envmap-dir*)
         (st (sg::get-derived-texture
              'prefilter-specular
              (list (merge-pathnames specular)
                    :type :file :target :texture-cube-map)
              :target :texture-cube-map :format :rgba16f)))
    (sg::load-env-map :file st :cube :specular)
    (sg::set-global '3bgl-sg2-shaders-common:prefiltered-specular-max-lod
                    (gl:get-texture-parameter (sg::texture st)
                                              :texture-max-level))
    (sg::load-env-map  :file (merge-pathnames diffuse) :cube :diffuse)))

(defmethod key-down :after ((w vrtest) k)
  (case k
    (:r
     (setf (sg w) nil)
     (sg::reset-resource-manager sg::*resource-manager*)
     (load-env "hdrlabs.com/Newport_Loft/Newport_Loft/Newport_Loft_Ref.hdr"
               "hdrlabs.com/Newport_Loft/Newport_Loft/Newport_Loft_Env.hdr")
     (ensure-sg w)
     (sg::set-global '3bgl-sg2-shaders-common:prefiltered-specular-lut
                     (sg::handle
                      (sg::get-handle
                       (sg::get-derived-texture 'prefilter-specular-lut
                                                nil :format :rg16)
                       (sg::get-sampler 'pfs-lut ;:max-anisotropy 16.0
                                        :min-filter :linear
                                        :wrap-s :clamp-to-edge
                                        :wrap-t :clamp-to-edge)
                       :resident t))))
    (:p
     (load-env "hdrlabs.com/Newport_Loft/Newport_Loft/Newport_Loft_Ref.hdr"
               "hdrlabs.com/Newport_Loft/Newport_Loft/Newport_Loft_Env.hdr"))
    (:o
     (load-env "pauldebevec.com/high-res/grace-new.hdr"
               "pauldebevec.com/high-res/grace-new_diffuse.hdr"))
    (:l
     (load-env "pauldebevec.com/high-res/glacier.hdr"
               "pauldebevec.com/high-res/glacier_diffuse.hdr"))
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
    #++(:p
        (lparallel:submit-task (channel w) (lambda () (list :print :string "foo!"))))
    (:f8
     (setf *vsync* (not *vsync*))
     (glop::%swap-interval (basecode::%glop-window w) 0)
     (glop:swap-interval (basecode::%glop-window w) (if *vsync* 2 0)))))



(defmethod basecode-vr::process-vr-event :after((w vrtest) event)
  #++ (format t "~&vr input event: ~s~%" event)
  (let* ((data (getf event :data))
         (button (basecode-vr::get-button data)))
    (case (getf event :event-type)
      (:button-unpress
       (when (eql (gethash :right-hand (basecode-vr::controller-role-ids w))
                  (getf event :tracked-device-index))
         (case button
           (:grip
            (load-env "hdrlabs.com/Newport_Loft/Newport_Loft/Newport_Loft_Ref.hdr"
                      "hdrlabs.com/Newport_Loft/Newport_Loft/Newport_Loft_Env.hdr"))
           (:steam-vr-trigger
            (load-env "pauldebevec.com/high-res/grace-new.hdr"
                      "pauldebevec.com/high-res/grace-new_diffuse.hdr"))
           (:steam-vr-touchpad
            (load-env "pauldebevec.com/high-res/glacier.hdr"
                      "pauldebevec.com/high-res/glacier_diffuse.hdr"))
           (:application-menu
            (setf basecode-vr::*debug-autoscale*
                  (not basecode-vr::*debug-autoscale*)))))
       (when (eql (gethash :right-hand (basecode-vr::controller-role-ids w))
                  (getf event :tracked-device-index))
         (case button
           (:grip
            (load-env "hdrlabs.com/Newport_Loft/Newport_Loft/Newport_Loft_Ref.hdr"
                      "hdrlabs.com/Newport_Loft/Newport_Loft/Newport_Loft_Env.hdr"))
           (:steam-vr-trigger
            (load-env "pauldebevec.com/high-res/grace-new.hdr"
                      "pauldebevec.com/high-res/grace-new_diffuse.hdr"))
           (:steam-vr-touchpad
            (load-env "pauldebevec.com/high-res/glacier.hdr"
                      "pauldebevec.com/high-res/glacier_diffuse.hdr"))))))))


(setf 3bgl-shaders::*print-shaders* t)
(setf 3bgl-shaders::*default-version* 460)
#++
(sg::dump-scenegraph (sg::root (sg *w*)) :id t)
;(basecode-run (make-instance 'vrtest :x 0 :y 0 :width 1920 :height 1100))
#++
(basecode-run (setf *w* (make-instance 'vrtest :x 1924 :y 15
                                               :width 1000 :height 555)))
