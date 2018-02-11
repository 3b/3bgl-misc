;(ql:quickload '(3bgl-misc 3b-openvr))
(in-package #:basecode-vr)

(defclass basecode-vr (basecode::basecode-glop
                       render-model-manager
                       basecode-shader-helper::basecode-shader-helper)

  ((vsync :initform nil :reader vsync)

   (tracked-device-pose :reader tracked-device-pose
                        :initform (make-array vr::+max-tracked-device-count+
                                              :initial-element nil))
   (device-pose :reader device-pose
                :initform (make-array vr::+max-tracked-device-count+
                                      :initial-element nil))
   (show-tracked-device :reader show-tracked-device
                        :initform (make-array vr::+max-tracked-device-count+
                                              :initial-element nil))
   ;; what classes we saw poses for this frame
   (pose-classes :accessor pose-classes :initform "")
   ;; for each device, a character representing its class
   (dev-class-char :accessor dev-class-char
                   :initform (make-array vr::+max-tracked-device-count+
                                         :initial-element nil))
   (valid-pose-count :accessor valid-pose-count :initform 0)
   (valid-pose-count-last :accessor valid-pose-count-last)

   (programs :initform (make-hash-table) :reader programs)

   (near-clip :reader near-clip :initarg :near-clip :initform 0.1)
   (far-clip :reader far-clip :initarg :far-clip :initform 100.0)

   (hmd-pose :accessor hmd-pose :initform (sb-cga:identity-matrix))
   (eye-poses :initform (make-hash-table) :reader eye-poses)
   (projections :initform (make-hash-table) :reader projections)

   (render-width :accessor render-width :initform 0)
   (render-height :accessor render-height :initform 0)
   (left-eye-desc :accessor left-eye-desc :initform nil)
   (right-eye-desc :accessor right-eye-desc :initform nil)

   (controller-role-ids :accessor controller-role-ids
                        :initform (make-hash-table))

   (mirror-window-geometry :accessor mirror-window-geometry :initform nil)))

(defun update-controller-roles (w)
  (format t "left hand id = ~s~%"
          (setf (gethash :left-hand (controller-role-ids w))
                (3b-openvr::get-tracked-device-index-for-controller-role
                 :left-hand)))
  (format t "right hand id = ~s~%"
          (setf (gethash :right-hand (controller-role-ids w))
                (3b-openvr::get-tracked-device-index-for-controller-role
                 :right-hand))))

(defmethod process-vr-event ((w basecode-vr) event)
  (let ((index (getf event :tracked-device-index)))
    (case (getf event :event-type)
      (:tracked-device-activated
       (setup-render-model-for-tracked-device w index)
       (format t "~&Device ~d attached. Setting up render model~%" index)
       (update-controller-roles w))
      (:tracked-device-deactivated
       (format t "~&Device ~d detached.~%" index)
       (update-controller-roles w))
      (:tracked-device-updated
       (format t "~&Device ~d updated.~%" index))
      (:tracked-device-role-changed
       (format t "~&Device ~d role changed?.~%"
               (getf event :tracked-device-index))
       (update-controller-roles w)))))

(defmethod handle-vr-input ((w basecode-vr))
  ;; process SteamVR events
  (loop for ev = (vr::poll-next-event)
        while ev
        do (process-vr-event w ev))

  ;; process SteamVR controller state
  #++
  (loop for device below vr::+max-tracked-device-count+
        for state = (vr::get-controller-state device)
        when state
          do (setf (aref (show-tracked-device w) device)
                   (zerop (getf state 'vr::button-pressed)))))


;;; Create/destroy GL a Render Model for a single tracked device
(defmethod setup-render-model-for-tracked-device :after ((w basecode-vr)
                                                         tracked-device-index)
  (when (tracked-device-to-render-model w tracked-device-index)
    (setf (aref (show-tracked-device w) tracked-device-index) t)))


(defmethod basecode-draw ((w basecode-vr))
  (gl:disable :depth-test)
  (gl:viewport 0 0 (basecode::width w) (basecode::height w))
  (gl:bind-framebuffer :framebuffer 0)
  (gl:clear-color 0.2 (random 0.03) 0 1)
  (gl:clear :color-buffer)
  (let ((p (gethash 'mirror (programs w))))
    (when (and p (mirror-window-geometry w))
      (destructuring-bind (vao index-size &rest r) (mirror-window-geometry w)
        (declare (ignore r))
        (gl:bind-vertex-array vao)
        (setf (3bgl-shaders::uniform p 'basecode-vr-shaders::diffuse)
              0)
        (3bgl-shaders::use-program p)
        (loop with c = (/ index-size 2)
              for desc in (list (left-eye-desc w) (right-eye-desc w))
              for start in (list 0 (* 2 c))
              do (gl:bind-texture :texture-2d (getf desc 'resolve-texture-id))
                 (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
                 (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
                 (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
                 (gl:tex-parameter :texture-2d :texture-min-filter :linear)
                 (%gl:draw-elements :triangles c :unsigned-short start)))))

  (gl:bind-vertex-array 0)
  (gl:use-program 0))

(defmethod eye-modelview ((w basecode-vr) eye)
  (sb-cga:matrix* (gethash eye (eye-poses w) sb-cga:+identity-matrix+)
                  (hmd-pose w)))

(defmethod eye-modelview-local ((w basecode-vr) eye)
  (sb-cga:matrix* (gethash eye (eye-poses w) sb-cga:+identity-matrix+)
                  (hmd-pose w)))

(defmethod eye-projection ((w basecode-vr) eye)
  (gethash eye (projections w) sb-cga:+identity-matrix+))

(defparameter *eye* nil)

(defun draw-rendermodels (w &optional (eye :left))
  (let ((is-input-focus-captured-by-another-process
          (vr::is-input-focus-captured-by-another-process))
        (p (gethash 'render-model (programs w))))
    (when p
      (setf (3bgl-shaders::uniform p 'basecode-vr-shaders::diffuse) 0)
      (3bgl-shaders::use-program p)
      (loop with pose = nil
            for i below vr::+max-tracked-device-count+
            when (and (tracked-device-to-render-model w i)
                      (aref (show-tracked-device w) i)
                      (setf pose (aref (tracked-device-pose w) i))
                      (getf pose 'vr::pose-is-valid)
                      (not (and is-input-focus-captured-by-another-process
                                (eql (vr::get-tracked-device-class i)
                                     :controller)))
                      (not (eql (vr::get-tracked-device-class i)
                                :hmd)))
              do (let* ((device-to-tracking (aref (device-pose w) i))
                        (mvp (sb-cga:matrix*
                              (eye-projection w eye)
                              (eye-modelview-local w eye)
                              device-to-tracking)))
                   (setf (3bgl-shaders::uniform p 'basecode-vr-shaders::matrix)
                         mvp)
                   (3bgl-shaders::use-program p)
                   (draw (tracked-device-to-render-model w i)))))))

(defmethod render-scene :around ((w basecode-vr) eye)
  (let ((*eye* eye))
    (call-next-method)
    ;; fixme: make optional
    (draw-rendermodels w eye)))

(defmethod render-scene ((w basecode-vr) eye)
  (break "?"))

(defmethod render-frame ((w basecode-vr))

  ;; draw each eye
  (with-accessors ((left left-eye-desc) (right right-eye-desc)
                   (width render-width) (height render-height)) w

    (loop
      for desc in (list left right)
      for eye in '(:left :right)
      do

         (gl:enable :multisample)

         (gl:bind-framebuffer :framebuffer (getf desc 'render-framebuffer-id))
         (gl:viewport 0 0 width height)

         (render-scene w eye)

         (gl:bind-framebuffer :framebuffer 0)

         (gl:disable :multisample)

         (gl:bind-framebuffer :read-framebuffer
                              (getf desc 'render-framebuffer-id))
         (gl:bind-framebuffer :draw-framebuffer
                              (getf desc 'resolve-framebuffer-id))

         (%gl:blit-framebuffer 0 0 width height 0 0 width height
                               :color-buffer
                               :linear)

         (gl:bind-framebuffer :read-framebuffer 0)
         (gl:bind-framebuffer :draw-framebuffer 0)))

  ;; draw mirror window
  (basecode-draw w) ;; possibly should use different function and skip this completely?

  ;; submit to API

  (vr::submit :left (list 'vr::handle (getf (left-eye-desc w)
                                            'resolve-texture-id)
                          'vr::type :open-gl
                          'vr::color-space :gamma))
  (vr::submit :right (list 'vr::handle (getf (right-eye-desc w)
                                             'resolve-texture-id)
                           'vr::type :open-gl
                           'vr::color-space :gamma))

  ;; swap, glfinish,etc
  (glop:swap-buffers (basecode::%glop-window w))
  ;;
  (update-hmd-matrix-pose w))

(defmethod update-hmd-matrix-pose ((w basecode-vr))
  (unless vr::*system*
    (return-from update-hmd-matrix-pose))

  (vr::wait-get-poses (tracked-device-pose w) nil)
  (setf (pose-classes w) "")
  (setf (valid-pose-count w) 0)
  (loop for device below vr::+max-tracked-device-count+
        for tracked-device = (aref (tracked-device-pose w) device)
        when (getf tracked-device 'vr::pose-is-valid)
          do (incf (valid-pose-count w))
             (setf (aref (device-pose w) device)
                   (getf tracked-device 'vr::device-to-absolute-tracking))
             (setf (aref (dev-class-char w) device)
                   (case (vr::get-tracked-device-class device)
                     (:controller #\C)
                     (:hmd #\H)
                     (:invalid #\I)
                     (:generic-tracker #\G)
                     (:tracking-reference #\T)
                     (t #\?)))
             (setf (pose-classes w) (format nil "~a~a" (pose-classes w)
                                            (aref (dev-class-char w) device))))

  (when (getf (aref (tracked-device-pose w) vr::+tracked-device-index-hmd+)
              'vr::pose-is-valid)
    (setf (hmd-pose w)
          (sb-cga:inverse-matrix
           (aref (device-pose w) vr::+tracked-device-index-hmd+)))))




(defmethod get-hmd-matrix-projection-eye ((o basecode-vr) eye)
  (unless vr::*system*
    (return-from get-hmd-matrix-projection-eye (sb-cga:identity-matrix)))

  (vr::get-projection-matrix eye
                             (float (near-clip o) 1.0)
                             (float (far-clip o) 1.0)))

(defmethod get-hmd-matrix-pose-eye ((o basecode-vr) eye)
  (unless vr::*system*
    (return-from get-hmd-matrix-pose-eye (sb-cga:identity-matrix)))

  (sb-cga:inverse-matrix
   (vr::get-eye-to-head-transform eye)))

(defmethod create-frame-buffer (width height)
  (let ((render-framebuffer-id (gl:gen-framebuffer))
        (depth-buffer-id (gl:gen-renderbuffer))
        (render-texture-id (gl:gen-texture))
        (resolve-framebuffer-id (gl:gen-framebuffer))
        (resolve-texture-id (gl:gen-texture)))
    (gl:bind-framebuffer :framebuffer render-framebuffer-id)

    (gl:bind-renderbuffer :renderbuffer depth-buffer-id)
    (gl:renderbuffer-storage-multisample :renderbuffer 4 :depth-component
                                         width height)
    (gl:framebuffer-renderbuffer :framebuffer :depth-attachment
                                 :renderbuffer depth-buffer-id)

    (gl:bind-texture :texture-2d-multisample render-texture-id)
    (%gl:tex-image-2d-multisample :texture-2d-multisample 4 :rgba8
                                  width height t)
    (gl:framebuffer-texture-2d :framebuffer :color-attachment0
                               :texture-2d-multisample render-texture-id 0)

    (let ((cfs (gl:check-framebuffer-status :framebuffer)))
      (unless (gl::enum= cfs :framebuffer-complete)
        (error "render framebuffer incomplete ~s?" cfs)))

    (gl:bind-framebuffer :framebuffer resolve-framebuffer-id)

    (gl:bind-texture :texture-2d resolve-texture-id)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-max-level 0)
    (gl:tex-image-2d :texture-2d 0 :rgba8 width height 0 :rgba
                     :unsigned-byte nil)
    (gl:framebuffer-texture-2d :framebuffer :color-attachment0
                               :texture-2d resolve-texture-id 0)
    (gl:disable :depth-test)
    (let ((cfs (gl:check-framebuffer-status :framebuffer)))
      (unless (gl::enum= cfs :framebuffer-complete)
        (error "resolve framebuffer incomplete ~s?" cfs)))


    (gl:bind-framebuffer :framebuffer 0)
    (list 'depth-buffer-id depth-buffer-id
          'render-texture-id render-texture-id
          'render-framebuffer-id render-framebuffer-id
          'resolve-texture-id resolve-texture-id
          'resolve-framebuffer-id resolve-framebuffer-id)))

(defun setup-mirror-window ()
  (unless vr::*system*
    (return-from setup-mirror-window nil))

  (static-vectors:with-static-vector (verts (* 4 8) :element-type 'single-float)
    (flet ((v (i &rest xyuv)
             (loop for j from (* i 4) repeat 4
                   for c in xyuv
                   do (setf (aref verts j) (float c 1.0)))))
      ;; left eye verts
      (v 0 -1 -1 0 0)
      (v 1 0 -1 1 0)
      (v 2 -1 1 0 1)
      (v 3 0 1 1 1)

      ;; right eye verts
      (v 4 0 -1 0 0)
      (v 5 1 -1 1 0)
      (v 6 0 1 0 1)
      (v 7 1 1 1 1)

      (static-vectors:with-static-vector (indices 12 :element-type
                                                  '(unsigned-byte 16))
        (replace indices '(0 1 3 0 3 2 4 5 7 4 7 6))
        (let ((index-size (length indices))
              (vao (gl:gen-vertex-array))
              (vbo (gl:gen-buffer))
              (ibo (gl:gen-buffer)))
          (gl:bind-vertex-array vao)

          (gl:bind-buffer :array-buffer vbo)
          (%gl:buffer-data :array-buffer (* 4 6 (length verts))
                           (static-vectors:static-vector-pointer verts)
                           :static-draw)

          (gl:bind-buffer :element-array-buffer ibo)
          (%gl:buffer-data :element-array-buffer (* 2 (length indices))
                           (static-vectors:static-vector-pointer indices)
                           :static-draw)
          (gl:enable-vertex-attrib-array 0)
          (gl:vertex-attrib-pointer 0 2 :float nil (* 4 4) 0)

          (gl:enable-vertex-attrib-array 1)
          (gl:vertex-attrib-pointer 1 2 :float nil (* 4 4) (* 2 4))

          (gl:bind-vertex-array 0)
          (gl:disable-vertex-attrib-array 0)
          (gl:disable-vertex-attrib-array 1)

          (list vao index-size vbo ibo))))))

;; fixme: move this to somewhere common (possibly cl-opengl?)
(cffi:defcallback debug-callback :void ((source %gl:enum)
                                        (type %gl:enum)
                                        (severity %gl:enum)
                                        (length %gl:sizei)
                                        (message :string)
                                        (user-param (:pointer :void)))
  (declare (ignorable source type severity length user-param))
  (format t "~&GL Error: ~a~%" message))

(defmethod run-main-loop :before ((w basecode-vr))
  (%gl:debug-message-callback (cffi:get-callback 'debug-callback)
                              (cffi:null-pointer))
  (%gl:debug-message-control :dont-care :dont-care :dont-care
                             0 (cffi:null-pointer) t)
  (gl:enable :debug-output-synchronous)


  ;; set up shaders used by basecode-vr itself

  (setf (gethash 'mirror (programs w))
        (3bgl-shaders::shader-program
         :vertex 'basecode-vr-shaders::mirror-window-vertex
         :fragment 'basecode-vr-shaders::mirror-window-fragment))

  (setf (gethash 'render-model (programs w))
        (3bgl-shaders::shader-program
         :vertex 'basecode-vr-shaders::render-model-vertex
         :fragment 'basecode-vr-shaders::render-model-fragment))

  (setf (gethash 'controller (programs w))
        (3bgl-shaders::shader-program
         :vertex 'basecode-vr-shaders::controller-transform-vertex
         :fragment 'basecode-vr-shaders::controller-transform-fragment))

  ;; set up eye/camera matrices
  (loop for eye in '(:left :right)
        do (setf (gethash eye (eye-poses w))
                 (get-hmd-matrix-pose-eye w eye))
           (setf (gethash eye (projections w))
                 (get-hmd-matrix-projection-eye w eye)))

  ;; set up render targets
  (when vr::*system*
    (destructuring-bind (width height) (vr::get-recommended-render-target-size)
      (setf (render-width w) width)
      (setf (render-height w) height)
      (setf (left-eye-desc w) (create-frame-buffer width height))
      (setf (right-eye-desc w) (create-frame-buffer width height))))

  (setf (mirror-window-geometry w) (setup-mirror-window))
  (setup-render-models w)

  (vr::vr-compositor))


(defmethod run-main-loop :after ((w basecode-vr))

  ;; free mirror window data
  (when (mirror-window-geometry w)
    (destructuring-bind (vao c vbo ibo) (shiftf (mirror-window-geometry w) nil)
      (declare (ignore c))
      (gl:delete-vertex-arrays (list vao))
      (gl:delete-buffers (list vbo ibo))))

  ;; and eye renderbuffers
  (macrolet ((del (f slot)
               `(,f (list (shiftf (getf d ',slot) nil)))))
    (loop for d in (list (left-eye-desc w)
                         (right-eye-desc w))
          do (del gl:delete-renderbuffers depth-buffer-id)
             (del gl:delete-textures render-texture-id)
             (del gl:delete-framebuffers render-framebuffer-id)
             (del gl:delete-textures resolve-texture-id)
             (del gl:delete-framebuffers resolve-framebuffer-id)))

  (%gl:debug-message-control :dont-care :dont-care :dont-care
                             0 (cffi:null-pointer) nil)
  (%gl:debug-message-callback (cffi:null-pointer) (cffi:null-pointer)))

(defmethod run-main-loop ((w basecode-vr))
  ;; override basecode-glop's method
  (declare (optimize debug)
           (notinline glop:swap-buffers))
  (let ((gw (basecode::%glop-window w)))
    (glop::%swap-interval gw (if (vsync w) 1 0))
    #+win32
    (setf (glop::win32-window-swap-interval gw) 0)

    (loop
      until (slot-value w 'basecode::%exit-main-loop)
      while (with-simple-restart (Continue "Continue")
              (glop:dispatch-events gw :blocking nil :on-foo nil))
      do (with-simple-restart (Continue "Continue")
           (handle-vr-input w))
         (with-simple-restart (Continue "Continue")
           (render-frame w))
         (setf (basecode::%frame-start-time w) (basecode::now)))))

(defmethod basecode-run :around ((w basecode-vr))
  (vr::with-vr ()
    (if vr::*system*
        (call-next-method)
        (error "VR API initialization failed?"))))


(defmethod basecode-shader-helper::mvp ((w basecode-vr) program
                                        &key (m (sb-cga:identity-matrix)))
  (let* ((v (basecode-vr::eye-modelview w *eye*))
         (p (basecode-vr::eye-projection w *eye*))
         (mv (sb-cga:matrix* v m))
         (mvp (sb-cga:matrix* p v m)))
    (setf (3bgl-shaders::uniform program "time")
          (float
           (/ (get-internal-real-time)
              internal-time-units-per-second)))
    (setf (3bgl-shaders::uniform program "m") m
          (3bgl-shaders::uniform program "v") v
          (3bgl-shaders::uniform program "p") p
          (3bgl-shaders::uniform program "mv") mv
          (3bgl-shaders::uniform program "mvp") mvp
          (3bgl-shaders::uniform program "normalMatrix") mv
          (3bgl-shaders::uniform program "eyePos") (sb-cga:transform-point
                                                    (sb-cga:vec 0.0 0.0 0.0)
                                                    v))))
