#++(asdf:load-systems '3bgl-misc)
(defpackage #:xterm-texture
  (:use :cl :basecode))
(in-package #:xterm-texture)

(defparameter *command* "xterm -b 0 -into ~d &")
#++
(defparameter *command* "xterm -b 0 -into ~d -e screen -xRR embed &")
#++
(defparameter *command* "xterm -b 0 -into ~d -e emacs -nw &")
#++
(defparameter *command* "xterm -b 0 -into ~d -e emacsclient -nw &")

(cffi:defcenum glx-bind-tex-image-buffer
  (:front-left #x20de)
  ;; todo: add rest of these
  ;; GLX_FRONT_RIGHT_EXT                0x20DF
  ;; GLX_BACK_LEFT_EXT                  0x20E0
  ;; GLX_BACK_RIGHT_EXT                 0x20E1
  ;; GLX_FRONT_EXT                      GLX_FRONT_LEFT_EXT
  ;; GLX_BACK_EXT                       GLX_BACK_LEFT_EXT
  ;; GLX_AUX0_EXT                       0x20E2
  ;; GLX_AUX1_EXT                       0x20E3
  ;; GLX_AUX2_EXT                       0x20E4
  ;; GLX_AUX3_EXT                       0x20E5
  ;; GLX_AUX4_EXT                       0x20E6
  ;; GLX_AUX5_EXT                       0x20E7
  ;; GLX_AUX6_EXT                       0x20E8
  ;; GLX_AUX7_EXT                       0x20E9
  ;; GLX_AUX8_EXT                       0x20EA
  ;; GLX_AUX9_EXT                       0x20EB
  )

(%gl::defglextfun ("glXBindTexImageEXT" glx-bind-tex-image-ext) :void
  (display :pointer)
  (window glop-xlib::drawable)
  (buffer glx-bind-tex-image-buffer)
  (attribs :pointer))

(%gl::defglextfun ("glXReleaseTexImageEXT" glx-release-tex-image-ext) :void
  (display :pointer)
  (window glop-xlib::drawable)
  (buffer glx-bind-tex-image-buffer))


(defclass xterm-texture (basecode-glop
                         perspective-projection
                         basecode-clear
                         fps-graph basecode-draw-ground-plane
                         freelook-camera
                         basecode-exit-on-esc)
  ((win :accessor win :initform nil)
   (pix :accessor pix :initform nil)
   (tex :accessor tex :initform nil)
   (input-window :accessor input-window :initform nil))
  (:default-initargs :look-at-eye '(3 2 15)))

(defparameter *w* nil)

(defparameter *focus* nil)
(defmethod run-main-loop :before ((w xterm-texture))
  ;; create an input-only window to use to redirect input to main window,
  ;; so it doesn't go to embedded child window unless we want it to
  (let ((xw (basecode::%glop-window w)))
    (setf (input-window w)
          (glop-xlib::%x-create-window (glop:x11-window-display xw)
                                       (glop::x11-window-id xw)
                                       0 0
                                       (basecode::width w)
                                       (basecode::height w)
                                       0 0
                                       2 ;; input-only
                                       (glop::x11-window-visual-infos xw)
                                       0 (cffi:null-pointer)))
    ;; fixme: figure out which of these actually need set on the input window
    (glop-xlib::x-select-input (glop::x11-window-display
                                (basecode::%glop-window w))
                          (input-window w)
                          '(:substructure-notify-mask
                            :exposure-mask
                            :key-press-mask :key-release-mask
                            :button-press-mask :button-release-mask
                            :structure-notify-mask
                            :visibility-change-mask
                            :pointer-motion-mask))
)
  ;; fixme: figure out how to tell glop to add :substructure-notify-mask
  ;; when creating main window?
  ;; (or track which were selected at creation, and allow
  ;;  adding to that more easily afterwards, instead of having to
  ;;  duplicate the list by hand like this...)
  (glop-xlib::x-select-input (glop::x11-window-display
                              (basecode::%glop-window w))
                  (glop::x11-window-id (basecode::%glop-window w))
                  '(:substructure-notify-mask
                    :exposure-mask
                    :key-press-mask :key-release-mask
                    :button-press-mask :button-release-mask
                    :structure-notify-mask
                    :visibility-change-mask
                    :pointer-motion-mask)))

(defmethod basecode-draw ((w xterm-texture))
  (setf *w* w)
  (gl:enable :depth-test)
  (gl:enable :texture-2d)
  (if (win w)
      (gl:color 1 1 1 1)
      (gl:color 0.5 0.5 0.5 1))
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
      (gl:vertex -10 1 10))
    ))

(defmethod key-up :after ((w xterm-texture) k)
  (case k
    (:l2
     (when (win w)
       (format t "changing focus to ~x~%" (win w))
       (let* ((xw (basecode::%glop-window w))
              (xd (glop::x11-window-display xw)))
         (glop-xlib::x-set-input-focus xd
                                       (win w)
                                       2           ; reverttoparent
                                       0 ;(print glop::*event-time*) ; CurrentTime
                                       )
         ;; possibly should send XEMBED_WINDOW_ACTIVATE when setting focus?
         ;; or more likely, XEMBED_FOCUS_IN?
         (cffi:with-foreign-objects ((win :int64) (revert :int))
           (glop-xlib::x-get-input-focus xd win revert)
           (unless (eql (win w) (cffi:mem-aref win :int64))
            (format t "tried to set focus to ~x, current focus = ~x, ~s~%"
                    (win w)
                    (cffi:mem-aref win :int64)
                    (cffi:mem-aref revert :int)))))))))

(defun cleanup-pixmap (w xd)
  (when (pix w)
    (glx-release-tex-image-ext xd (pix w) :front-left)
    #++(glop-xlib::x-free-pixmap xd (shiftf (pix w) nil))
    (glop-glx::glx-destroy-pixmap xd (shiftf (pix w) nil))))

(defun update-pixmap-for-resize (w xd xw)
  ;; fixme: texture_from_pixmap spec says we should rebind texture for every use
  ;; rather than just keeping it around like this...
  (cleanup-pixmap w xd)

  (format t "cwin attrs = ~s~%" (glop-xlib::x-get-window-attributes
                                 xd (win w)))
  (format t "   = ~s~%"
          (cffi:mem-aref
           (getf (glop-xlib::x-get-window-attributes
                  xd (win w))
                 'glop-xlib::visual)
           '(:struct glop-xlib::visual)))

  (format t "input attrs = ~s~%" (glop-xlib::x-get-window-attributes
                                  xd (input-window w)))
  (format t "   = ~s~%"
          (cffi:mem-aref
           (getf (glop-xlib::x-get-window-attributes
                  xd (input-window w))
                 'glop-xlib::visual)
           '(:struct glop-xlib::visual)))

  ;; possibly should store fbconfig for later reuse?
  (let ((config (glop-glx:glx-choose-fb-config
                 xd
                 (glop::x11-window-screen xw)
                 '(:bind-to-texture-rgba-ext t
                   :drawable-type (:pixmap-bit)
                   :bind-to-texture-targets-ext (:texture-2d-bit-ext)
                   :double-buffer nil
                   :y-inverted-ext :dont-care))))

    (format t "configs = ~s~%" config)
    (when config
      (let ((xpm (glop-xlib::x-composite-name-window-pixmap xd (win w))))
        (format t "window ~x geometry: ~s~%"
                (win w)
                (glop-xlib::x-get-geometry xd (win w)))
        (format t "pixmap geometry: ~s~%"
                (glop-xlib::x-get-geometry xd xpm))

        (format t "~&xpm = ~x~%" xpm)
        (glop-xlib:x-flush xd)
        (setf (pix w)
              (glop-glx::glx-create-pixmap xd config
                                           xpm
                                           :texture-target-ext :texture-2d-ext
                                           :texture-format-ext :texture-format-rgb-ext)))
      (glop-xlib:x-flush xd)
      (format t "pix = ~x~%" (pix w))
      (unless (tex w)
        (setf (tex w) (car (gl:gen-textures 1))))
      (gl:bind-texture :texture-2d (tex w))
      (glx-bind-tex-image-ext xd (pix w) :front-left (cffi:null-pointer))
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      ))

  )

(defmethod basecode::basecode-child-mapped ((w xterm-texture) c)
  (unless (eql c (input-window w))
    ;; fixme: handle multiple child windows...
    (setf (win w) c)
    ;; make sure input window is on top (possibly should move child
    ;; window offscreen instead?)

    (format t "adding child window ~x~%" c)
    ;; todo: make sure glx extension is present
    (let* ((xw (basecode::%glop-window w))
           (xd (glop::x11-window-display xw)))

      (glop-xlib::x-map-raised xd (input-window w))

      ;; todo: check return value
      (print (multiple-value-list (glop-xlib::x-composite-query-extension xw)))

      ;; todo: check return value, figure out which version we actually need...
      (print (multiple-value-list (glop-xlib::x-composite-query-version xd 0 4)))

      (glop-xlib::x-composite-redirect-window xd (win w) 1)
      ;; is there some better way to make sure redirect finished?
      (glop-xlib:x-flush xd)

      (update-pixmap-for-resize w xd xw))))

(defmethod basecode::basecode-child-unmapped ((w xterm-texture) c)
  (when (pix w)
    (cleanup-pixmap w (glop::x11-window-display (basecode::%glop-window w))))
  (format t "child window ~x unmapped~%" c)
  (setf (win w) nil)
)


(defmethod basecode::basecode-child-resized ((w xterm-texture) c width height)
  (format t "child window ~x resized to ~sx~s~%" c width height)
  (when (and (pix w) (/= c (input-window w)))
    (let ((xw (basecode::%glop-window w)))
      (update-pixmap-for-resize w (glop::x11-window-display xw) xw)))
)

(defmethod basecode::basecode-child-reparent ((w xterm-texture) c p x y)
  (format t "child window ~x reparented to ~x~%" c p)
  ;; make sure it is mapped
  (glop-xlib::x-map-window (glop::x11-window-display (basecode::%glop-window w))
                           c))

(defmethod mouse-down ((w xterm-texture) b x y)
  ;; move input focus back to main window on mouse click for now
  (let* ((xw (basecode::%glop-window w))
         (xd (glop::x11-window-display xw)))
    (cffi:with-foreign-objects ((win :int64) (revert :int))
      ;; possibly should send XEMBED_WINDOW_DEACTIVATE or
      ;; XEMBED_FOCUS_OUT to subwindow when removing focus?
      (glop-xlib::x-get-input-focus xd win revert)
      #++
      (format t "input focus = ~x (~x)~%" (cffi:mem-aref win :int64)  (win w))
      (when (eql (win w) (cffi:mem-aref win :int64))
        (glop-xlib::x-set-input-focus xd
                           (input-window w)
                           2           ; reverttoparent
                           0 ;(print glop::*event-time*) ; CurrentTime
                           )))))

(defmethod key-down :after ((w xterm-texture) k)
  (print k)
  (case k
    ((#\m :m)
     (uiop/run-program:run-program
      (print
       (format nil *command* (glop::x11-window-id (basecode::%glop-window w))))
      :output nil))
    (:f7
     )
    (:f10
)
    (:l1
     (glop-xlib:x-set-geometry (glop::x11-window-display
                                (basecode::%glop-window w))
                               (win w)
                               0 0
                               (* 80 6) (* 160 13))

     )
))


;; todo:
;; input focus
;;   grab a hotkey while child focused to allow switching back?
;; make sure extensions exist/ check versions
;;   xcomposite (>= 0.2?)
;;   glx extensions
;; handle unmapping (need to recreate/rebind pixmap when remapped?)
;; do we need to adjust gravity to make sure child window doesn't get
;;   moved when parent is resized?
;; handle subwindow trying to iconify/unmap/etc?
;; reparent events?
;; distinguish child map/unmap from current window in glop-xlib:process-event


;; xterm -into (win *w*)
#++(glop:x11-window-id (basecode::%glop-window *w*))
#++
(basecode-run (make-instance 'xterm-texture))
