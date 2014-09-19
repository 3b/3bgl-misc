(defpackage #:3bgl-embed
  (:use :cl :basecode))
(in-package #:3bgl-embed)

;;; glop window class to be used as a subwindow, to wrap an embedded
;;; window and provide a texture from it, and basecode mixin to add an
;;; input redirecting window, to keep pointer from focusing embeds by
;;; accident


(defclass basecode-embed-helper ()
  ((input-window :accessor input-window :initform nil)))

(defmethod run-main-loop :around ((w basecode-embed-helper))
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
                            ;; structure-notify-mask confuses resizing
                            ;; :structure-notify-mask
                            :visibility-change-mask
                            :pointer-motion-mask))
    (glop-xlib::x-map-raised (glop::x11-window-display xw) (input-window w))
    (call-next-method)))


(defmethod basecode::basecode-reshape :after ((w basecode-embed-helper))
  ;; resize input-window to match parent
  (format t "resize input helper to ~sx~s~%"
          (basecode::width w) (basecode::height w))
  (when (input-window w)
    (let* ((xd (glop:x11-window-display (basecode::%glop-window w)))
           (iw (input-window w)))
      (glop-xlib:x-set-geometry xd
                                iw
                                0 0
                                (basecode::width w)
                                (basecode::height w))
      (glop-xlib::x-sync xd nil))))




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
  (drawable glop-xlib::drawable)
  (buffer glx-bind-tex-image-buffer)
  (attribs :pointer))

(%gl::defglextfun ("glXReleaseTexImageEXT" glx-release-tex-image-ext) :void
  (display :pointer)
  (drawable glop-xlib::drawable)
  (buffer glx-bind-tex-image-buffer))


;; user ops:
;;  create
;;     for now, probably will just pass in a shell command to run, with a ~d
;;     or ~x to replace with window ID to embed in (as a format string,
;;     so ~* can be used to repeat it if needed)
;;  bind
;;     bind as a texture
;;     (possibly with an option to do the full x server sync stuff?)
;;  unbind?
;;  focus in
;;     set keyboard focus to embedded window
;;  focus out
;;     no-op for now?
;;  click/drag/release
;;     send a synthetic mouse event to child?
;;  resize (and get resize increment?) by pixels or characters?
;;    probably want both options, since for debugger we might want to resize
;;    to fit window, and would be easier to just resize to match main window
;;    (and probably have rounded down to next character increment automatically)
;;  delete? (or reparent to root?)
;;     not sure if we should kill child, or just reparent it to root
;;     if it hasn't exited yet?
;;     probably send wm_delete message and wait a bit?
;;     - probably want both reparent to root and wm_delete options,
;;       delete for normal use, reparent for when host closes unexpectedly?
;;  check state
;;     is child still active, mapped, current size, resize increment, etc
;;     inverted flag
;; events:
;;    child mapped
;;      flag to recreate pixmap on next bind, flag mapped
;;    child resized
;;      delete old pixmap, update size, flag to recreate pixmap on next bind
;;    child unmapped
;;      delete old pixmap, remove mapped flag
;;    child closed
;;      delete old pixmap (and any other resources), remove mapped
;;      flag, flag closed?
;;    reparent (to/from this window)

(defclass glop-embedded (glop:window)
  (;;; external state
   ;; nil = no child,
   ;; :unmapped = exists but not unmapped
   ;; :mapped = valid contents
   ;; :destroyed = no longer exists
   (child-state :accessor child-state :initform nil)
   ;; probably better to grab resize increment directly from
   ;; properties in case it changes...
   #++(resize-increment :accessor resize-increment :initform '(1 1))
   ;; fixme: better name for dimensions in units of resize-increment?
   (width* :reader width* :writer %width* :initform 0)
   (height* :reader height* :writer %height* :initform 0)
   ;; pixel dimensions of this window should match size of child, so no specific
   ;; accessor for that
   (invert-y :accessor invert-y :initform nil)
   ;; parent window, so we can tell it when child is mapped/unmapped
   (parent :accessor parent :initarg :parent :initform nil)
   ;;; internal state:
   (win :accessor win :initform nil)
   (pix :accessor pix :initform nil)
   (tex :accessor tex :initform nil)
   (fbconfig :accessor fbconfig :initform nil)
   (xpm :accessor xpm :initform nil)
   (command :accessor command :initform nil :initarg :command)))

(defmethod run-embedded ((w glop-embedded) command)
  ;; possibly should store the process for later?
  ;; currently just interacting through window...
  (uiop/run-program:run-program
   (print
    (format nil command (glop::x11-window-id w)))
   :output nil))

(defmethod glop:show-window :after ((w glop-embedded))
  ;; request substructure-notify events
  ;; fixme: figure out how to tell glop to add :substructure-notify-mask
  ;; when creating window?
  ;; (or track which were selected at creation, and allow
  ;;  adding to that more easily afterwards, instead of having to
  ;;  duplicate the list by hand like this...)
  (format t "show embedded window ~x~%" (glop::x11-window-id w))
  (glop-xlib::x-select-input (glop::x11-window-display w)
                             (glop::x11-window-id w)
                             '(:substructure-notify-mask
                               :substructure-redirect-mask
                               :exposure-mask
                               :key-press-mask :key-release-mask
                               :button-press-mask :button-release-mask
                               :structure-notify-mask
                               :visibility-change-mask
                               :pointer-motion-mask))
  ;; redirect window to a texture
  ;; fixme: probably should make sure we actually have xcomposite extension first?
  ;; (but preferably only once per run, instead of for every child window?)
  (glop-xlib::x-composite-redirect-window (glop::x11-window-display w)
                                          (glop::x11-window-id w)
                                          1)
  (when (command w)
    (run-embedded w (command w))))


(defmethod bind-texture ((w glop-embedded) )
  ;; for now just assuming :texture-2d, possibly should add in
  ;; support for :texture-rectangle for old hardware?

  ;; create pixmap if needed (either don't have one yet or got resized)
  (when (and (win w) (eq (child-state w) :mapped))
    (let ((xd (glop::x11-window-display w))
          (xw (glop::x11-window-id w))
          (new nil))
      #++(progn
        (format t "~&xpm = ~x ~s, fb = ~s~%" (xpm w)  (xpm w) (fbconfig w))
        (format t "id = ~x (~x), win = ~x~%" (glop:x11-window-id w) xw (win w))
        (format t "~&pix = ~x ~s~%" (pix w) (pix w)))

      (unless (pix w)
        (format t "create (pix w)~%")
        (unless (fbconfig w)
          (setf (fbconfig w)
                (glop-glx:glx-choose-fb-config
                 xd (glop::x11-window-screen w)
                 '(:bind-to-texture-rgba-ext t
                   :drawable-type (:pixmap-bit)
                   :bind-to-texture-targets-ext (:texture-2d-bit-ext)
                   :double-buffer nil
                   :y-inverted-ext :dont-care))))
        (when (xpm w)
          (glop-xlib::x-free-pixmap xd (shiftf (xpm w) nil)))
        (setf (xpm w) (glop-xlib::x-composite-name-window-pixmap xd xw))
        (format t "~&xpm = ~x ~s, fb = ~s~%" (xpm w)  (xpm w) (fbconfig w))
        (format t "id = ~x (~x), win = ~x~%" (glop:x11-window-id w) xw (win w))

        (glop-xlib:x-flush xd)
        (setf (pix w) (glop-glx::glx-create-pixmap
                       xd (fbconfig w) (xpm w)
                       :texture-target-ext :texture-2d-ext
                       :texture-format-ext :texture-format-rgb-ext))
        (format t "~&pix = ~x ~s~%" (pix w)  (pix w))
        (glop-xlib:x-flush xd)
        (setf new t)
        #+=(format t "pix = ~x~%" (pix w)))
      (unless (tex w)
        (format t "create (tex w)~%")
        (setf (tex w) (car (gl:gen-textures 1)))
        (gl:bind-texture :texture-2d (tex w))
        ;; fixme: make these configurable
        ;; for now just only setting once so caller can change it after binding
        (gl:tex-parameter :texture-2d :texture-min-filter :linear)
        (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
        (setf new t))
      (gl:bind-texture :texture-2d (tex w))
      (when new
        ;; release isn't working for some reason, and just binding
        ;; without release makes things get slow when also using FBOs (or
        ;; something like that, didn't narrow it down exactly), so
        ;; just bind once and hope it has valid data in it when we
        ;; use it...
        ;; seems to work well enough for now on current drivers at least
        (glx-bind-tex-image-ext xd (pix w) :front-left (cffi:null-pointer)))
)))

(defmethod release-texture ((w glop-embedded))
  (when (and (tex w) (pix w))
    ;; not sure if texture needs to be bound or not?
    #++(gl:bind-texture :texture-2d (tex w))
    #++(glx-release-tex-image-ext (glop:x11-window-display w) (pix w) :front-left)
    (gl:bind-texture :texture-2d 0)))

(defmethod focus-embedded ((w glop-embedded) &key (timestamp 0))
  (when (and (win w) (eq (child-state w) :mapped))
    (glop-xlib::x-set-input-focus (glop:x11-window-display w)
                                  (win w)
                                  2 ; 2 = reverttoparent
                                  timestamp)))


(defmethod unfocus-embedded ((w glop-embedded) target &key (timestamp 0))
  ;; move focus to another window if embedded window has focus
  (let* ((xw (etypecase target
               (basecode:basecode-glop
                (basecode::%glop-window target))
               (glop:window
                target)))
         (xd (glop::x11-window-display xw)))
    (cffi:with-foreign-objects ((win :int64) (revert :int))
      ;; todo: send XEMBED_WINDOW_DEACTIVATE or XEMBED_FOCUS_OUT or whatever
      (glop-xlib::x-get-input-focus xd win revert)
      (format t "unfocus? ~s / ~s ? ~s / ~s~%"
              (win w) (cffi:mem-aref win :int64)
              (glop::x11-window-id w) (glop::x11-window-id xw))
      #++
      (format t "input focus = ~x (~x)~%" (cffi:mem-aref win :int64)  (win w))
      (when (or (eql (win w) (cffi:mem-aref win :int64))
                (eql (glop:x11-window-id w) (cffi:mem-aref win :int64)))
        (glop-xlib::x-set-input-focus xd
                                      (glop::x11-window-id xw)
                                      2 ; reverttoparent
                                      timestamp)))))

;; todo: synthetic mouse events

(defmethod resize-increment ((w glop-embedded))
  (destructuring-bind (&key width-inc height-inc aspect-min aspect-max
                       &allow-other-keys)
      (glop-xlib::x-get-wm-normal-hints (glop:x11-window-display w)
                                        (glop:x11-window-id w))
    (list :width-inc width-inc :height-inc height-inc
          :aspect-min aspect-min :aspect-max aspect-max)))

(defmethod resize-embedded ((w glop-embedded) width height &key pixels)
  "Resize child to WIDTH x HEIGHT characters, or fit in WIDTH x HEIGHT
   pixels if PIXELS is true"
  ;; for now ignoring resizes if we don't have a child
  ;; probably should allow resizing anyway, to allow reducing
  ;; resource usage though
  (when (win w)
    (destructuring-bind (&key min-width min-height
                           max-width max-height
                           base-width base-height
                           width-inc height-inc
                           ;; todo: aspect ratio
                         &allow-other-keys)
        (glop-xlib::x-get-wm-normal-hints (glop:x11-window-display w)
                                          (win w))

      (when pixels
        ;; find nearest size below requested pixel size that is a multiple
        ;; of window size increment
        (setf width (floor (max 0 (- width (or base-width min-width 0)))
                           (or width-inc 1))
              width (floor (max 0 (- height (or base-height min-height 0)))
                           (or height-inc 1))))
      ;; clamp size to child-window's min/max size
      (when (and max-width (plusp max-width))
        (setf width (min width max-width)))
      (when (and max-height (plusp max-height))
        (setf height (min height max-height)))
      (setf width (max width (or min-width 1)))
      (setf height (max height (or min-height 1)))
      (format t "-> ~s x ~s = ~s x ~s~%" width height
              (+ (or base-width min-width 0)
                 (* width (or width-inc 1)))
              (+ (or base-height min-height 0)
                 (* height (or height-inc 1))))
      (let ((pixel-width (+ (or base-width min-width 0)
                            (* width (or width-inc 1))))
            (pixel-height (+ (or base-height min-height 0)
                             (* height (or height-inc 1)))))
        (glop:set-geometry w 0 0 pixel-width pixel-height)
        #++(glop-xlib:x-set-geometry (glop:x11-window-display w) (win w) 0 0
                                     pixel-width pixel-height)))))


(defmethod close-child ((w glop-embedded))
  ;; todo: send wm_delete_window to child
)

(defmethod reparent-child ((w glop-embedded))
  ;; todo: unredirect and reparent child to root window
)


(defun cleanup-pixmaps (w)
  (format t "delete pix ~x, xpm ~x~%" (pix w) (xpm w))
  (let ((xd (glop:x11-window-display w)))
    (when (pix w)
      (when (tex w)
        (gl:bind-texture :texture-2d (tex w))
        #++(glx-release-tex-image-ext xd (pix w) :front-left)
        (gl:bind-texture :texture-2d 0))
      (glop-glx::glx-destroy-pixmap xd (shiftf (pix w) nil)))
    (when (xpm w)
      (glop-xlib::x-free-pixmap xd (shiftf (xpm w) nil)))))

(defun resize-to-fit-child (w &key (child (win w)) first)
  (let* ((xd (glop:x11-window-display w))
         (g (glop-xlib::x-get-geometry xd child))
         (width (getf g :width))
         (height (getf g :height)))
    (format t "resize-to-fit-child ~s ~s ~s~%" w child first)
      (when first
        (format t "1force resize child to ~s,~s...~%"
                (glop:window-width w) (glop:window-height w))
        ;; xterm seems to decide rows/columns based on size of parent
        ;; when reparented, so resize it so it adjusts to new size
        ;; (resizing it to same size doesn't change its idea of
        ;; rows/columns, so resize it to match parent, then resize
        ;; back to its original size)
        (glop-xlib:x-set-geometry xd
                                  child
                                  0 0
                                  (glop:window-width w)
                                  (glop:window-height w))
        (glop-xlib::x-sync xd nil)
        (glop-xlib:x-set-geometry xd
                                  child
                                  0 0
                                  width height)
        ;; send a synthetic structurenotify?
        #++
        (cffi:with-foreign-object (msg '(:union glop-xlib::x-event))
          (cffi:with-foreign-slots ((type) msg (:union glop-xlib::x-event))
            (setf type
                  (cffi:foreign-enum-value 'glop-xlib::x-event-name :configure-notify))
            (cffi:with-foreign-slots ((glop-xlib::win
                                       glop-xlib::x glop-xlib::y
                                       glop-xlib::width glop-xlib::height)
                                      msg
                                      (:struct glop-xlib::x-configure-event))
              (setf glop-xlib::win child
                    glop-xlib::x 0
                    glop-xlib::y 0
                    glop-xlib::width (1- width)
                    glop-xlib::height height)))
          (glop-xlib::x-send-event xd child nil
                                   (cffi:foreign-bitfield-value
                                    'glop-xlib::x-event-mask-flags
                                    '(:structure-notify-mask))
                                   msg)))


    (unless (or first
             (and (eql width (glop:window-width w))
                  (eql height (glop:window-height w))))
      (format t "resize to ~s,~s~%" width height)
      (glop:set-geometry w 0 0 width height)
      (glop-xlib::x-sync xd nil)
      ;; return true to indicate we resized
      t)))

(defgeneric embedded-window-mapped (parent child))
(defgeneric embedded-window-unmapped (parent child))

(defmethod glop:on-event ((w glop-embedded)
                          (event glop::child-visibility-unobscured-event))
  (declare (optimize debug))
  ;; child mapped
  ;; add window, resize to match child, clear pixmap if set
  ;; todo: handle multiple children somehow?
  ;;   probably error or ignore one, not sure which though?

  (format t "child mapped ~x~%" (glop::child event))
  (cleanup-pixmaps w)

  (when (and (win w) (not (eql (win w) (glop::child event))))
    (format t "got multiple children in glop-embedded window? ~x, ~x~%"
            (win w) (glop::child event)))

  (resize-to-fit-child w :child (glop::child event) :first (not (win w)))
  (setf (win w) (glop::child event))

  (setf (child-state w) :mapped)
  (when (parent w)
    (embedded-window-mapped (parent w) w))) 

(defmethod glop:on-event((w glop-embedded)
                          (event glop::child-created-event))
  (format t "child created ~x~%" (glop::child event))
  (cleanup-pixmaps w)

  (when (and (win w) (not (eql (win w) (glop::child event))))
    (format t "got multiple children in glop-embedded window? ~x, ~x~%"
            (win w) (glop::child event)))

  (resize-to-fit-child w :child (glop::child event) :first (not (win w)))
  (setf (win w) (glop::child event))
  (glop-xlib::x-map-window (glop::x11-window-display w) (glop::child event)))

(defmethod glop:on-event ((w glop-embedded)
                          (event glop::child-visibility-obscured-event))
  (declare (optimize debug))
  (format t "child unmapped ~x~%" (glop::child event))

  (cleanup-pixmaps w)
  (setf (win w) nil)
  (setf (child-state w) :unmapped)
  (when (parent w)
    (embedded-window-unmapped (parent w) w)))


(defmethod glop:on-event ((w glop-embedded)
                          (event glop::child-resize-event))
  (declare (optimize debug))
  (format t "child #x~x resized to ~dx~d event~%"
          (glop::child event) (glop::width event) (glop::height event))

  ;; resize to match child, clear pixmap

  (cleanup-pixmaps w)
  ;; should we resize when child isn't mapped?
  (unless (and (eql (glop:width event) (glop:window-width w))
               (eql (glop:height event) (glop:window-height w)))
    (glop:set-geometry w 0 0 (glop:width event) (glop:height event))))

(defmethod glop:on-event ((w glop-embedded)
                          (event glop::child-reparent-event))
  (declare (optimize debug))
  (format t "child reparent ~x -> ~x (~x)~%" (glop::child event)
          (glop::parent event) (glop:x11-window-id w))
  ;; if parent = this window, add child
  ;; if parent /= this window (and /= child), and child = (win w), remove child
  (format t "  ~x ?= ~x~%" (glop:x11-window-id w) (glop::parent event))
  (cond
    ((= (glop:x11-window-id w) (glop::parent event))
     (format t "add ~x ? ~x~%" (glop::child event) (win w))
     ;; added a child, make sure child is mapped
     ;; todo: make sure we still get an map event for already mapped child
     (resize-to-fit-child w :child (glop::child event) :first (not (win w)))
     (setf (win w) (glop::child event))
     (glop-xlib::x-map-window (glop::x11-window-display w) (glop::child event)))
    ((and (/= (glop:x11-window-id w) (glop::parent event))
          (eql (win w) (glop::child event)))
     ;; child got reparented elsewhere
     (cleanup-pixmaps w)
     (setf (win w) nil)
     ;; what state should this use?
     (setf (child-state w) nil))))

;; todo: resize child if someone resizes a glop-embedded window?



(defmethod glop:on-event ((w glop-embedded)
                          (event glop::visibility-unobscured-event))
  (declare (optimize debug))
  ;; 
  (format t "window mapped (~x)~%"  (glop:x11-window-id w))

)


(defmethod glop:on-event ((w glop-embedded)
                          (event glop::resize-event))
  (declare (optimize debug))
  (format t "window resized (~x) ~s,~s~%"  (glop:x11-window-id w)
          (glop:width event) (glop:height event))
  (cleanup-pixmaps w)
  (when (win w)
    (format t " resize child ~x to ~s,~s (~s,~s)...~%"
            (win w)
            (glop:width event) (glop:height event)
            (glop:window-width w) (glop:window-height w))
    (glop-xlib:x-set-geometry (glop:x11-window-display w) (win w) 0 0
                              (glop:width event) (glop:height event)))
  
)
