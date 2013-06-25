(in-package #:basecode)

;; limited version of basecode window class for offscreen drawing
;; (probably doesn't get any input events, but gets
;;  resize/create/draw/etc as normal)

(defclass basecode-fbo (basecode)
  ;; we don't specify any slots for renderbuffers or textures, since it
  ;; seems like it would be too annoying to come up with a convenient
  ;; but generic way to specify them.
  ;; for now, subclasses can just define whatever they need
  ;; (possibly with some mixins or helpers for common cases?)
  ((fbo :reader fbo)
   ;; probably don't need a title slot, but might want it for
   ;; debugging (for example when showing contents of various
   ;; intermediate buffers) so leaving in for now
   (title :reader title :initarg :title)
   (width :reader width :initarg :width)
   (height :reader height :initarg :height))
  (:default-initargs :width 512 :height 512 :title "fbo"
                     ))

(defmethod initialize-instance :after ((w basecode-fbo) &key)
  ;; not sure if this should be here or basecode-init :before?
  (setf (slot-value w 'fbo) (car (gl:gen-framebuffers 1))))

(defmethod basecode-cleanup :after ((w basecode-fbo))
  (when (fbo w)
    (gl:delete-framebuffers (list (fbo w))))
  (setf (slot-value w 'fbo) nil))

;; not sure about this one, might want more control over which target
;; it is bound to?
(defmethod basecode-draw :around ((w basecode-fbo))
  (gl:bind-framebuffer :framebuffer (fbo w))
  (unwind-protect
       (call-next-method)
    (gl:bind-framebuffer :framebuffer 0)))

(defmethod basecode-draw ((w basecode-fbo))
)



;;; example with some simple defaults:
;; single renderable texture bound to color 0,
;; depth renderbuffer bound to depth
(defclass basecode-simple-fbo (basecode-fbo)
  ((texture :reader texture :initform nil)
   (renderbuffers :initform nil))
  (:default-initargs :texture-format :rgba8
                     :multisample nil))

(defmethod basecode-cleanup :after ((w basecode-simple-fbo))
  (when (slot-value w 'renderbuffers)
    (gl:delete-renderbuffers (slot-value w 'renderbuffers)))
  (setf (slot-value w 'renderbuffers) nil)
  (when (texture w)
    (gl:delete-textures (list (texture w))))
  (setf (slot-value w 'texture) nil))

(defmethod initialize-instance :after ((w basecode-simple-fbo)
                                       &key texture-format multisample)
  (when multisample
    (error "todo: multisample"))
  (setf (slot-value w 'texture) (car (gl:gen-textures 1)))
  (gl:bind-texture :texture-2d (texture w))
  (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
  (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
  (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
  (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
  (gl:tex-image-2d :texture-2d 0 texture-format (width w) (height w)
                   0 :rgba :unsigned-int (cffi:null-pointer))
  (gl:bind-framebuffer :framebuffer (fbo w))
  (gl:framebuffer-texture-2d :framebuffer :color-attachment0
                             :texture-2d (texture w) 0)
  (setf (slot-value w 'renderbuffers) (gl:gen-renderbuffers 1))
  (let ((rb (car (slot-value w 'renderbuffers))))
    (gl:bind-renderbuffer :renderbuffer rb)
    (gl:renderbuffer-storage :renderbuffer :depth-component24 (width w) (height w))
    (gl:framebuffer-renderbuffer :framebuffer :depth-attachment
                                 :renderbuffer rb))
  (format t "created renderbuffer status = ~s~%"
          (gl:check-framebuffer-status :framebuffer))
  (gl:bind-framebuffer :framebuffer 0)
  
)

