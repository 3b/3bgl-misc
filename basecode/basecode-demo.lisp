(in-package #:basecode)

(defun basecode-demo-draw (w)
  (declare (ignorable w))
  (gl:enable :depth-test)
  (gl:with-pushed-matrix* (:modelview)
   (gl:disable :cull-face :lighting :texture-2d)
   (gl:rotate (/ (get-internal-real-time) 50.0) 1 1 1)
    (gl:with-primitives :triangles
      (gl:color 1 0 0 1)
      (gl:vertex 0 0 0)
      (gl:vertex 0 1 0)
      (gl:vertex 1 0 0)
      (gl:color 0 1 0 1)
      (gl:vertex 0 0 0)
      (gl:vertex 0 0 1)
      (gl:vertex 1 0 0)
      (gl:color 0 0 1 1)
      (gl:vertex 0 0 0)
      (gl:vertex 0 1 0)
      (gl:vertex 0 0 1))))

(defclass basecode-demo (basecode-glut perspective-projection basecode-clear
                         fps-graph basecode-draw-axes
                         basecode-draw-ground-plane freelook-camera)
  ()
  (:default-initargs :look-at-eye '(8 1 4)))

(defmethod basecode-draw ((w basecode-demo))
  (basecode-demo-draw w))

; (basecode-run (make-instance 'basecode-demo))

(defclass basecode-fbo-demo (basecode-glop perspective-projection
                             fps-graph
                             basecode-clear
                             freelook-camera
                             basecode-exit-on-esc)
  ((fbo :accessor fbo :initform nil))
  (:default-initargs :look-at-eye '(8 1 4)))

(defclass basecode-fbo-demo-fbo (basecode-simple-fbo
                                 basecode-clear basecode-draw-axes
                                 basecode-draw-ground-plane)
  ()
  (:default-initargs :width 512 :height 512))

(defmethod basecode-draw ((w basecode-fbo-demo-fbo))
  (basecode-demo-draw w))

(defmethod basecode-draw ((w basecode-fbo-demo))
  (unless (fbo w)
    (setf (fbo w) (make-instance 'basecode-fbo-demo-fbo
                                 :texture-format :rgba8
                                 :width (width w)
                                 :height (height w))))
  (when (basecode::%resized w)
    ;; not sure if updating on every resize is good, or if it
    ;; should rate-limit it or just check every second or so, or what
    ;; (or maybe even just disable resize from WM and add keys/menu
    ;;  for resizing?)
    (basecode-cleanup (fbo w))
    (setf (fbo w) (make-instance 'basecode-fbo-demo-fbo
                                 :texture-format :rgba8
                                 :width (width w)
                                 :height (height w)))
    )
  (when (fbo w)
    (basecode-draw (fbo w)))
  (when (fbo w)
    (gl:bind-texture :texture-2d (texture (fbo w)))
    (gl:enable :texture-2d)
    (gl:disable :depth-test :cull-face)
    (gl:color 1 1 1 1)
    (with-pixel-ortho-projection (w :origin :lower-left)
      (gl:load-identity)
      (gl:with-primitives :quads
        (gl:tex-coord 0 0)
        (gl:vertex 0 0)
        (gl:tex-coord 0 1)
        (gl:vertex 0 (height w))
        (gl:tex-coord 1 1)
        (gl:vertex (width w) (height w))
        (gl:tex-coord 1 0)
        (gl:vertex (width w) 0))))
)

; (basecode-run (make-instance 'basecode-fbo-demo))
