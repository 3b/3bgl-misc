(defpackage #:hdr-test
  (:use :cl :basecode))
(in-package #:hdr-test)

(defclass hdr-test (basecode-glop perspective-projection basecode-clear
                    fps-graph basecode-draw-ground-plane
                    freelook-camera
                    basecode-exit-on-esc)
  ((program :accessor program :initform nil)
   (tex :accessor tex :initform nil)
   (exposure :accessor exposure :initform 1.0)
   (drag :accessor drag :initform nil))
  (:default-initargs :look-at-eye '(3 2 15)))

(defparameter *w* nil)

(defmethod basecode-draw ((w hdr-test))
  (setf *w* w)
  (gl:color 1 1 1 1)
  (gl:disable :cull-face)
  (gl:enable :texture-cube-map :depth-test)
  (gl:enable :framebuffer-srgb)
  (when (tex w)
    (gl:bind-texture :texture-cube-map (tex w)))
  (when (program w)
    (let ((p1 (program w)))
      (gl:use-program p1)
      (3bgl-shaders::uniformi p1 "texture" 0)
      (3bgl-shaders::uniformf p1 "exposure" (exposure w))
      (3bgl-shaders::uniform-matrix p1 "mvp"
                                    (sb-cga:matrix*
                                     (basecode::projection-matrix w)
                                     (basecode::freelook-camera-modelview w)))
      (3bgl-shaders::uniform-matrix p1 "mv"
                                    (basecode::freelook-camera-modelview w))))
  (flet ((tc (u v)
           (gl:vertex-attrib 1 u v)
           #++(gl:tex-coord u v)))
    #++(gl:with-primitives :quads
         (tc 0 1)
         (gl:vertex -20 0 0)

         (tc 0 0)
         (gl:vertex -20 20 0)

         (tc 1 0)
         (gl:vertex 20 20 0)

         (tc 1 1)
         (gl:vertex 20 0 0))

    (gl:with-primitives :quad-strip
      (loop with p = 180
            with h = (floor p 2)
            for i upto p
            for tc = (float (+ 0.08 (/ i p 1/1)))
            do (tc tc 1)
               (gl:vertex (* -20 (sin (* i (/ pi h))))
                          (* -20 pi)
                          (* -20 (cos (* i (/ pi h)))))
               (tc tc 0)
               (gl:vertex (* -20 (sin (* i (/ pi h))))
                          (* 20 pi)
                          (* -20 (cos (* i (/ pi h))))))))
  (gl:use-program 0))

(defmethod mouse-down ((w hdr-test) b x y)
  (when (= b 1)
    (setf (drag w) (list (exposure w) y))))

(defmethod mouse-up ((w hdr-test) b x y)
  (when (= b 1)
    (setf (drag w) nil)))

(defmethod mouse-move ((w hdr-test) x y)
  (when (drag w)
    (setf (exposure w) (* (first (drag w))
                          (exp (/ (- (second (drag w)) y) 100.0))))))

(defmethod key-down :after ((w hdr-test) k)
  (case k
    #++
    ((#\t :t)
     (unless (tex w)
       (setf (tex w) (print (car (gl:gen-textures 1)))))
     (gl:bind-texture :texture-2d (tex w))
     (3bgl-radiance-hdr::tex-image-2d "/tmp/test.hdr")
     (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
     (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

     (gl:generate-mipmap :texture-2d)
     )
    ((#\t :c)
     (unless (tex w)
       (setf (tex w) (print (car (gl:gen-textures 1)))))
     (gl:bind-texture :texture-cube-map (tex w))
     (3bgl-radiance-hdr::tex-image-cross-cube "/tmp/test.hdr")
     (gl:enable :texture-cube-map-seamless)
     (gl:tex-parameter :texture-cube-map :texture-min-filter :linear-mipmap-linear)
     (gl:tex-parameter :texture-cube-map :texture-mag-filter :linear)
     (gl:generate-mipmap :texture-cube-map))
    ((#\r :r)
     (setf (program w)
           (3bgl-shaders::reload-program (program w)
                                         'hdr-test-shaders::vertex
                                         'hdr-test-shaders::fragment)))
    (:n
     #++(setf (basecode::projection-fov w) 70)
     (print (basecode::freelook-camera-offset w))
     (setf (basecode::freelook-camera-offset w) 0.0)
     )
    (:m
     (setf (basecode::projection-fov w) 45)
     )
    (:l
     (setf (basecode::projection-fov w) 70)
     )
    (:p
     (setf (basecode::projection-fov w) 170)
     )))



; (basecode-run (make-instance 'hdr-test))
