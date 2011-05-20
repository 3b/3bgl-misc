;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;; ttf-tess.lisp --- Render 3d characters using zpb-ttf and glu-tess,
;;; with geometry drawn through VBO/VAO

(defpackage #:glu-ttf-extrude-example
  (:use :cl :glu-ttf-extrude :zpb-ttf :alexandria)
  (:export #:ttf-tess-window))

(in-package #:glu-ttf-extrude-example)

(defclass ttf-tess-window (glut:window)
  ((vbos :accessor vbos :initform nil)
   (vaos :accessor vaos :initform nil)
   (counts :accessor counts :initform nil)
   (font-path :accessor font-path :initarg :font-path)
   (mesh-count :accessor mesh-count :initarg :mesh-count)
   (angle :accessor angle :initform 0.0))
  (:default-initargs :width 640 :height 480 :title "ttf-tess.lisp"
                     :mode '(:double :rgb :depth :multisample)
                     :mesh-count 64
                     :tick-interval 16))

(defmethod free-buffers ((w ttf-tess-window))
  (gl:delete-buffers (vbos w))
  (gl:delete-vertex-arrays (vaos w)))

(defmethod load-glyphs ((w ttf-tess-window))
  (free-buffers w)
  (with-font-loader (loader (font-path w))
    ;; we pick some random characters from the font, up to the number
    ;; specified in the class instance, or the number available
    ;; whichever is lower
    (let* ((c (min (glyph-count loader)
                   (mesh-count w)))
           ;; select C glyphs at random (not very efficiently, but
           ;; we only need to do it once...)
           (glyphs (mapcar (lambda (i)
                             (index-glyph i loader))
                           #++(make-list c :initial-element 1187)
                           (subseq (shuffle (iota (glyph-count loader)))
                                   0 c)))
           ;; for each character, we want 2 VBOs and a VAO
           (buffers (gl:gen-buffers (* c 2)))
           (vaos (gl:gen-vertex-arrays c))
           (counts (make-array c :initial-element 0)))
      (setf (vbos w) buffers
            (vaos w) vaos
            (counts w) counts)
      ;; loop through all of the characters, triangulate and extrude them,
      ;; then build the VBOs/VAOs from the triangle data
      (loop
         for i from 0
         for va in vaos
         for (vb ib) on buffers by #'cddr
         for glyph in glyphs
         for extruded = (extrude-glyph glyph loader)
         do
         ;; and build the VBOs and VAO
           (setf (aref (counts w) i)
                 (fill-buffers extruded vb ib va))
           )

)
)
  )
;;; First, we create buffers for our vertex and index data, load a
;;;  then build some meshes out of the ttf outline data
(defmethod glut:display-window :before ((w ttf-tess-window))
  (declare (optimize debug))

  (load-glyphs w))


(defmethod glut:tick ((w ttf-tess-window))
  (let ((seconds-per-revolution 6))
    (incf (angle w)
          (/ (* 2 pi) (* 5 seconds-per-revolution))))

  (glut:post-redisplay))

(defparameter *tris* 0)

(defmethod glut:display ((w ttf-tess-window))
  (gl:clear-color 0.0 0.0 0.2 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (gl:matrix-mode :modelview)
  (let ((tris 0))
    (flet ((rx ()
            (gl:load-identity)

            (gl:rotate (angle w) 1 1 0)
            (gl:light :light0 :position (list 7 7 7 1.0))

            (gl:translate -1.3 -0.6 0)
            #++(gl:translate -0.8 -0.0 0)
            (gl:scale 0.2 0.2 0.1)
            ))
     (gl:enable :line-smooth :point-smooth :blend :depth-test :lighting :light0)
     (gl:point-size 1)
     (gl:blend-func :src-alpha :one-minus-src-alpha)
     (gl:disable :cull-face)
     (gl:light :light0 :position (list 0.2 0.7 0.2 1.0))
     (gl:with-pushed-matrix* (:modelview)
       (rx)
       (loop for vao in (vaos w)
          for i from 1
          for count across (counts w)
          for tx = 1
          do
            (incf tris count)
          (when (zerop (mod i 16))
            (rx)
            (gl:translate (- tx) (* tx (floor  i 8)) 0))
          (gl:translate tx 0 0)

          (gl:bind-vertex-array vao)
          (gl:color 1 0 0 1)
          (gl:polygon-mode :front-and-back :fill)
          (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count count)
          (gl:color 0 0 1 0.5)
          (gl:polygon-mode :front-and-back :line)
          (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count count)
          #++(gl:color 0 1 0 0.4)
          #++(gl:draw-elements :points (gl:make-null-gl-array :unsigned-short) :count count))))
    (setf *tris* tris))

  (glut:swap-buffers))

(defmethod glut:reshape ((w ttf-tess-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  ;; Ensure that projection matrix ratio always matches the window size ratio,
  ;; so the polygon will always look square.
  (let ((right (max (float (/ width height)) 1.0))
	(top (max (float (/ height width)) 1.0)))
    (glu:ortho-2d (- right) right (- top) top))

  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod glut:keyboard ((w ttf-tess-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:leave-main-loop))
    (#\space (load-glyphs w))))

;; Cleanup.
;; Most of the objects we created have analogous deletion function.
(defmethod glut:close ((w ttf-tess-window))

  (free-buffers w))

(defun ttf-tess ()
  (let ((w (make-instance 'ttf-tess-window
                          :font-path "/usr/share/fonts/truetype/msttcorefonts/Georgia.ttf")))
    (unwind-protect
	 (glut:display-window w)
      (glut:destroy-window (glut:id w)))))

;; (ttf-tess)