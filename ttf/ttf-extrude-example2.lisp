;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;; Render 3d characters using zpb-ttf and glu-tess, with geometry
;;; drawn through VBO/VAO

(defpackage #:glu-ttf-extrude-example
  (:use :cl :glu-ttf-extrude :zpb-ttf :alexandria :basecode)
  (:export #:ttf-tess-window))

(in-package #:glu-ttf-extrude-example)

(defclass ttf-tess-window (basecode-glut perspective-projection basecode-clear
                                         fps-graph basecode-draw-ground-plane
                                         freelook-camera)
  ((vbos :accessor vbos :initform nil)
   (vaos :accessor vaos :initform nil)
   (counts :accessor counts :initform nil)
   (font-path :accessor font-path :initarg :font-path)
   (mesh-count :accessor mesh-count :initarg :mesh-count)
   (angle :accessor angle :initform 0.0)
   (wireframe :accessor wireframe :initform nil))
  (:default-initargs :width 640 :height 480 :title "ttf-extrude"
                     :mesh-count 64
                     :look-at-eye '(-5 10 -15)))

(defmethod free-buffers ((w ttf-tess-window))
  (gl:delete-buffers (vbos w))
  (gl:delete-vertex-arrays (vaos w)))

(defmethod load-glyphs ((w ttf-tess-window))
  (declare (optimize debug))
  (free-buffers w)
  (let ((s (random most-positive-fixnum)))
    (format t "seed = ~s~%" s)
    (setf *random-state* (sb-ext:seed-random-state s)))
  (with-font-loader (loader (font-path w))
    ;; we pick some random characters from the font, up to the number
    ;; specified in the class instance, or the number available
    ;; whichever is lower
    (let* ((c (min (glyph-count loader)
                   (mesh-count w)))
           ;; select C glyphs at random (not very efficiently, but
           ;; we only need to do it once...)
           (glyphs (loop for i in (shuffle (iota (glyph-count loader)))
                      for g = (index-glyph i loader)
                      when (plusp (code-point g))
                      collect g
                      and count 1 into count
                      while (< count c)))
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
                 (fill-buffers extruded vb ib va))))))

(defmethod basecode-init ((w ttf-tess-window))
  (declare (optimize debug))

  (load-glyphs w))

(defmethod glut:tick ((w ttf-tess-window))
  (glut:post-redisplay))

(defparameter *tris* 0)

(defmethod basecode-draw ((w ttf-tess-window))
  (declare (optimize debug))
  (let ((seconds-per-revolution 6))
    (incf (angle w)
          (/ (* 2 pi) (* 5 seconds-per-revolution))))

  (gl:clear-color 0.0 0.0 0.2 1.0)

  (gl:with-pushed-matrix* (:modelview)
    (let ((tris 0))
      (flet ((rx ()
               (gl:with-pushed-matrix* (:modelview)
                 (gl:rotate (* 2 (angle w)) 0 0 1)
                 (gl:light :light0 :position (list  10 0 10 1.0)))))
        (gl:enable :line-smooth :point-smooth :polygon-smooth :blend
                   :depth-test :lighting :light0 :multisample)
        (gl:point-size 1)
        (gl:blend-func :src-alpha :one-minus-src-alpha)
        (gl:enable :cull-face)
        (gl:light :light0 :position (list 0.2 0.7 0.2 1.0))
        (gl:with-pushed-matrix* (:modelview)
          (rx)
          (loop for vao in (vaos w)
             for i from 0
             for count across (counts w)
             for tx = 2
             for d = 6
             for x = (mod i d)
             for y = (mod (floor i d) d)
             for z = (floor i (expt d 2))
             do
               (gl:with-pushed-matrix* (:modelview)
                 (incf tris count)
                 (gl:translate (* (- x (* d 0.5)) tx)
                               (* y tx)
                               (* (+ z (* d -0.5)) tx))

                 (gl:bind-vertex-array vao)
                 (gl:point-size 5)
                 (gl:disable :lighting)
                 (gl:color 0 1 0 0.4)
                 #++(gl:draw-elements :points (gl:make-null-gl-array :unsigned-short) :count count)
                 (if (wireframe w)
                     (progn
                       (gl:disable :lighting)
                       (gl:color 0 0 0 1)
                       (gl:polygon-mode :front-and-back :fill)

                       (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count count)
                       (gl:line-width 1.5)
                       (gl:color 0 1 0 1)
                       (gl:polygon-mode :front-and-back :line)
                       (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count count))
                     (progn
                       (gl:color 1 0 0 1)
                       (gl:enable :lighting)
                       (gl:polygon-mode :front-and-back :fill)
                       (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count count)))))))
      (setf *tris* tris))))


(defmethod glut:keyboard :after ((w ttf-tess-window) key x y)
  (declare (ignore x y) (optimize debug))
  (case key
    (#\1 (setf (wireframe w) (not (wireframe w))))
    (#\space (load-glyphs w))))

(defmethod glut:close :before ((w ttf-tess-window))
  (free-buffers w))

(defun ttf-tess ()
  (let ((w (make-instance 'ttf-tess-window
                          :font-path
                          "/usr/share/fonts/truetype/msttcorefonts/Georgia.ttf")))
    (unwind-protect
	 (basecode-run w)
      (glut:destroy-window (glut:id w)))))

;; (ttf-tess)
