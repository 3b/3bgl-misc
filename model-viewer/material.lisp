(in-package #:3bgl-model-viewer)

(defclass material ()
  ())

(defmethod bind-material (m)
  )

(defclass ai-material ()
  ((ambient :accessor ambient :initform (v4 0.1 0.5 0.1) :initarg :ambient)
   (diffuse :accessor diffuse :initform (v4 0.9 0.1 0.9) :initarg :diffuse)
   (specular :accessor specular :initform (v4 0.0 0.8 0.0) :initarg :specular)
   (emissive :accessor emissive :initform (v4 0.0 0.0 0.0) :initarg :emissive)
   (textures :accessor textures :initform nil :initarg :textures)))

(defun load-material (m)
  (let ((r (make-instance
            'material
            :ambient (v4v (gethash "$clr.ambient" m #(0.2 0.2 0.2 1.0)))
            :diffuse (v4v (gethash "$clr.diffuse" m #(0.9 0.2 0.9 1.0)))
            :specular (v4v (gethash "$clr.specular" m #(0.1 0.9 0.1 1.0)))
            :emissive (v4v (gethash "$clr.emissive" m) nil))))
    ;; not sure if we should 'disable' things with NIL or just #(0 0 0 0)?
    (when (or (eq :ai-shading-mode-gouraud (gethash "$mat.shadingm" m))
              (not (gethash "$mat.shininess" m))
              (zerop (gethash "$mat.shininess" m)))
      (setf (specular r) nil))
    (let* ((tex (gethash "$tex.file" m))
           (tex-name (getf (cdddr (assoc :ai-texture-type-diffuse tex))
                           :texture-name)))
      (when *dump* (format t "tex = ~s / ~s~%" tex tex-name))
      (if tex-name
          (progn
            (gl:enable :texture-2d)
            (gl:bind-texture :texture-2d tex-name)
            (let ((uvx (car (gethash "$tex.uvtrafo" material))))
              (gl:matrix-mode :texture)
              (gl:load-identity)
              (when *invert-texture-v* (gl:scale 1.0 -1.0 1.0))
              (when uvx
                ;; not sure about order of these...
                (destructuring-bind (type index (x s r)) uvx
                  (declare (ignore type index))
                  (gl:translate (aref x 0) (aref x 1) 0.0)
                  (gl:scale (aref s 0) (aref s 1) 1.0)
                  (gl:translate 0.5 0.5 0.0)
                  (gl:rotate (- (* (/ 180 pi) r)) 0.0 0.0 1.0)
                  (gl:translate -0.5 -0.5 0.0)))
              (gl:matrix-mode :modelview)))
          (gl:disable :texture-2d)))
    (setf ))
  )

(defun unload-material (material)
  (loop for i in (textures material)
        do (gl:delete-textures (list i))))

