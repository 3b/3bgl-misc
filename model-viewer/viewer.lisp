(in-package #:3bgl-model-viewer)


(defclass model-viewer (basecode-glop perspective-projection basecode-clear
                        fps-graph basecode-draw-ground-plane
                        basecode-exit-on-esc
                        freelook-camera basecode::key-dumper)
  ((model :accessor model
          :initform nil)
   (filename :initarg :filename :accessor filename))
  (:default-initargs :look-at-eye '(5 12 -20)))


(defclass model ()
  ((scene :accessor scene :initform nil)
   (materials :accessor materials
              :initform (make-array 1 :fill-pointer 0 :adjustable t))))

(defun unload-model (model)
  (when model
    (loop for m across (materials model)
         do (unload-material m))))

(defun reload-model (w)
  (format t "loading ~s (~s) ~%" (filename w) (probe-file (filename w)))
  (let ((model (make-instance 'model)))
    (ai:with-log-to-stdout ()
      (setf (scene model)
            (ai:import-into-lisp
             (cffi-sys:native-namestring (filename w))
                   :processing-flags '(:ai-process-preset-target-realtime-quality))))
    #++(loop for m in (materials (scene model))
          do (vector-push-extend (load-material m)
                                 (materials model)))
    (unload-model (shiftf (model w) model))))

(defmethod basecode-draw ((w model-viewer))
  (unless (model w)
    (reload-model w))
  (gl:with-pushed-matrix* (:modelview)

    ))


(defmethod mouse-down ((w model-viewer) button x y)
  )

(defparameter *dbg* nil)
(defmethod key-down ((w model-viewer) key)
  (case key
    (:1 (setf *dbg* (model w)))
    (:v (format t "version = ~s.~s~%" (gl:major-version) (gl:minor-version)))
    ((#\z :z) (reload-model w)))
  )

(defmethod basecode::run-main-loop ((w model-viewer))
  (if (gl::features-present-p (or (>= :gl-version 4.2)
                                  (and (>= :gl-version 3)
                                       "EXT_bindable_uniform"
                                       "ARB_shading_language_420pack"
                                       ;; "ARB_texture_storage"?
                                       )))
      (when (next-method-p)
        (call-next-method))
      (format t "need at least GL version 4.2, or 3.3+ + various extensions")))

#++
(basecode-run
 (make-instance
  'model-viewer
  :filename (merge-pathnames
             #++ "src/assimp/test/models-nonbsd/X/dwarf.x"
             "src/assimp/test/models-nonbsd/Ogre/Assassine/Koerper.mesh.xml"
             (user-homedir-pathname))))


