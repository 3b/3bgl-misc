(defpackage #:gpuanim-test
  (:use :cl :basecode))
(in-package #:gpuanim-test)
(defclass gpuanim-test (basecode-glop perspective-projection basecode-clear
                        fps-graph basecode-draw-ground-plane
                        freelook-camera
                        basecode-exit-on-esc
                        basecode-shader-helper::basecode-shader-helper)
  ((mesh :accessor mesh :initform nil)
   (num-instances :accessor num-instances :initform 0)
   (programs :accessor programs :initform nil))
   (:default-initargs :look-at-eye '(3 2 15) :projection-near 0.1))

(defparameter *w* nil)

(defmethod program ((s gpuanim-test) program-name)
  (getf (programs s) program-name))

(defmethod (setf program) (program (s gpuanim-test) program-name)
  (setf (getf (programs s) program-name) program))


(defmethod run-main-loop :around ((w gpuanim-test))
  (3bgl-gpuanim:with-gpu-anim ()
    (call-next-method)))

(defmethod run-main-loop :before ((w gpuanim-test))
  ;; can't do this in :around since it depends on some stuff that happens in
  ;; later :around methods :/
  (setf (program w :draw-skel)
        (3bgl-shaders::shader-program :vertex
                                      '3bgl-gpuanim-shaders::draw-skel-vs
                                      :fragment
                                      '3bgl-gpuanim-shaders::draw-skel-fs)))

(defmethod basecode-draw ((w gpuanim-test))
  (setf (basecode::clear-color w) (list 0.2 0.25 0.4 1.0))
  (setf *w* w)
  (let ((p (program w :draw-skel)))
    (gl:point-size 10)
    (3bgl-shaders::with-program (p :error-p t)
      (3bgl-gpuanim::bind-anim-buffers)
      (basecode-shader-helper::mvp
       w p :m (sb-cga:matrix*
               (sb-cga:translate* -1.0 1.0 0.0)
               (sb-cga:rotate* (float (/ pi -2)
                                      1.0)
                               0.0 0.0)
               (sb-cga:scale* 0.1 0.1 0.1)))
      (setf (3bgl-shaders::uniform p
                                   '3bgl-gpuanim-shaders::draw-skel-skel-index)
            0)
      (setf (3bgl-shaders::uniform p
                                   '3bgl-gpuanim-shaders::draw-skel-anim-index)
            0)
      (when (plusp (num-instances w))
       (restart-case
           (gl:with-primitive :points
             (loop for i below 64
                   do (gl:vertex-attrib 4 i 0 0 1)
                      (gl:vertex-attrib 0 (* 0(/ i 64.0)) 0 0 1)))
         (clear-tex () :report "clear mesh"
           (setf (num-instances w) 0))))
      
      ))
  
)


(defmethod basecode-shader-helper::shader-recompiled ((w gpuanim-test) sp)
  (format t "~s modified~%" sp)
  (3bgl-gpuanim::%notice-modified-programs sp))


(defparameter *meshfile* #P"d:/temp/models/arxeos/models/md5/characters/npcs/knight/knight.md5mesh")
(defparameter *animfiles* (directory "d:/temp/models/arxeos/models/md5/characters/npcs/knight/*.md5anim"))

(defun load-md5 (filename
                 &key (flags '(:ai-process-preset-target-realtime-quality)))
  (assert (probe-file filename))
  (ai:with-log-to-stdout ()
    (ai:import-into-lisp filename :processing-flags flags)))

(defparameter *mesh* (load-md5 *meshfile*))
(defparameter *anims* (mapcar 'load-md5 *animfiles*))

(defun dump-node (x &key (depth 0))
  (format t "~&")
  (loop repeat depth do (format t " "))
  (format t "~s~%" (ai:name x))
  (loop for i across (ai:children x)
        do (dump-node i :depth (1+ depth))))
#++(dump-node (ai:root-node *mesh*))
#++ (loop for i in *anims* do (dump-node (ai:root-node i)))

(defun flatten-skeleton (node &key top)
  (let ((nodes nil))
    (labels ((r (x &optional dump)
               (when dump
                 (push x nodes))
               (loop with dump-children = (or dump (string= top (ai:name x)))
                     for i across (ai:children x)
                     do (r i dump-children))))
      (r node (not top))
      (nreverse nodes))))
#++
(mapcar 'ai:name (flatten-skeleton (ai:root-node *mesh*)
                                   :top "<MD5_Hierarchy>"))

(defun upload-ai-skeleton (index scene
                           &key (include-children-of "<MD5_Hierarchy>")
                             name-map)
  (let* ((nodes (flatten-skeleton (ai:root-node scene)
                                  :top include-children-of))
         (parents (make-array (length nodes)))
         (positions (make-array (length nodes)))
         (orientations (make-array (length nodes)))
         (local-matrices (make-array (length nodes)))
         (inverse-bind-matrices (make-array (length nodes)))
         (name-map (or name-map (make-hash-table :test 'equal)))
         (global-matrices (make-array (length nodes))))
    (clrhash name-map)
    (loop for i from 0
          for name in (mapcar 'ai:name nodes)
          do (setf (gethash name name-map) i))
    ;; translate data
    (loop for i from 0
          for node in nodes
          for name = (ai:name node)
          for parent = (ai:parent node)
          for parent-index = (when parent
                               (gethash (ai:name parent) name-map))
          do (setf (aref parents i) (or parent-index -1))
             ;; fixme: calculate a better value or upload matrix instead
             (setf (aref positions i) (sb-cga:vec 0.0 0.0 0.0))
             (setf (aref orientations i) (kit.math::angle-axis->quaternion
                                          0 (sb-cga:vec 1.0 0.0 0.0)))
             (setf (aref local-matrices i) (sb-cga:transpose-matrix
                                            (ai:transform node)))
             (setf (aref global-matrices i) (sb-cga:matrix*
                                             (if parent-index
                                                 (aref global-matrices
                                                       parent-index)
                                                 (sb-cga:identity-matrix))
                                             (aref local-matrices i)))
             (setf (aref inverse-bind-matrices i) (sb-cga:inverse-matrix
                                                   (aref global-matrices i))))
    ;; upload to gpu
    (3bgl-gpuanim:update-skeleton index parents inverse-bind-matrices
                                  local-matrices)))

(defun upload-ai-anim (index scene map
                       &key (include-children-of "<MD5_Hierarchy>")))

(defun load-files (w)
  ;; todo: handle multiple meshes + anim sets at once
  (let ((skeleton-map (make-hash-table :test 'equal)))
    ;; load mesh
    (setf (mesh w) (load-md5 *meshfile*))
    ;;   upload skeleton
    (upload-ai-skeleton 0 (mesh w) :name-map skeleton-map)
    ;; load anims
    ;;   upload anim data
    #++
    (loop for i from 0
          for file in *animfiles*
          for scene = (load-md5 file)
          do (upload-ai-anim i scene skeleton-map))
    ;; create anim instances
    ;; update (num-instances w)
    (setf (num-instances w) 1)
    )
)



(defmethod key-down ((w gpuanim-test) k)
  (with-simple-restart (continue "continue")
    (case k
      (:l
       (load-files w))
      ((:f12 :l1)
       (basecode::reset-freelook-camera w)))))

(setf 3bgl-shaders::*print-shaders* t)
;(setf 3bgl-shaders::*verbose* nil)

; (basecode-run (make-instance 'gpuanim-test))
