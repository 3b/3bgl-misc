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
  ;;(sleep 0.1)
  (setf (basecode::clear-color w) (list 0.2 0.25 0.4 1.0))
  (setf *w* w)
  (3bgl-gpuanim::update-anim-state (num-instances w))
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
           (progn
             (gl:with-primitive :points
               (loop for i below 64
                     do (gl:vertex-attrib 4 i 0 0 1)
                        (gl:vertex-attrib 0 (* 0(/ i 64.0)) 0 0 1)))
             (gl:with-primitive :lines
               (loop for p in '(-1 0 1 2 3 2 5 6 7 8 7 10 11 7 2 14 15
                                16 17 16 19 20 16 1 23 24 25 24 1 28 29 30 29)
                     for i below 64 ;(1+ (mod (get-universal-time) 63))
                     when (plusp p)
                       do (gl:vertex-attrib 4 p 0 0 1)
                          (gl:vertex-attrib 0 0 0 0 1)
                          (gl:vertex-attrib 4 i 0 0 1)
                          (gl:vertex-attrib 0 0 0 0 1))))
         (clear-tex () :report "clear mesh"
           (setf (num-instances w) 0)))))))


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
         #++(positions (make-array (length nodes)))
         #++(orientations (make-array (length nodes)))
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
          #++(setf (aref positions i) (sb-cga:vec 0.0 0.0 0.0))
          #++(setf (aref orientations i) (kit.math::angle-axis->quaternion
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

(defun translate-ai-anim (anim map)
  (with-simple-restart (continue "skip loading")
    (flet ((ts (k) ;; timestamp of keyframe in ms
             (round (ai:key-time k) 1/1000))
           (qkeys (k)
             (let ((q (map '(vector (signed-byte 16) 4)
                           (lambda (a) (truncate (* 32767 a)))
                           (ai:value k))))
               ;;(rotatef (aref q 0) (aref q 1) (aref q 2) (aref q 3))
               ;(setf (aref q 0) (- (aref q 0)))
               q))
           (vkeys (k)
             (ai:value k)))
      ;; fixme: filter scale/translation keys
      ;; scale -> 1,1,1 if close, remove duplicates
      ;; translation -> remove duplicates if within 0.0001 or so
      ;;   (but keep key on each end of range of duplicates if keeping
      ;;    more than 1, so interpolation is correct
      (sort
       (loop for channel across (ai:channels anim)
             for channel-index = (gethash (ai:node-name channel) map)
             do (assert channel-index)
             collect (list channel-index
                           (map 'vector #'ts (ai:rotation-keys channel))
                           (map 'vector #'qkeys (ai:rotation-keys channel))
                           (map 'vector #'ts (ai:position-keys channel))
                           (map 'vector #'vkeys (ai:position-keys channel))
                           (map 'vector #'ts (ai:scaling-keys channel))
                           (map 'vector #'vkeys (ai:scaling-keys channel))))
       '< :key 'car))))
(defun load-files (w)
  ;; todo: handle multiple meshes + anim sets at once
  (let ((skeleton-map (make-hash-table :test 'equal)))
    ;; load mesh
    (setf (mesh w) (load-md5 *meshfile*))
    ;;   upload skeleton
    (upload-ai-skeleton 0 (mesh w) :name-map skeleton-map)
    ;; load anims
    ;;   upload anim data
    ;;   todo: handle files with no anim, check for anim in meshfile?
    (let ((anim-data
            (loop
              with i = 0
              for file in (nthcdr 7 *animfiles*)
              for scene = (load-md5 file)
              for anims = (ai:animations scene)
              append (loop
                       for anim across anims
                       for a = (mapcar 'cdr (translate-ai-anim anim skeleton-map))
                       when a
                         collect (list :index i :anim a
                                       :length (round (ai:duration anim)
                                                      1/1000)
                                       :channels (ai:channels anim)
                                       :name (list (ai:name anim)
                                                   (pathname-name file)))
                         and do (incf i)))))
      (3bgl-gpuanim:build-anim-data (loop for i in anim-data
                                          append (getf i :anim)))
      ;; create anim instances
      (3bgl-gpuanim:update-instance-data 0))

    ;; update (num-instances w)
    (setf (num-instances w) 1)))


(defmethod key-down ((w gpuanim-test) k)
  (with-simple-restart (continue "continue")
    (case k
      (:l
       (load-files w))
      ((:f12 :l1)
       (basecode::reset-freelook-camera w)))))

(setf 3bgl-shaders::*print-shaders* t)
;(setf 3bgl-shaders::*verbose* nil)

; (basecode-run (make-instance 'gpuanim-test     :width 1920 :height 1080))
