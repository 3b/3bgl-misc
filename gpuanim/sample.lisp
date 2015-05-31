(defpackage #:gpuanim-test
  (:use :cl :basecode))
(in-package #:gpuanim-test)
(defclass gpuanim-test (basecode-glop perspective-projection basecode-clear
                        fps-graph basecode-draw-ground-plane
                        freelook-camera
                        basecode-exit-on-esc
                        basecode-shader-helper::basecode-shader-helper)
  ((vaos :accessor vaos :initform nil)
   (textures :accessor textures :initform (make-hash-table :test 'equalp))
   (instances :accessor instances :initform ())
   (programs :accessor programs :initform nil))
   (:default-initargs :look-at-eye '(3 2 15)
                      :projection-near 0.1 :projection-far 1000))



(defparameter *basedir* #P"d:/temp/models/arxeos/")
(defparameter *files*
  (loop for p in (loop for i in '("models/md5/characters/**/*.md5mesh"
                                  "models/md5/monsters/**/*.md5mesh"
                                  "models/md5/creatures/**/*.md5mesh")
                       append (directory (merge-pathnames i *basedir*)))
        for a = (directory (merge-pathnames "*.md5anim" p))
        unless (or
                ;; breaks something?
                (search "skeleton" (namestring p))
                ;; too many bones :/ ~68?
                (search "snake_woman" (namestring p))
                ;; no interesting anims?
                (search "cannon" (namestring p)))
          collect (list p a)))

#++
(defparameter *meshfile* #P"d:/temp/models/arxeos/models/md5/characters/npcs/knight/knight.md5mesh")
#++
(defparameter *animfiles* (directory "d:/temp/models/arxeos/models/md5/characters/npcs/knight/*.md5anim"))



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
                                      '3bgl-gpuanim-shaders::draw-skel-fs))
  (setf (program w :draw-anim)
        (3bgl-shaders::shader-program :vertex
                                      '3bgl-gpuanim-shaders::draw-anim-vs
                                      :fragment
                                      '3bgl-gpuanim-shaders::draw-anim-fs)))

(defun tex-image-tga (path)
  (let ((image (tga:read-tga path))
        (tn (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d tn)
    (unless (eql (tga:image-data image) 0)
     (gl:tex-image-2d :texture-2d 0 :rgba
                      (tga:image-width image) (tga:image-height image)
                      0
                      (ecase (tga:image-channels image)
                        (1 :luminance)
                        (3 :bgr)
                        (4 :bgra))
                      :unsigned-byte (tga:image-data image)))
    (gl:generate-mipmap :texture-2d)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
    (gl:bind-texture :texture-2d 0)
    tn))

(defun load-texture-file (path)
  (if (alexandria:ends-with-subseq ".TGA"
                                   (if (pathnamep path)
                                       (namestring path)
                                       path)
                                   :test 'equalp)
      (tex-image-tga path)
      (3bgl-opticl:tex-image-2d :texture-2d 9 :rgba
                                (opticl:read-image-file path))))

(defun bind-material (w mat)
  (flet ((texture (n)
           (+ n #.(cffi:foreign-enum-value '%gl:enum :texture0))))
    ;; clear textures
    (unless mat
      (loop for i from 4 downto 0
            do (gl:active-texture (texture i))
               (gl:bind-texture :texture-2d 0))
      (return-from bind-material nil))
    ;; try to bind textures
    (let ((files (gethash "$tex.file" mat)))
      (flet ((bind (unit key)
               (gl:active-texture (texture unit))
               (let* ((name (third (assoc key files)))
                      (path (merge-pathnames name *basedir*)))
                 (multiple-value-bind (tex found) (gethash name (textures w))
                   (unless found
                     (if (probe-file path)
                         (setf (gethash name (textures w))
                               (load-texture-file path))
                         (setf (gethash name (textures w)) 0)))
                   (gl:bind-texture :texture-2d (or tex 0))))))
        (bind 0 :ai-texture-type-diffuse)
        (bind 1 :ai-texture-type-normals)
        (bind 2 :ai-texture-type-specular)
        (bind 3 :ai-texture-type-height))
      (gl:active-texture (texture 0)))))

(defmethod basecode-draw ((w gpuanim-test))
  ;(sleep 0.01)
  (gl:enable :depth-test)
  (setf (basecode::clear-color w) (list 0.2 0.25 0.4 1.0))
  (setf *w* w)
  (3bgl-gpuanim::update-anim-state (length (instances w)))
  (let* ((p (program w :draw-anim))
         (lr 8)
         (irt ;12000
              (get-internal-real-time)
              )
         (lz (* lr (cos (/ irt 500))))
         (ly (+ 8 (* 16 (sin (/ irt 4444)))))
         (lx (* lr (sin (/ irt 500)))))
    (gl:point-size 5)
    (3bgl-shaders::with-program (p :error-p t)
      (3bgl-gpuanim::bind-anim-buffers)
      (setf (3bgl-shaders::uniform p
                                   '3bgl-gpuanim-shaders::draw-skel-skel-index)
            0)
      (setf (3bgl-shaders::uniform p '3bgl-gpuanim-shaders::diffuse-tex) 0)
      (setf (3bgl-shaders::uniform p '3bgl-gpuanim-shaders::normal-tex) 1)
      (setf (3bgl-shaders::uniform p '3bgl-gpuanim-shaders::specular-tex) 2)
      (setf (3bgl-shaders::uniform p '3bgl-gpuanim-shaders::light-pos)
            (list lx ly lz 1.0))
      (loop
        with n = (length (instances w))
        with sn = (ceiling (sqrt n))
        with space = 4.0
        for i below n
        for instance in (instances w)
        for x = (float (- (floor i sn) (/ (1- sn) 2.0)))
        for y = (float (- (mod i sn)  (/ (1- sn) 2.0)))
        do
           (setf (3bgl-shaders::uniform p
                                        '3bgl-gpuanim-shaders::draw-skel-anim-index)
                 (or (getf instance :instance) i))
           (basecode-shader-helper::mvp
            w p :m (sb-cga:matrix*
                    (sb-cga:rotate* (float (/ pi -2)
                                           1.0)
                                    0.0 0.0)
                    (sb-cga:translate* (* space x) (* space y) 0.0)
                    (sb-cga:scale* 0.05 0.05 0.05)))
           (restart-case
               (progn
                 #++
                 (gl:with-primitive :points
                   (loop for i below 33
                         do (gl:vertex-attrib 4 1 0 0 0)
                            (%gl:vertex-attrib-i4i 5 i 0 0 0)
                            (gl:vertex-attrib 0 0 0 0 1)))
                 #++
                 (gl:with-primitive :lines
                   (loop for p in '(-1 0 1 2 3 2 5 6 7 8 7 10 11 7 2 14 15
                                    16 17 16 19 20 16 1 23 24 25 24 1 28
                                    29 30 29)
                         for i below 64 ;(1+ (mod (get-universal-time) 63))
                         when (plusp p)
                           do (gl:vertex-attrib 4 1 0 0 0)
                              (%gl:vertex-attrib-i4i 5 p 0 0 0)
                              (gl:vertex-attrib 0 0 0 0 1)
                              (gl:vertex-attrib 4 1 0 0 0)
                              (%gl:vertex-attrib-i4i 5 i 0 0 0)
                              (gl:vertex-attrib 0 0 0 0 1)))
                 (loop with mats = (coerce (getf instance :materials) 'list)
                       for m in (getf instance :meshes)
                       for i below 4
                       for mat = (pop mats)
                       do (bind-material w mat)
                          (3bgl-mesh::draw m)))
             (clear-tex () :report "clear mesh"
               (setf (instances w) nil)))))))


(defmethod basecode-shader-helper::shader-recompiled ((w gpuanim-test) sp)
  (format t "~s modified~%" sp)
  (3bgl-gpuanim::%notice-modified-programs sp))

(defun load-md5 (filename
                 &key (flags '(:ai-process-preset-target-realtime-quality)))
  (assert (probe-file filename))
  (ai:with-log-to-stdout ()
    (ai:import-into-lisp filename
                         ;; need at least sort-by-p-type to remove types
                         :processing-flags (adjoin :ai-process-sort-by-p-type
                                                   flags)
                         ;; remove points and lines
                         :properties '(:pp-sbp-remove 3))))

#++
(defparameter *mesh* (load-md5 *meshfile*))
#++
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
      (make-instance
       '3bgl-gpuanim:anim-data
       :length-ms (floor (ai:duration anim) 1/1000)
       :anim-rate (ai:ticks-per-second anim)
       :bone-data
       (mapcar
        'cdr
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
         '< :key 'car))))))

(defparameter *foo* 0)
(defun load-files (w)
  (setf (instances w) nil)
  #++(when (plusp (hash-table-count (textures w)))
       (gl:delete-textures (alexandria:hash-table-values (textures w)))
       (clrhash (textures w)))
  (when (vaos w)
    (mapcar #'3bgl-mesh::free-mesh (shiftf (vaos w) nil)))
  ;; todo: share data if multiple mesh use same anims
  (loop
    with anim-count = 0
    for (meshfile animfiles) in *files*
    for skeleton-id from 0
    for skeleton-map = (make-hash-table :test 'equal)
    for mesh = (when animfiles (load-md5 meshfile))
    for vao = nil
    when animfiles
      do (upload-ai-skeleton skeleton-id mesh :name-map skeleton-map)
         (setf vao (3bgl-mesh::ai-mesh mesh skeleton-map))
      and append vao into vaos
      and append
          (loop
            for file in (cons meshfile animfiles)
            for scene = (load-md5 file)
            for anims = (ai:animations scene)
            when anims
              append (loop
                       for anim across anims
                       for a = (translate-ai-anim anim skeleton-map)
                       for name = (list (ai:name anim) (pathname-name file))
                       when a
                         collect (list :anim-index anim-count
                                       :skeleton-index skeleton-id
                                       :anim a
                                       :name name
                                       :mesh mesh
                                       :vaos vao)
                         and do (incf anim-count)
                                (setf (3bgl-gpuanim::name a) name)))
      into anim-data
    finally
       (3bgl-gpuanim:build-anim-data (loop for i in anim-data
                                           collect (getf i :anim)))
       ;; create anim instances
       (setf (instances w)
             (loop
               for r below 16
               append
               (loop for i from (* r (length anim-data))
                     for a in anim-data
                     for vao = (getf a :vaos)
                     for mesh = (getf a :mesh)
                     for anim = (getf a :anim-index)
                     for skel = (getf a :skeleton-index)
                     do (3bgl-gpuanim:update-instance-data i
                                                           :skeleton-index skel
                                                           :anim-index anim)
                     collect (list :instance i :meshes vao
                                   :materials (ai:materials mesh)))))
       (setf (vaos w) vaos)))


(defmethod key-down ((w gpuanim-test) k)
  (with-simple-restart (continue "continue")
    (case k
      (:l
       (load-files w))
      (:f9
       )
      ((:f12 :l1)
       (basecode::reset-freelook-camera w)))))

(setf 3bgl-shaders::*print-shaders* t)
;(setf 3bgl-shaders::*verbose* nil)

; (basecode-run (make-instance 'gpuanim-test :width 1920 :height 1080))

