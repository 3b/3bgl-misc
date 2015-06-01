(cl:in-package #:3bgl-gpuanim-shaders)


;; vertex attributes
(input position :vec4 :location 0)
(input uv :vec4 :location 1)
(input normal :vec3 :location 2)
(input tangent :vec3 :location 3)
(input bone-weights :vec4 :location 4)
(input bone-indices :ivec4 :location 5)
(input bitangent :vec3 :location 6)

(input color :vec4 :location 10)

;;; uniforms
;; matrices
(uniform m :mat4 :stage :vertex)
(uniform v :mat4 :stage :vertex)
(uniform p :mat4 :stage :vertex)
(uniform mv :mat4 :stage :vertex)
(uniform mvp :mat4 :stage :vertex)
(uniform normal-matrix :mat4 :stage :vertex)
;; lights
(uniform light-pos :vec4 :stage t)
(uniform eye-pos :vec3 :stage t)
;; textures
(uniform diffuse-tex :sampler-2d :stage :fragment)
(uniform specular-tex :sampler-2d :stage :fragment)
(uniform normal-tex :sampler-2d :stage :fragment)
;; debugging
(uniform skip-inv-bind-matrix :bool :stage :compute :default false)

;; varyings
(interface varyings (:out (:vertex outs)
                     :in (:fragment ins :geometry (ins "ins" :*) ))
  (position :vec4)
  (normal :vec3)
  (tangent :vec3)
  (bitangent :vec3)
  (color :vec4)
  (uv :vec4)
  (eye-direction :vec3)
  (light-direction :vec3))


;; fragment outputs
(output color :vec4 :stage :fragment)


(uniform time-ms :uint)

;; each row is initialization data for 1 instance
;; #, skeletonid,anim id, start time, bone mapping
(uniform anim-instance-init :image-2d :location 0
                                      :layout (:r32i t)
                                      :qualifiers (:restrict))

;; ssbo for bones (constants hard-coded for now)
#++(uniform max-bone :int)
#++(uniform max-bone-models :int)
;; (defconstant +max-bones+ 64 :int)
;;(defconstant +max-active-anims+ 4096 :int)

;; data for state of a particular running anim
;; fixme: add std430 etc to struct options?
;; may want to split out the parts written by GPU to reduce conflicts
;;   when updating CPU parts?
(defstruct anim-instance-data
  (skeleton :uint) ;; index of skeleton data in skeletons
  (anim :uint) ;; index of anim data  in anims
  (start-time :uint) ;; ms, hopefully don't need to care about 49+day uptimes
  (state :uint) ;; 0 = ?, 1 = before start, 2 = playing, 3 = after end?, 4=done
  (bone-map :int) ;; index of mapping of anim bones to skeleton bones (-1 = 1:1)
  ;; last-keyframe = rot,pos,scale,flag
  ;;  flag = 1 = bone no longer needs updated (all channels past last keyframe)
  (last-keyframe (:uvec4 64)) ;; index of last keyframe for each bone
  ;; this should probably be indexed by skeleton bones rather than anim bones?
  (matrices (:mat4 64)))

;; data for a specific anim
(defstruct anim-data
  (length :uint) ;; in ms
  (num-bones :uint)
  (keys (:uint 64)) ;; indices into anim-metadata
)

(defstruct bone-map
  (map (:int 64)))

(defstruct skeleton-data
  ;; possibly should split some of this up for better alignment/packing?
  ;; (might be enough to just switch to shared layout since host code
  ;;  looks up offsets?)
  ;; fixme: fix dependencies on constants in array size, and use a constant
  (max-depth :uint) ;; max depth of hierarchy
  (depth (:uint 64)) ;; depth of bone in hierarchy (for parallel updates)
  (parent (:int 64)) ;; -1 = none? or point to itself?
  ;; default local transform to use when bone has no animation keyframes
  (local-matrix (:mat4 64))
  ;; inverse bind-pose transform
  (inverse-bind-matrix (:mat4 64)))

(defstruct anim-metadata1
  (quat-count :uint)
  (quat-timestamp-offset :uint)
  (quat-offset :uint)
  (pos-count :uint)
  (pos-timestamp-offset :uint)
  (pos-offset :uint)
  (scale-count :uint)
  (scale-timestamp-offset :uint)
  (scale-offset :uint))

(interface -anim-instances (:buffer t :layout (:binding 0 :std430 t))
  (anim-instances (anim-instance-data :*)))

(interface -anims (:buffer t :layout (:binding 1 :std430 t))
  (anims (anim-data :*)))

(interface -skeletons (:buffer t :layout (:binding 2 :std430 t))
  (skeletons (skeleton-data :*)))

(interface -anim-metadata (:buffer t :layout (:binding 3 :std430 t))
  (anim-metadata (anim-metadata1 :*)))

(interface -quats (:buffer t :layout (:binding 4 :std430 t))
  (quats (:uvec2 :*)))

;; vec3 gets padded to 4 floats, so use a 3 element struct instead
(defstruct xyz
  (x :float) (y :float) (z :float))

(interface -vectors (:buffer t :layout (:binding 5 :std430 t))
  (vectors (xyz :*)))

(interface -timestamps (:buffer t :layout (:binding 6 :std430 t))
  (timestamps (:uint :*)))

(interface -anim-bone-map (:buffer t :layout (:binding 7 :std430 t))
  (anim-bone-map (bone-map :*)))

(defun read-timestamp (x)
  (let* ((l (logand x 1))
         (h (uint (/ x 2)))
         (i (aref timestamps h)))
    (return (uint (bitfield-extract i (* l 16) 16)))))

(defun read-quat (x)
  (let* ((v (aref quats x))
         (wa (unpack-snorm-2x16 (.x v)))
         (bc (unpack-snorm-2x16 (.y v))))
    ;; fixme: verify order?
    ;; x = low 16 bits, y = high
    (return (vec4 (.xy wa) (.xy bc)))))

(defun read-vec (x)
  (let ((v (aref vectors x)))
    (return (vec3 (@ v x) (@ v y) (@ v z)))))



(defun quat-matrix (q)
  ;; todo: try http://www.euclideanspace.com/maths/geometry/rotations/conversions/quaternionToMatrix/jay.htm ?

  ;; todo: benchmark various versions and see which is faster
  #++
  (let ((qq (* q q))
        (ab (* 2 (.x q) (.y q)))
        (ac (* 2 (.x q) (.z q)))
        (ad (* 2 (.x q) (.w q)))
        (bc (* 2 (.y q) (.z q)))
        (bd (* 2 (.y q) (.w q)))
        (cd (* 2 (.z q) (.w q))))
    (return
      (mat4 (- (+ (.x qq) (.y qq)) (.z qq) (.w qq))
            (+ ad bc)
            (- bd ac)
            0.0

            (- bc ad)
            (- (+ (.x qq) (.z qq)) (.y qq) (.w qq))
            (+ ab cd)
            0.0

            (+ ac bd)
            (- cd ab)
            (- (+ (.x qq) (.w qq)) (.y qq) (.z qq))
            0.0

            0.0 0.0 0.0 1.0)))

  ;; attempted optimized version
  (let* ((qq (* q q))
         (q2 (* (vec3 2) (.xyz q)))
         (aq (* (.xxx q2) (.yzw q)))
         (bc (* (.yyz q2) (.zww q))))
    (return
      (mat4 (- (+ (.x qq) (.y qq)) (.z qq) (.w qq))
            (+ (.z aq) (.x bc))
            (- (.y bc) (.y aq))
            0.0

            (- (.x bc) (.z aq))
            (- (+ (.x qq) (.z qq)) (.y qq) (.w qq))
            (+ (.x aq) (.z bc))
            0.0

            (+ (.y aq) (.y bc))
            (- (.z bc) (.x aq))
            (- (+ (.x qq) (.w qq)) (.y qq) (.z qq))
            0.0

            0.0
            0.0
            0.0
            1.0))))

(defun interpolate-keyframe (time offset count last)
  ;; find keyframes surrounding TIME in COUNT timestamps starting from OFFSET
  ;; (using LAST (0..COUNT) as starting point)
  ;; return (vec2 new-last mix)
  ;; where mix = 0..1, 0 = keyframe NEW-LAST, 1=keyframe (1+ NEW-LAST)
  (when (<= count 1)
    (return (vec2 0 0)))
  (let* ((now (read-timestamp (int (+ offset last))))
         (next now)
         (mix 0.0))
    (when (< last (1- count))
      (setf next (read-timestamp (int (+ offset (1+ last))))))
    (when (>= time next)
      (3bgl-glsl::%for
       (()
        ((and (>= time next)
              (< (1+ last) count)))
        ())
       (incf last)
       (setf now next)
       (when (< last (1- count))
         (setf next (read-timestamp (int (+ offset (1+ last))))))))
    (unless (= time now)
      (setf mix  (/ (float (- time now))
                    (float (- next now)))
            ))
    (return (vec2 last mix)))
  )

(defun nqlerp (x y f)
  ;; try to stabilize things a bit compared to straight mix
  ;; (i think negating it when they point away from eachother is
  ;;  valid, but can't find any justification at the moment... seems
  ;;  to work though so good enough for now)
  (if (< (dot x y) 0.0)
      (return (normalize (mix x (- y) f)))
      (return (normalize (mix x y f)))))

(defun update-bone (bone time instance-id anim skel)
  (declare (anim-data anim) (:uint instance-id)
           (skeleton-data skel))
  ;; update an individual bone:
  ;;   find prev/next key for pos/rot/scale
  ;;   find interpolation amount for each
  ;;   find interpolated values
  ;;   mix into matrix
  ;;   store matrix
  (symbol-macrolet ((instance (aref anim-instances instance-id))
                    #++(anim (aref anims (@ anim-instance anim)))
                    #++(skeleton (aref skeletons (@ anim-instance skeleton)))
                    #++(anim-time (- time-ms (@ anim-instance start-time)))
                    (last (aref (@ instance last-keyframe) bone)))
    (let ((key-data (aref anim-metadata
                          #++(+ bone (* instance-id 33))
                          (aref (@ anim keys) bone)))
         (matrix (mat4 1)) ;; identity matrix
         (done 1))
      (when (= (.w last) 1)
       (return 1))
      ;; fixme: decide if this is correct order, or if we need to be
      ;; able to reorder them?
      ;; translation
;    ...
;    todo:use local-matrix as default instead of bone-position/bone-orientation
;    ...

      (let* ((position (vec3 0) #++(aref (@ skel bone-positions) bone))
             (orientation (vec4 1 0 0 0)
                          #++(aref (@ skel bone-orientations) bone)))
        timestamps
        ;; todo: get rid of this once times actually match anim state
        (progn ;when (< time 0.1)
          (setf last (ivec4 0)))
        (unless (= 0 bone) ;; 0 is position, so skip it to run/walk in place
          (let* ((c (@ key-data pos-count))
                 (timestamps-offset (@ key-data pos-timestamp-offset))
                 (keys-offset (@ key-data pos-offset))
                 (interp (interpolate-keyframe time timestamps-offset c
                                               (.y last)))
                 (nl (int (.x interp)))
                 (pk (if (< nl (1- c))
                         (mix (read-vec (+ keys-offset nl))
                              (read-vec (+ keys-offset (1+ nl)))
                              (.y interp ))
                         (read-vec (+ keys-offset nl
                                      )))))
            (setf (.y last) nl)
            (unless (= 0 (dot pk pk))
              (setf position pk))))

        (let* ((c (@ key-data quat-count))
               (timestamps-offset (@ key-data quat-timestamp-offset))
               (keys-offset (@ key-data quat-offset))
               (interp (interpolate-keyframe time timestamps-offset c
                                             (.x last)
                                             ))
               (nl (int (.x interp)))
               (pk (if (< nl (1- c))
                       (nqlerp (read-quat (+ keys-offset nl))
                               (read-quat (+ keys-offset (1+ nl)))
                               (.y interp))
                       (read-quat (+ keys-offset nl)))))
          (setf (.x last) nl)
          (setf orientation pk)
          )

        ;; todo: scale
        ;; calculate local transform from pos/orientation
        ;; fixme: adjust for bone-map
        (setf (aref (@ instance matrices) bone)
              #++(aref (@ skel local-matrix) bone)
              (* (mat4 (vec4 1 0 0 0) (vec4 0 1 0 0) (vec4 0 0 1 0)
                       (vec4 position 1.0))
                 (quat-matrix
                  orientation)))))))


(defun update-anim ()
  (declare (layout (:in nil :local-size-x 64)))
  anim-bone-map
  anim-instances skeletons
  (let* ((bone (.x gl-global-invocation-id))
         (anim-instance-id (.y gl-global-invocation-id)))
    (symbol-macrolet ((anim-instance (aref anim-instances anim-instance-id))
                      (anim (aref anims (@ anim-instance anim)))
                      (skeleton (aref skeletons (@ anim-instance skeleton)))
                      (anim-time (- time-ms (@ anim-instance start-time))))
      (when (<= bone (@ anim num-bones))
        (update-bone bone (if (< time-ms 60000)
                              (float time-ms)
                              (/ (float (mod (+ time-ms
                                                (* anim-instance-id 1))
                                             (@ anim length)))
                                 1))
                     anim-instance-id anim skeleton))
      ;; loop over depth of tree accumulating global transforms
      (let ((parent (aref (@ skeleton parent) bone))
            (depth (aref (@ skeleton depth) bone)))
        ;; fixme: figure out better max value?
        ;; have some skeletons with depth 11 or so, need to look at more
        ;; (or maybe just go up to 64?)
        ;; not sure if (@ skeleton max-depth) is "uniform flow control" or not

        (dotimes (i 16 #++(min 16 (1+ (@ skeleton max-depth))))
          (barrier)
                                        ;(group-memory-barrier)
                                        ;(memory-barrier-buffer)
          (when (and (= i depth)
                     ;; we can't skip outer loop, size we need to
                     ;; hit the (barrier) in every thread
                     (>= parent 0))
            (setf (aref (@ anim-instance matrices) bone)
                  (* (aref (@ anim-instance matrices) parent)
                     (aref (@ anim-instance matrices) bone)))))
        (barrier)
        (unless skip-inv-bind-matrix
          (setf (aref (@ anim-instance matrices) bone)
                (* (aref (@ anim-instance matrices) bone)
                   (aref (@ skeleton inverse-bind-matrix) bone))))))))


(uniform draw-skel-anim-index :int) ;; index in -anim-instances
(uniform draw-skel-skel-index :int) ;; index in -skeletons

(defun draw-skel-vs ()
  (declare (values))
  ;; vertex = pos, bone index
  ;; output = mvp * (bones[index]*vec4(0,0,0))
  (let* ((i (int (.x bone-weights)))
         (s (aref skeletons draw-skel-skel-index))
         (ai (aref anim-instances draw-skel-anim-index)))
    draw-skel-skel-index
    (macrolet ((w (xyz)
                 `(* (aref (@ ai matrices) (,xyz bone-indices))
                     position
                     (,xyz bone-weights))))
      (setf gl-position
            (* mvp (+ (w .x)
                      (w .y)
                      (w .z)
                      (w .w)))))
    (setf (@ outs color) (vec4
                          (/ (.x bone-indices) 33.0)
                          (/ (.y bone-indices) 33.0)
                          ; (/ (float draw-skel-anim-index) 16)
                          ;#.(random 1.0)
                          ;#.(random 1.0)
                          (+ 0.5 #. (random 0.5))
                          1))))


(defun draw-skel-fs ()
  (declare (values))
  (setf color (@ ins color)))

(defun read-bone-a (i)
  (return
    (aref (@ (aref anim-instances draw-skel-anim-index) matrices)  i)))

(defun draw-anim-vs ()
  (declare (values))
  light-pos bone-weights bone-indices
  (let* ((xn (normalize (* (mat3 normal-matrix) normal)))
         (xt (normalize (* (mat3 normal-matrix) tangent)))
         #++(xb (normalize (* (cross xn xt) (vec3 (.z uv)))))
         (xb (normalize (* (mat3 normal-matrix) bitangent)))
         (bone-position
           #++(* (read-bone-a (.x bone-indices)) position)

           (+ (* (.x bone-weights) (* (read-bone-a (.x bone-indices)) position))
              (* (.y bone-weights) (* (read-bone-a (.y bone-indices)) position))
              (* (.z bone-weights) (* (read-bone-a (.z bone-indices)) position))
              (* (.w bone-weights) (* (read-bone-a (.w bone-indices)) position)))))
    (setf gl-position (* mvp bone-position))
    ;(setf gl-position (* mvp position))

    v
    uv
    (setf (@ outs color)
          ;(vec4 .bone-weights 1)
          (vec4 ;(/ (.xyz bone-indices) 43.0)
                                           (/ (float (.x bone-indices)) 43)
                                           (/ (float (.x bone-indices)) 21)
                                           (/ (float (.x bone-indices)) 10)

                                           1
                                           )
                                        ;color
          (@ outs uv) (vec4 (.xy uv) 0.0 0.0)
          (@ outs normal) xn
          (@ outs tangent) xt
          (@ outs bitangent) xb
          (@ outs light-direction) (.xyz light-pos)
          #++(vec3 (- (* v light-pos)
                      (* v m position)))
          (@ outs eye-direction) (vec3 (* mv position))
                                 )))

(defun draw-anim-fs ()
  (declare (values))
  normal-tex light-pos
  (let* ((uv (vec2 (@ ins uv)))
         (dtex #++(vec4 1 1 1 1)
               (texture-2d diffuse-tex uv))
         (stex #++(vec4 1 1 1 1)
               (texture-2d specular-tex uv))
         (ntex (normalize (- (* 2.0 (.rgb (texture-2d normal-tex uv)))
                             1.0)))
         (distance (length (@ ins light-direction)))
         (one-over-d-squared (/ (* distance distance)))
         (tangent-space
           (transpose
            (mat3 (normalize (@ ins tangent))
                  (normalize (@ ins bitangent))
                  (normalize (@ ins normal)))))
         (n (normalize ntex)
            #++(normalize (@ ins normal)))
         (l (normalize (* tangent-space (@ ins light-direction))))
         #++(l (normalize (@ ins light-direction)))
         (e (normalize (@ ins eye-direction))
            #++(normalize (* tangent-space (@ ins eye-direction))))
         (r (reflect l n))
         (cos-theta (clamp (dot n l) 0.0 1.0))
         (cos-alpha (clamp (dot e r) 0.0 1.0))
         ;; attenuate specular reflections from fragments facing away
         ;; from light in case normal map points back towards light
         (spec-back-falloff (smooth-step -0.8 0.0 (dot (- e) l))))

    (setf color
          (vec4
           #++(* tangent-space (vec3 0 0 1))
           #++(.xyz ntex)
           #++(vec3 (* 0.2 (length (.xyz (@ ins eye-direction)))))
           #++(vec3 cos-theta)
           #++(vec3 (if (< distance 1) 1 0))
           #++(+ 0.5 (/ (.yyy l) 2))
           #++(vec3 (- spec-back-falloff 0.9))
           #++(vec3 (* 8 one-over-d-squared))
           #++(* 0.1 (vec3 distance))
           #++(vec3 (@ ins bitangent))
           #++(.xyz  (@ ins color))
           (+ (* spec-back-falloff
                 (.xyz dtex) cos-theta
                 ;; 32 one-over-d-squared
                 ;; fixme: uniforms for light colors...
                 (vec3 1 1 1))
              #++(* (.xyz dtex) (clamp (dot n e) 0 1))
              (* (.xyz dtex) 0.1) ;; ambient
              0
              (* spec-back-falloff
                 (* (.xyz stex)
                                        ;(vec3 0.8 0.3 0.8) ; light color
                                        ;5.0 one-over-d-squared
                    (pow cos-alpha (* 16.0 (.a stex))))))
           1.0))))
