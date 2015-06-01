(in-package #:3bgl-gpuanim)

;; these are actually constants in the shaders for the moment.
(defparameter *max-bone* 64)
(defparameter *max-bone-models* 1024)

;; runtime state, probably needs valid GL context
(defclass gpu-anim-state ()
  ((programs :accessor programs :initform nil) ; plist: name -> program
   (buffers :accessor buffers :initform nil)) ; plist: name -> data?
  )
(defmethod program ((s gpu-anim-state) program-name)
  (getf (programs s) program-name))

(defmethod (setf program) (program (s gpu-anim-state) program-name)
  (setf (getf (programs s) program-name) program))


(defmethod buffer ((s gpu-anim-state) buffer-name)
  (getf (buffers s) buffer-name))

(defmethod (setf buffer) (buffer (s gpu-anim-state) buffer-name)
  (setf (getf (buffers s) buffer-name) buffer))

(defparameter *gpu-anim-state* nil) ;; gpu-anim-state

(defparameter *anim-state-buffers* nil)

;; debugging, specify anim time
(defparameter *hack-time* nil)


;; input = sequence of anim data for 1 bone
;;  anim data for 1 bone =
;;     sequence of timestamp + quat
;;     sequence of timestamp + pos
;;     sequence of timestamp + scale
;; output = sequence of anim metadata for corresponding bones
;;   metadata for 1 bone =
;;       count + timestamp offset + quat offset
;;       count + timestamp offset + pos offset
;;       count + timestamp offset + scale offset
;; processing
;;   loop through entries, for each of quat/pos/scale
;;     extract timestamps and data as separate sequences
;;      - or maybe just count elements?
;;      (for quat, convert to s16?)
;;     (optional: find previous instances of same sequence
;;        either in an equalp hash table, or by hashing contents
;;        and looking up in input data?)
;;     allocate GPU space for data, copy to GPU, tracking start/counts
;;     return start/counts
;;  if possible, should reuse matching data
;;    for example most scale will be single 1,1,1 key at 0 time
;; ---
;; probably just upload metadata to GPU also, return 2 SSBO IDs?
;; probably also send input as soa (seq of tx + seq of keys) instead
;;   of aos, so we can just hash them directly to find dupe?
(defclass anim-data ()
  ((length-ms :accessor length-ms :initarg :length-ms :initform 0)
   ;; sequence of lists of 6 sequences (1 6 element list per bone):
   ;;     quaternion key timestamps (integer ms)
   ;;     quaternion keys (as 4 element (signed-byte 16) vectors)
   ;;     position key timestamps (integer ms)
   ;;     position keys (sb-cga:vec)
   ;;     scale key timestamps (integer ms)
   ;;     scale keys (sb-cga:vec)
   ;;  timestamp seq and corresponding key seq should be same length
   ;;  but length may be 1 for a given pair, and distinct pairs can be
   ;;  different length (pos and scale will tend to be length 1, with
   ;;  appropriate 'identity' value for the single key)
   ;;  - bones with no animation can have NIL or list of 6 NILS
   (bone-data :accessor bone-data :initarg :bone-data :initform nil)
   ;; not sure if these are needed?
   (anim-rate :accessor anim-rate :initarg :anim-rate :initform 30)
   (name :accessor name :initarg :name :initform "")))

(defun build-anim-data (anim-data &key (state *gpu-anim-state*))
  ;; anim-data is sequence of ANIM-DATA instances

  ;; todo: find duplicates of subsequences of keyframes/timestamps?
  ;;  (more likely just need to worry about prefixes, since we
  ;;   always start from 0)
  ;;  probably need to do separate hashing rather than using hash table
  ;;    and possibly extra pass to determine which size prefixes are
  ;;    worth hashing?
  ;;    = find all lengths used, hash whole seq, if not found add and hash
  ;;      prefixes of shorter lengths and add those as well


  (let ((ts-hash (make-hash-table :test 'equalp))
        (quat-hash (make-hash-table :test 'equalp))
        ;; storing pos and size together for now
        (vec-hash (make-hash-table :test 'equalp))
        (next-ts 0)
        (next-quat 0)
        (next-vec 0))
    (flet ((map-anim-data (tsf qf vf &key post-anim pre-bone)
             (loop
               for a in anim-data
               do (map 'nil
                       (lambda (anim)
                         (when pre-bone (funcall pre-bone))
                         (destructuring-bind (qts q pts p sts s) anim
                           ;; order matters here, since metadata pass
                           ;; writes them in order called
                           (funcall tsf qts)
                           (funcall qf q)
                           (funcall tsf pts)
                           (funcall vf p)
                           (funcall tsf sts)
                           (funcall vf s)))
                       (bone-data a))
               when post-anim do (funcall post-anim a))))
      ;; find distinct sequences and allocate ranges
      (flet ((ts (x)
               (unless (gethash x ts-hash)
                 (setf (gethash x ts-hash)
                       (list next-ts (incf next-ts (length x))))))
             (q (x)
               (unless (gethash x quat-hash)
                 (setf (gethash x quat-hash)
                       (list next-quat (incf next-quat (length x))))))
             (v (x)
               (unless (gethash x vec-hash)
                 (setf (gethash x vec-hash)
                       (list next-vec (incf next-vec (length x)))) )))
        (map-anim-data #'ts #'q #'v))
      ;; allocate/fill buffers
      (destructuring-bind (tsb qb vb mdb adb) (gl:gen-buffers 5)
        (3bgl-mesh::with-static-vectors ((ts-data next-ts :unsigned-short)
                                         (q-data (* 4 next-quat) :signed-short)
                                         (v-data (* 3 next-vec) :float)
                                         (md-data (* 9
                                                     (loop for a in anim-data
                                                           sum (length
                                                                (bone-data a))))
                                                  :unsigned-int)
                                         (ad-data (* 66 (length anim-data))
                                                  :unsigned-int))
          (maphash (lambda (k v)
                     (replace ts-data k
                              :start1 (first v)))
                   ts-hash)
          (maphash (lambda (k v)
                     (loop for x across (coerce k 'vector)
                           for i from (* 4 (first v)) by 4
                           do (setf (aref q-data (+ i 0)) (aref x 0))
                              (setf (aref q-data (+ i 1)) (aref x 1))
                              (setf (aref q-data (+ i 2)) (aref x 2))
                              (setf (aref q-data (+ i 3)) (aref x 3))))
                   quat-hash)
          (maphash (lambda (k v)
                     (loop for x across (coerce k 'vector)
                           for i from (* 3 (first v)) by 3
                           do (setf (aref v-data (+ i 0)) (aref x 0))
                              (setf (aref v-data (+ i 1)) (aref x 1))
                              (setf (aref v-data (+ i 2)) (aref x 2))
                           ))
                   vec-hash)
          (format t "building buffers:~%")
          (format t "  timestamps = ~s = ~s~%" next-ts (* 2 next-ts))
          (format t "  quaternions = ~s = ~s~%" next-quat (* 4 2 next-quat))
          (format t "  vectors = ~s = ~s~%" next-vec (* 4 3 next-vec))
          (format t "  metadata = ~s = ~s = ~s~%" (length anim-data)
                      (loop for a in anim-data
                            sum (length
                                 (bone-data a)))
                  (* 4 (length md-data)))
          (gl:bind-buffer :shader-storage-buffer tsb)
          (%gl:buffer-data :shader-storage-buffer (* 2 next-ts)
                           (static-vectors:static-vector-pointer ts-data)
                           :static-draw)
          (gl:bind-buffer :shader-storage-buffer qb)
          (%gl:buffer-data :shader-storage-buffer (* 4 2 next-quat)
                           (static-vectors:static-vector-pointer q-data)
                           :static-draw)
          (gl:bind-buffer :shader-storage-buffer vb)
          (%gl:buffer-data :shader-storage-buffer (* 3 4 next-vec)
                           (static-vectors:static-vector-pointer v-data)
                           :static-draw)
          ;; fill metadata buffer
          ;; fixme: this should probably be SoA rather than AoS ordering
          (let ((i 0)
                (k nil)
                (ai 0)
                (bi 0))
            (labels ((md (x)
                       (setf (aref md-data i) x)
                       (incf i))
                     (ts (x)
                          (destructuring-bind (s e) (gethash x ts-hash)
                            (md (- e s))
                            (md s)))
                     (q (x)
                       (destructuring-bind (s e) (gethash x quat-hash)
                         (declare (ignore e))
                         (md s)))
                     (v (x)
                       (destructuring-bind (s e) (gethash x vec-hash)
                         (declare (ignore e))
                         (md s)))
                     (b ()
                       (push bi k)
                       (incf bi))
                     (ad (x)
                       (setf (aref ad-data ai) x)
                       (incf ai))
                     (a (a)
                       (ad (length-ms a))
                       (ad (length k))
                       (loop with rk = (reverse k)
                             repeat 64
                             for x = (pop rk)
                             do (ad (or x 0)))
                       (setf k nil)))
              (map-anim-data #'ts #'q #'v :post-anim #'a :pre-bone #'b)))
          (format t "anim-data = ~%")
          (loop for i below (length ad-data) by 66
                do (format t "~s ~s : ~s~%"
                           (aref ad-data i)
                           (aref ad-data (1+ i))
                           (subseq ad-data (+ 2 i) (+ 66 i))
                           ))

          #++
          (break "md-data" (copy-seq md-data)
                 (copy-seq ts-data)
                 (copy-seq v-data)
                 (copy-seq q-data)
                 vec-hash
                 quat-hash)
          (gl:bind-buffer :shader-storage-buffer mdb)
          (%gl:buffer-data :shader-storage-buffer (* 4 (length md-data))
                           (static-vectors:static-vector-pointer md-data)
                           :static-draw)
          (gl:bind-buffer :shader-storage-buffer adb)
          (%gl:buffer-data :shader-storage-buffer (* 66 4 (length anim-data))
                           (static-vectors:static-vector-pointer ad-data)
                           :static-draw)
          (gl:bind-buffer :shader-storage-buffer 0)
          (setf (buffer state :metadata) mdb
                (buffer state :timestamps) tsb
                (buffer state :quaternions) qb
                (buffer state :vectors) vb
                (buffer state :anims) adb
                )
          (list :metadata mdb :timestamps tsb :quaternions qb :vectors vb))))))


(defun make-ssbo (stride count &key (usage :dynamic-draw)
                                 (buffer nil))
  (static-vectors:with-static-vector (x (* stride count))
    (fill x 0)
    (let ((bo (or buffer (car (gl:gen-buffers 1)))))
      (gl:bind-buffer :shader-storage-buffer bo)
      (%gl:buffer-data :shader-storage-buffer (length x)
                       (static-vectors:static-vector-pointer x)
                       usage)
      bo)))

(defun %update-buffers (state key stride count &key (usage :dynamic-draw))
  (setf (buffer state key)
        (make-ssbo stride count :usage usage
                   :buffer (buffer state key))))

(defun update-instance-data (anim-instance-index
                             &key (skeleton-index 0)
                               (anim-index 0)
                               (start-time-ms
                                (floor (/ (get-internal-real-time)
                                          internal-time-units-per-second)
                                       1/1000))
                               (?state 0)
                               bone-map
                               (state *gpu-anim-state*))
  ;; fixme: make sure program has been successfully compiled
  (let* ((p (3bgl-shaders::program (program state :update-anim)))
         (props (basecode-piq:get-program-resource p :shader-storage-block
                                                   "AnimInstances"))
         (vars (alexandria:plist-hash-table
                (loop for i in (getf props :active-variables)
                      collect (basecode-piq:get-program-resource-name
                               p :buffer-variable i)
                      collect (basecode-piq:get-program-resource
                               p :buffer-variable i))
                :test 'equalp))
         (size (getf (print (gethash "animInstances[0].skeleton" vars))
                     :top-level-array-stride)))
    (assert size)
    (cffi:with-foreign-object (fp :char size)
      (loop for i below size do (setf (cffi:mem-aref fp :char i) 0))
      (labels ((offset (n)
                 (getf (gethash (format nil "animInstances[0].~a" n) vars)
                       :offset))
               #++(array-stride (n)
                 (getf (gethash n vars) :array-stride))
               (suint (n v)
                 (setf (cffi:mem-ref fp :uint (offset n)) v))
               (sint (n v)
                 (setf (cffi:mem-ref fp :int (offset n)) v)))
        (format t "~&update anim instance ~s, ~s / ~s~%" anim-instance-index
                anim-index skeleton-index)
        (suint "skeleton" skeleton-index)
        (suint "anim" anim-index)
        (suint "startTime" (print start-time-ms))
        (suint "state" ?state)
        (sint "boneMap" (or bone-map -1))
        (%gl:named-buffer-sub-data (buffer state :anim-instance)
                                   (* anim-instance-index size)
                                   size fp)
        (gl:bind-buffer :shader-storage-buffer 0)))))


(defun update-skeleton (index parents inverse-bind-matrices local-matrices
                        &key
                          (state *gpu-anim-state*))
  (assert (< -1 index (or (buffer state :max-skeletons) 0)))
  ;; fixme: query this stuff once when program changes instead of every update
  (let* ((p (3bgl-shaders::program (program state :update-anim)))
         (props (basecode-piq:get-program-resource p :shader-storage-block
                                                   "Skeletons"))
         (vars (alexandria:plist-hash-table
                (loop for i in (getf props :active-variables)
                      collect (basecode-piq:get-program-resource-name
                               p :buffer-variable i)
                      collect (basecode-piq:get-program-resource
                               p :buffer-variable i))
                :test 'equalp))
         (size (getf (print (gethash "skeletons[0].maxDepth" vars))
                     :top-level-array-stride))
         (depths (loop with depths = (make-array (length parents)
                                                 :initial-element 0)
                       for i from 1 below (length parents)
                       for p = (aref parents i)
                       do (setf (aref depths i)
                                (if (array-in-bounds-p depths p)
                                    (1+ (aref depths p))
                                    0))
                       finally (return depths)))
         (max-depth (reduce 'max depths)))
    (assert size)
    (flet ((offset (n)
             (getf (gethash n vars) :offset))
           (array-stride (n)
             (getf (gethash n vars) :array-stride)))
      (format t "~&update skeleton ~s, max-depth = ~s~%" index max-depth)
      (format t "parents=~s~%depths=~s~%" parents depths)
      (cffi:with-foreign-object (fp :char size)
        ;; fixme: don't hardcode this stuff, check types in props?
        (setf (cffi:mem-ref fp :unsigned-int
                            (offset "skeletons[0].maxDepth"))
              max-depth)
        (loop with pp = (cffi:inc-pointer fp (offset "skeletons[0].depth[0]"))
              for i below 64
              for d across depths
              do (setf (cffi:mem-aref pp :unsigned-int i) d))
        (loop with pp = (cffi:inc-pointer fp (offset "skeletons[0].parent[0]"))
              for i below 64
              for d across parents
              do (setf (cffi:mem-aref pp :int i) d))
        (loop with pp = (cffi:inc-pointer fp (offset "skeletons[0].localMatrix[0]"))
              with stride = (/ (array-stride "skeletons[0].localMatrix[0]") 4)
              for i below 64
              for v across local-matrices
              when (vectorp v)
                do (loop for j below 16
                         for e across v
                         do (setf (cffi:mem-aref pp :float (+ j (* i stride)))
                                  e)))
        #+=
        (loop with pp = (cffi:inc-pointer fp (offset "skeletons[0].bonePositions[0]"))
              with stride = (/ (array-stride "skeletons[0].bonePositions[0]") 4)
              for i below 64
              for v across positions
              when (vectorp v)
                do (loop for j below 3
                         for e across v
                         do (setf (cffi:mem-aref pp :float (+ j (* i stride)))
                                  e)))
        #++
        (loop with pp = (cffi:inc-pointer fp (offset "skeletons[0].boneOrientations[0]"))
              with stride = (/ (array-stride "skeletons[0].boneOrientations[0]")
                               4)
              for i below 64
              for v across orientations
              when (vectorp v)
                do (loop for j below 4
                         for e across v
                         do (setf (cffi:mem-aref pp :float (+ j (* i stride)))
                                  e)))
        (loop with pp = (cffi:inc-pointer
                         fp (offset "skeletons[0].inverseBindMatrix[0]"))
              for i below 64
              for m across inverse-bind-matrices
              do (loop for j below 16
                       for e across m
                       do (setf (cffi:mem-aref pp :float (+ j (* i 16))) e)))
        (gl:bind-buffer :shader-storage-buffer (buffer state :skeletons))
        (%gl:buffer-sub-data :shader-storage-buffer (* index size) size fp)
        (gl:bind-buffer :shader-storage-buffer 0)))))

(defun %anim-state-sizes (p)
  (flet ((stride (x)
           (max
            (or (getf x :buffer-data-size) 0)
            (or (getf (basecode-piq:get-program-resource
                       p :active-variable
                       (car (last (getf x :active-variables))))
                      :top-level-array-stride)
                0))))
    (let* ((anim-instance
             (basecode-piq:get-program-resource p :shader-storage-block
                                                "AnimInstances"))
           (skeletons
             (basecode-piq:get-program-resource p :shader-storage-block
                                                "Skeletons"))
           (a-i-stride (when anim-instance (stride anim-instance)))
           (s-stride (when skeletons (stride skeletons)))
           (sizes (list :anim-instance a-i-stride :skeleton s-stride)))
      sizes)))

#++
(defun state-buffers-need-reallocated-p (&key (buffers *anim-state-buffers*)
                                           (shaders *gpu-anim-programs*))
  ;; lots of relatively slow API calls, so probably should only be
  ;; called when shader program is modified...
  (or
    ;; missing some buffers
   (loop for k in '(:anim-instance :skeletons
                    :metadata :quaternions :vectors :timestamps)
           thereis (not (getf buffers k)))
    ;; no sizes set or sizes changed
    (let ((p (3bgl-shaders::program (getf shaders :update-anim))))
      (and p (not (equalp (%anim-state-sizes p)
                          (getf buffers :anim-state-sizes)))))))

(defun maybe-rebuild-state-buffers (&key (state *gpu-anim-state*)
                                      force
                                      (max-anim-instances 4096)
                                      (max-skeletons 1024)
                                      )
  ;; lots of relatively slow API calls, so probably should only be
  ;; called when shader program is modified...

  ;; for normal use only need to call before uploading anim/skel data,
  ;; but when working on shaders need to be prepared to call it and
  ;; reupload data after changing anim shaders, in case struct layout
  ;; changes
  (let ((p (3bgl-shaders::program (program state :update-anim)))
        (sizes))
    (unless p
      (3bgl-shaders::use-program (program state :update-anim))
      (gl:use-program 0)
      (setf p (3bgl-shaders::program (program state :update-anim)))
      (format t "maybe-rebuild-state-buffers p=~s~%" p))
    (assert p)
    (setf sizes (%anim-state-sizes p))
    (when (or force
            (not
             (eql max-skeletons (buffer state :max-skeletons)))
            (not
             (eql max-anim-instances (buffer state :max-active-anims)))
            (not
             (equalp sizes (buffer state :anim-state-sizes))))
      (%update-buffers state :anim-instance
                       (getf sizes :anim-instance) max-anim-instances)
      (%update-buffers state :skeletons
                       (getf sizes :skeleton) max-skeletons)
      (setf (buffer state :max-active-anims) max-anim-instances)
      (setf (buffer state :max-skeletons) max-skeletons)
      (setf (buffer state :anim-state-sizes) sizes)
      t ;; caller needs to upload skeleton/instance data again
      )))

(defun live-programs-hook ()
  (when *gpu-anim-state*
    (loop for (k v) on (programs *gpu-anim-state*) by #'cddr
          collect v)))

(pushnew 'live-programs-hook basecode-shader-helper::*live-program-hooks*)

(defun %notice-modified-programs (shader)
  (let ((s (loop for (k v) on (programs *gpu-anim-state*) by #'cddr
                 when (eq v shader)
                   return k)))
    (format t "%nms ~s~%" shader)
    (when (eq s :update-anim)
      (format t "modified gpuanim shader~%")
      (maybe-rebuild-state-buffers :force nil))))

#+todo
(defun allocate-anim-buffers ())
#+todo
(defun update-anim-buffers ((w ...))
  (when (anim-data-buffers w)
    (gl:delete-buffers (remove-if-not 'integerp
                                      (shiftf (anim-data-buffers w) nil))))
  (let ((keys (make-array 1 :adjustable t :fill-pointer 0))
        (anim-data (make-array (length (anim w))))
        (s1 (vector (sb-cga:vec 1.0 1.0 1.0)))
        (t0 (vector (sb-cga:vec 0.0 0.0 0.0))))
    (flet ((q16 (x)
             (map '(vector (signed-byte 16))
                  (lambda (a) (floor (* a 32767)))
                  x)))
      (loop with offset = 0
            for anim in (anim w)
            ;; fixme: figure out if ?keys can have more than 1 entry,
            ;; and what it means if it does...
            for tx = (getf anim :?keys)
            for r = (getf anim :qkeys)
            for nb = (length (getf anim :qkeys))
            for i from 0
            do (setf (aref anim-data i)
                     (list :time (getf anim :time)
                           :num-bones nb
                           :keys (alexandria:iota nb :start offset)))
               (incf offset nb)
            do (format t "anim ~s~%" (getf anim :name))
            do (loop for (nil rk) in r
                     for tk = (pop tx)
                     do (vector-push-extend
                         (list (mapcar 'first rk)
                               (mapcar #'q16
                                       (mapcar 'second rk))
                               (if tk (mapcar 'first tk) #(0))
                               (if tk (mapcar (lambda (a)
                                                (coerce (second a)
                                                        'vector))
                                              tk)
                                   t0)
                               #(0) ;; no scale keys yet...
                               s1)
                         keys))))
    ;;(break "ad" anim-data keys (anim w))
    (setf (anim-data-buffers w)
          (build-anim-data keys)))

  ;; rebuild anim state data
  (maybe-rebuild-state-buffers w :force t)

  ;; start some anims
  (loop for anim in (anim w)
        for i from 0
        do (update-instance-data w i :anim-index i)))

(defun update-anim-state (count &key (state *gpu-anim-state*))
  (when (zerop count)
    (return-from update-anim-state nil))
  (assert (< 0 count (buffer state :max-active-anims)))
  (let ((p (program state :update-anim)))
    (when (and p (buffers state))
      (3bgl-shaders::with-program (p)
        (loop for k in '(:anim-instance :skeletons
                         :metadata :quaternions :vectors :timestamps)
              for buffer = (buffer state k)
              for binding in '(0 2 3 4 5 6)
              when buffer
              do (%gl:bind-buffer-base :shader-storage-buffer binding buffer))
       #++ (setf (3bgl-shaders::uniform p '3bgl-gpuanim-shaders:max-bone)
              *max-bone*)
        (setf (3bgl-shaders::uniform p '3bgl-gpuanim-shaders:time-ms)
              (or *hack-time*
                  (* 1000
                     (/ (get-internal-real-time)
                        internal-time-units-per-second))))
        #++(setf (3bgl-shaders::uniform p '3bgl-gpuanim-shaders:max-bone-models)
              *max-bone-models*)
        (%gl:memory-barrier #.(cffi:foreign-enum-value
                               '%GL:ENUM :shader-storage-barrier-bit))
        (%gl:dispatch-compute 1 (max 1 count) 1))
      (gl:use-program 0)
      (%gl:memory-barrier #.(cffi:foreign-enum-value
                             '%GL:ENUM :shader-storage-barrier-bit)))))


(defun bind-anim-buffers (&key (state *gpu-anim-state*))
  (flet ((bind (index key)
           (let ((b (buffer state key)))
             (when b(%gl:bind-buffer-base :shader-storage-buffer index b)))))
    (bind 0 :anim-instance)
    (bind 1 :anims)
    (bind 2 :skeletons)
    (bind 3 :metadata)
    (bind 4 :quaternions)
    (bind 5 :vectors)
    (bind 6 :timestamps))
  ;; 7 = anim-bone-map
)

(defun call-with-gpu-anim-state (thunk)
  (let ((*gpu-anim-state* (make-instance 'gpu-anim-state)))
    (setf (program *gpu-anim-state* :update-anim)
          (3bgl-shaders::shader-program :compute
                                        '3bgl-gpuanim-shaders::update-anim))
    (maybe-rebuild-state-buffers :force t)
    (funcall thunk)))

(defmacro with-gpu-anim (() &body body)
  `(call-with-gpu-anim-state (lambda () ,@body)))
