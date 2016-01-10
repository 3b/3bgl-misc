(in-package #:scenegraph)

;;; cache of implementation limits and other runtime defined values
;; should be bound to a hash table while running state functions
(defvar *runtime-values-cache*)

;; map of 'canonicalized' list describing state -> state object
;; 'canonical' form is plist of non-default state values (including
;; :vertex-attrib-bindings and :fbo-bindings), sorted by key
;;
;; should be bound to equalp hash while running state functions
(defvar *known-states*)

(defun get-cached (key)
  (assert (boundp '*runtime-values-cache*))
  (or (gethash key *runtime-values-cache*)
      (setf (gethash key *runtime-values-cache*)
            (gl:get* key))))

(defparameter *enables*
  ;; state properties handles directly by gl:enable/diaable
  '(:color-logic-op
    :cull-face
    :depth-clamp
    :depth-test
    :dither ;; t
    :framebuffer-srgb
    :line-smooth
    :multisample ;; t
    :polygon-offset-fill
    :polygon-offset-line
    :polygon-offset-point
    :polygon-smooth
    :primitive-restart
    :primitive-restart-fixed-index
    :rasterizer-discard
    :sample-alpha-to-coverage
    :sample-alpha-to-one
    :sample-coverage
    :sample-mask
    :sample-shading
    :stencil-test
    :texture-cube-map-seamless
    :program-point-size
    ;; deprecated state
    :alpha-test
    :line-stipple
    :point-smooth
    :point-sprite
    :polygon-stipple
    ;; handled manually (for example if there is an indexed version as well)
    ;;:blend
    ;;:scissor-test
    ))

(defparameter *state-vars*
  ;; list of keywords naming properties of a STATE object
  ;; (some are also used as enable flag for corresponding feature)
  (list :clip-distances
        :stencil-func :stencil-func-front :stencil-func-back
        :stencil-op :stencil-op-front :stencil-op-back
        :blend :blend-func :blend-equation
        :depth-func
        :logic-op
        :cull-face
        :front-face
        :polygon-mode
        :point-size :point-fade-threshold-size :point-sprite-coord-origin
        :primitive-restart :primitive-restart-index
        :provoking-vertex
        :patch-vertices
        :sample-coverage :sample-mask :sample-mask-value
        :depth-range
        :scissor-test
        :color-write-mask :depth-write-mask
        :stencil-write-mask-front :stencil-write-mask-back
        :draw-buffers
        :read-buffer
        ;; default values for vertex attributes with no buffer bound
        :vertex-attrib-immediates
        ;; 3bgl-shaders::shader-program instance
        :program
        ;; a vao
        :vertex-format))


(defparameter *scene-state-vars*
  ;; GL state that isn't included in STATE object and is updated infrequently
  '(:viewport :scissor))

(defparameter *per-object-state-vars*
  ;; GL state that isn't included in STATE object and is updated frequently
  ;; (based on nv_command_list, so may need changed for vulkan)
  '(:blend-color
    :stencil-ref :alpha-ref
    :line-width
    :polygon-offset
    :front-face))

;; order of vars in state bitmask
#++
(defparameter *bitmask-vars* (append *state-vars*
                                     *enables*))
(defclass state ()
  ((state-values :reader state-values :initform (make-hash-table))
   (enables :reader enables
            :initform (make-array (length *enables*)
                                  :element-type 'bit
                                  :initial-contents
                                  ;; enables default to off except
                                  ;; for dither & multisample
                                  (loop for e in *enables*
                                        when (or (eq e :dither)
                                                 (eq e :multisample))
                                          collect 1 else collect 0)))
   ;; which *state-vars* are not at default state (used for calculating
   ;; diffs between 2 states more easily)
   ;; todo: implement bitmask and state diff stuff
   #++
   (bitmask :reader bitmask :initform (make-array (length *bitmask-vars*)
                                                  :element-type 'bit
                                                  :initial-element 0))
   ;; vertex attrib binding state (format, stride, offset, etc for all
   ;;  vertex attribs expected to be bound to buffers by this state)
   (vertex-attrib-bindings :reader vertex-attrib-bindings :initform nil)
   ;; fbo count, formats etc
   (fbo-bindings :reader fbo-bindings :initform nil)))


(defvar *state-defaults*) ;; bind to EQ hash table

(defun intern-value (v)
  ;; not sure if this should use a separate hash or if storing it with
  ;; interened states is ok?
  ;; fixme: if NIL is a valid state, split this out or handle it specially
  (or (gethash v *known-states*)
      (typecase v
        (cons
         (map nil #'intern-value v)
         (setf (gethash v *known-states*) v))
        ((or number symbol)
         ;; don't bother interning numbers,symbols,etc
         v)
        (t
         (setf (gethash v *known-states*) v)))))

(defun init-defaults (hash) ;; call with valid GL context
  (flet ((def (x v)
           (setf (gethash x hash) (intern-value v))))
    (def :blend nil)
    (def :clip-distances (make-list (get-cached :max-clip-distances)
                                    :initial-element nil))
    (def :blend-func '(:one :zero))
    (def :cull-face :back)
    (def :front-face :ccw)
    (def :polygon-mode :fill)
    (def :scissor-test nil)
    (def :color-write-mask '(t t t t))
    (def :depth-write-mask t)
    (def :vertex-format 0)
    (def :framebuffer-format 0)
    (def :program nil)
    (def :enables '(:dither :multisample))))

(defun get-state (state key)
  (let ((v (if state (gethash key (state-values state) :default) :default)))
    (if (eq v :default)
        (gethash key *state-defaults*)
        v)))

(defun apply-state (new old &key force bindings)
  ;; bindings is a list of buffer names or (buffer-name offset) lists
  ;; to bind to corresponding binding index

  (loop for o across (enables old)
        for n across (enables new)
        ;; fixme: use enum values instead of keywords to avoid runtime lookup
        for s in *enables*
        when (or force (/= o n))
          do (if (zerop n)
                 (gl:disable s)
                 (gl:enable s)))
  ;; blend is T,NIL or a list of T/NIL
  (let ((blend1 (get-state old :blend))
        (blend2 (get-state new :blend)))
    ;; values (and components of the values if any) are 'intern'ed so
    ;; we can compare them with EQL (not EQ since they might be
    ;; numbers)
    (when (or force (not (eql blend1 blend2)))
      (cond
        ((and (consp blend1) (consp blend2))
         (loop for b1 in blend1
               for b2 in blend2
               for i from 0
               when (or force (not (eq b1 b2)))
                 do (if b2
                        (%gl:enable-i :blend i)
                        (%gl:disable-i :blend i))))
        ((consp blend1)
         (loop for b = (pop blend1)
               for i below (get-cached :max-draw-buffers)
               when (or force b)
                 do (%gl:disable-i :blend i))
         (if blend2
             (gl:enable :blend)
             (gl:disable :blend)))
        ((consp blend2)
         (when (or force blend1)
           (gl:disable :blend))
         (loop for b in blend2
               for i from 0
               when (or force b)
                 do (%gl:disable-i :blend i)))
        (t
         (if blend2
             (gl:enable :blend)
             (gl:disable :blend))))))

  ;; clip-distances is a list of T/NIL
  (let ((clip1 (get-state old :clip-distances))
        (clip2 (get-state new :clip-distances)))
    (when (or force (not (eql clip1 clip2)))
      (loop for c1 in clip1
            for c2 in clip2
            for i from #.(cffi:foreign-enum-value '%gl:enum :clip-distance0)
            when (or force (not (eq c1 c2)))
              do (if c2
                     (%gl:enable i)
                     (%gl:disable i)))))

  ;; blend-func is a list of 2 or 4 keywords, or a list of lists of 2
  ;; or 4 keywords
  (let ((blend1 (get-state old :blend-func))
        (blend2 (get-state new :blend-func)))
    (when (or force (not (eql blend1 blend2)))
      (cond
        ((and (consp (car blend1)) (consp (car blend2)))
         (loop for b1 in blend1
               for b2 in blend2
               for (s d . a) = b2
               for i from 0
               when (or force (not (eq b1 b2)))
                 do (if a
                        (%gl:blend-func-separate-i i s d (car a) (cadr a))
                        (%gl:blend-func-i i s d))))
        ((consp (car blend1))
         ;; not sure if the indexed values need reset or what?
         ;; for now, setting indexed values to same as non-indexed
         ;; value in case someone enabled specific buffers but specifies
         ;; a single value
         (loop with (s d . a) = blend2
               for i below (get-cached :max-draw-buffers)
               do (if a
                      (%gl:blend-func-separate-i i s d (car a) (cadr a))
                      (%gl:blend-func-i i s d))
               finally (if a
                           (%gl:blend-func-separate s d (car a) (cadr a))
                           (%gl:blend-func s d))))
        ((consp (car blend2))
         ;; not sure what we should set the single value to in this case?
         ;; just resetting to defaul for now...
         (gl:blend-func :one :zero)
         (loop for (s d . a) in blend2
               for i below (get-cached :max-draw-buffers)
               do (if a
                      (%gl:blend-func-separate-i i s d (car a) (cadr a))
                      (%gl:blend-func-i i s d))))
        (t
         (if (cddr blend2)
             (apply #'%gl:blend-func-separate blend2)
             (apply #'%gl:blend-func blend2))))))


  ;; front-face is :cw or :ccw
  (let ((front1 (get-state old :front-face))
        (front2 (get-state new :front-face)))
    (when (or force (not (eql front1 front2)))
      (gl:front-face (or front2 :ccw))))

  ;; cull-face is :front :back :front-and-back or nil to dfisable
  (let ((cull1 (get-state old :cull-face))
        (cull2 (get-state new :cull-face)))
    (when (or force (not (eql cull1 cull2)))
      (gl:cull-face (or cull2 :back))))

  ;; polygon-mode is :point, :line, :fill
  (let ((mode1 (get-state old :polygon-mode))
        (mode2 (get-state new :polygon-mode)))
    (when (or force (not (eql mode1 mode2)))
      ;; only :front-and-back is supported in current GL
      (gl:polygon-mode :front-and-back mode2)))

  ;; scissor-test is T,NIL or a list of T/NIL
  (let ((scissor1 (get-state old :scissor-test))
        (scissor2 (get-state new :scissor-test)))
    (when (or force (not (eql scissor1 scissor2)))
      (cond
        ((and (consp scissor1) (consp scissor2))
         (loop for s1 in scissor1
               for s2 in scissor2
               for i from 0
               when (or force (not (eq s1 s2)))
                 do (if s2
                        (%gl:enable-i :scissor-test i)
                        (%gl:disable-i :scissor-test i))))
        ((consp scissor1)
         (loop for s = (pop scissor1)
               for i below (get-cached :max-draw-buffers)
               when (or force s)
                 do (%gl:disable-i :scissor-test i))
         (if scissor2
             (gl:enable :scissor-test)
             (gl:disable :scissor-test)))
        ((consp scissor2)
         (when (or force scissor1)
           (gl:disable :scissor-test))
         (loop for s in scissor2
               for i from 0
               when (or force s)
                 do (%gl:disable-i :scissor-test i))))))


  ;; color-write-mask is list of 4 T/NIL, or a list of lists of 4 T/NIL
  (let ((mask1 (get-state old :color-write-mask))
        (mask2 (get-state new :color-write-mask)))
    (when (or force (not (eql mask1 mask2)))
      (cond
        ((and (consp (car mask1)) (consp (car mask2)))
         (loop for s1 in mask1
               for s2 in mask2
               for (r g b a) = s2
               for i from 0
               when (or force (not (eq s1 s2)))
                 do (%gl:color-mask-i i r g b a)))
        ((not (consp (car mask2)))
         ;; man page sounds like color-mask overwrites individual settings?
         (apply #'%gl:color-mask mask2))
        ((consp mask2)
         (gl:color-mask t t t t)
         (loop for (r g b a) in mask2
               for i from 0
               do (%gl:color-mask-i i r g b a))))))


  ;; depth-write-mask is T or NIL
  (let ((mask1 (get-state old :depth-write-mask))
        (mask2 (get-state new :depth-write-mask)))
    (when (or force (not (eql mask1 mask2)))
      (gl:depth-mask mask2)))


  ;; vertex-format is a VAO name
  ;;
  ;; :vertex-format is required for final state objects (intermediate
  ;; state objects may only have :vertex-attrib until combined with
  ;; actual geometry objects in scene-graph)
  (assert (get-state new :vertex-format))
  (let ((vao1 (get-state old :vertex-format))
        (vao2 (get-state new :vertex-format)))
    (when (or force (not (eql (car vao1) (car vao2))))
      (gl:bind-vertex-array (car vao2)))
    (when bindings
      ;; todo: pass in previous bindings (or store in state object?)
      ;; so we can skip setting them if not changed...
      (loop for i from 0
            for bo in bindings
            for b = (if (consp bo) (first bo) bo)
            for offset = (if (consp bo) (second bo) 0)
            for stride in (cdr vao2)
            do (%gl:bind-vertex-buffer i b offset stride))))

  ;; framebuffer-format is an FBO name
  (let ((fbo1 (get-state old :framebuffer-format))
        (fbo2 (get-state new :framebuffer-format)))
    (when (or force (not (eql fbo1 fbo2)))
      (gl:bind-framebuffer :framebuffer fbo2)))

  ;; program is a shader program object
  ;;
  ;; (for nv_command_list, we will have to notice changes and rebuild
  ;;  state either by having a 'dirty' flag and marking it from
  ;;  program when programs change or by keeping a counter and
  ;;  watching it from state object)
  (let ((program1 (get-state old :program))
        (program2 (get-state new :program)))
    (when (or force (not (eql program1 program2)))
      ;; not worrying about checking for changes in program since last
      ;; state change, since we will probably do a "force" at the
      ;; beginning of every frame (if we use this at all for more than
      ;; building nv_command_list states)
      (3bgl-shaders::use-program program2))))



(defmethod canonicalize-state (key value)
  ;; if key doesn't name a state var, or value is the default value,
  ;; return NIL, otherwise return a list of the KEY and VALUE in
  ;; canonical form (for example expand key aliases, sort indexed
  ;; lists convert number types, etc), including "intern"ing lists so
  ;; they can be EQ compared
  (let* ((not-found '(nil))
         (e (member key *enables*))
         (d (getf *state-vars* key not-found)))
    (when e
      (return-from canonicalize-state (list key value)))
    (when (eq d not-found)
      (error "state ~s not implemented yet..." key))
    nil))

(defmacro defcan (key (value-var) &body body)
  (let ((default (gensym "DEFAULT")))
    `(defmethod canonicalize-state ((key (eql ,key)) ,value-var)
       (let ((,default (getf *state-vars* ,key)))
         (when (functionp ,default)
           (setf ,default (funcall ,default)))
         (when (or (member ,value-var '(nil :default))
                   (equalp ,value-var ,default))
           (return-from canonicalize-state nil))
         (list ,key
               (intern-value
                (block nil
                  ,@body)))))))

(defun bool (x)
  (not (not x)))

(defcan :clip-distances (value)
  ;; T,NIL or list of T/NIL
  ;; not sure if enabling all clip-distances at once is useful, since
  ;; we probably only care about ones shader writes to?
  (when (atom value)
    (return (make-list (get-cached :max-clip-distances)
                       :initial-element (bool value))))
  (assert (<= (length value) (get-cached :max-clip-distances)))
  (map-into (make-list (get-cached :max-clip-distances)
                       :initial-element nil)
            'bool value))

(defcan :blend (value)
  ;; T,NIL or list of T/NIL
  (when (atom value)
    (return (bool value)))
  (assert (<= (length value) (get-cached :max-draw-buffers)))
  (if (every 'bool value)
      t
      (map-into (make-list (get-cached :max-draw-buffers)
                           :initial-element nil)
                'bool value)))

(defcan :blend-func (value)
  ;; 2 element list of keywords, or list of nil/2-element lists of keywords
  (when (atom (elt value 0))
    (assert (and (= 2 (length value))
                 (every 'keywordp value)))
    (return value))
  (assert (<= (length value) (get-cached :max-draw-buffers)))
  ;; substitute instead of initial to replace NILs in input too
  (substitute '(:one :zero) nil
              (map-into (make-list (get-cached :max-draw-buffers)
                                   :initial-element nil)
                        'identity value)))

(defcan :cull-face (value)
  ;; todo: restrct to valid values?
  (assert (keywordp value))
  value)

(defcan :polygon-mode (value)
  ;; todo: restrct to valid values?
  (assert (keywordp value))
  value)

(defcan :scissor-test (value)
  ;; T,NIL or list of T/NIL
  (when (atom value)
    (return (bool value)))
  (assert (<= (length value) (get-cached :max-viewports)))
  (if (every 'bool value)
      t
      (map-into (make-list (get-cached :max-viewports)
                           :initial-element nil)
                'bool value)))

(defcan :color-write-mask (value)
  ;; input is T,NIL,list of 4 T/NIL list of lists of 4 T/NIL
  (when (atom value)
    (return (if value '(t t t t) '(nil nil nil nil))))
  (if (every 'bool value)
      (progn
        (assert (= 4 (length value)))
        (mapcar 'bool value))
      (progn
        (assert (<= (length value) (get-cached :max-draw-buffers)))
        (loop for i below (get-cached :max-draw-buffers)
              ;; any extra buffers default to T,T,T,T
              for v = (if value (pop value) '(t t t t))
              collect (mapcar 'bool v)))))

(defcan :depth-write-mask (value)
  (bool value))

(defmethod canonicalize-state ((key (eql :program)) value)
  ;; :program should be a program object or plist of shader stage ->
  ;; entry point (not handling generating shaders from components at
  ;; this level, since it is too hard to generalize, and simple case
  ;; can just use specific entry points directly)
  ;;
  ;; 'shader stage' is one of :vertex :fragment :geometry
  ;; :tess-control :tess-evaluation
  (if (typep value '3bgl-shaders::shader-program)
      (list key value)
      (progn
        ;; require at least a vertex and fragment shader
        (assert (and (getf value :vertex) (getf value :fragment)))
        ;; see if we already have a program for this combination of
        ;; entry points
        (setf value (alexandria:alist-plist
                     (sort (alexandria:plist-alist value) 'string<
                           :key (lambda (a) (symbol-name (car a))))))
        (if (gethash value *known-states*)
            (list key (gethash value *known-states*))
            (list key (setf (gethash value *known-states*)
                            (apply #'3bgl-shaders::shader-program value)))))))


(defmethod canonicalize-state ((key (eql :vertex-attribs)) value)
  ;; :vertex-attribs is a list of N keywords or NIL, for vertex
  ;; attributes 0..N, with no NILs at end
  ;; (possibly symbols in general rather than just keywords, depending
  ;;  on how default shaders libs end up?)
  (assert (every 'symbolp value))
  ;; remove trailing NILs if any
  ;; todo: optimize this?
  (setf value (reverse value))
  (loop while (and value (null (car value)))
        do (pop value))
  (setf value (reverse value))
  (list key value))


(defmethod canonicalize-state ((key (eql :vertex-format)) value)
  ;; :vertex-format is a list of lists like

  ;; (<attrib-index> <binding-index> <size> <type> <normalized>
  ;;  <offset> &key :il (:divisor 0) (:stride 0)
  ;;
  ;; required args correspond to the arguments of
  ;; %gl:vertex-attrib-binding and %gl:vertex-attrib-format
  ;;
  ;; if :IL is provided, it can be NIL, :I, OR :L meaning
  ;; %gl:vertex-attrib-format, %gl:vertex-attrib-i-format,
  ;; %gl:vertex-attrib-l-format respectively
  ;;
  ;; :divisor specifies value to be passed to %gl:vertex-binding-divisor
  ;;
  ;; :stride is number of bytes from start of data for one vertex to start
  ;;  of next vertex
  ;;
  ;; ex: ((0 0 3 :float nil 0)
  ;;      (1 0 3 :float nil 12))
  ;;
  ;; would specify we bind vertex attribs 0 and 1 to 3-element floats
  ;;  from binding 0, at offsets 0 and 12 respectively
  ;;
  ;; canonical form is (vao-name stride1 striden)
  ;;  where strideX is stride for binding index X
  (setf value
        (loop with h = (make-hash-table)
              for entry in (sort (copy-list value) '< :key 'car)
              collect (destructuring-bind (attrib binding size type
                                           normalized offset
                                           &key il (divisor 0) (stride 0))
                          entry
                        ;; divisor and stride are per binding, so make
                        ;; sure they match
                        (when (gethash binding h)
                          (assert (equalp (gethash binding h)
                                          (list divisor stride))))
                        (setf (gethash binding h) (list divisor stride))
                        (list attrib binding size type normalized offset
                              :il (or il :f) :divisor divisor :stride stride))))
  (when (gethash value *known-states*)
    (return-from canonicalize-state (list key (gethash value *known-states*))))
  (let ((vao (gl:gen-vertex-array))
        (o/s))
    ;; todo: use bindless versions?
    (gl:bind-vertex-array vao)
    (loop with configured-bindings = (make-hash-table)
          for entry in value
          collect (destructuring-bind (attrib binding size type
                                       normalized offset
                                       &key il divisor stride) entry
                    (push (list binding stride) o/s)
                    (ecase il
                      (:f (%gl:vertex-attrib-format attrib size type normalized
                                                    offset))
                      (:i (%gl:vertex-attrib-i-format attrib size type offset))
                      (:l (%gl:vertex-attrib-l-format attrib size type offset)))
                    (%gl:vertex-attrib-binding attrib binding)
                    (gl:enable-vertex-attrib-array attrib)
                    (unless (gethash binding configured-bindings)
                      (%gl:vertex-binding-divisor binding divisor)
                      ;; i think using buffer 0 to only set stride is
                      ;; correct? not sure from docs
                      (%gl:bind-vertex-buffer binding 0 0 stride)
                      (setf (gethash binding configured-bindings) t))))
    (gl:bind-vertex-array 0)
    (setf o/s
          (mapcar 'second
                  (sort (delete-duplicates o/s :key 'car)
                        '< :key 'car)))
    (list key
          (setf (gethash value *known-states*) (list* vao o/s)))))

(defun %make-state (canonical-state)
  (let ((s (make-instance 'state)))
    (loop for v in *state-vars*
          ;;for i from 0
          when (find v canonical-state)
            do (setf (gethash v (state-values s))
                     (getf canonical-state v))
               #++ ;; todo
               (setf (aref (bitmask s) i) 1))
    (loop for v in *enables*
          for i from 0
          when (find v canonical-state)
            do (setf (aref (enables s) i)
                     (if (getf canonical-state v) 1 0)))
    s))

(defmethod canonicalize-state ((key (eql '%state)) value)
  (let ((c (loop with seen = (make-hash-table)
                 for (k v) on value by #'cddr
                 for can = (canonicalize-state k v)
                 when (and can (not (gethash k seen)))
                   append can
                 do (setf (gethash k seen) t))))
    (setf c (alexandria:alist-plist
             (sort (alexandria:plist-alist c)
                   'string<
                   :key (lambda (a) (string (car a))))))
    (or (gethash c *known-states*)
        (setf (gethash c *known-states*)
              (%make-state c)))))

(defmethod make-state (state)
  (canonicalize-state '%state state))

(defmethod make-state* (&rest state)
  (make-state state))
