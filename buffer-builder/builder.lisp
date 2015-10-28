(in-package #:buffer-builder)

;;; dsl for building VBOs
#++
(with-vbo-builder (b 16 size
                     :adjustable t
                     :layout-var layout
                     :layout ((vertex :vec4)
                              (normal :vec3) ;; prev location + 1
                              (foo :vec4)
                              ;; specify location, scale 0..1 to 0..255
                              ;; 'ub' clamps to 0..255 before storing
                              ;; (possibly add error option as well?)
                              (foo2 :vec2ub)
                              (color :vec4ub :location 8 :scale 255)))
  ;; stores previous value until called again
  (color 1 0 1 1)
  (foo2 123 45)
  (loop for i below 10
        do (vertex i) ;; coerce ints to floats, default to x,0,0,1 for vec4
           (normal (foo i)) ;; accept sb-cga vec
           ;; 
           (foo (foo2 i) 0) ;; accept typed array + extra element(s) for vec4?
           (emit-vertex))
  ;; upload SIZE bytes from foreign pointer B to GL
  (... size ... b)
  ;; configure vertex attribs...
  ;; stride = (+ 16 12 16 4 4)52, size of each element is rounded up
  ;;   to multiple of 4 bytes
  ;; layout = (52 (:location 0 :offset 0) (:location 1 :offset 16) ...)
  (loop for stride = (car layout)
        for l in (cdr layout)
        do (destructuring-bind (&key location offset) l
             ;; set up vertex arrays
             )))

(defparameter *type-metadata*
  (flet ((f (n)
           ;; code fragment to be added to writers, with R bound to &rest list
           ;; of values, and TYPE bound to type name
           `(loop with i = 0
                  for v in r
                  do (etypecase v
                       (sequence
                        (map 'nil (lambda ()))))
                  when (> i ,n)
                    do (error "too many elements for type ~s" type)
                  until (= i ,n)))
         (u (b n))
         (s (b n)))

   (alexandria:plist-hash-table
    ;; size (multiple of 4), default scale, default clamp, base type count
    `(:vec1 (4 nil nil single-float 1) :vec2 (8 nil nil single-float 2)
      :vec3 (12 nil nil single-float 3) :vec4 (16 nil nil single-float 4)
      :vec1u8 (4 255 (0 1) (unsigned-byte 8) 1)
      :vec2u8 (4 255 (0 1) (unsigned-byte 8) 2)
      :vec3u8 (4 255 (0 1) (unsigned-byte 8) 3)
      :vec4u8 (4 255 (0 1) (unsigned-byte 8) 4)
      :vec1s8 (4 127 (-1 1) (signed-byte 8) 1)
      :vec2s8 (4 127 (-1 1) (signed-byte 8) 2)
      :vec3s8 (4 127 (-1 1) (signed-byte 8) 3)
      :vec4s8 (4 127 (-1 1) (signed-byte 8) 4)
      :vec1u16 (4 65535 (0 1) (unsigned-byte 16) 1)
      :vec2u16 (4 65535 (0 1) (unsigned-byte 16) 2)
      :vec3u16 (8 65535 (0 1) (unsigned-byte 16) 3)
      :vec4u16 (8 65535 (0 1) (unsigned-byte 16) 4)
      :vec1s16 (4 32767 (-1 1) (signed-byte 16) 1)
      :vec2s16 (4 32767 (-1 1) (signed-byte 16) 2)
      :vec3s16 (8 32767 (-1 1) (signed-byte 16) 3)
      :vec4s16 (8 32767 (-1 1) (signed-byte 16) 4)))))

(defun calc-vbo-layout (layout)
  (loop for (fn type . options) in layout
        for location = (or (getf options :location) 0)
          then (or (getf options :location) (1+ location))
        for metadata = (or (gethash type *type-metadata*)
                           (error "unknown vbo element type ~s?" type))
        for offset = 0 then (+ offset element-size)
        for element-size = (first metadata)
        ;; NIL overrides defaults for scale/clamp
        for scale = (if (find :scale options)
                        (getf options :scale)
                        (second metadata))
        for clamp = (if (find :clamp options)
                        (getf options :clamp)
                        (third metadata))
        for element-type = (fourth metadata)
        for count = (fifth metadata)
        for default = (getf options :default)
        sum element-size into stride
        collect (list :fn fn :type type :location location
                      :size element-size :offset offset
                      :scale scale :clamp clamp
                      :element-type element-type
                      :count count
                      :default (or default
                                   (coerce
                                    (loop
                                      for i in '(0.0 0.0 0.0 1.0)
                                      when clamp
                                        do (setf i (min (second clamp)
                                                        (max (first clamp)
                                                             i)))
                                      when scale
                                        do (setf i (* scale i))
                                      when (and (consp type)
                                                (member (car type)
                                                        '(unsigned-byte
                                                          signed-byte)))
                                        do (setf i (floor i))
                                      collect i)
                                    'vector)))
          into l
        finally (return (values stride l))))

#++
(multiple-value-list
 (calc-vbo-layout '((vertex :vec4)
                    (normal :vec3)
                    (foo :vec4)
                    (foo2 :vec2u8)
                    (color :vec4u8 :location 8 :scale 255))))

(defun make-accessors (attribs buf)
  (flet ((setter (var n size base-type o)
           (ecase base-type
             (single-float
              `(setf (cffi:mem-ref ,buf :float (+ ,o (* ,n ,size)))
                     ,var))
             (signed-byte
              `(setf (cffi:mem-ref ,buf ,(ecase size
                                      (1 :int8)
                                      (2 :int16))
                                   (+ ,o (* ,n ,size)))
                     ,var))
             (unsigned-byte
              `(setf (cffi:mem-ref ,buf ,(ecase size
                                      (1 :uint8)
                                      (2 :uint16))
                                   (+ ,o (* ,n ,size)))
                     ,var))))
         (cast (var scale clamp type)
           (when clamp
             (setf var `(max ,(first clamp) (min ,(second clamp) ,var))))
           (when scale
             (setf var `(* ,var ,scale)))
           (ecase type
             ((signed-byte unsigned-byte)
              (setf var `(floor ,var)))
             (single-float
              (setf var `(coerce ,var 'single-float)))
             ((nil)))
           var))
    (loop for a in attribs
          for n = (getf a :fn)
          for type = (getf a :element-type)
          for base-type = (if (consp type) (car type) type)
          for size = (if (consp type)
                         (/ (second type) 8)
                         (if (eq type 'single-float)
                             4
                             (error "type ~s not supported yet?" base-type)))
          for count = (getf a :count)
          for cast = (getf a :cast)
          for scale = (getf a :scale)
          for clamp = (getf a :clamp)
          for default = (getf a :default)
          for offset = (getf a :offset)
         collect `(,n (x &optional ,@(loop for v in '(y z w)
                                           repeat (1- count)
                                           collect v))
                      #+sbcl
                      (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
                      (etypecase x
                        ((simple-array ,type (*)) ;; fast path
                         (progn
                           (assert (<= (length x) ,count))
                           (loop for i from 0
                                 for v across x
                                 do ,(setter (cast 'v scale clamp nil)
                                             'i size base-type offset))
                           (when (< (length x) ,count)
                             (loop for i from (length x) below ,count
                                   do ,(setter `(aref ,default i)
                                               'i size base-type offset)))))
                        (vector ;; slower path for untyped/displaced vectors
                         (progn
                           (assert (<= (length x) ,count))
                           (loop for v across x
                                 for i from 0
                                 do ,(setter (cast 'v scale clamp base-type)
                                             'i size base-type offset))
                           (when (< (length x) ,count)
                             (loop for i from (length x) below ,count
                                   do ,(setter `(aref ,default i)
                                               'i size base-type offset)))))
                        (real ;; separate args
                         ,@(loop for i below count
                                 for v in '(x y z w)
                                 for d across default
                                 collect (setter `(if ,v ,(cast v scale clamp base-type)
                                                      ,d)
                                                 i size base-type offset))))))))

(defmacro with-vbo-builder ((buffer initial-size size-var
                             ;; should there be a default layout?
                             &key adjustable layout-var layout)
                            &body body)
  (assert layout)
  ;; allow using a (compile-time) constant to specify layouts
  (when (and (symbolp layout)
             (constantp layout))
    (setf layout (symbol-value layout)))
  (alexandria:with-gensyms (index actual-size vert-buffer)
    (multiple-value-bind (stride attribs)
        (calc-vbo-layout layout)
      `(let ((,index 0)
             (,actual-size ,initial-size)
             ,@ (when layout-var
                  `((,layout-var '(,stride
                                   ,@(loop
                                       for a in attribs
                                       collect (list :location
                                                     (getf a :location)
                                                     :offset
                                                     (getf a :offset))))))))
         (symbol-macrolet (,@ (when size-var
                                `((,size-var (* ,stride ,index)))))
           (cffi:with-foreign-objects ((,vert-buffer :uint8 ,stride)
                                       (,buffer :uint8 (* ,initial-size ,stride)))
             (loop for i below (* ,initial-size ,stride)
                  do (setf (cffi:mem-ref ,buffer :uint8 i)
                           (mod i 256)))
             (loop for i below (* ,stride)
                  do (setf (cffi:mem-ref ,vert-buffer :uint8 i)
                           (mod i 256)))
             (labels ((emit-vertex ()
                        (when (>= ,index ,actual-size)
                          ,(if adjustable
                              `(error "adjustable not implemented yet")
                              `(error "too many elements in VBO (max ~s)"
                                      ,actual-size)))
                        (loop for d from (* ,stride ,index)
                              for s below ,stride
                              do (setf (cffi:mem-aref ,buffer :uint8 d)
                                       (cffi:mem-aref ,vert-buffer :uint8 s)))
                        (incf ,index))
                      ,@(make-accessors attribs vert-buffer))
               (declare (ignorable ,@ (loop for i in attribs
                                            collect (list 'function (getf i :fn)))))
               ,@body)))))))

#++
(with-vbo-builder (b 16 size
                     :adjustable t
                     :layout-var layout
                     :layout ((vertex :vec4)
                              (normal :vec3)
                              (foo :vec4)
                              (foo2 :vec2u8 :scale nil :clamp (0 255))
                              (color :vec4u8 :location 8 :scale 255)))
  (color 1 0 1 1)
  (foo2 123 456)
  (loop for i below 10
        do (vertex i)
           (normal (sb-cga:normalize (sb-cga:vec 0.1 0.2 0.3)))
           (foo i i (* i 2) (* i 3))
           (emit-vertex))
  (print layout)
  (format t "buffer = ~s size = ~s~%" b size)
  (loop for stride = (car layout)
        for l in (cdr layout)
        for i from 0
        do (destructuring-bind (&key location offset) l
             (format t "~s: binding ~s @ ~s/~s~%" i location offset stride)))
  (let ((stride (car layout)))
   (loop for i below size
         do (format t " ~s" (cffi:mem-aref b :uint8 i))
         when (zerop (mod (1+ i) stride))
           do (format t "~%"))
    (loop for o below size by stride
          do (loop for i below 11
                   do (format t " ~s" (cffi:mem-ref b :float (+ o (* i 4)))))
             (loop for i below 8
                   do (format t " ~s" (cffi:mem-ref b :uint8 (+ o 44 i))))
          (format t "~%"))))





