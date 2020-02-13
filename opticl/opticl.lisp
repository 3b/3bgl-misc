(in-package #:3bgl-opticl)

;;; routines for transferring opticl images to/from opengl


(defun guess-element-type (a &optional default)
  (let ((element-type (array-element-type a)))
    ;; clisp doesn't have specialized float or fixnum arrays, so check
    ;; an element when element-type is T
    (when (eq element-type 't)
      (setf element-type
            (typecase (row-major-aref a 0)
              (double-float 'double-float)
              (single-float 'single-float)
              (t (or default (error "can't determine array element type for texture upload ~s" a))))))
    element-type))

(defparameter *type-map*
  (alexandria:plist-hash-table '((unsigned-byte 8) :unsigned-byte
                                 (unsigned-byte 16) :unsigned-short
                                 (unsigned-byte 32) :unsigned-int
                                 single-float :float
                                 double-float :double)
                               :test #'equalp))



(defmacro expand-types ((object &rest types) &body body)
  `(typecase ,object
     ,@(loop for type in types
          collect `(,type
                    (locally (declare (type ,type ,object))
                      ,@body)))))

#++
(defmacro ffi-pixel (pointer type source x y w &environment env)
  ;; variant of opticl:pixel for flat ffi arrays (with dimensions
  ;; copied from an opticl array)
  (let ((image-dimensions (opticl::%get-image-dimensions source env)))
    (format t "dimensions = ~s~%" image-dimensions)
    (if image-dimensions
        (progn
          (ecase (length image-dimensions)
            (2 `(cffi:mem-aref ,pointer ,type (+ (* ,w ,y) ,x)))
            (3 `(values ,@(loop with c = (third image-dimensions)
                             for i below c
                             collect `(cffi:mem-aref ,pointer ,type
                                                     (+ (* ,c (+ (* ,y ,w) ,x)) ,i)))))))
        `(ecase (array-rank ,source)
           (2 (aref ,pointer ,type (+ (* ,w ,y) ,x)))
           (3 (ecase (array-dimension ,source 2)
                (1 (values
                    (aref ,pointer (+ (* ,y ,w) ,x))))
                (2 (values
                    (aref ,pointer (+ (* 2 (+ (* ,y ,w) ,x)) 0))
                    (aref ,pointer (+ (* 2 (+ (* ,y ,w) ,x)) 1))))
                (3 (values
                    (aref ,pointer (+ (* 3 (+ (* ,y ,w) ,x)) 0))
                    (aref ,pointer (+ (* 3 (+ (* ,y ,w) ,x)) 1))
                    (aref ,pointer (+ (* 3 (+ (* ,y ,w) ,x)) 2))))
                (4 (values
                    (aref ,pointer (+ (* 4 (+ (* ,y ,w) ,x)) 0))
                    (aref ,pointer (+ (* 4 (+ (* ,y ,w) ,x)) 1))
                    (aref ,pointer (+ (* 4 (+ (* ,y ,w) ,x)) 2))
                    (aref ,pointer (+ (* 4 (+ (* ,y ,w) ,x)) 3))))))))))


;; not sure which way this should be interpreted, currently NIL means
;; "just send data directly", which means origin of image (probably
;; upper right in opticl) is origin of texture (lower left in GL)
(defparameter *flip-texture-y* nil)

(defmacro do-pixels% ((j i wy) image &body body)
  ;; like DO-PIXELS, but with %PIXEL locally fbonud to flip the image vertically
  `(if *flip-texture-y*
       (do-pixels (,j ,i) ,image
         (macrolet ((%pixel (image-var y x) `(pixel ,image-var (- ,',wy ,y 1) ,x)))
           ,@body))
       (do-pixels (,j ,i) ,image
         (macrolet ((%pixel (&rest r) `(pixel ,@r)))
           ,@body))))

(defun call-with-image-in-unsigned-bytes (image thunk)
  (cffi:with-foreign-object (p :unsigned-char (apply '* (array-dimensions image)))
    (with-image-bounds (wy wx c) image
      (macrolet ((v (c)
                   `(values
                     ,@(loop for i below c
                          collect `(cffi:mem-aref p :unsigned-char
                                                  (+ ,i (* i ,c) (* j wx ,c)))))))
        (ecase c
          ((1 nil) (expand-types (image 1-bit-gray-image 2-bit-gray-image
                                    4-bit-gray-image 8-bit-gray-image)
                 (do-pixels% (j i wy) image
                   (setf (v 1) (%pixel image j i))))
                 (funcall thunk p :luminance))
          (3 (expand-types (image 4-bit-rgb-image 8-bit-rgb-image)
               (do-pixels% (j i wy) image
                 (setf (v 3) (%pixel image j i))))
             (funcall thunk p :rgb))
          (4 (expand-types (image 4-bit-rgba-image 8-bit-rgba-image)
               (do-pixels% (j i wy) image
                 (setf (v 4) (%pixel image j i))))
             (funcall thunk p :rgba)))))))


(defun call-with-image-in-unsigned-shorts (image thunk)
  (cffi:with-foreign-object (p :unsigned-short (apply '* (array-dimensions image)))
    (with-image-bounds (wy wx c) image
      (macrolet ((v (c)
                   `(values
                     ,@(loop for i below c
                          collect `(cffi:mem-aref p :unsigned-short
                                                  (+ ,i (* i ,c) (* j wx ,c)))))))
        (ecase c
          (1 (expand-types (image 16-bit-gray-image)
               (do-pixels% (j i wy) image
                 (setf (v 1) (%pixel image j i))))
             (funcall thunk p :luminance))
          (3 (expand-types (image 16-bit-rgb-image)
               (do-pixels% (j i wy) image
                 (setf (v 3) (%pixel image j i))))
             (funcall thunk p :rgb))
          (4 (expand-types (image 16-bit-rgba-image)
               (do-pixels% (j i wy) image
                 (setf (v 4) (%pixel image j i))))
             (funcall thunk p :rgba)))))))

(defun call-with-image-in-unsigned-ints (image thunk)
  (cffi:with-foreign-object (p :unsigned-int (apply '* (array-dimensions image)))
    (with-image-bounds (wy wx c) image
      (macrolet ((v (c)
                   `(values
                     ,@(loop for i below c
                          collect `(cffi:mem-aref p :unsigned-int
                                                  (+ ,i (* i ,c) (* j wx ,c)))))))
        (ecase c
          (1 (expand-types (image 32-bit-gray-image)
               (do-pixels% (j i wy) image
                 (setf (v 1) (%pixel image j i))))
             (funcall thunk p :luminance))
          #++(3 (expand-types (image 32-bit-rgb-image)
               (do-pixels% (j i wy) image
                 (setf (v 3) (%pixel image j i)))))
          #++(4 (expand-types (image 32-bit-rgba-image)
               (do-pixels% (j i wy) image
                 (setf (v 4) (%pixel image j i))))))))
    (funcall thunk)))

(defun call-with-image-in-single-floats (image thunk)
  (cffi:with-foreign-object (p :float (apply '* (array-dimensions image)))
    (with-image-bounds (wy wx c) image
      (macrolet ((v (c)
                   `(values
                     ,@(loop for i below c
                          collect `(cffi:mem-aref p :float
                                                  (+ ,i (* i ,c) (* j wx ,c)))))))
        (ecase c
          (1 (expand-types (image opticl::single-float-gray-image)
               (do-pixels% (j i wy) image
                 (setf (v 1) (%pixel image j i))))
             (funcall thunk p :luminance))
          (3 (expand-types (image opticl::single-float-rgb-image)
               (do-pixels% (j i wy) image
                 (setf (v 3) (%pixel image j i))))
             (funcall thunk p :rgb))
          (4 (expand-types (image opticl::single-float-rgba-image)
               (do-pixels% (j i wy) image
                 (setf (v 4) (%pixel image j i))))
             (funcall thunk p :rgba)))))
    (funcall thunk)))

(defmacro with-pointer-to-image-data (((pointer-var format-var) image element-type) &body body)
  `(flet ((thunk (,pointer-var ,format-var)
            ,@body))
     (let ((f ,element-type))
       (cond
         ((subtypep f '(unsigned-byte 8))
          (call-with-image-in-unsigned-bytes ,image #'thunk))
         #++((subtypep f '(signed-byte 8))
          (call-with-image-in-signed-bytes ,image #'thunk))
         ((subtypep f '(unsigned-byte 16))
          (call-with-image-in-unsigned-shorts,image #'thunk))
         #++((subtypep f '(signed-byte 16))
          (call-with-image-in-signed-shorts ,image #'thunk))
         ((subtypep f '(unsigned-byte 32))
          (call-with-image-in-unsigned-ints,image #'thunk))
         #++((subtypep f '(signed-byte 32))
          (call-with-image-in-signed-ints ,image #'thunk))
         ((subtypep f 'single-float)
          (call-with-image-in-single-floats,image #'thunk))
         #++((subtypep f 'double-float)
          (call-with-image-in-double-floats ,image #'thunk))
         (t (error "unimplemented or unknown type ~s" f))
)))
)


(defun tex-image-2d (target level internal-format image
                     &key border flip-y)
  ;; target = :texture-2d
  ;; level = mipmap level
  ;; internal-format = same as gl:tex-image-2d (:rgba, :compressed-rgb, etc)
  ;; width/height from image
  ;; border = NIL/true
  (let* ((width (array-dimension image 1))
         (height (array-dimension image 0))
         #++(components (array-dimension image 2))
         (element-type (guess-element-type image))
         (internal-size (gl::internal-format->int internal-format))
         (type (gethash element-type *type-map* nil))
         (*flip-texture-y* flip-y))
    (unless type
      (error "can't figure out how to upload array of type ~s?"
             (type-of image)))
    (gl:pixel-store :unpack-alignment 1)
    (with-pointer-to-image-data ((pointer format) image element-type)
      (format t "upload texture ~s~%"
              (list target level internal-size width height (if border 1 0)
                        format type pointer))
      (%gl:tex-image-2d target level internal-size width height (if border 1 0)
                        format type pointer))))



