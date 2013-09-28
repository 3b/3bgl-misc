(in-package #:3bgl-radiance-hdr)

;;; file format:
;; see http://radsite.lbl.gov/radiance/refer/filefmts.pdf
;;     http://radsite.lbl.gov/radiance/refer/Notes/picture_format.html
;; text header
;;   `#?RADIANCE` on first line = magic line
;;   `#..` = comment line
;;   `FOO=...` = key/value line
;;      FORMAT = 32-bit_rle_rgbe or 32-bit_rle_xyze
;;      EXPOSURE = float value to multiply colors by (multiply if more than 1)
;;      COLORCORR = 3x? float values to multiply color comps by
;;      PIXASPECT = pixel aspect ratio (probably ignore)
;;      SOFTWARE, VIEW, PRIMARIES = ignore for now
;;   blank line = end of header
;;   -Y #### +X #### = height, width
;;     (flip sign before X,Y to flip axis, swap x,y to swap x/y axes)
;;   binary image data:
;;     r,g,b,e 8 bit data (at least 1 of r,g,b > 127)
;;     1,1,1,x = old rle, repeat prev color X times
;;     2,2,h,l (h<127) = new rle, scanline length = (+ l (* 256 h))
;;       1 scanline each for r,g,b,e, rle encoded as
;;          run octet: >128 = repeat next octet (n & 127) times
;;                     <128 = copy next N octets
;;          repeat until LENGTH octets, then repeat for next component
;;

;; quick "buffered stream" hack, since using bivalent streams and read-byte
;; is slow
(defclass buf ()
  ((stream :initarg :stream :reader buf-stream)
   (pos :initform 0 :accessor buf-pos)
   (end :initform 0 :accessor buf-end)
   (buf :initform (make-array 8192 :element-type '(unsigned-byte 8)
                                  :initial-element 0)
        :accessor buf-buf)))

(defun buf-empty (buf)
  (>=  (buf-pos buf) (buf-end buf)))

(defun refill-buf (buf)
  (when (buf-empty buf)
    (setf (buf-pos buf) 0
          (buf-end buf) (read-sequence (buf-buf buf) (buf-stream buf)))))

(defun buf-eof (buf)
  (refill-buf buf)
  (buf-empty buf))

(declaim (inline buf-read-byte))
(defun buf-read-byte (buf)
  ;; note: read-scanline ignores this for speed and operates directly
  ;; on the internal buffer
  (if (buf-eof buf)
      (read-byte (buf-stream buf)) ;; trigger an EOF error on original stream
      (aref (buf-buf buf) (1- (incf (buf-pos buf))))))

(defun buf-peek-byte (buf)
  (if (buf-eof buf)
      (read-byte (buf-stream buf)) ;; trigger an EOF error on original stream
      (aref (buf-buf buf) (buf-pos buf))))


(defun buf-read-line (buf)
  (let ((n nil)
        (next nil))
    (prog1
        (babel:octets-to-string
         (coerce (loop until (or
                              (member (setf n (buf-peek-byte buf))
                                      '(10 13))
                              (buf-eof buf))
                       collect (buf-read-byte buf))
                 '(vector (unsigned-byte 8))))
      (unless (buf-eof buf)
        (loop do (buf-read-byte buf)
              while (and (not (buf-eof buf))
                         ;; don't eat consecutive newlines
                         (not (eql n (setf next (buf-peek-byte buf))))
                         (member next
                                 '(10 13))))))))

(defun read-hdr-header (buf)
  (flet ((parse-key (line)
           (when (and line
                      (string/= line "")
                      (char/= #\# (char line 0)))
            (let* ((= (position #\= line))
                   (k (intern (string-upcase
                               (string-trim '(#\space #\newline #\tab)
                                            (subseq line 0 =)))
                              :keyword))
                   (v (string-trim '(#\space #\newline #\tab)
                                   (subseq line (1+ =)))))
              (case k
                (:format
                 (list k (intern (string-upcase v) :keyword)))
                (:exposure
                 (list k (parse-number:parse-number v)))
                (:colorcorr
                 (list k (mapcar 'parse-number:parse-number
                                 ;; should this allow other separators?
                                 (split-sequence:split-sequence
                                  #\space k :remove-empty-subseqs t))))
                (:pixaspect
                 (list k (parse-number:parse-number v)))
                (t (list k v))))))
         (parse-xy (line)
           (destructuring-bind (axis1 dimension1 axis2 dimension2)
               (split-sequence:split-sequence #\space line
                                              :remove-empty-subseqs t)
             (unless (and (string= axis1 "-Y")
                          (string= axis2 "+X"))
               ;; todo: parse out sign/order of X,Y markers, and
               ;; rotate/translate to matche before uploading to GL
               (error "can't handle file with dimensions ~s yet?" line))
             (list :width (parse-integer dimension2)
                   :height (parse-integer dimension1)))))
    (loop with exposure = nil
          with colorcorr = nil
          for line = (buf-read-line buf)
          for (k v) = (parse-key (string-trim '(#\space #\newline #\tab) line))
          unless line do (error "invalid header parsing .hdr file?")
          until (equal line "")
          if (eq k :exposure)
            do (setf exposure (* v (or exposure 1.0)))
          else if (eq k :colorcorr)
                 do (setf colorcorr (mapcar '* v (or colorcorr '(1.0 1.0 1.0))))
          else if k
                 collect k into kv and collect v into kv
          finally (return (append (parse-xy (buf-read-line buf))
                                  (list :exposure (if (and exposure
                                                           (= exposure 1.0))
                                                      nil
                                                      exposure)
                                        :colorcorr colorcorr)
                                  kv)))))

(defun read-scanline (%buf length destination &key (offset 0))
  ;; read a scanline of a .hdr file into opengl 9/9/9/5 shared exponent format
  (declare (optimize speed)
           (fixnum length offset))
  (check-type destination (simple-array (unsigned-byte 32) (*)))
  (let ((buf (buf-buf %buf))
        (pos (buf-pos %buf))
        (end (buf-end %buf)))
      (declare (type (simple-array (unsigned-byte 32) (*)) destination)
               (type (simple-array (unsigned-byte 8) (*)) buf)
               (type (unsigned-byte 24) pos end))
    (labels ((%read-byte ()
               (when (= pos end)
                 (setf pos 0
                       end (read-sequence buf (buf-stream %buf)))
                 (when (zerop end) (read-byte (buf-stream %buf))))
               (aref buf (1- (incf pos))))
             (read-pixel ()
               (values (%read-byte) (%read-byte) (%read-byte) (%read-byte)))
             (valid-pixel-p (r g b e)
             (declare (ignore e))
             (or (> r 127) (> g 127) (> b 127)))
           (old-rle-p (r g b e)
             (declare)
             (when (= r g b 1)
               e))
           (new-rle-p (r g b e)
             (when (and (= r g 2) (< b 127))
               (dpb b (byte 7 8) e)))
           (write-pixel (p r g b e)
             (let ((w 0))
               (setf (ldb (byte 8 1) w) r
                     (ldb (byte 8 10) w) g
                     (ldb (byte 8 19) w) b
                     (ldb (byte 5 27) w) (- e 113))
               (setf (aref destination (+ offset p)) w)))
           (write-component (p c v)
             (declare (type (unsigned-byte 8) v))

             (ecase c
               (0 (setf (ldb (byte 8 1) (aref destination (+ offset p))) v))
               (1 (setf (ldb (byte 8 10) (aref destination (+ offset p))) v))
               (2 (setf (ldb (byte 8 19) (aref destination (+ offset p))) v))
               (3 (setf (ldb (byte 5 27) (aref destination (+ offset p)))
                        ;; gl-exp = hdr-exp - 128 + 15
                        (- v 113))))))
      (declare (inline read-pixel valid-pixel-p old-rle-p new-rle-p
                       write-pixel write-component))
      ;; limit width a bit to avoid optimization notes from sbcl
      ;; (doesn't really affect speed, but probably don't want
      ;;  with > (expt 2 24) anyway
      (check-type length (unsigned-byte 23))
      (loop with p of-type (unsigned-byte 24) = 0
            with rle of-type (or null (unsigned-byte 16)) = 0
            with lr of-type (unsigned-byte 8) = 0
            with lg of-type (unsigned-byte 8) = 0
            with lb of-type (unsigned-byte 8) = 0
            with le of-type (unsigned-byte 8) = 0
            while (< p length)
            do (multiple-value-bind (r g b e)
                   (read-pixel)
                 (declare (type (unsigned-byte 8) r g b e))
                 (cond
                   ((valid-pixel-p r g b e)
                    (write-pixel p r g b e)
                    (incf p))
                   ((setf rle (old-rle-p r g b e))
                    (loop repeat rle
                          do (write-pixel p r g b e))
                    (incf p rle))
                   ((setf rle (new-rle-p r g b e))
                    (assert (= p 0))
                    (assert (= rle length))
                    (loop for c below 4
                          for p2 of-type (unsigned-byte 16) = 0
                          do (loop while (< p2 rle)
                                   do (let ((r2 (%read-byte)))
                                        (declare (type (unsigned-byte 8) r2))
                                        (if (> r2 128)
                                            (loop with v = (%read-byte)
                                                  repeat (ldb (byte 7 0) r2)
                                                  do (write-component p2 c v)
                                                     (incf p2))
                                            (loop repeat r2
                                                  do (write-component
                                                      p2 c (%read-byte))
                                                     (incf p2))))))
                    (incf p rle)))
                 (setf lr r lg g lb b le e)))
      ;; fixme: probably should put this in UWP?
      ;; (not that current callers would handle errors any better)
      (setf (buf-pos %buf) pos
            (buf-end %buf) end))))

(defclass hdr-file ()
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (data :initarg :data :accessor data)
   (exposure :initarg :exposure :accessor exposure)))

(defun read-hdr-file (file)
  (with-open-file (f file :element-type '(unsigned-byte 8))
    (let* ((b (make-instance 'buf :stream f))
           (header (read-hdr-header b))
           (width (getf header :width))
           (height (getf header :height))
           (buf (make-array (* width height)
                            :element-type '(unsigned-byte 32)
                            :initial-element #xffffffff)))
      (when (and (getf header :exposure)
                 (/= (getf header :exposure) 1.0))
        (format t "ignoring exposure ~s for file ~s~%"
                (getf header :exposure) file))
      (loop for y below height
            do (read-scanline b width buf :offset (* y width )))
      (make-instance 'hdr-file :width width :height height
                     :data buf :exposure (or (getf header :exposure) 1.0)))))


(defmethod tex-image-2d ((hdr hdr-file) &key (level 0))
  (let ((width (width hdr))
        (height (height hdr))
        (data (data hdr)))
    (gl:tex-image-2d :texture-2d level
                     :rgb9-e5
                     width height 0
                     :rgb :unsigned-int-5-9-9-9-rev
                     data)))

(defmethod tex-image-2d ((filename string) &key (level 0))
  (tex-image-2d (read-hdr-file filename) :level level))

(defmethod tex-image-2d ((filename pathname) &key (level 0))
    (tex-image-2d (read-hdr-file filename) :level level))

