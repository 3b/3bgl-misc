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

(defun read-hdr-header (text-stream)
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
          for line = (read-line text-stream nil nil)
          for (k v) = (parse-key (string-trim '(#\space #\newline #\tab) line))
          unless line do (error "invalid header parsing .hdr file?")
          until (equal line "")
          if (eq k :exposure)
            do (setf exposure (* v (or exposure 1.0)))
          else if (eq k :colorcorr)
                 do (setf colorcorr (mapcar '* v (or colorcorr '(1.0 1.0 1.0))))
          else if k
                 collect k into kv and collect v into kv
          finally (return (append (parse-xy (read-line text-stream))
                                  (list :exposure (if (and exposure
                                                           (= exposure 1.0))
                                                      nil
                                                      exposure)
                                        :colorcorr colorcorr)
                                  kv)))))

(declaim (inline read-scanline))
(defun read-scanline (stream length destination &key (offset 0))
  (declare (optimize speed)
           (fixnum length offset)
           (inline read-byte))
  (check-type destination (simple-array (unsigned-byte 8) (*)))
  (locally
      (declare (type (simple-array (unsigned-byte 8) (*)) destination))
    (flet ((read-pixel ()
             (values (read-byte stream) (read-byte stream)
                     (read-byte stream) (read-byte stream)))
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
             (setf (aref destination (+ offset (* p 4) 0)) r
                   (aref destination (+ offset (* p 4) 1)) g
                   (aref destination (+ offset (* p 4) 2)) b
                   (aref destination (+ offset (* p 4) 3)) e))
           (write-component (p c v)
             (setf (aref destination (+ offset (* p 4) c)) v)))
      (declare (inline read-pixel valid-pixel-p old-rle-p new-rle-p write-pixel write-component))
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
                                   do (let ((r2 (read-byte stream)))
                                        (declare (type (unsigned-byte 8) r2))
                                        (if (> r2 128)
                                            (loop with v = (read-byte stream)
                                                  repeat (ldb (byte 7 0) r2)
                                                  do (write-component p2 c v)
                                                     (incf p2))
                                            (loop repeat r2
                                                  do (write-component
                                                      p2 c (read-byte stream))
                                                     (incf p2))))))
                    (incf p rle)))
                 (setf lr r lg g lb b le e))))))
