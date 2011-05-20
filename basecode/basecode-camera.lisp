(in-package #:basecode)

(defclass basecode-look-at ()
  ((look-at-eye :initform '(1 1 1) :initarg :look-at-eye :accessor look-at-eye)
   (look-at-target :initform '(0 0 0) :initarg :look-at-target
                   :accessor look-at-target)
   (look-at-up :initform '(0 1 0) :initarg :look-at-up :accessor look-at-up)))

(defmethod basecode-draw :before ((w basecode-look-at))
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (glu:look-at (elt (look-at-eye w) 0)
               (elt (look-at-eye w) 1)
               (elt (look-at-eye w) 2)
               (elt (look-at-target w) 0)
               (elt (look-at-target w) 1)
               (elt (look-at-target w) 2)
               (elt (look-at-up w) 0)
               (elt (look-at-up w) 1)
               (elt (look-at-up w) 2)))



(defclass freelook-camera ()
  ;; probably should store orientation as a quaternion or 3x3 matrix
  ;; instead of a full 4x4...
  ((freelook-camera-orientation :accessor freelook-camera-orientation
                                :initform (sb-cga:identity-matrix))
   (freelook-camera-position :accessor freelook-camera-position
                             :initform (sb-cga:vec 0.0 0.0 0.0))
   (freelook-camera-offset :accessor freelook-camera-offset :initform -0.6)

   (freelook-camera-move-state :accessor freelook-camera-move-state
                               :initform (make-hash-table))
   (freelook-camera-move-speed :accessor freelook-camera-move-speed
                               :initform 8)
   (freelook-camera-turn-speed :accessor freelook-camera-turn-speed
                               :initform (/ (* 45 (/ pi 180)) 1000))
   (freelook-camera-last-updated :accessor freelook-camera-last-updated
                                 :initform 0)
   (freelook-camera-dragging :accessor freelook-camera-dragging :initform nil)
   (freelook-camera-last-mx :accessor freelook-camera-last-mx :initform 0)
   (freelook-camera-last-my :accessor freelook-camera-last-my :initform 0)
   (freelook-camera-mouselook-up :accessor freelook-camera-mouselook-up
                                 :initform (sb-cga:vec 0.0 1.0 0.0))
   ;; default to initial view matching a 'look at' camera
   (look-at-eye :initform '(1 1 1) :initarg :look-at-eye :accessor look-at-eye)
   (look-at-target :initform '(0 0 0) :initarg :look-at-target
                   :accessor look-at-target)
   (look-at-up :initform '(0 1 0) :initarg :look-at-up :accessor look-at-up)))

(defun reset-freelook-camera (w)
  (setf (freelook-camera-move-state w) (make-hash-table))
  ;; fixme: remember any initarg default speed
  (setf (freelook-camera-move-speed w) 8)
  (flet ((v (l)
           (apply #'sb-cga:vec
                  (map 'list (lambda (a) (float a 1.0))
                       l))))
    (let* ((eye (v (look-at-eye w)))
           (target (v (look-at-target w)))
           (up (v (look-at-up w)))
           (z (sb-cga:normalize (sb-cga:vec- eye target)))
           (x (sb-cga:normalize (sb-cga:cross-product z up)) )
           (y (sb-cga:cross-product x z)))
      (setf (freelook-camera-position w) (sb-cga:vec- (sb-cga:vec 0.0 0.0 0.0)
                                                      eye))
      (setf (freelook-camera-orientation w)
            (sb-cga:matrix (aref x 0) (aref y 0) (aref z 0) 0.0
                           (aref x 1) (aref y 1) (aref z 1) 0.0
                           (aref x 2) (aref y 2) (aref z 2) 0.0
                           0.0 0.0 0.0 1.0)))))

(defmethod shared-initialize :after ((i freelook-camera) slot-names
                                     &rest initargs &key)
  (declare (ignorable slot-names initargs))
  (reset-freelook-camera i))

(defun update-freelook-camera (w)
  (let ((dt (- (now) (freelook-camera-last-updated w)))
        (localy (sb-cga:transform-direction (sb-cga:vec 0.0 1.0 0.0)
                                            (freelook-camera-orientation w)))
        (localx (sb-cga:transform-direction (sb-cga:vec 1.0 0.0 0.0)
                                            (freelook-camera-orientation w)))
        (localz (sb-cga:transform-direction (sb-cga:vec 0.0 0.0 1.0)
                                            (freelook-camera-orientation w))))
    (setf (freelook-camera-last-updated w) (now))
    (labels ((move-dir (plus minus)
               (let ((a 0))
                 (when (plusp (gethash plus (freelook-camera-move-state w) 0))
                   (incf a))
                 (when (plusp (gethash minus (freelook-camera-move-state w) 0))
                   (decf a))
                 a))
             (axis (rate vector)
               (let ((v (sb-cga:vec* vector
                                     (* dt rate
                                        (freelook-camera-move-speed w)))))
                 v))
             (axis-rot (rate axis)
               (if (zerop rate)
                   nil
                   (sb-cga:rotate-around axis
                                         (* dt rate
                                            (freelook-camera-move-speed w)))))
             (orthogonalize-rotation (m)
               (let* ((x (sb-cga:normalize
                          (sb-cga:vec (aref m 0) (aref m 1) (aref m 2))))
                      (y (sb-cga:normalize
                          (sb-cga:vec (aref m 4) (aref m 5) (aref m 6))))
                      (z (sb-cga:cross-product y x)))
                 (sb-cga:matrix (aref x 0) (aref y 0) (aref z 0) 0.0
                                (aref x 1) (aref y 1) (aref z 1) 0.0
                                (aref x 2) (aref y 2) (aref z 2) 0.0
                                0.0 0.0 0.0 1.0))))
      (let* ((forward (axis (move-dir :forward :backward)
                            localz))
             (strafe (sb-cga:vec+
                      (axis (move-dir :strafe-left :strafe-right)
                            localx)
                      (axis (move-dir :strafe-down :strafe-up)
                            localy)))
             (turn (axis-rot (move-dir :turn-left :turn-right)
                             (sb-cga:vec 0.0 1.0 0.0)))
             (mx (- (first (mouse-position w)) (freelook-camera-last-mx w)))
             (my (- (second (mouse-position w)) (freelook-camera-last-my w)))
             (mouse-turn (or turn (sb-cga:identity-matrix))))
        (when (freelook-camera-dragging w)
          (when (not (zerop mx))
            (setf mouse-turn
                  (sb-cga:matrix* mouse-turn
                                  (sb-cga:rotate-around
                                   (sb-cga:vec 0.0 1.0 0.0)
                                   (float (* mx 2 (* 75 (/ pi 180 1000))) 1.0)))))
          (when (not (zerop my))
            (setf mouse-turn
                  (sb-cga:matrix* mouse-turn
                                  (sb-cga:rotate-around
                                   localx
                                   (float (* my 2 (* 75 (/ pi 180 1000))) 1.0)
                                   )))))
        (setf (freelook-camera-last-mx w) (first (mouse-position w))
              (freelook-camera-last-my w) (second (mouse-position w)))
        (setf (freelook-camera-position w)
              (sb-cga:vec+ (freelook-camera-position w)
                           (sb-cga:vec+ forward strafe)))
        (when (or turn (and (freelook-camera-dragging w)
                            (or (not (zerop mx))
                                (not (zerop my)))))
          (setf (freelook-camera-orientation w)
                (orthogonalize-rotation
                 (sb-cga:matrix* mouse-turn
                                 (freelook-camera-orientation w)))))))))

(defmethod basecode-draw :before ((w freelook-camera))
  (update-freelook-camera w)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (let ((pos (freelook-camera-position w))
        (mat (freelook-camera-orientation w)))
    (gl:translate 0.0 0.0 (- (freelook-camera-offset w)))
    (gl:mult-matrix (sb-cga:transpose-matrix mat))
    (gl:translate (aref pos 0)
                  (aref pos 1)
                  (aref pos 2))))

(defmethod glut:keyboard :before ((w freelook-camera) key x y)
  (case key
    (#\backspace
     (reset-freelook-camera w))
    ((#\w \W)
     (incf (gethash :forward (freelook-camera-move-state w) 0)))
    ((#\s #\S)
     (incf (gethash :backward (freelook-camera-move-state w) 0)))
    ((#\a #\A)
     (incf (gethash :strafe-left (freelook-camera-move-state w) 0)))
    ((#\d #\D)
     (incf (gethash :strafe-right (freelook-camera-move-state w) 0)))
    ((#\e #\E)
     (incf (gethash :strafe-up (freelook-camera-move-state w) 0)))
    ((#\q #\Q)
     (incf (gethash :strafe-down (freelook-camera-move-state w) 0)))))

(defmethod glut:keyboard-up :before ((w freelook-camera) key x y)
  ;; fixme: don't decrement below 0
  (case key
    ((#\w \W)
     (decf (gethash :forward (freelook-camera-move-state w) 0)))
    ((#\s #\S)
     (decf (gethash :backward (freelook-camera-move-state w) 0)))
    ((#\a #\A)
     (decf (gethash :strafe-left (freelook-camera-move-state w) 0)))
    ((#\d #\D)
     (decf (gethash :strafe-right (freelook-camera-move-state w) 0)))
    ((#\e #\E)
     (decf (gethash :strafe-up (freelook-camera-move-state w) 0)))
    ((#\q #\Q)
     (decf (gethash :strafe-down (freelook-camera-move-state w) 0)))))

(defmethod glut:special :before ((w freelook-camera) key x y)
  (case key
    ((:key-up)
     (incf (gethash :forward (freelook-camera-move-state w) 0)))
    ((:key-down)
     (incf (gethash :backward (freelook-camera-move-state w) 0)))
    ((:key-left)
     (incf (gethash :turn-left (freelook-camera-move-state w) 0)))
    ((:key-right)
     (incf (gethash :turn-right (freelook-camera-move-state w) 0)))
    ((:page-up)
     (incf (gethash :strafe-up (freelook-camera-move-state w) 0)))
    ((:page-down)
     (incf (gethash :strafe-down (freelook-camera-move-state w) 0)))))

(defmethod glut:special-up :before ((w freelook-camera) key x y)
  (case key
    ((:key-up)
     (decf (gethash :forward (freelook-camera-move-state w) 0)))
    ((:key-down)
     (decf (gethash :backward (freelook-camera-move-state w) 0)))
    ((:key-left)
     (decf (gethash :turn-left (freelook-camera-move-state w) 0)))
    ((:key-right)
     (decf (gethash :turn-right (freelook-camera-move-state w) 0)))
    ((:page-up)
     (decf (gethash :strafe-up (freelook-camera-move-state w) 0)))
    ((:page-down)
     (decf (gethash :strafe-down (freelook-camera-move-state w) 0)))))

(defmethod glut:mouse :before ((w freelook-camera) button state x y)
  (when (eql button :right-button)
    (setf (freelook-camera-last-mx w) x
          (freelook-camera-last-my w) y
          (freelook-camera-dragging w) (eql state :down)
          (freelook-camera-mouselook-up w) (sb-cga:transform-direction
                                            (sb-cga:vec 0.0 1.0 0.0)
                                            (freelook-camera-orientation w))))
    (when (and (eql button :button5) (eql state :down))
      (incf (projection-fov w) 5))
    (when (and (eql button :button4) (eql state :down))
      (decf (projection-fov w) 5))
    (when (and (eql button :wheel-up) (eql state :down))
      (setf (freelook-camera-move-speed w)
            (* (freelook-camera-move-speed w) 1.1)))
    (when (and (eql button :wheel-down) (eql state :down))
      (setf (freelook-camera-move-speed w)
            (* (freelook-camera-move-speed w) 0.9))))



(defclass key-dumper ()
  ())

(defmethod glut:keyboard :before ((w key-dumper) k x y)
  (format t "pressed key ~s at ~s ~s~%" k x y))
(defmethod glut:keyboard-up :before ((w key-dumper) k x y)
  (format t "released key ~s at ~s ~s~%" k x y))

(defmethod glut:special :before ((w key-dumper) k x y)
  (format t "pressed special key ~s at ~s ~s~%" k x y))
(defmethod glut:special-up :before ((w key-dumper) k x y)
  (format t "released special key ~s at ~s ~s~%" k x y))
