(in-package #:basecode-vr)

(defclass locomotion ()
  ((pos :initform (sb-cga:vec 0.0 0.0 0.0) :accessor pos)
   (rot :initform 0.0 :accessor rot)
   (target :initform nil :accessor target)
   (last-move :initform 0 :accessor last-move)))


(defmethod eye-modelview :around ((w locomotion) eye)
  (let ((mv (call-next-method)))
    ;; fixme: rotate around HMD instead of room origin
   (sb-cga:matrix* mv
                   (sb-cga:rotate-around (sb-cga:vec 0.0 1.0 0.0)
                                         (3bgl-math::deg-to-rad (rot w)))
                   (sb-cga:translate (pos w)))))

;;; just simple jump forward/back for now, then point to teleport to
;;; location on xz plane after that. need to figure out how to
;;; interact with scene/physics for real version (intersect with
;;; navmesh, simplified convex hull stuff, etc)

;; todo: pick (from controller view, not eye) against simplified
;; collision geometry, only allow moves if hit is 'flat' enough (and
;; large enough?).  or pick against collision + navmesh, only allow if
;; it hits navmesh (need to make sure navmesh is always above floor
;; though, which probably isn't true)?  maybe just pick against
;; navmesh, with separate check for walls?  or pick against (connected
;; parts of) navmesh, with some sort of LoS check?

;; todo: figure out touchpad input and use it for movement dir

(defun move (w d &key (dir (sb-cga:vec 0.0 0.0 1.0)))
  (let* ((m (sb-cga:matrix*
             (hmd-pose w)
             (sb-cga:rotate-around (sb-cga:vec 0.0 1.0 0.0)
                                   (3bgl-math::deg-to-rad (rot w)))))
         (v (sb-cga:transform-direction dir m)))
    (setf (aref v 0) (- (aref v 0)))
    (setf (aref v 1) 0.0)
    (setf v (sb-cga:vec* (sb-cga:normalize v) (float d 1.0)))
    (setf (pos w) (sb-cga:vec+ (pos w) v))))

(defun get-button (data)
  (let ((b (when (consp data)
             (ignore-errors (getf data '3B-OPENVR::BUTTON)))))
    ;; :system 0
    ;; :application-menu 1
    ;; :grip 2
    ;; :dpad-left 3
    ;; :dpad-up 4
    ;; :dpad-right 5
    ;; :dpad-down 6
    ;; :a 7
    ;; :proximity-sensor 31
    ;; :axis-0 32
    ;; :axis-1 33
    ;; :axis-2 34
    ;; :axis-3 35
    ;; :axis-4 36
    ;; :steam-vr-touchpad 32
    ;; :steam-vr-trigger 33
    ;; :dashboard-back 2
    (case b
      ((nil) nil)
      ;; force interpretation of some with duplicates
      (2 :grip)
      (32 :steam-vr-touchpad)
      (33 :steam-vr-trigger)
      (t
       (or
        (cffi:foreign-enum-keyword '3b-openvr::vr-button-id b :errorp nil)
        b)))))

(defmethod process-vr-event :after((w locomotion) event)
  (let* ((data (getf event :data))
         (button (get-button data)))
    (case (getf event :event-type)
      (:button-unpress
       (unless (or (eql (gethash :right-hand (controller-role-ids w))
                        (getf event :tracked-device-index))
                   (gethash :left-hand (controller-role-ids w)))
         (loop for i below vr::+max-tracked-device-count+
               do (format t "~&~s:" i) (finish-output)
                  (format t " = ~s~%" (vr::get-controller-role-for-tracked-device-index i)))
         (update-controller-roles w))
       (when (eql (gethash :left-hand (controller-role-ids w))
                  (getf event :tracked-device-index))
         (format t "left-hand button ~s (~s)~%" button
                 (when (consp data)
                   (ignore-errors (getf data '3B-OPENVR::BUTTON))))
         (case button
           (:steam-vr-trigger
            (move w 0.31))
           (:grip (setf (pos w)
                        (sb-cga:vec 0.0 0.0 0.0))
            #++(format t "~&names ~s~%"
                       (vr::get-render-model-names))
            #++(loop
                 for n in (vr::get-render-model-names)
                 do (format t "~& ~s:~%" n)
                    (loop
                      for c in (vr::get-component-names n)
                      do (format t "   ~s (~s)~%" c
                                 (vr::get-component-render-model-name n c)))))
           (:steam-vr-touchpad (move w -0.1))))))))

(defun recenter (w)
  ;; fixme: (option to) set pos so HMD is at 0,x,0 instead of just
  ;; moving center of room back to 0,0,0,?
  (setf (rot w) 0.0)
  (fill (pos w) 0.0))

;;; wasd for debugging...
(defmethod basecode:basecode-draw :after ((w locomotion))
  (let ((d 0.24)
        (a 10.0)
        (skip 0.1)
        (now (basecode::now)))
    (when  (and (> (- now (last-move w)) skip))
      (loop for k in '(:w :a :d :s :q :e)
            when (gethash k (basecode::key-state w))
              do (setf (last-move w) now)
                 (case k
                   (:backspace
                    (recenter w))
                   (:w (move w d))
                   (:a (move w (- d) :dir (sb-cga:vec 1.0 0.0 0.0)))
                   (:d (move w d :dir (sb-cga:vec 1.0 0.0 0.0)))
                   (:s (move w (- d)))
                   (:q (setf (rot w) (mod (+ (rot w) 360.0 (- a)) 360.0)))
                   (:e (setf (rot w) (mod (+ (rot w) a) 360.0))))))))



(defmethod basecode::key-down :after ((w locomotion) k)
  (case k
    (:backspace
     (recenter w))))
