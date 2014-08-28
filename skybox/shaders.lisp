(cl:defpackage #:skybox-shaders
  (:use :cl :glsl)
  (:shadowing-import-from #:glsl #:defun #:defconstant))
(cl:in-package #:skybox-shaders)

;; vertex attributes
(input position :vec4 :location 0)
(input uv :vec3 :location 1)

;; final output
(output out-color :vec4 :stage :fragment)

#++(uniform texture :sampler-2d :stage :fragment)

;;(uniform m :mat4)
;;(uniform v :mat4)
;;(uniform p :mat4)
(uniform mv :mat4)
(uniform mvp :mat4)
;;(uniform normal-matrix :mat4)
;;
;;(uniform center :vec2)
;;(uniform p1 :float)
;;(uniform p2 :float)
;;(uniform p3 :float)
;;(uniform p4 :float)

(uniform foo :float)

(uniform eye-pos :vec3)
(uniform light-dir :vec3)
(uniform eye-pos-planet :vec3)

(uniform altitude :float) ;; height above earth's surface


(interface varyings (:out (:vertex outs)
                     :in (:fragment ins))
  (eye-dir :vec3))

(defun vertex ()
  (if (> foo 0)
      (progn
        (setf (@ outs eye-dir)  (* (vec3 1 1 1) (vec3 uv)))
        ;; (setf (@ outs eye-dir) (vec3 1.0 1.0 1.0))
        ;;(setf gl-position (* mvp position))
        (setf gl-position  position))
      (progn
        (setf (@ outs eye-dir)  (- (vec3 uv) (- eye-pos))
              gl-position (* mvp position))
        ))
  )


(defun rayleigh-phase (cos-theta)
  ;; may need to divide result by 4pi?
  (return (* (/ 3.0 4.0)
             (+ 1 (* cos-theta cos-theta)))))

(defun mie-phase-cs (cos-theta g)
  ;; Cornette/Shanks approx
  (let ((g2 (pow g 2.0)))
    (return
      (* (/ (* 3.0 (- 1 g2))
           (* 2.0 (+ 2 g2) 4 3.141592653589793))
        (/ (+ 1.0 (* cos-theta cos-theta))
           (expt (+ 1.0 g2 (* -2.0 g cos-theta)) 1.5))))))

(defun mie-phase-hg (cos-theta g)
  ;; Henyey/Greenstein
  (let ((g2 (pow g 2.0)))
    (return
      (/ (- 1.0 g2)
         (* 4.0 3.141592653589793 (expt (+ 1.0 g2 (* -2.0 g cos-theta)) 1.5))))))

;; from "Physically Based Rendering"
(defun phase-g-to-k (g)
  (return (- (* 1.55 g) (* 0.55 g g g))))

(defun mie-phase-schlick (cos-theta k)
  (let ((tmp (- 1 (* k cos-theta))))
    (return
      (/ (- 1 (* k k))
         (* 4 3.141592653589793
            tmp tmp)))))


;; from "Efficient Rendering of Atmospheric Scattering" and "Precomputed Atmospheric Scattering"
;; by way of http://www.scratchapixel.com/lessons/3d-advanced-lessons/simulating-the-colors-of-the-sky/atmospheric-scattering/
;; @ 680nm, 550nm, 440nm
(defconstant +rayleigh-scattering+ (vec3 5.8e-6 13.5e-6 33.1e-6) :vec3)
;; other values from scratchapixel
;;(defconstant +rayleigh-scattering+ (vec3 5.5e-6 13.0e-6 22.4e-6) :vec3)
(defconstant +mie-scattering+ 2.10e-5 :float) ; ??? * (wavelength ^ -0.84)

;; from "Modelling of Daylight for Computer Graphics"
;; @ 700nm, 540nm, 470nm
;(defconstant +rayleigh-scattering+ (vec3 4.184e-6 1.181e-5 2.059e-5))

;; from "modelling of daylight for computer graphics"
(defconstant +rayleigh-scale-height+ 7994 :float) ;; ~7-8k depending on temp
(defconstant +mie-scale-height+ 1200 :float)

(defconstant +surface-height+ 6360e3 :float)
(defconstant +atmosphere-height+ 6420e3 :float)
(defconstant +atmosphere-height-sq+ (expt 6420e3 2.0) :float)

(defun intersect-atmosphere (start dir)
  ;; returns distance along DIR from START of (last) intersection with
  ;; atmosphere
  ;; returns <=0 for no intersection
  (let* ((a 1.0)
         ;; possibly should optimize for sun at +z?
         (b (* 2 (dot start dir)))
         (c (- (dot start start) +atmosphere-height-sq+))
         (discriminant (- (* b b) (* 4 a c))))
    (when (< discriminant 0.0)
      ;; outside atmosphere, no intersection
      (return -2.0))
    ;; todo: possibly should do same calculation for earth radius, to
    ;; reject rays that hit surface?
    ;; (or maybe sample some approximation of illumination at surface?)
    ;; - ignoring for now, since we will usually have actual ground geometry
    ;;   blocking direct views
    (let* ((q (+ (- b)
                 (* (sign b) (sqrt discriminant))))
           (t1 (/ q (* 2 a)))
           (t2 (/ (* 2 c) q))
           (max (max t1 t2)))
      ;; if max < 0, we are outside atmosphere, looking away,
      ;; otherwise we can see atmosphere, assume we are inside for now
      ;; (so '(min t1 t2)' is distance to edge of atmosphere behind
      ;; view, max is distance along view) todo: handle looking
      ;; through atmosphere from outside (might just replace start
      ;; with point at min in that case?  more likely to need surface
      ;; intersection for those cases though)
      (return max))))

(defun calculate-sunlight (eye-pos view-dir sun-dir
                           view-steps sun-steps)
  (let* ((view-dist (intersect-atmosphere eye-pos view-dir))
         (view-step-length (/ view-dist view-steps))
         (view-step (* view-dir view-step-length))
         (view-sample (+ eye-pos
                         (* view-step 0.5)))
         (rayleigh (vec3 0.0 0.0 0.0))
         (mie (vec3 0.0 0.0 0.0))
         (view-depth-rayleigh 0.0)
         (view-depth-mie 0.0))
    (when (<= view-dist 0)
      (return (vec3 1.0 1.0 0.0)))
    (dotimes (v (- view-steps 1))
      (incf view-sample view-step)
      (let* ((view-sample-height (min 0.0 (- +surface-height+ (length view-sample))))
             (sun-dist (intersect-atmosphere view-sample sun-dir))
             (sun-step-length (/ sun-dist sun-steps))
             (sun-step (* sun-dir sun-step-length))
             (sun-sample (+ view-sample
                            (* sun-step 0.5)))
             (sun-depth-rayleigh 0.0)
             (sun-depth-mie 0.0)
             (hr (* view-step-length
                    (exp (/ view-sample-height +rayleigh-scale-height+))))
             (hm (* view-step-length
                    (exp (/ view-sample-height +mie-scale-height+)))))

        ;; fixme: decide if this should handle hitting ground or not?
        ;; probably not, since we would want to hit actual ground rather than
        ;;  just the sphere approx used for skylight, and use actual rendered
        ;;  color of ground for base color
        (when (<= (length view-sample) +surface-height+)
              (return (vec3 0.2 0.2 0.2)))
        (incf view-depth-rayleigh hr)
        (incf view-depth-mie hm)
        (dotimes (s (- sun-steps 1))
          (incf sun-sample sun-step)
          (let* ((l (length sun-sample))
                 (sun-sample-height (min 0.0 (- +surface-height+ l))))
            #++(when (< l +surface-height+)
              (setf sun-sample-height 0.0))

            (incf sun-depth-rayleigh
                  (* sun-step-length
                     (exp (/ sun-sample-height +rayleigh-scale-height+))))
            (incf sun-depth-mie
                  (* sun-step-length
                     (exp (/ sun-sample-height +mie-scale-height+))))))
        (let ((attenuation
                (exp (- (+ (* +rayleigh-scattering+
                              (+ sun-depth-rayleigh view-depth-rayleigh))
                           (* +mie-scattering+ 1.5
                              (+ sun-depth-mie view-depth-mie)))))))
          (incf rayleigh (* hr attenuation))
          (incf mie (* hm attenuation)))))
    ;;(cos (* 0.53 pi 1/180))0.9999572167930806d0
    ;;(cos (* 0.275 pi 1/180))0.9999884816754271d0
    ;; draw a 'sun' disc ~ 0.53 deg
    (when (>= (dot view-dir sun-dir) 0.99984769515
              ;0.9999572167930806d0
             ;0.9907498873825243d0
             )
      (return (* 100 (exp (- (+ (* +rayleigh-scattering+ view-depth-rayleigh)
                          (* +mie-scattering+ 1.1 view-depth-mie)))))))


    (return (* 1 ; (/ 1 (* 3.141592))
               (+ (* rayleigh (rayleigh-phase (dot view-dir sun-dir))
                     +rayleigh-scattering+)
                  (* mie (mie-phase-schlick (dot view-dir sun-dir)
                                            (phase-g-to-k 0.8))
                         #++(mie-phase-cs (dot view-dir sun-dir)
                                       0.8)
                         #++(mie-phase-hg (dot view-dir sun-dir)
                                       0.8)
                     +mie-scattering+)
                  )))))


;;; assuming planet is at 0,0,0
;;; sun is at +z
;;; and view is on xz plane (+x = 90deg from sun)
;;; (probably should calculate eye pos/dir in planet space outside shaders?
;;;  or at least transform from eye space to planet space?)
;;; -- (planet space probably != 'world' space, since not enough precision
;;;     in single float to be 6km frmo origin)



(defun fragment ()
  (setf out-color (vec4 (calculate-sunlight (vec3 0.0  +surface-height+ 0.0)
                                            (normalize (@ ins eye-dir))
                                            (normalize light-dir)
                                            50
                                            50)
                        1.0)))

;; find intersection with outside of atmosphere
;; step from



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; light enters atmosphere
;;; light is scattered/absorbed depending on 'optical depth'
;;; remaining light crosses view ray, some is scattered into view dir
;;; light scattered into view dir is scattered/absorbed depending
;;;   on optical depth between eye and point of scattering

;;; optical depth = integral of (exp (- height / scale-height)) between 2 points
;;;   -> precalculate for both types of scattering


;;; need (for both mie and rayleigh):
;;;   optical depth between eye and arbitrary point in atmosphere
;;;   optical depth between arbitrary point and edge of atmosphere in dir of sun


;;; precalculate table
;;;   assume atmosphere is symmetrical around earth/sub axis
;;;   precalculate optical depth towards sun from all points in plane
;;;   through earth-sun axis
;;;      x = angle from point
;;;          xx -- angle from earth-sun axis doesn't work well for calculating
;;;                between arbitrary points
;;;      y = height above ground level (normalized to height of atmos)
;;;
;;;   arbitrary point-point optical depth
