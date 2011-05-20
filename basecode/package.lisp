(defpackage #:basecode
  (:use :cl)
  (:export #:basecode
           #:init
           #:draw-frame
           #:ortho
           #:perspective
           #:freelook-cam
           #:basecode-init
           #:basecode-cleanup
           #:basecode-tick
           #:basecode-draw
           #:basecode-key-state
           #:basecode-button-state
           #:basecode-glut
           #:basecode-run
           #:fps-graph
           #:basecode-clear
           #:basecode-draw-axes
           #:basecode-draw-ground-plane
           #:perspective-projection
           #:ortho-projection-fixed
           #:basecode-look-at
           #:freelook-camera))
