(in-package #:scenegraph)

;;; asset data, render state, etc that don't go directly in scenegraph
;;; (managed by separate 'manager' classes)
(defclass texture ()
  ;; represents a single texture 
  ()
  )
(defclass mesh ()
  ()
  )

;;; scene wrapper, contains scenegraph tree(s) and cached render state
(defclass scene ()
  ;; list? of node trees (ex. world, hud, disjoint sections of world etc)
  ((roots)
   ;; internal state (render tree, dirty flags, etc
   ))


;;; scenegraph tree
(defclass node ()
  ((name) ;; separate from PROPERTIES for faster access?
   (properties)
   (property-cache)
   (bounding-box?)
   (scene)))

(defclass geometry (node)
  ((mesh-ref))
  )

(defclass camera (node)
  ()
  )

(defclass light (node)
  ()
  )

(defclass group (node)
  ((children))
  )

(defclass transform (group)
  ((local-matrix)
   (world-matrix)))

(defclass skinned-mesh (group)
  ((anim) ;; or maybe anim-controller?
   (skeleton))
)
;;; operations
;; add/remove child
;; add/remove attachment
;; modify tranform
;; 
