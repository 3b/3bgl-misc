(in-package 3bgl-sg2)

;; simplified scenegraph stuff

;;; all nodes have field for (unique?) name to allow searching

;;; most internal nodes are just TRANSFORM nodes, with transform (in
;;; various formats) and arbitrary # of children

;;; geometry is represented by INSTANCE nodes, which contain link to
;;; OBJECT, material parameters, optional material overrides, and
;;; arbitrary # of 'attached' children with ref to some node in
;;; object's skeleton (non-attached children should be siblings
;;; instead, since INSTANCE has no local transform or inherited
;;; properties)
;;  not sure about format of material parameters. Hash table or maybe
;;  pre-filled blob of octets ready to dump into UBO? possibly both

;;; children can be any node type (so can attach LIGHTs/CAMERAs to
;;; animated objects)

;;; todo: local LIGHT nodes, containing light parameters
;;; todo: CAMERA nodes, containing camera parameters

;;; todo: controller nodes? ex. animating position of an
;;; object/light/camera, keyboard/gamepad control, etc.
;;? not sure if controllers are special nodes, extra field of normal
;;  nodes, or external to scenegraph completely?
;;;

(defclass scenegraph ()
  ;; top level of a scenegraph, so we can add extra things like an
  ;; index of nodes or caches or whatever
  ((root :initform nil :accessor root :initarg :root)
   (index :initform (make-hash-table :test 'equal) :reader index)))

(defclass node ()
  ((name :initarg :name :initform nil :accessor name)
   (parent :initarg :parent :accessor parent)
   (children :initform nil :accessor children)))

(defclass transform (node)
  ((matrix :initarg :matrix :accessor matrix)))

;; todo: quat, euler angles, translate, scale, dual-quat, quat+xlate, etc?

(defclass instance (node)
  ((object :initarg :object :accessor object)
   (material :initarg :materrial :accessor material)))

(defclass light (node)
  ;; todo: lighting: color/intensity in LIGHT, subclasses for
  ;; SUN/DIRECTIONAL, POINT, SPOT, AREA, ?
  ())

(defclass camera (node)
  ;; todo: camera nodes: projection, fov, near/far, aspect?, ?
  ;; resolution/quality hints? not sure if it needs any
  ;; orientation/position info, for now assuming that is controlled by
  ;; parent TRANSFORM nodes
  ())

(defun add-node* (sg node parent)
  (if parent
      (push node (children parent))
      (setf (root sg) node))
  (setf (gethash (name node) (index sg)) node))

(defun add-node (sg type name parent-name &rest initargs)
  (assert (not (gethash name (index sg))))
  (let* ((p (gethash parent-name (index sg)))
         (n (apply #'make-instance type :name name :parent p initargs)))
    (format t "creating node for ~s @ ~s (~s)~%" name parent-name p)
    (add-node* sg n p)
    n))
