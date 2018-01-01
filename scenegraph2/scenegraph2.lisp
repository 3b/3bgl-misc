(in-package 3bgl-sg2)

;; simplified scenegraph stuff

;;; all nodes have field for (unique?) name to allow searching

;;; internal nodes are just TRANSFORM nodes, with transform and
;;; arbitrary # of children

;;; nodes with content inherit from TRANSFORM:

;;; geometry is represented by INSTANCE nodes, which contain link to
;;; OBJECT, material parameters, optional material overrides, and
;;; arbitrary # of 'attached' children with ref to some node in
;;; object's skeleton.

;;; also probably need somewhere to store skinning info (possibly in
;;; subclass?). probably can have ref to skeleton/inv bind and anim
;;; info in controller, Instance would just add cache of current bone
;;; data (or ref to current bone data on GPU)

;;  not sure about format of material parameters. Hash table or maybe
;;  pre-filled blob of octets ready to dump into UBO? possibly both

;;; children can be any node type (so can attach LIGHTs/CAMERAs to
;;; animated objects)

;;; todo: local LIGHT nodes, containing light parameters
;;; todo: CAMERA nodes, containing camera parameters

(defclass scenegraph ()
  ;; top level of a scenegraph, so we can add extra things like an
  ;; index of nodes or caches or whatever
  ((root :initform nil :accessor root :initarg :root)
   (index :initform (make-hash-table :test 'equal) :reader index)))

(defclass node ()
  ((name :initarg :name :initform nil :reader name)
   (base-name :initarg :name :initform nil :reader base-name)
   ;; PARENT = parent node, or senegraph if at root node
   (parent :initarg :parent :accessor parent)
   (children :initform nil :initarg :children :accessor children)))

(defclass transform (node)
  ((matrix :initarg :matrix :accessor matrix
           :initform sb-cga:+identity-matrix+)))

(defclass instance (transform)
  ;;; fixme: abstract out this object stuff (use object/mesh from res-mgr?)
  ;; 'object' is list of mesh draws, each is a plist of :index
  ;; :material :vertex where material is list of (material-id material
  ;; name) material-id
  ((object :initarg :object :accessor object)
   #++
   (material :initarg :material :accessor material)))

(defclass light (transform)
  ;; todo: lighting: color/intensity in LIGHT, subclasses for
  ;; SUN/DIRECTIONAL, POINT, SPOT, AREA, ?
  ())

(defclass camera (transform)
  ;; todo: camera nodes: projection, fov, near/far, aspect?, ?
  ;; resolution/quality hints?
  ())

(defmethod walk-graph ((node node) &rest keys &key pre post destructive)
  (when pre (funcall pre node))
  (loop for c in (if destructive
                     (copy-list (children node))
                     (children node))
        do (apply 'walk-graph c keys))
  (when post (funcall post node)))

(defun convert-transform-to-instance (sg name instance)
  (let ((n (find-node sg name)))
    (assert (eq (type-of n) 'transform))
    (change-class n 'instance :object instance)))


(defgeneric initargs-from-node (node)
  (:method-combination append))

(defmethod initargs-from-node append ((node node))
  (list :name (base-name node) :parent nil
        :children (mapcar 'copy-node (children node))))

(defmethod initargs-from-node append ((node transform))
  (list :matrix (copy-seq (matrix node))))

(defmethod initargs-from-node append ((node instance))
  ;;todo: :material (material node)
  (list :object (object node)))

(defmethod initargs-from-node append ((node light))
  (error "todo"))

(defmethod initargs-from-node append ((node camera))
  (error "todo"))

(defun copy-node (node)
  (apply #'make-instance (type-of node)
         (initargs-from-node node)))

(defmethod sg ((node node))
  (loop for p = (parent node) then (parent p)
        until (not (typep p 'node))
        finally (return p)))

(defun add-node-to-index (sg node)
  (let ((old (gethash (name node) (index sg))))
    (unless (eql old node)
      (when old
        (setf (slot-value node 'name) (base-name node))
        (loop with base = (base-name node)
              for i from 0
              for n = (format nil "~a.~a" base i)
              ;; todo: add restart to keep trying?
              repeat 32000 ;; give up if we get a lot of collisions...
              unless (gethash n (index sg))
                do (setf (slot-value node 'name) n)
                and return t
              finally (error "name collision adding node ~s to index"
                             (base-name node))))
      (setf (gethash (name node) (index sg)) node))))

(defun remove-node-from-index (sg node)
  (remhash (name node) (index sg))
  (setf (slot-value node 'name) (base-name node)))

(defun add-node* (sg node parent &key copy)
  (when copy
    (setf node (copy-node node)))
  (if parent
      (push node (children parent))
      (setf (root sg) node))
  (setf (parent node) (or parent sg))
  #++(add-node-to-index sg node)
  (walk-graph node :pre (lambda (a) (add-node-to-index sg a)))
  node)

(defun add-node (sg type name parent-name &rest initargs)
  ;;(assert (not (gethash name (index sg))))
  (let* ((p (if (typep parent-name 'node)
                parent-name
                (gethash parent-name (index sg))))
         (n (apply #'make-instance type :name name :parent p initargs)))
    (when (and parent-name (not p))
      (error "couldn't find parent node ~s when adding ~s?" parent-name name))
    #++
    (format t "~%creating node for ~s @ ~s (~s)~%" name parent-name p)
    #++
    (format t "  ~s~%" (substitute 'sb-cga:+identity-matrix+
                                   sb-cga:+identity-matrix+ initargs
                                   :test 'equalp))
    (add-node* sg n p)
    #++
    (format t "added node with name ~s~%" (name n))
    n))

(defun find-node (sg name)
  (gethash name (index sg)))

(defun remove-node (node)
  (let ((sg (sg node))
        (up (parent node)))
    (if (typep up 'node)
        (setf (children up) (remove node (children up)))
        (setf (root up) nil))
    (setf (parent node) nil)
    (walk-graph node :pre (lambda (a)
                            (format t "remove node ~s/~s~%" (name a) a)
                            (remove-node-from-index sg a))))
  node)

(defun dump-scenegraph (root &key id)
  (labels ((r (r i)
             (format t "~va~:[+~; ~]~c "
                     i ""
                     (every 'identity
                            (map 'list '= sb-cga:+identity-matrix+ (matrix r)))
                     (char (symbol-name (type-of r)) 0))
             (if id
                 (print-unreadable-object (r t :identity t)
                   (format t "~a" (name r)))
                 (format t "~a" (name r)))
             (terpri)
             (loop for c in (children r) do (r c (+ 2 i)))))
    (fresh-line)
    (r root 0)))


;;; todo: controllers
;; set TRANSFORM node matrix from (possibly animated) quat, euler
;; angles, translate, scale, dual-quat, quat+xlate, etc?

;; skinning

;; material animation?

;; light property animation (including time of day for sun)

;; keyboard/gamepad control

(defun add-object-to-draw-list (meshes matrix)
  (loop for mesh in meshes
        for index = (getf mesh :index)
        for (mat-name mat-index) = (getf mesh :material)
        for vertex = (getf mesh :vertex)
        do (add-draw mat-name mat-index index vertex matrix)))

(defmethod draw-node ((n transform) &key mv)
  (when (children n)
    (let* ((mv (sb-cga:matrix* mv (matrix n))))
      (loop for c in (children n)
            do (draw-node c :mv mv)))))

(defmethod draw-node ((n instance) &key mv)
  (call-next-method)
  (add-object-to-draw-list (object n) mv))

(defun draw-sg (sg mv)
  (mark *timing-helper* :id :draw-sg-start)
  (draw-node (root sg) :mv mv)
  (mark *timing-helper* :id :draw-sg-done)
  (submit-draws))
