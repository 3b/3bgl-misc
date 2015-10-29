(in-package #:scenegraph)

(defparameter *v* nil)

(defclass scenegraph-state-helper ()
  ())

(defmethod basecode::run-main-loop :around ((w scenegraph-state-helper))
  (let ((*runtime-values-cache* (make-hash-table))
        (*known-states* (make-hash-table :test #'equalp))
        (*state-defaults* (make-hash-table)))
    (setf *v* (list :cache *runtime-values-cache*
                    :known *known-states*
                    :defaults *state-defaults*))
    (init-defaults *state-defaults*)
    (call-next-method)))
