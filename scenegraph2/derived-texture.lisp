(in-package 3bgl-sg2)

(defmethod load-texture ((type (eql :derived)) name &key target filter format)
  (funcall filter name target format))

;; fixme: figure out how to combine this with get-texture so we can
;; use derived textures as inputs without loading manually
(defun get-derived-texture (filter inputs &key (target :texture-2d)
                                            (format :rgb))
  ;; name is either texture designator or list of texture designators
  ;; normalize to a list of lists in first case. Designator is texture
  ;; object or list of arguments for get-texture.
  (when (and inputs
             (or (not (consp inputs))
                 (and (consp inputs)
                      (not (consp (car inputs)))
                      (not (typep (car inputs) 'texture)))))
    (setf inputs (list inputs)))
  ;; load all input textures if not already
  (setf inputs
        (loop for n in inputs
              if (typep n 'texture)
                collect n
              else
                collect (apply #'get-texture n)))
  ;; store filter function instead of just name, both to avoid
  ;; duplicates when both are passed, and so things get reloaded when
  ;; function is redefined (latter will leave old data in cache, but
  ;; should only happen during dev, so probably not a problem)
  (when (symbolp filter)
    (setf filter (symbol-function filter)))
  (let ((s (list :derived
                 (map 'list 'source inputs)
                 target filter format)))
    (or (gethash s (textures *resource-manager*))
        ;; todo: handle deferred loading once that is implemented...
        (let ((tx (load-texture :derived inputs :target target :filter filter
                                :format format)))
          (when tx
            (setf (gethash s (textures *resource-manager*))
                  (make-instance 'texture
                                 :texture (if (typep tx 'texture)
                                              (texture tx)
                                              tx)
                                 :target target
                                 :source s)))))))
