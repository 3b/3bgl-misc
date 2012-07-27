(in-package #:basecode)

(defclass basecode-exit-on-esc ()
  ())

(defmethod key-down :before ((w basecode-exit-on-esc) key)
  (case key
    ((#\esc :escape) (exit-main-loop w))))


