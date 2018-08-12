(in-package :afp-mapgen)

(defvar *state*)

(defclass state ()
  ((%rng :reader rng)))

(defmethod initialize-instance :after ((instance state) &key seed)
  (setf (slot-value instance '%rng) (pcg:make-pcg :seed seed)))
