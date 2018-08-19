(in-package :afp-mapgen)

(defvar *state*)

(defclass state ()
  ((%rng :reader rng)
   (%current-region :accessor current-region
                    :initform 0)
   (%regions :reader regions
             :initform (afp-utils:dict #'eql))
   (%connections :reader connections
                 :initform (afp-utils:dict #'equal))
   (%dead-ends :accessor dead-ends
               :initform nil)))

(defmethod initialize-instance :after ((instance state) &key seed)
  (setf (slot-value instance '%rng) (pcg:make-pcg :seed seed)))
