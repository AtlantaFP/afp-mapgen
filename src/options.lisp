(in-package :afp-mapgen)

(defclass options ()
  ((%width :reader width
           :initarg :width
           :initform 49)
   (%height :reader height
            :initarg :height
            :initform 49)
   (%seed :reader seed
          :initarg :seed
          :initform (make-seed))))

(defun verify-options (options)
  (unless (and (oddp (width options))
               (plusp (width options)))
    (error "Width must be an odd positive integer."))
  (unless (and (oddp (height options))
               (plusp (height options)))
    (error "Height must be an odd positive integer."))
  (unless (plusp (seed options))
    (error "Seed must be a positive integer.")))

(defun make-options (&rest args)
  (let ((options (apply #'make-instance 'options args)))
    (verify-options options)
    options))
