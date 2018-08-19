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
          :initform (make-seed))
   (%density :reader density
             :initarg :density
             :initform 0.5)
   (%room-extent :reader room-extent
                 :initarg :room-extent
                 :initform 11)
   (%wild-factor :reader wild-factor
                 :initarg :wild-factor
                 :initform 0.25)))

;; TODO: Reimplement with the CL condition system
(defun verify-options (options)
  (unless (and (oddp (width options))
               (plusp (width options)))
    (error "Width must be an odd positive integer."))
  (unless (and (oddp (height options))
               (plusp (height options)))
    (error "Height must be an odd positive integer."))
  (unless (plusp (seed options))
    (error "Seed must be a positive integer."))
  (unless (<= 0.1 (density options) 1.0)
    (error "Density must be between 0.1 and 1.0."))
  (let ((room-extent (room-extent options))
        (max-extent (- (ceiling (max (/ (width options) 2)
                                     (/ (height options) 2)))
                       2)))
    (unless (and (integerp room-extent)
                 (oddp room-extent)
                 (<= 3 room-extent max-extent))
      (error "Room extent must be an odd integer between 3 and ~d" max-extent)))
  (unless (<= 0.0 (wild-factor options) 1.0)
    (error "Wild factor must be between 0.0 and 1.0.")))

(defun make-options (&rest args)
  (let ((options (apply #'make-instance 'options args)))
    (verify-options options)
    options))

(defun stage-options->plist (stage)
  (with-slots (%width %height %seed %density %room-extent) (options stage)
    (list :width (width (options stage))
          :height (height (options stage))
          :seed (seed (options stage))
          :density (density (options stage))
          :room-extent (room-extent (options stage)))))
