(in-package :afp-mapgen)

(defstruct (options (:constructor %make-options)
                    (:conc-name nil))
  (width 49)
  (height 49)
  (seed (make-seed)))

(defun make-seed ()
  (values
   (parse-integer
    (afp-utils:shuffle
     (format nil "~d~d"
             (get-universal-time)
             (get-internal-real-time))))))

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
  (let ((options (apply #'%make-options args)))
    (verify-options options)
    options))
