(in-package :afp-mapgen)

(defclass stage ()
  ((%options :reader options
             :initarg :options)
   (%grid :reader grid
          :initarg :grid)))

(afp-utils:define-printer (stage stream :type t)
  (let ((width (width (options stage)))
        (height (height (options stage))))
    (format stream "~sx~s" width height)))

(defun make-grid (stage)
  (let* ((options (options stage))
         (width (width options))
         (height (height options)))
    (setf (slot-value stage '%grid) (make-array (* width height)))
    (dotimes (x width)
      (dotimes (y height)
        (make-cell stage x y)))))

(defun make-stage (&rest args)
  (let* ((options (apply #'make-options args))
         (*state* (make-instance 'state :seed (seed options)))
         (stage (make-instance 'stage :options options)))
    (make-grid stage)
    (carve-rooms stage)
    stage))
