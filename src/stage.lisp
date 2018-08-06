(in-package :afp-mapgen)

(defvar *state*)

(defclass stage ()
  ((options :reader options
            :initarg :options)
   (grid :reader grid
         :initarg :grid)))

(defun make-grid (stage)
  (let* ((options (options stage))
         (width (width options))
         (height (height options)))
    (setf (slot-value stage 'grid) (make-array (* width height)))
    #++(dotimes (x width)
         (dotimes (y height)
           (make-cell stage x y)))))

(defun make-stage (&rest args)
  (let* ((options (apply #'make-options args))
         (*state* (make-state (seed options))))
    *state*)

  #|
  (make-grid)
  (carve-rooms)
  (carve-corridors)
  (connect-regions)
  (carve-junctions)
  (erode-dead-ends) ;; carve corridors part 2
  |#
  )
