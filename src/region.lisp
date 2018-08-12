(in-package :afp-mapgen)

(defun make-region ()
  (incf (current-region *state*)))

(defun add-cell-to-region (cell)
  (let ((region (current-region *state*)))
    (push cell (afp-utils:href (regions *state*) region))
    (setf (region cell) region)))
