(in-package :afp-mapgen)

(defun filter-carvable (kernel)
  (not (kernel-detect kernel #'carved-p)))

(defun choose-corridor-cell (stage cells)
  (if (> (random-float (rng *state*)) (wild-factor (options stage)))
      (random-element (rng *state*) cells)
      (first cells)))

;;;

(defun choose-corridor-direction (kernel)
  (let ((results))
    (dolist (dir '((0 1) (0 -1) (1 0) (-1 0)))
      (afp-utils:when-let ((cell1 (apply #'select kernel dir))
                           (cell2 (apply #'select kernel (mapcar #'+ dir dir))))
        (unless (carved-p cell2)
          (push (list cell1 cell2) results))))
    (random-element (rng *state*) results)))

(defun carve-direction (kernel cells)
  (let ((origin (select kernel 0 0)))
    (afp-utils:if-let ((choice (choose-corridor-direction kernel)))
      (loop :for cell :in choice
            :do (carve cell :corridor)
            :finally (return (push cell cells)))
      (progn
        (push origin (dead-ends *state*))
        (delete origin cells :count 1)))))

;; TODO: Write tail-recursive with LABELS
(defun carve-corridor-cell (kernel)
  (let ((origin (select kernel 0 0)))
    (make-region)
    (carve origin :corridor)
    (loop :with stage = (stage kernel)
          :with layout = (layout :orthogonal :max-x 2 :max-y 2)
          :with cells = (list origin)
          :while cells
          :for cell = (choose-corridor-cell stage cells)
          :for kernel = (cell->kernel stage cell layout)
          :do (setf cells (carve-direction kernel cells)))))

(defun carve-corridors (stage)
  (convolve stage (layout :rectangle) #'filter-carvable #'carve-corridor-cell))
