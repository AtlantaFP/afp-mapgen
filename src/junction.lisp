(in-package :afp-mapgen)

(defun filter-connectable (kernel)
  (and (not (carved-p (select kernel 0 0)))
       (or (cell-regions-distinct-p (select kernel 0 1) (select kernel 0 -1))
           (cell-regions-distinct-p (select kernel 1 0) (select kernel -1 0)))))

(defun make-connector (kernel)
  (let ((cell (select kernel 0 0))
        (regions (remove 0 (kernel-map kernel #'region))))
    (add-feature cell :connector)
    (push cell (afp-utils:href (connections *state*) regions))))

(defun connect-regions (stage)
  (convolve stage (layout :orthogonal) #'filter-connectable #'make-connector))
