(in-package :afp-mapgen)

(defstruct (cell (:constructor %make-cell))
  x
  y
  (features (list :wall)))

(defun cell-index (stage x y)
  (let ((width (width (options stage))))
    (+ (* y width) x)))

(defun make-cell (stage x y)
  (let ((index (cell-index stage x y)))
    (setf (aref (grid stage) index)
          (%make-cell :x x :y y))))

(defun feature-present-p (cell feature)
  (member feature (cell-features cell)))

(defun add-feature (cell feature)
  (pushnew feature (cell-features cell)))

(defun remove-feature (cell feature)
  (afp-utils:deletef (cell-features cell) feature))

(defun feature-intersect (cell &rest features)
  (intersection features (cell-features cell)))
