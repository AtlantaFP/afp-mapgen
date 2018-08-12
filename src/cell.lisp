(in-package :afp-mapgen)

(defclass cell ()
  ((%x :reader x
       :initarg :x)
   (%y :reader y
       :initarg :y)
   (%features :accessor features
              :initform (list :wall))))

(defun cell-index (stage x y)
  (let ((width (width (options stage))))
    (+ (* y width) x)))

(defun make-cell (stage x y)
  (let ((index (cell-index stage x y)))
    (setf (aref (grid stage) index)
          (make-instance 'cell :x x :y y))))

(defun feature-present-p (cell feature)
  (member feature (features cell)))

(defun add-feature (cell feature)
  (pushnew feature (features cell)))

(defun remove-feature (cell feature)
  (afp-utils:deletef (features cell) feature))

(defun feature-intersect (cell &rest features)
  (intersection features (features cell)))
