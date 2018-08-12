(in-package :afp-mapgen)

(defclass cell ()
  ((%x :reader x
       :initarg :x)
   (%y :reader y
       :initarg :y)
   (%features :accessor features
              :initform (list :wall))
   (%region :accessor region
            :initform 0)))

(afp-utils:define-printer (cell stream :type t)
  (format stream "~{~a~^, ~}" (features cell)))

(defun cell-index (stage x y)
  (let ((width (width (options stage))))
    (+ (* y width) x)))

(defun get-cell (stage x y)
  (when (and (not (minusp x))
             (not (minusp y))
             (< x (width (options stage)))
             (< y (height (options stage))))
    (aref (grid stage) (cell-index stage x y))))

(defun make-cell (stage x y)
  (let ((index (cell-index stage x y)))
    (setf (aref (grid stage) index)
          (make-instance 'cell :x x :y y))))

;;; Cell features

(defun feature-present-p (cell feature)
  (member feature (features cell)))

(defun add-feature (cell feature)
  (pushnew feature (features cell)))

(defun remove-feature (cell feature)
  (afp-utils:deletef (features cell) feature))

(defun feature-intersect (cell &rest features)
  (intersection features (features cell)))

;;;

(defun carve (cell feature &key (change-region-p t))
  (add-feature cell feature)
  (remove-feature cell :wall)
  (when change-region-p
    (add-cell-to-region cell)))

(defun carved-p (cell)
  (not (feature-present-p cell :wall)))
