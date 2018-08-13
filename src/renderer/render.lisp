(in-package :afp-mapgen)

(defvar *stage*)
(defvar *cell-size* 10)
(defvar *room-color* '(0.1 0.5 1))
(defvar *wall-color* '(0.2 0.2 0.2))

(defmethod select-color ((object (eql :rect)) cell)
  (cond ((feature-present-p cell :room) *room-color*)
        ((feature-present-p cell :wall) *wall-color*)
        (t '(0.0 0.0 0.0))))

(defmethod select-color ((object (eql :circle)) cell)
  '(0.0 0.0 0.0))
