(in-package :afp-mapgen-renderer)

(defvar *stage*)
(defvar *cell-size* 29)
(defvar *room-color* '(0.1 0.5 1))
(defvar *wall-color* '(0.2 0.2 0.2))
(defvar *corridor-color* '(0.4 0.4 0.4))
(defvar *connector-color* '(1.0 0.6 0.0))

(defmethod select-color ((object (eql :rect)) cell)
  (cond ((feature-present-p cell :room) *room-color*)
        ((feature-present-p cell :wall) *wall-color*)
        ((feature-present-p cell :corridor) *corridor-color*)
        (t '(0.0 0.0 0.0))))

(defmethod select-color ((object (eql :circle)) cell)
  (cond ((feature-present-p cell :connector)
         *connector-color*)
        (t '(0.0 0.0 0.0))))
