(in-package :afp-mapgen)

(defun draw-rect (cell)
  (sketch:with-pen (sketch:make-pen :fill (apply #'sketch:rgb (select-color :rect cell)))
    (sketch:rect (* (x cell) *cell-size*)
                 (* (y cell) *cell-size*)
                 (1- *cell-size*)
                 (1- *cell-size*))))

(defun draw ()
  (sketch:background (apply #'sketch:rgb *wall-color*))
  (dotimes (x (width (options *stage*)))
    (dotimes (y (height (options *stage*)))
      (let ((cell (get-cell *stage* x y)))
        (draw-rect cell)))))

(sketch:defsketch afp-mapgen
    ((sketch:title "Example Map")
     (sketch:width (* *cell-size* (width (options *stage*))))
     (sketch:height (* *cell-size* (height (options *stage*))))
     (sketch:y-axis :up)
     (attrs nil :accessor nil))
  (draw))

(defmethod kit.sdl2:mousebutton-event :after (window state ts button x y)
  (when (and (eq state :MOUSEBUTTONUP)
             (= button 1))
    (let ((options (stage-options->plist *stage*)))
      (setf *stage* (apply #'make-stage (afp-utils:plist-remove options :seed))))))

(defmethod render ((method (eql :sketch)) &rest attrs)
  (setf *stage* (apply #'make-stage attrs))
  (make-instance 'afp-mapgen :attrs attrs))
