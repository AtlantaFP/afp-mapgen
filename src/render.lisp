(in-package :afp-mapgen)

(defvar *stage*)
(defvar *cell-size* 19)
(defvar *room-color* '(0.1 0.5 1))

(defmethod select-color ((object (eql :rect)) cell)
  (cond ((feature-present-p cell :room)
         (apply #'sketch:rgb *room-color*))))

(defmethod select-color ((object (eql :circle)) cell))

(defun draw-rect (cell)
  (sketch:with-pen (sketch:make-pen :fill (select-color :rect cell))
    (sketch:rect (* (x cell) *cell-size*)
                 (* (y cell) *cell-size*)
                 (1- *cell-size*)
                 (1- *cell-size*))))

(defun draw ()
  (sketch:background (sketch:gray 0.2))
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

(defmethod render ((method (eql :text)) &rest attrs)
  (let ((stage (apply #'make-stage attrs)))
    (format t "~&")
    (loop :with width = (width (options stage))
          :with height = (height (options stage))
          :for y :from (1- height) :downto 0
          :do (loop :for x :below width
                    :for cell = (get-cell stage x y)
                    :do (format t "~a"
                                (cond ((feature-present-p cell :door-horizontal) "──")
                                      ((feature-present-p cell :door-vertical) "│ ")
                                      ((feature-present-p cell :stairs-up) "↑↑")
                                      ((feature-present-p cell :stairs-down) "↓↓")
                                      ((feature-intersect cell :corridor :room :junction) "  ")
                                      (t "██"))))
              (format t "~%"))))

(defmethod render ((method (eql :sketch)) &rest attrs)
  (setf *stage* (apply #'make-stage attrs))
  (make-instance 'afp-mapgen :attrs attrs))
