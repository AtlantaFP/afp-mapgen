(in-package :afp-mapgen)

(defvar *stage*)
(defvar *cell-size* 10)
(defvar *room-color* '(0.1 0.5 1))
(defvar *wall-color* '(0.2 0.2 0.2))

(defmethod select-color ((object (eql :rect)) cell)
  (cond ((feature-present-p cell :room) *room-color*)
        ((feature-present-p cell :wall) *wall-color*)
        (t '(0.0 0.0 0.0))))

(defmethod select-color ((object (eql :circle)) cell) '(0.0 0.0 0.0))

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

(clim:define-application-frame mcclim-mapgen-renderer ()
  ((attrs :initarg :attrs :reader attrs)
   (stage :reader stage))
  (:panes (toplevel :application-pane :display-function 'render-with-mcclim
                                      :background (apply #'clim:make-rgb-color
                                                         *wall-color*)
                                      :width 1 :height 1))
  (:layouts (default toplevel)))

(defmethod initialize-instance :after ((renderer mcclim-mapgen-renderer) &key attrs)
  (setf (slot-value renderer 'stage) (apply #'make-stage attrs)))

(defun render-with-mcclim (frame pane)
  (let* ((stage (stage frame))
         (opts (options stage)))
    (clim:with-output-recording-options (pane :record t :draw t)
      (loop :for x :below (width opts) :do
        (loop :for y :below (height opts) :do
          (when (feature-present-p (get-cell stage x y) :room)
            (clim:draw-rectangle* pane
                                  (* x *cell-size*) (* y *cell-size*)
                                  (- (* (1+ x) *cell-size*) 1) (- (* (1+ y) *cell-size*) 1)
                                  :ink (apply #'clim:make-rgb-color
                                              (select-color :rect (get-cell stage x y))))))))))

(define-mcclim-mapgen-renderer-command (com-redisplay :name t :menu t)
    ())

(define-mcclim-mapgen-renderer-command (com-new-map :name t :menu t)
    ()
  (setf (slot-value clim:*application-frame* 'stage)
        (apply #'make-stage (afp-utils:plist-remove (attrs clim:*application-frame*) :seed))))


(defmethod render ((method (eql :mcclim)) &rest attrs)
  (let* ((fm (clim:find-frame-manager :port (clim:find-port)))
         (frame (clim:make-application-frame 'mcclim-mapgen-renderer :frame-manager fm
                                                                     :attrs attrs)))
    (labels ((run ()
               (unwind-protect (clim:run-frame-top-level frame)
                 (clim:disown-frame fm frame))))
      (clim-sys:make-process #'run))))



#|
(ql:quickload :clim-listener)


|#


