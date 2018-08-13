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

(defclass stage-display-pane (clim:basic-gadget)
  ((pixmap :reader pixmap :initform nil)))

(defun draw-stage-to-pixmap (stage pane)
  (let* ((opts (options stage))
         (width (width opts))
         (height (height opts)))
    (clim:with-output-to-pixmap (pixmap pane :width (* width *cell-size*)
                                             :height (* height *cell-size*))
      (clim:draw-rectangle* pixmap 0 0 (* width *cell-size*) (* height *cell-size*)
                            :ink (apply #'clim:make-rgb-color *wall-color*))
      (loop :for x :below width :do
        (loop :for y :below height :do
          (when (feature-present-p (get-cell stage x y) :room)
            (clim:draw-rectangle* pane
                                  (* x *cell-size*) (* y *cell-size*)
                                  (- (* (1+ x) *cell-size*) 1) (- (* (1+ y) *cell-size*) 1)
                                  :ink (apply #'clim:make-rgb-color
                                              (select-color :rect (get-cell stage x y))))))))))

(defmethod clim:handle-repaint ((pane stage-display-pane) region)
  (declare (ignore region))
  (with-slots (pixmap) pane
    (unless pixmap
      (setf (slot-value pane 'pixmap)
            (draw-stage-to-pixmap (stage clim:*application-frame*) pane))
      (clim:change-space-requirements pane :width (clim:pixmap-width pixmap)
                                           :height (clim:pixmap-height pixmap)))
    (clim:copy-from-pixmap pixmap
                           0 0 (clim:pixmap-width pixmap) (clim:pixmap-height pixmap)
                           pane 0 0)))

(clim:define-application-frame mcclim-mapgen-renderer ()
  ((attrs :initarg :attrs :reader attrs)
   (stage :reader stage))
  (:panes (toplevel (clim:make-pane 'stage-display-pane)))
  (:layouts (default toplevel)))


(defmethod initialize-instance :after ((renderer mcclim-mapgen-renderer) &key attrs)
  (let ((stage (apply #'make-stage attrs)))
    (setf (slot-value renderer 'stage) stage)))

(define-mcclim-mapgen-renderer-command (com-redisplay :name t :menu t)
    ())

(define-mcclim-mapgen-renderer-command (com-new-map :name t :menu t)
    ()
  (let ((stage (apply #'make-stage (afp-utils:plist-remove (attrs clim:*application-frame*)
                                                           :seed))))
    (let ((p (clim:find-pane-named clim:*application-frame* 'toplevel)))
      (when (pixmap p)
        (clim:deallocate-pixmap (pixmap p)))
      (setf (slot-value clim:*application-frame* 'stage) stage
            (slot-value p 'pixmap) nil)
      (clim:handle-repaint p clim:+everywhere+))))


(defmethod render ((method (eql :mcclim)) &rest attrs)
  (let* ((fm (clim:find-frame-manager :port (clim:find-port)))
         (frame (clim:make-application-frame 'mcclim-mapgen-renderer :frame-manager fm
                                                                     :attrs attrs)))
    (labels ((run ()
               (unwind-protect (clim:run-frame-top-level frame)
                 (when (pixmap (clim:find-pane-named frame 'toplevel))
                   (clim:deallocate-pixmap (pixmap (clim:find-pane-named frame 'toplevel))))
                 (clim:disown-frame fm frame))))
      (clim-sys:make-process #'run))))



#|
(ql:quickload :clim-listener)


|#


