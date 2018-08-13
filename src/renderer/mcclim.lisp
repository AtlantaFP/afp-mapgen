(in-package :afp-mapgen)

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
                                  :ink (apply #'clim:make-rgb-color (select-color :rect (get-cell stage x y))))))))))

(defmethod clim:handle-repaint ((pane stage-display-pane) region)
  (declare (ignore region))
  (unless (pixmap pane)
    (setf (slot-value pane 'pixmap) (draw-stage-to-pixmap (stage clim:*application-frame*) pane))
    (clim:change-space-requirements pane :width (clim:pixmap-width (pixmap pane))
                                         :height (clim:pixmap-height (pixmap pane))))
  (clim:copy-from-pixmap (pixmap pane)
                         0 0 (clim:pixmap-width (pixmap pane)) (clim:pixmap-height (pixmap pane))
                         pane 0 0))

(clim:define-application-frame mcclim-mapgen-renderer ()
  ((attrs :initarg :attrs :reader attrs)
   (stage :reader stage))
  (:panes (toplevel (clim:make-pane 'stage-display-pane)))
  (:layouts (default (clim:scrolling () toplevel))))


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
