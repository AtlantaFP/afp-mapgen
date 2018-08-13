(in-package :afp-mapgen-renderer)

(defclass stage-display-pane (clim:basic-gadget)
  ((pixmap :reader pixmap :initform nil))
  (:default-initargs :background (apply #'clim:make-rgb-color *wall-color*)))

(defmethod redraw-pixmap ((pane stage-display-pane))
  (when (pixmap pane)
    (clim:deallocate-pixmap (pixmap pane)))
  (setf (slot-value pane 'pixmap) nil)
  (clim:handle-repaint pane clim:+everywhere+))

(defun draw-stage-to-pixmap (stage pane)
  (let* ((opts (options stage))
         (width (width opts))
         (height (height opts))
         (cell-size (cell-size clim:*application-frame*)))
    (clim:with-output-to-pixmap (pixmap pane :width (* width cell-size)
                                             :height (* height cell-size))
      (clim:draw-rectangle* pixmap 0 0 (* width cell-size) (* height cell-size)
                            :ink (apply #'clim:make-rgb-color *wall-color*))
      (loop :for x :below width :do
        (loop :for y :below height :do
          (when (feature-present-p (get-cell stage x y) :room)
            (clim:draw-rectangle* pane
                                  (* x cell-size) (* y cell-size)
                                  (- (* (1+ x) cell-size) 1) (- (* (1+ y) cell-size) 1)
                                  :ink (apply #'clim:make-rgb-color (select-color :rect (get-cell stage x y))))))))))

(defmethod clim:handle-repaint ((pane stage-display-pane) region)
  (declare (ignore region))
  (unless (pixmap pane)
    (setf (slot-value pane 'pixmap) (draw-stage-to-pixmap (stage clim:*application-frame*) pane))
    (clim:change-space-requirements pane :width (clim:pixmap-width (pixmap pane))
                                         :height (clim:pixmap-height (pixmap pane))))
  (clim:with-bounding-rectangle* (min-x min-y max-x max-y) (clim:pane-viewport-region pane)
    (clim:draw-rectangle* pane min-x min-y max-x max-y :ink clim:+background-ink+)
    (clim:copy-from-pixmap (pixmap pane)
                           0 0 (clim:pixmap-width (pixmap pane)) (clim:pixmap-height (pixmap pane))
                           pane 0 0)))

(clim:define-application-frame mcclim-mapgen-renderer ()
  ((attrs :initarg :attrs :reader attrs)
   (cell-size :initarg :cell-size :initform *cell-size* :accessor cell-size)
   (stage :reader stage))
  (:panes (toplevel (clim:make-pane 'stage-display-pane))
          (cell-width-slider :slider :value *cell-size*
                                     :orientation :horizontal
                                     :show-value-p t
                                     :value-changed-callback 'new-cell-size
                                     :min-value 5 :max-value 20)
          (apply-button :push-button :activate-callback
                        (lambda (gadget)
                          (declare (ignore gadget))
                          (redraw-pixmap (clim:find-pane-named clim:*application-frame* 'toplevel)))))
  (:layouts (default (clim:horizontally (:equalize-height nil)
                       (clim:scrolling (:height 9000) toplevel)
                       (clim:scrolling ()
                         (clim:vertically ()
                           (clim:make-pane :label :label "Cell Size:")
                           (clim:horizontally ()
                             (1/8 (clim:make-pane :label :label "5px"))
                             cell-width-slider
                             (1/8 (clim:make-pane :label :label "20px")))
                           apply-button))))))

(defun new-cell-size (slider value)
  (declare (ignore slider))
  (setf (cell-size clim:*application-frame*) value)
  (redraw-pixmap (clim:find-pane-named clim:*application-frame* 'toplevel)))


(defmethod initialize-instance :after ((renderer mcclim-mapgen-renderer) &key attrs)
  (let ((stage (apply #'make-stage attrs)))
    (setf (slot-value renderer 'stage) stage)))

(define-mcclim-mapgen-renderer-command (com-new-map :name t :menu t)
    ()
  (let ((stage (apply #'make-stage (afp-utils:plist-remove (attrs clim:*application-frame*)
                                                           :seed))))
    (setf (slot-value clim:*application-frame* 'stage) stage)
    (redraw-pixmap (clim:find-pane-named clim:*application-frame* 'toplevel))))

(define-mcclim-mapgen-renderer-command (com-redraw-pixmap :name t :menu t)
    ()
  (redraw-pixmap (clim:find-pane-named clim:*application-frame* 'toplevel)))


(defmethod render ((method (eql :mcclim)) &rest attrs)
  (let* ((fm (clim:find-frame-manager :port (clim:find-port)))
         (frame (clim:make-application-frame 'mcclim-mapgen-renderer :frame-manager fm
                                                                     :attrs attrs)))
    (labels ((run ()
               (unwind-protect (clim:run-frame-top-level frame)
                 (when (pixmap (clim:find-pane-named frame 'toplevel))
                   (clim:deallocate-pixmap (pixmap (clim:find-pane-named frame 'toplevel))))
                 (clim:disown-frame fm frame))))
      (clim-sys:make-process #'run)
      frame)))



#|
(ql:quickload :clim-listener)


|#
