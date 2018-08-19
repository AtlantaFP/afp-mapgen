(in-package :afp-mapgen-renderer)

(defclass stage-display-pane (clim:basic-gadget)
  ((pixmap :accessor pixmap :initform nil)
   (cell-size :accessor cell-size :initform *cell-size* :initarg :cell-size)
   (stage :accessor stage :initarg :stage :initform nil))
  (:default-initargs :background (apply #'clim:make-rgb-color *wall-color*)))

(defmethod (setf pixmap) :after (pixmap (pane stage-display-pane))
  (clim:change-space-requirements pane :width (clim:pixmap-width pixmap)
                                       :height (clim:pixmap-height pixmap)))

(defmethod (setf stage) :after (stage (pane stage-display-pane))
  (redraw-pixmap pane))

(defmethod redraw-pixmap ((pane stage-display-pane))
  (when (pixmap pane)
    (clim:deallocate-pixmap (pixmap pane)))
  (setf (slot-value pane 'pixmap) nil)
  (clim:handle-repaint pane clim:+everywhere+))

(defun draw-stage (stage width height medium cell-size)
  (clim:draw-rectangle* medium 0 0 (* width cell-size) (* height cell-size)
                        :ink (apply #'clim:make-rgb-color *wall-color*))
  (loop :for x :below width :do
    (loop :for y :below height :do
      (when (feature-present-p (get-cell stage x y) :room)
        (clim:draw-rectangle* medium
                              (* x cell-size) (* y cell-size)
                              (- (* (1+ x) cell-size) 1) (- (* (1+ y) cell-size) 1)
                              :ink (apply #'clim:make-rgb-color (select-color :rect (get-cell stage x y))))))))

(defun draw-stage-to-pixmap (stage pane &optional (cell-size (cell-size pane)))
  (let* ((opts (options stage))
         (width (width opts))
         (height (height opts)))
    (clim:with-output-to-pixmap (pixmap pane :width (* width cell-size)
                                             :height (* height cell-size))
      (draw-stage stage width height pixmap cell-size))))

(defmethod clim:handle-repaint ((pane stage-display-pane) region)
  (declare (ignore region))
  (unless (pixmap pane)
    (setf (pixmap pane) (draw-stage-to-pixmap (stage pane) pane)))
  (let ((pixmap (pixmap pane)))
    (afp-utils:when-let ((vp (clim:pane-viewport-region pane)))
      (clim:with-bounding-rectangle* (min-x min-y max-x max-y) vp
        (clim:draw-rectangle* pane min-x min-y max-x max-y :ink clim:+background-ink+)))
    (clim:copy-from-pixmap pixmap
                           0 0 (clim:pixmap-width pixmap) (clim:pixmap-height pixmap)
                           pane 0 0)))


(clim:define-application-frame mcclim-mapgen-renderer ()
  ((width :accessor stage-width :initarg :width :initform 49)
   (height :accessor stage-height :initarg :height :initform 49)
   (attrs :initarg :attrs :accessor attrs))
  (:panes (toplevel (clim:make-pane 'stage-display-pane :cell-size *cell-size*))
          (width-box :text-editor
                     :value-changed-callback
                     (lambda (gadget value)
                       (declare (ignore gadget))
                       (setf (stage-width clim:*application-frame*) (or (parse-integer value :junk-allowed t) 49))))
          (height-box :text-editor
                      :value-changed-callback
                      (lambda (gadget value)
                        (declare (ignore gadget))
                        (setf (stage-height clim:*application-frame*) (or (parse-integer value :junk-allowed t) 49))))
          (cell-width-slider :slider :value *cell-size*
                                     :orientation :horizontal
                                     :show-value-p t
                                     :value-changed-callback 'new-cell-size
                                     :min-value 5 :max-value 20)
          (apply-button :push-button
                        :activate-callback
                        (lambda (gadget)
                          (declare (ignore gadget))
                          (update-stage clim:*application-frame*))
                        :label ""))
  (:layouts (default (clim:horizontally (:equalize-height nil)
                       (clim:scrolling () toplevel)
                       (clim:scrolling ()
                         (clim:vertically ()
                           (clim:make-pane :label :label "Cell Size:")
                           (clim:horizontally ()
                             (1/8 (clim:make-pane :label :label "5px"))
                             cell-width-slider
                             (1/8 (clim:make-pane :label :label "20px")))
                           (clim:horizontally (:width 400)
                             (clim:make-pane :label :label "Width:")
                             width-box
                             (clim:make-pane :label :label "Height:")
                             height-box)
                           apply-button))))))

(defun new-cell-size (slider value)
  (declare (ignore slider))
  (let ((top (clim:find-pane-named clim:*application-frame* 'toplevel)))
    (setf (cell-size top) value)
    (redraw-pixmap top)))

(defun next-lowest-odd (num)
  (+ (* 2 (truncate (- num 1) 2)) 1))

(defun update-stage (frame)
  (with-accessors ((height stage-height)
                   (width stage-width))
      frame
    (let ((rounded (next-lowest-odd width)))
      (when (/= width rounded)
        (setf width rounded)
        (setf (clim:gadget-value (clim:find-pane-named frame 'width-box)) (write-to-string rounded))))
    (let ((rounded (next-lowest-odd height)))
      (when (/= height rounded)
        (setf height rounded)
        (setf (clim:gadget-value (clim:find-pane-named frame 'height-box)) (write-to-string rounded))))
    (let ((stage (apply #'make-stage :height height :width width (attrs frame)))
          (top (clim:find-pane-named frame 'toplevel)))
      (setf (stage top) stage))))

(defmethod clim:run-frame-top-level :before ((renderer mcclim-mapgen-renderer) &key)
  (setf (clim:gadget-value (clim:find-pane-named renderer 'width-box)) (write-to-string (stage-width renderer)))
  (setf (clim:gadget-value (clim:find-pane-named renderer 'height-box)) (write-to-string (stage-height renderer)))
  (update-stage renderer))

(define-mcclim-mapgen-renderer-command (com-new-map :name t :menu t)
    ()
  (let ((stage (apply #'make-stage (afp-utils:plist-remove (attrs clim:*application-frame*)
                                                           :seed)))
        (top (clim:find-pane-named clim:*application-frame* 'toplevel)))
    (setf (stage top) stage)
    (redraw-pixmap top)))

(define-mcclim-mapgen-renderer-command (com-redraw-pixmap :name t :menu t)
    ()
  (redraw-pixmap (clim:find-pane-named clim:*application-frame* 'toplevel)))


(defmethod render ((method (eql :mcclim)) &rest attrs &key (width 49) (height 49))
  (let* ((fm (clim:find-frame-manager :port (clim:find-port)))
         (frame (clim:make-application-frame 'mcclim-mapgen-renderer :frame-manager fm
                                                                     :width width
                                                                     :height height
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
