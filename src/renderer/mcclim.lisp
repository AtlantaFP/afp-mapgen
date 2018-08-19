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
      (unless (feature-present-p (get-cell stage x y) :wall)
        (clim:draw-rectangle* medium
                              (* x cell-size) (* y cell-size)
                              (- (* (1+ x) cell-size) 1) (- (* (1+ y) cell-size) 1)
                              :ink (apply #'clim:make-rgb-color (select-color :rect (get-cell stage x y)))))
      (when (feature-present-p (get-cell stage x y) :connector)
        (clim:draw-circle* medium
                           (+ (* x cell-size) (floor cell-size 2))
                           (+ (* y cell-size) (floor cell-size 2))
                           (* cell-size 1/5)
                           :ink (apply #'clim:make-rgb-color (select-color :circle (get-cell stage x y))))))))

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
  ((attrs :initarg :attrs :accessor attrs))
  (:panes (toplevel (clim:make-pane 'stage-display-pane :cell-size *cell-size*))
          (stage-width :text-editor :value (write-to-string (getf (attrs clim:*application-frame*) :width 49)))
          (stage-height :text-editor :value (write-to-string (getf (attrs clim:*application-frame*) :height 49)))
          (stage-seed :text-editor :value (write-to-string (getf (attrs clim:*application-frame*) :seed 1)))
          (stage-room-extent :text-editor :value (write-to-string (getf (attrs clim:*application-frame*) :room-extent 11)))
          (stage-density :slider :value (getf (attrs clim:*application-frame*) :density 0.5)
                                 :min-value 0.1 :max-value 1.0
                                 :orientation :horizontal
                                 :decimal-places 3
                                 :show-value-p t)
          (stage-door-rate :slider :value (getf (attrs clim:*application-frame*) :door-rate 0.5)
                                   :min-value 0.1 :max-value 1.0
                                   :orientation :horizontal
                                   :decimal-places 3
                                   :show-value-p t)
          (stage-wild-factor :slider :value (getf (attrs clim:*application-frame*) :wild-factor 0.5)
                                     :min-value 0.1 :max-value 1.0
                                     :orientation :horizontal
                                     :decimal-places 3
                                     :show-value-p t)
          (stage-cycle-factor :slider :value (getf (attrs clim:*application-frame*) :cycle-factor 0.5)
                                      :min-value 0.1 :max-value 1.0
                                      :orientation :horizontal
                                      :decimal-places 3
                                      :show-value-p t)
          (apply-button :push-button
                        :label "Apply new settings"
                        :activate-callback
                        (lambda (button)
                          (declare (ignore button))
                          (let* ((attrs (attrs clim:*application-frame*))
                                 (stage (apply #'make-stage attrs))
                                 (top (clim:find-pane-named clim:*application-frame* 'toplevel)))
                            (setf (stage top) stage)
                            (redraw-pixmap top))))
          (cell-width-slider :slider :value *cell-size*
                                     :orientation :horizontal
                                     :show-value-p t
                                     :value-changed-callback 'new-cell-size
                                     :min-value 5 :max-value 20))
  (:layouts (default (clim:horizontally ()
                       (1/2 (clim:vertically ()
                              (clim:scrolling () toplevel)
                              (clim:horizontally ()
                                (1/16 (clim:make-pane :label :label "Cell Size:"))
                                (1/32 (clim:make-pane :label :label "5px"))
                                (7/8 cell-width-slider)
                                (1/32 (clim:make-pane :label :label "20px")))))
                       (1/2 (clim:vertically ()
                              (clim:horizontally ()
                                (clim-label "Stage width (odd integer): ")
                                stage-width)
                              (clim:horizontally ()
                                (clim-label "Stage height (odd integer): ")
                                stage-height)
                              (clim:horizontally ()
                                (clim-label "Stage room extent (integer): ")
                                stage-room-extent)
                              (clim:horizontally ()
                                (clim-label "Stage room density (between 0.1 and 1.0): ")
                                stage-density)
                              (clim:horizontally ()
                                (clim-label "Stage cycle factor (between 0.0 and 1.0): ")
                                stage-cycle-factor)
                              (clim:horizontally ()
                                (clim-label "Stage door rate (between 0.0 and 1.0): ")
                                stage-door-rate)
                              (clim:horizontally ()
                                (clim-label "Stage meander rate (between 0.0 and 1.0): ")
                                stage-wild-factor)
                              (clim:horizontally ()
                                (clim-label "Stage seed (integer): ")
                                stage-seed
                                (clim:make-pane :push-button
                                                :label "New Seed"
                                                :activate-callback
                                                (lambda (button)
                                                  (declare (ignore button))
                                                  (setf (clim:gadget-value stage-seed :invoke-callback nil)
                                                        (write-to-string (make-seed))))))
                              apply-button))))))

(defun try-parse-integer (string)
  (multiple-value-bind (num idx) (parse-integer string :junk-allowed t)
    (values (or num 0) (= idx (length string)))))

(defmethod clim:value-changed-callback :after (gadget (frame mcclim-mapgen-renderer) id value)
  (declare (ignore gadget id value))
  (unless (eq :disowned (clim:frame-state frame))
    (afp-utils:mvlet* ((width parsed-full-width
                              (try-parse-integer (clim:gadget-value (clim:find-pane-named frame 'stage-width))))
                       (height parsed-full-height
                               (try-parse-integer (clim:gadget-value (clim:find-pane-named frame 'stage-height))))
                       (seed parsed-full-seed
                             (try-parse-integer (clim:gadget-value (clim:find-pane-named frame 'stage-seed))))
                       (room-extent parsed-full-room-extent
                                    (try-parse-integer (clim:gadget-value (clim:find-pane-named frame 'stage-room-extent))))
                       (density (clim:gadget-value (clim:find-pane-named frame 'stage-density)))
                       (door-rate (clim:gadget-value (clim:find-pane-named frame 'stage-door-rate)))
                       (cycle-factor (clim:gadget-value (clim:find-pane-named frame 'stage-cycle-factor)))
                       (wild-factor (clim:gadget-value (clim:find-pane-named frame 'stage-wild-factor)))
                       (max-extent (- (ceiling (min (/ width 2) (/ height 2))) 2))
                       (attrs (append (list :width width :height height
                                            :density density :room-extent room-extent
                                            :door-rate door-rate
                                            :cycle-factor cycle-factor
                                            :wild-factor wild-factor)
                                      (when (/= seed 0) (list :seed seed))))
                       (valid-p (and parsed-full-width (oddp width)
                                     parsed-full-height (oddp height)
                                     parsed-full-seed
                                     parsed-full-room-extent (oddp room-extent) (<= 3 room-extent max-extent))))
      (if valid-p
          (progn
            (clim:activate-gadget (clim:find-pane-named frame 'apply-button))
            (setf (attrs frame) attrs))
          (clim:deactivate-gadget (clim:find-pane-named frame 'apply-button))))))

(defun new-cell-size (slider value)
  (declare (ignore slider))
  (let ((top (clim:find-pane-named clim:*application-frame* 'toplevel)))
    (setf (cell-size top) value)
    (redraw-pixmap top)))

(defun next-lowest-odd (num)
  (+ (* 2 (truncate (- num 1) 2)) 1))

(defun update-stage (frame)
  (let ((stage (apply #'make-stage (attrs frame)))
        (top (clim:find-pane-named frame 'toplevel)))
    (setf (stage top) stage)))

(defmethod clim:run-frame-top-level :before ((renderer mcclim-mapgen-renderer) &key)
  (update-stage renderer))

(define-mcclim-mapgen-renderer-command
    (com-new-map :name t :menu t)
    ()
  (let ((stage (apply #'make-stage (afp-utils:plist-remove (attrs clim:*application-frame*)
                                                           :seed)))
        (top (clim:find-pane-named clim:*application-frame* 'toplevel)))
    (setf (stage top) stage)
    (redraw-pixmap top)))

(define-mcclim-mapgen-renderer-command (com-redraw-pixmap :name t :menu t)
    ()
  (redraw-pixmap (clim:find-pane-named clim:*application-frame* 'toplevel)))

(defun clim-label (text)
  (clim:make-pane :label :label text))

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
