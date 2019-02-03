(in-package :afp-mapgen)

(defun estimate-room-count (stage)
  (with-slots (%width %height %density %room-extent) (options stage)
    (let* ((min 9)
           (max (expt %room-extent 2))
           (average (+ min (/ (- max min) 2))))
      (values (floor (/ (* %width %height %density) average))))))

(defun generate-room-properties (stage)
  (with-slots (%width %height %room-extent) (options stage)
    (with-slots (%rng) *state*
      (let* ((w (random-int %rng :min 3 :max %room-extent :parity-p t))
             (h (random-int %rng :min 3 :max %room-extent :parity-p t))
             (x (random-int %rng :min 1 :max (- %width w) :parity-p t))
             (y (random-int %rng :min 1 :max (- %height h) :parity-p t)))
        (values x y w h)))))

(defun carve-room (stage)
  (multiple-value-bind (x y w h) (generate-room-properties stage)
    (let* ((rx (floor w 2))
           (ry (floor h 2))
           (px (+ rx x))
           (py (+ ry y))
           (k1 (funcall (layout :rectangle :max-x rx :max-y ry) stage px py))
           (k2 (funcall (layout :rectangle :max-x (1+ rx) :max-y (1+ ry)) stage px py)))
      (unless (kernel-detect k2 #'carved-p)
        (make-region)
        (kernel-map k1 (lambda (x) (carve x :room)))))))

(defun carve-rooms (stage)
  (loop :with max = (estimate-room-count stage)
        :with count = 0
        :with tries = 0
        :until (or (= count max)
                   (>= tries 500))
        :do (if (carve-room stage)
                (progn
                  (setf tries 0)
                  (incf count))
                (incf tries))))
