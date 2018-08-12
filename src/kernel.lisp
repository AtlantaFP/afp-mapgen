(in-package :afp-mapgen)

(defclass kernel-extent ()
  ((%min-x :reader min-x
           :initform 0)
   (%min-y :reader min-y
           :initform 0)
   (%max-x :reader max-x
           :initform 1)
   (%max-y :reader max-y
           :initform 1)))

(defclass kernel ()
  ((%stage :reader stage)
   (%origin-x :reader origin-x)
   (%origin-y :reader origin-y)
   (%extent :reader extent
            :initform (make-instance 'kernel-extent))
   (%selector :reader kernel-selector
              :initform :rectangle)
   (%mapper :reader kernel-mapper
            :initform :default)))

(defun make-kernel (selector mapper &rest extent-args)
  (lambda (stage x y)
    (make-instance 'kernel
                   :stage stage
                   :origin-x x
                   :origin-y y
                   :selector selector
                   :mapper mapper
                   :extent (apply #'make-instance 'extent extent-args))))

(defun resolve-coords (kernel x y)
  (values (+ (origin-x kernel) x)
          (+ (origin-y kernel) y)))

(defun select (kernel x y)
  (with-slots (%stage %selector %extent) kernel
    (when (selector %selector x y %extent)
      (multiple-value-bind (stage-x stage-y) (resolve-coords kernel x y)
        (get-cell %stage stage-x stage-y)))))

(defun kernel-map (kernel func)
  (mapper (kernel-mapper kernel) kernel func))

(defmacro kernel-map/short (kernel func reduce)
  (afp-utils:with-unique-names (block-name cell)
    `(block ,block-name
       (,reduce
        (kernel-map ,kernel
                    (lambda (,cell)
                      (when (funcall ,func ,cell)
                        (return-from ,block-name))))))))

(defun cell->kernel (stage cell layout)
  (funcall layout stage (x cell) (y cell)))

(defun kernel-filter (kernel filter)
  (remove nil
          (kernel-map kernel
                      (lambda (x)
                        (when (funcall filter x)
                          x)))))

;;; Selectors
;;; Define the shape of the kernel

(defmethod selector ((shape (eql :horizontal)) x y extent)
  (and (zerop y)
       (<= (min-x extent) (abs x) (max-x extent))))

(defmethod selector ((shape (eql :vertical)) x y extent)
  (and (zerop x)
       (<= (min-y extent) (abs y) (max-y extent))))

(defmethod selector ((shape (eql :orthogonal)) x y extent)
  (or (selector :horizontal x y extent)
      (selector :vertical x y extent)))

;; TODO: Simplify rectangle selector
(defmethod selector ((shape (eql :rectangle)) x y extent)
  (and (>= x (- (max-x extent)))
       (>= y (- (max-y extent)))
       (<= x (max-x extent))
       (<= y (max-y extent))
       (not (and (> x (- (min-x extent)))
                 (> y (- (min-y extent)))
                 (< x (min-x extent))
                 (< y (min-y extent))))))

(defmethod selector ((shape (eql :ellipse)) x y extent)
  (and (<= (+ (* x x) (* y y)) (* (max-x extent) (max-y extent)))
       (>= (+ (* x x) (* y y)) (* (min-x extent) (min-y extent)))))

;;; Mappers
;;; Define how cells in the kernel are mapped over
;;; Purely an optimization

(defmethod mapper ((shape (eql :horizontal)) kernel func)
  (loop :with max = (max-x (extent kernel))
        :for x :from (- max) :to max
        :for cell = (select kernel x 0)
        :when cell
          :collect (funcall func cell)))

(defmethod mapper ((shape (eql :vertical)) kernel func)
  (loop :with max = (max-y (extent kernel))
        :for y :from (- max) :to max
        :for cell = (select kernel 0 y)
        :when cell
          :collect (funcall func cell)))

(defmethod mapper ((shape (eql :orthogonal)) kernel func)
  (let ((results)
        (max-x (max-x (extent kernel)))
        (max-y (max-y (extent kernel))))
    (loop :for y :from (- max-y) :to max-y
          :for cell = (select kernel 0 y)
          :when cell
            :do (push (funcall func cell) results))
    (loop :for x :from (- max-x) :below 0
          :for cell = (select kernel x 0)
          :when cell
            :do (push (funcall func cell) results))
    (loop :for x :from 1 :to max-x
          :for cell = (select kernel x 0)
          :when cell
            :do (push (funcall func cell) results))
    results))

(defmethod mapper ((shape (eql :default)) kernel func)
  (loop :with max-x = (max-x (extent kernel))
        :with max-y = (max-y (extent kernel))
        :for y :from max-y :downto (- max-y)
        :append (loop :for x :from (- max-x) :to max-x
                      :for cell = (select kernel x y)
                      :when cell
                        :collect (funcall func cell))))

;;; Layouts

(defmethod layout ((shape (eql :horizontal)) &rest extent-args)
  (apply #'make-kernel :horizontal :horizontal extent-args))

(defmethod layout ((shape (eql :vertical)) &rest extent-args)
  (apply #'make-kernel :vertical :vertical extent-args))

(defmethod layout ((shape (eql :orthogonal)) &rest extent-args)
  (apply #'make-kernel :orthogonal :orthogonal extent-args))

(defmethod layout ((shape (eql :rectangle)) &rest extent-args)
  (apply #'make-kernel :rectangle :default extent-args))

(defmethod layout ((shape (eql :ellipse)) &rest extent-args)
  (apply #'make-kernel :ellipse :default extent-args))

;;;

(defun convolve (stage layout filter effect)
  (loop :for x :from 1 :below (1- (width (options stage)))
        :do (loop :for y :from 1 :below (1- (height (options stage)))
                  :for kernel = (funcall layout stage x y)
                  :when (funcall filter kernel)
                    :do (funcall effect kernel))))

(defun collect (stage layout filter)
  (let ((items))
    (convolve stage layout filter (lambda (x) (push x items)))
    items))

(defun process (stage layout filter processor &key items (generator #'identity))
  (loop :with items = (or items (collect stage layout filter))
        :while items
        :for kernel = (funcall generator (pop items))
        :when (funcall filter kernel)
          :do (afp-utils:when-let ((new (funcall processor kernel)))
                (push new items))))
