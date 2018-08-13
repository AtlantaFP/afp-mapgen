(in-package :afp-mapgen-renderer)

(defmethod render ((method (eql :repl)) &rest attrs)
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
