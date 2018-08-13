(in-package :cl-user)

(defpackage #:afp-mapgen
  (:use #:cl)
  (:export #:make-stage
           #:width
           #:height
           #:seed
           #:grid
           #:x
           #:y
           #:region
           #:features
           #:feature-present-p
           #:feature-intersect))
