(in-package :cl-user)

(defpackage #:afp-mapgen
  (:use #:cl)
  (:export #:make-stage
           #:options
           #:stage-options->plist
           #:width
           #:height
           #:seed
           #:grid
           #:x
           #:y
           #:get-cell
           #:region
           #:features
           #:feature-present-p
           #:feature-intersect))
