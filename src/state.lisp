(in-package :afp-mapgen)

(defstruct (state (:constructor make-state (seed))
                  (:conc-name nil))
  (rng (pcg:make-pcg :seed seed)))
