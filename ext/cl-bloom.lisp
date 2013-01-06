(ql:quickload :cl-bloom)

(defpackage :com.clearly-useful.gci.cl-bloom
  (:use :cl
        :alexandria
        :com.clearly-useful.generic-collection-interface
        :com.clearly-useful.protocols))

(in-package :com.clearly-useful.gci.cl-bloom)


(extend-type bloom::bloom-filter
  collection
  (empty (o) (bloom:make-compatible-filter o))
  (empty-p (o)
           ;;must be a more efficient way
           ;;to check for a zeroed bit vector
           (notany 'plusp (slot-value o 'array)))
  (in (o a)
      ;;would be nice to return a float
      ;;representing probability
      (bloom:memberp o a)))

(defmethod conj ((a bloom::bloom-filter) val)
  (let ((f (bloom:copy-filter a)))
    (bloom:add f val)
    f))
