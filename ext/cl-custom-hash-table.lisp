(ql:quickload :cl-custom-hash-table)

(defpackage :com.clearly-useful.gci.cl-custom-hash-table
  (:use :cl
        :alexandria
        :com.clearly-useful.generic-collection-interface
        :com.clearly-useful.protocols))

(in-package :com.clearly-useful.gci.cl-custom-hash-table)

#+custom-hash-table-fallback
(error "FIXME on an impl that needs it.")
