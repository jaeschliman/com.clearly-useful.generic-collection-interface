(in-package #:com.clearly-useful.generic-collection-interface)

(defun %adjustable (vector &optional (pad 0))
  (let* ((n (length vector))
	 (vec (make-array (+ n pad)
			  :adjustable t
			  :fill-pointer n
			  :element-type (array-element-type vector))))
    (prog1 vec
      (loop
	 for i from 0
	 for elt across vector
	 do (setf (aref vec i) elt)))))

(defun subvec (vec start &optional (end (length vec)))
  (make-array (- end start)
	      :displaced-to vec
	      :displaced-index-offset start
	      :element-type (array-element-type vec)))


#-ccl
(defun %empty-hash (it)
  (make-hash-table
	       :test (hash-table-test it)
	       :size 0))
#+ccl
(defun %empty-hash (it)
  (make-hash-table
	       :test (hash-table-test it)
	       :size 0
	       :weak (ccl:hash-table-weak-p it)
	       ;; TODO : add other options ?
	       ))
