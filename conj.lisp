(in-package :com.clearly-useful.generic-collection-interface)


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


(defun %conj! (adjustable-vector val)
  (vector-push-extend val adjustable-vector (length adjustable-vector))
  adjustable-vector)

(defgeneric conj (a val)
  (:method ((a null) v) (list v))
  (:method ((a cons) v) (cons v a))
  ;;should be non-destructive
  (:method ((a vector) v)
    ;;;horrible performance, but anyway.
    (let ((vec (%adjustable a)))
      (vector-push-extend v vec)
      vec)))


(defun into (collection reduceable)
  (etypecase collection
    (vector (fold-left #'%conj! reduceable
			       :initial-value (%adjustable collection)))
    (t (fold-left #'conj reduceable :initial-value collection))))

