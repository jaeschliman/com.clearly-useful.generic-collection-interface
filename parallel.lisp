(in-package :com.clearly-useful.generic-collection-interface)

(defpun fold-vector (v n combiner reducer)
  (cond
    ((= 0 (length v)) (funcall combiner))
    ((<= (length v) n) (fold-left reducer v
					  :initial-value (funcall combiner)))
    (t (let* ((mid (floor (length v) 2))
	      (v1 (subvec v 0 mid))
	      (v2 (subvec v mid)))
	 (plet ((a (fold-vector v1 n combiner reducer))
		(b (fold-vector v2 n combiner reducer)))
	   (funcall combiner a b)
	   )))))

