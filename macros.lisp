(in-package #:com.clearly-useful.generic-collection-interface)


(defmacro doseq ((var form &optional return-clause) &body body)
  "   bind var to each item in (seq form) and execute body.
similar to dolist."
  (let ((seq (gensym))
	(tail (gensym)))
    `(let ((,seq (seq ,form)))
       (when ,seq
	 (do* ((,tail ,seq (tail ,tail))
	       (,var (head ,tail)
		     (head ,tail)))
	      ((null ,tail)
	       ,@(when return-clause
		       (list return-clause)))
	   ,@body)))))


(defmacro doindexable ((var form &optional return-clause) &body body)
  "   bind var to each item in (element-at indexed n)
and execute body.
where n is 0..(count-elements indexed)
      and indexed is (indexable form)
similar to dolist."
  (let ((idx (gensym))
	(i (gensym))
	(count (gensym)))
    `(let* ((,idx (indexable ,form))
	    (,count (count-elements ,idx))
	    )
       (when (plusp ,count)
	 (do* ((,i 0 (1+ ,i))
	       (,var (element-at ,idx ,i)
		     (if (= ,i ,count) nil ;;like dolist
			 (element-at ,idx ,i))))
	      ((= ,i ,count)
	       ,@(when return-clause
		       (list return-clause)))
	   ,@body)))))



