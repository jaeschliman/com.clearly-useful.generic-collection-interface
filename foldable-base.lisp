(in-package :com.clearly-useful.generic-collection-interface)


;;; TODO: pick a kernel.
;;; testing is easier with just one
;;; kernel, but may change.

;; (defvar *fold-kernel* nil)

;; (defmacro with-fold-kernel (&body body)
;;   `(let ((lparallel:*kernel* (or *fold-kernel*
;; 				 (setf *fold-kernel*
;; 				       (lparallel:make-kernel 4)))))
;;      ,@body))


(defmacro with-fold-kernel (&body body)
  `(progn
     (unless lparallel:*kernel*
       (setf lparallel:*kernel*
	     (lparallel:make-kernel 8)))
     ,@body))


(defun fold (reducefn collection
	     &key
	       (divide-by 512)
	       (combine-with reducefn))

  #+com.clearly-useful.threading-supported 
  (with-fold-kernel
    (coll-fold collection divide-by combine-with reducefn))

  #-com.clearly-useful.threading-supported
  (fold-left reducefn collection :initial-value (funcall combine-with)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (%folder (:include %reducer))))


(extend-type %folder
  foldable
  (coll-fold (self n combine reduce)
	     (coll-fold (%reducer-coll self)
			n
			combine
			(funcall (%reducer-transformer self)
				 reduce))))


(defun folder (collection transformer)
  (make-%folder
   :coll collection
   :transformer transformer))
