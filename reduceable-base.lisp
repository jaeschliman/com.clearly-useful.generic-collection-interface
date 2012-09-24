(in-package #:com.clearly-useful.generic-collection-interface)

(defun monoid (fn epsilon)
  (lambda (&optional (result nil result-given) value)
    (if result-given
	(funcall fn result value)
	epsilon)))

(defun reduce-indexable (r fn seed)
  (let ((result seed))
    (doindexable (v r result)
      (setf result (funcall fn result v)))))

(defun reduce-seq (r fn seed)
  (let ((result seed))
    (doseq (v r result)
      (setf result (funcall fn result v)))))

(defun reduced (v)
  (throw 'coll-reduce v))

(defun reduceable-reduce (fn collection &key (initial-value (funcall fn)))
  (catch 'coll-reduce
    (coll-reduce collection fn initial-value)))

(defun %reduce (fn collection &key (initial-value (funcall fn)))
  (reduceable-reduce fn collection :initial-value initial-value))

(defun fold-left (fn collection &key (initial-value (funcall fn)))
  (reduceable-reduce fn collection :initial-value initial-value))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct %reducer
    coll transformer))

(extend-type %reducer
  reduceable
  (coll-reduce (self fn seed)
	(coll-reduce (%reducer-coll self)
		     (funcall (%reducer-transformer self) fn)
		     seed)))

(defun reducer (collection transformer)
  (make-%reducer :coll collection
		 :transformer transformer))



;;transformers
(defun mapping (fn)
  (lambda (f1)
    (lambda (result value)
      (funcall f1 result (funcall fn value)))))

(defun filtering (pred)
  (lambda (f1)
    (lambda (result value)
      (if (funcall pred value)
	  (funcall f1 result value)
	  result))))

(defun mapcatting (f)
  (lambda (f1)
    (lambda (result value)
      (fold-left f1 result
			 :initial-value (funcall f value)))))

