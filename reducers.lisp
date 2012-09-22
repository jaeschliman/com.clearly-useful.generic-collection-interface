(in-package :com.clearly-useful.generic-collection-interface)

#|
;;;; from cl-reducers
(defun curry-right (function &rest args)
  "Curry the ARGS to FUNCTION, placing ARGS last."
  (lambda (&rest more-args)
    (apply function (append more-args args))))
|#

;;; adapted from cl-reducers. adapted for optional docstring
;;; and discriminating nil from no argument
;;; would be good to add compiler macros too.
(defmacro defcurried (name args doc &optional body)
  (flet ((do-curried (name args doc body)
	   (let ((cargs (butlast args))
		 (larg (last args))
		 (x (gensym))
		 (given (gensym)))
	     `(defun ,name  (,@cargs &optional (,@larg nil ,given)) 
		,doc
		(if ,given
		    ,body
		    (lambda (,x) (,name ,@cargs ,x)))))))
    (if (stringp doc)
	(do-curried name args doc body)
	(do-curried name args nil (if body
				      (cons doc body)
				      doc)))))


;;;;;;;;;;;;;; f*

(defcurried fmap (f collection)
  (folder collection (mapping f)))

(defcurried ffilter (pred collection)
  (folder collection (filtering pred)))

(defcurried fmapcat (f collection)
  (folder collection (mapcatting f)))

(defcurried fremove (pred collection)
  (ffilter (complement pred) collection))

(defun sequential? (v)
  (macrolet ((of-type ((&rest types) v)
	       `(or ,@(loop for type in types
			   collect `(typep ,v ',type)))))
    (of-type (cons vector array reduceable indexable seq) v)))

(defcurried fflatten (collection)
  (folder collection
	  (lambda (f1)
	    (lambda (&optional (ret nil given) v)
	      (if (not given)
		  (funcall f1)
		  (if (sequential? v)
		      (fold-left f1 (fflatten v) :initial-value ret)
		      (funcall f1 ret v)))))))

(defcurried fflatten-if (pred collection)
  (folder collection
	  (lambda (f1)
	    (lambda (&optional (ret nil given) v)
	      (if (not given)
		  (funcall f1)
		  (if (and (sequential? v) (funcall pred v))
		      (fold-left f1 (fflatten v) :initial-value ret)
		      (funcall f1 ret v)))))))
;;;;



(defcurried take-while (pred coll)
  (reducer coll
	   (lambda (f1)
	     (lambda (ret val)
	       (if (funcall pred val)
		   (funcall f1 ret val)
		   (reduced ret))))))


(defcurried take (n coll)
  (reducer coll
	   (lambda (f1)
	     (let ((i n))
	       (lambda (ret val)
		 (decf i)
		 (if (< -1 i)
		     (funcall f1 ret val)
		     (reduced ret)))))))

(defcurried drop (n coll)
  (reducer coll
	   (lambda (f1)
	     (let ((i n))
	       (lambda (ret val)
		 (decf i)
		 (if (< -1 i)
		     ret
		     (funcall f1 ret val)))))))
