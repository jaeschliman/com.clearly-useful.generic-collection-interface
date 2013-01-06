(ql:quickload :optima)

(defpackage #:com.clearly-useful.gci.optima
  (:use #:cl
        #:alexandria
        #:optima
        #:com.clearly-useful.gci))

(in-package #:com.clearly-useful.gci.optima)

(defun %pair-p (o) (typep o 'seq))
(defun %pair-fst (o) (fst o))
(defun %pair-rst (o) (rst o))

(defpattern pair (a b)
  `(%pair- (fst ,a) (rst ,b)))

;;; need to add a length check.

(defpattern seq (&rest args)
  (let ((v (member '&rest args))
        (r (lambda (v a) `(pair ,v ,a))))
    (if v
        (reduce r args :from-end t
                :end (position '&rest args)
                :initial-value (cadr v))
        (reduce r args :from-end t :initial-value nil))))


;;--------------------------------------------------

(defun x-p (o) (declare (ignore o)) t)
(defun x-x (o) (declare (ignore o)) 17)
(defun x-f (o b) (list o b))

(defun key? (o k)
  (declare (ignore o))
   (eq k :key))

(defun val-of (o k)
  (declare (ignore o k))
  10)

(match 20
  ((and (when (key? * :key))
        (eql (val-of * :key) a)) a))

(match 10
  ((x- (x a) (f :x b)) (list a b)) )


(defvar *c (make-hash-table))
(setf (gethash :x *c) 100
      (gethash :y *c) 200)

(defmethod optima::parse-constructor-pattern ((name (eql 'ht))
                                              &rest args)
  (let (ks vs)
    (loop for (k v) on args by #'cddr
         do (push k ks)
         do (push v vs))
    (unless (= (length ks)
               (length vs))
      (error "death has resluted "))
    (optima::make-constructor-pattern
     :signature `(ht ,@ks)
     :arguments (mapcar #'optima::parse-pattern vs)
     :predicate (lambda (v) `(typep ,v 'hash-table))
     :accessor (lambda (v i) `(gethash (nth ,i ',ks) ,v)))))

(defmethod optima::parse-constructor-pattern ((name (eql 'keys))
                                              &rest args)
  (let (ks vs)
    (loop for k in args
         do (push k ks)
         do (push (if (keywordp k)
                      (intern (symbol-name k))
                      k) vs))
    
    (optima::make-constructor-pattern
     :signature `(keys ,@ks)
     :arguments (mapcar #'optima::parse-pattern vs)
     :predicate (lambda (v) `(typep ,v 'hash-table))
     :accessor (lambda (v i) `(gethash (nth ,i ',ks) ,v)))))



(match (list *c 'x 'y 'z)
  ((seq (keys :x :y) &rest foos) (list x y foos)))



