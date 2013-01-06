(defpackage #:com.clearly-useful.generic-collection-interface.test
  (:use
   #:cl
   #:com.clearly-useful.protocols
   #:com.clearly-useful.generic-collection-interface))

(in-package #:com.clearly-useful.generic-collection-interface.test)




;;;;;;; some mock types to ensure the translation functions
;;;;;;; work correctly.



;; seq 
(defstruct kons
    kar kdr)
  
;; indexable
(defstruct vektor v)
  
;; associative
(defstruct dikt v)

;; constructors

(defvar *knil* (make-kons))

(defun kons (a b)
  (if (eq b *knil*)
      ;;don't want to end up mocking
      ;;null etc
      (kons a nil)
      (make-kons :kar a :kdr b)))

(defun kar (x)
  (unless (eq x *knil*)
    (kons-kar x)))

(defun kdr (x)
  (unless (eq x *knil*)
    (kons-kdr x)))

(defun vektor (&rest list)
  (if list
      (make-vektor :v (make-array (length list) :initial-contents list))
      (make-vektor :v #())))

(defun dikt (&rest list)
  (if list
      (let* ((tbl (make-hash-table :test #'equalp)))
	(loop for (k v) on list by #'cddr
	   do (setf (gethash k tbl) v))
	(make-dikt :v tbl))
      (make-dikt :v (make-hash-table :test #'equalp))))

(defun liszt (&rest list)
  (if list
      (let* ((list (reverse list))
	     (kons (kons (first list) nil)))
	(loop for v in (rest list)
	   do (setf kons (kons v kons))
	   finally (return kons)))
      *knil*))

;;; implement individual protocols

(extend-type kons
  collection
  (empty (o) *knil*)
  (empty-p (o) (eq o *knil*))
  (in (o v) (doseq (q o nil)
              (when (equalp q v)
                (return-from in t))))
  
  seqable
  (seq (o) (unless (eq o *knil*)
	     o))
  seq
  (fst (o) (kar o))
  (rst (o) (let ((v (kdr o)))
             (unless (eq v *knil*)
               v))))

(assert (seq (liszt 1 2 3)))
(assert (equalp '(1 2 3)
		(seq-to-list (liszt 1 2 3))))


(extend-type vektor
  collection
  (empty (o) (declare (ignorable o)) (vektor))
  (empty-p (o) (empty-p (vektor-v o)))
  (in (o val) (in (vektor-v o) val))

  counted-collection
  (counted-p (o) (declare (ignorable o)) t)
  (len (o) (length (vektor-v o)))

  indexed-collection
  (idx (o n) (elt (vektor-v o) n)))

(assert (= 3 (len (vektor 1 2 3))))


(extend-type dikt
  collection
  (empty (o) (declare (ignorable o)) (dikt))
  (empty-p (o) (empty-p (dikt-v o)))
  (in (o k) (in (dikt-v o) k))
  
  associative-collection
  (keys (o) (keys (dikt-v o)))
  (vals (o) (vals (dikt-v o)))
  (key (o k) (key (dikt-v o) k)))

(assert (in (dikt 1 2 3 4) 1))

;;;; check our a b c's

(defvar *a-kons* (liszt 'a 'b 'c))

(defvar *a-vektor* (vektor 'a 'b 'c))

(defvar *a-dikt* (dikt 'a 1 'b 2 'c 3))

(defvar *a-cons* (list 'a 'b 'c))

(defvar *a-vector* (vector 'a 'b 'c))

(defvar *a-hash-table* (dikt-v (dikt 'a 1 'b 2 'c 3)))

(defun sequal (list seq)
  (equalp list (seq-to-list seq)))

(defun set-sequal (list seq)
  (let ((s (seq-to-list seq)))
    (and (null (set-difference s list))
	 (null (set-difference list s)))))

(defun seq-abc (seq)
  (assert (null (seq (empty seq))))
  (assert (eq 'a (fst seq)))
  (assert (eq 'b (fst (rst seq))))
  (assert (sequal '(a b c) seq)))

(defun count-abc (count)
  (assert (= 3 (len count)))
  (assert (eq 'b (idx count 1))))

(defun assoc-abc (assoc &optional
			  (keys '(a b c))
			  (vals '(1 2 3)))
  (assert (set-sequal keys (keys assoc)))
  (assert (set-sequal vals (vals assoc)))
  (assert (in assoc (fst keys))))

;;;; seq conversions

(seq-abc *a-kons*)

(count-abc (indexed-collection *a-kons*))

(assoc-abc (associative-collection *a-kons*)
	   '(0 1 2)
	   '(a b c))


(seq-abc *a-cons*)

(count-abc (indexed-collection *a-cons*))

(assoc-abc (associative-collection *a-cons*)
	   '(0 1 2)
	   '(a b c))

;;;; countable conversions

(count-abc *a-vektor*)

(seq-abc (seq *a-vektor*))

(assoc-abc (associative-collection *a-vektor*)
	   '(0 1 2)
	   '(a b c))

(count-abc *a-vector*)

(seq-abc (seq *a-vector*))

(assoc-abc (associative-collection *a-vector*)
	   '(0 1 2)
	   '(a b c))

;;;; associative conversions

(assoc-abc *a-dikt*)
(assert (not (typep *a-dikt* 'seq)))
(assert (= 3 (len (seq *a-dikt*))))

(assoc-abc *a-hash-table*)
(assert (not (typep *a-hash-table* 'seq)))
(assert (= 3 (len (seq *a-hash-table*))))

;;;; hash table into/conj

(let* ((vals '((a 1) (b 2) (c 3)))
       (tbl (into (make-hash-table) vals)))
  (doseq (list vals)
    (assert (eql (key tbl (car list))
		 (cadr list)))))
