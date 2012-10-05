;; simple tests and examples for generic sequences.
;;
;; this file should compile and load without complaint.

;; FIXME: some the tests in this file are more relevant to
;; the generic collection interface. this file should test
;; only what can be done with the package in this system

(defpackage #:com.clearly-useful.sequence-protocol.test
  (:use #:cl
	;;;should be testing .sequence-protocol
	#:com.clearly-useful.generic-collection-interface))

(in-package #:com.clearly-useful.sequence-protocol.test)

(defmacro with-collector ((&optional (collector-name 'collect)) &body body)
  (let ((result (gensym)))
    `(let ((,result (list)))
       (flet ((,collector-name (arg) (push arg ,result)))
         (progn ,@body)
         (when ,result
           (nreverse ,result))))))

(defun test-builtin (seqable-object expected-head expected-tail)
  "test the extensions to builtin sequences, i.e. those
that respond to equalp. seqs will respond to indexable as well."
  (assert (seqable-p seqable-object))
  (let ((object (seq seqable-object)))
    (assert (seq-p object))
    (assert (equalp (head object) expected-head))
    (assert (equalp (tail object) expected-tail))
    (let ((as-list (seq-to-list object))
	  (length (count-elements object)))
      ;;counts ok
      (assert (= length
		 (list-length as-list)))
      ;;elts ok
      (loop
	 for i below length
	 for o in as-list
	 do (assert (equalp o (element-at object i)))))))


(defun mapply (fn list)
  (mapcar (lambda (list) (apply fn list)) list))

(mapply #'test-builtin
	'(;null
	  (nil nil nil)
	  
	  ;cons
	  ((a b c) a (b c))
	  
	  ;vector
	  ("abc" #\a "bc")
	  
	  ;vector again
	  (#(a b c) a #(b c))
	  
	  ;array
	  (#2A((a b) (c d)) a #(b c d))))

(defun test-dolist-return ()
  ;;test early return
  (assert (equalp '(0 0 1)
		  (with-collector ()
		    (doseq (x '(0 0 1 0))
		      (collect x)
		      (when (plusp x) (return))))))
  ;;test optional return clause

  (assert (= (let ((r 1))
	       (doseq (x #(1 2 3) r)
		      (when (typep x 'number) ;; an attempt to placate sbcl
			(setf r (* x r)))))
	     6)))

(test-dolist-return)


(defun test-dolist-like-return-clause ()
  "the iteration var should be bound to nil in the return
clause, ala dolist"
  (assert (null (doseq (x '(a b c) x))))
  (assert (null (doindexable (x #(a b c) x)))))

(test-dolist-like-return-clause)

;;this test doesn't make as much sense
;;anymore
(defun test-count-seq ()
  (assert (= (count-elements '(a #(b c)))
	     2))
  (assert (= (count-elements '(a . #(b c)))
	     3)))

(test-count-seq)


;;todo: hash-table test

