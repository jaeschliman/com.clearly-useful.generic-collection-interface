(in-package #:com.clearly-useful.generic-collection-interface)

(defgeneric all-keys-and-values (o)
  (:documentation "return a seq of proper lists in the form (key value)")
  (:method (o)
    (let ((o (associative-collection o))
	  (result (list)))
      (doseq (k (keys o) (nreverse result))
	(push (list k (key o k)) result)))))



(defun getkey (object key &optional default)
  "calls associative on object. if the result contains
key, returns value-for-key, else default"
  (let ((assoc (associative-collection object)))
    (if (in assoc key)
	(key assoc key)
	(values default nil))))


(defun %seq-to-list (seq)
  (let ((result (list)))
    (doseq (o seq (nreverse result))
      (push o result))))

(defun seq-to-list (seq)
  "a fresh list will all elements of seq"
  (%seq-to-list (seq seq)))
