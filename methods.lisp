(in-package #:com.clearly-useful.generic-collection-interface)

(defgeneric all-keys-and-values (o)
  (:documentation "return a seq of proper lists in the form (key value)")
  (:method (o)
    (let ((o (associative o))
	  (result (list)))
      (doseq (key (all-keys o) (nreverse result))
	(push (list key (value-for-key o key)) result)))))



(defun getkey (object key &optional default)
  "calls associative on object. if the result contains
key, returns value-for-key, else default"
  (let ((assoc (associative object)))
    (if (contains-key-p assoc key)
	(value-for-key assoc key)
	default)))


(defun seq-to-list (seq)
  "a fresh list will all elements of seq"
  (%seq-to-list (seq seq)))
