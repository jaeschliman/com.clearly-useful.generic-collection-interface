(in-package #:com.clearly-useful.generic-collection-interface)

;;;;;;; note that the internals of this package are subject to change.


;;;an integer range

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct %range
    "an immutable integer range from
low to high, exclusive."
    low high))


(defun %range-size (range)
  "the number of elements in a range"
  (- (%range-high range)
     (%range-low range)))

(defun %next-range (range)
  "return the next range by incrementing
the lower bound of range, or nil"
  (if (<= (%range-size range) 1)
      nil
      (make-%range :low (1+ (%range-low range))
		  :high (%range-high range))))

(extend-type %range
  seq
  (head (range) (%range-low range))
  (tail (range) (%next-range range))

  countable
  (counted-p (range) t)
  (count-elements (range) (%range-size range))

  indexable
  (element-at (range index)
	      (if (and
		   (integerp index)
		   (<= (%range-low range) index)
		   (< index (%range-high range)))
		  (+ (%range-low range) index)
		  (error "~S is an invalid index for ~S" index range))))


;;;;convert a countable to a seq:

;;;;;; NOTE: the following needs must get converted from
;;;;;; 'countable' to 'indexable'

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct %countable-sequence
    length index countable))

(defun %next-countable-sequence (c)
  (let ((length (%countable-sequence-length c))
	(new-index (1+ (%countable-sequence-index c))))
    (unless (zerop (- length new-index))
      (make-%countable-sequence
       :length length
       :index new-index
       :countable (%countable-sequence-countable c)))))

(defun %countable-to-seq (countable)
  (make-%countable-sequence
   :length (count-elements countable)
   :index 0
   :countable countable))

(extend-type %countable-sequence
  seq
  (head (c) (element-at (%countable-sequence-countable c)
			(%countable-sequence-index c)))
  (tail (c) (%next-countable-sequence c)))



;;;;convert a countable to an associative:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct %countable-associative
    length countable))

(defun %countable-to-associative (countable)
  (make-%countable-associative
   :length (count-elements countable)
   :countable countable))

(extend-type %countable-associative
  associative
  (all-keys (o) (make-%range :low 0
			     :high (%countable-associative-length o)))
  (all-values (o) (%countable-to-seq (%countable-associative-countable o)))
  (contains-key-p (o key) (and (integerp key)
			       (< -1 key (%countable-associative-length o))))
  (value-for-key (o key)
		 (if (contains-key-p o key)
		     (element-at (%countable-associative-countable o) key)
		     (error "~S does not contain key ~S" o key))))


;;;;; seq functions

(defun %count-seq (seq)
  (loop for i from 0
     for tail = seq then (tail tail)
     while tail
     finally (return i)))


(defun %seq-indexable-by (seq n)
  (loop for i below (1+ n)
       for tail = seq then (tail tail)
       unless tail do (return nil)
       finally (return t)))


(defun %seq-nth-or-error (seq n)
  (flet ((lose ()
	   (error "~S is an invalid index for ~S" n seq)))
    (unless (and (integerp n)
		 (< -1 n))
      (lose))
    (loop for i below (1+ n)
       for tail = seq then (tail tail)
       unless tail do (lose)
       finally (return (head tail)))))

(defun %seq-to-list (seq)
  (let ((result (list)))
    (doseq (o seq (nreverse result))
      (push o result))))

;;;;; convert a seq to a countable:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct %seq-countable
    seq
    ubound ;;highest index known to be valid
    fully-counted ;;when fully counted
                  ;;ubound == count - 1
    ))

(defun %seq-countable-contains-index (sc index)
  (if (%seq-countable-fully-counted sc)
      (<= index (%seq-countable-ubound sc))
      (if (<= index (%seq-countable-ubound sc))
	  t
	  (if (%seq-indexable-by (%seq-countable-seq sc) index)
	      (prog1 t (setf (%seq-countable-ubound sc) index))))))


(defun %seq-countable-count (sc)
  (if (%seq-countable-fully-counted sc)
      (1+ (%seq-countable-ubound sc))
      (let ((c (%count-seq (%seq-countable-seq sc))))
	(prog1 c
	  (setf (%seq-countable-ubound sc) (1- c)
		(%seq-countable-fully-counted sc) t)))))


(defun %seq-to-countable (seq)
  (make-%seq-countable
   :seq seq
   :ubound -1
   :fully-counted nil))

(extend-type %seq-countable
  countable
  (counted-p (o) (declare (ignore o)) nil)
  (count-elements (o) (%seq-countable-count o))

  indexable
  (element-at (o index) (%seq-nth-or-error (%seq-countable-seq o) index)))



;;;;; convert a seq to an associative:


(defun %seq-to-associative (seq)
  (%countable-to-associative (%seq-to-countable seq)))

;;;; convert associative to indexable:
(defun %associative-to-indexable (assoc)
  (%seq-to-countable (all-keys-and-values assoc)))
