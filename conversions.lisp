(in-package #:com.clearly-useful.generic-collection-interface)

;;;;;;; note that the internals of this package are subject to change.


;;;an integer range

(defstruct %range
    "an immutable integer range from
low to high, exclusive."
    low high)

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
  collection
  (empty (range) (make-%range :low 0 :high 0))
  (empty-p (range) (not (plusp (%range-size range))))
  (in (range val)
      (and (integerp val)
           (<= (%range-low range) val (1- (%range-high range)))))
  
  seqable
  (seq (range) (when (plusp (%range-size range))
		 range))
  seq
  (fst (range) (%range-low range))
  (rst (range) (%next-range range))

  counted-collection
  (counted-p (range) t)
  (len (range) (%range-size range))

  indexed-collection
  (idx (range index)
       (if (in range index)
           (+ (%range-low range) index)
           (error "~S is an invalid index for ~S" index range))))


;;;;convert an indexable to a seq:

(defstruct %indexable-sequence
    length index indexable)

(defun %indexable-sequence-size (c)
  (- (%indexable-sequence-length c)
     (%indexable-sequence-index c)))

(defun %next-indexable-sequence (c)
  (let ((length (%indexable-sequence-length c))
	(new-index (1+ (%indexable-sequence-index c))))
    (unless (zerop (- length new-index))
      (make-%indexable-sequence
       :length length
       :index new-index
       :indexable (%indexable-sequence-indexable c)))))

(defun %indexable-to-seq (indexable)
  (when (plusp (len indexable))
    (make-%indexable-sequence
     :length (len indexable)
     :index 0
     :indexable indexable)))

(extend-type %indexable-sequence
  collection
  (empty (c) (%indexable-to-seq (empty (%indexable-sequence-indexable c))))
  (empty-p (c) (empty-p (%indexable-sequence-indexable c)))
  (in (c v) (in (%indexable-sequence-indexable c) v))
  
  seqable
  (seq (c) (when (plusp (%indexable-sequence-size c))
	     c))
  seq
  (fst (c) (idx (%indexable-sequence-indexable c)
			(%indexable-sequence-index c)))
  (rst (c) (%next-indexable-sequence c)))



;;;;convert a indexable to an associative:

(defstruct %indexable-associative
    length indexable)


(defun %indexable-to-associative (indexable)
  (make-%indexable-associative
   :length (len indexable)
   :indexable indexable))

(extend-type %indexable-associative
  collection
  (empty (o)
	 (%indexable-to-associative
	  (empty (%indexable-associative-indexable o))))
  (empty-p (o)
	   (empty-p (%indexable-associative-indexable o)))
  (in (o v) (in (keys (%indexable-associative-indexable o)) v))
  
  associative-collection
  (keys (o) (make-%range :low 0
                         :high (%indexable-associative-length o)))
  (vals (o) (%indexable-to-seq (%indexable-associative-indexable o)))
  (key (o key)
       (if (in o key)
           (values (idx (%indexable-associative-indexable o) key) t)
           (values nil nil))))


;;;;; seq functions

(defun %count-seq (seq)
  (loop for i from 0
     for tail = seq then (rst tail)
     while tail
     finally (return i)))


(defun %seq-indexable-by (seq n)
  (loop for i below (1+ n)
       for tail = seq then (rst tail)
       unless tail do (return nil)
       finally (return t)))


(defun %seq-nth-or-error (seq n)
  (flet ((lose ()
	   (error "~S is an invalid index for ~S" n seq)))
    (unless (non-negative-integer-p n)
      (lose))
    (loop for i below (1+ n)
       for tail = seq then (rst tail)
       unless tail do (lose)
       finally (return (fst tail)))))

(defun %seq-nth-or-nil-with-values (seq n)
  (let ((foundp nil))
    (values
     (loop for i upto n
        for tail = seq then (rst tail)
        unless tail do (return nil)
        finally (progn
                  (setf foundp t)
                  (return (fst tail))))
     foundp)))



;;;;; convert a seq to a indexable:

(defstruct %seq-indexable
    seq
    ubound ;;highest index known to be valid
    fully-counted ;;when fully counted
                  ;;ubound == count - 1
    )

(defun %seq-indexable-contains-index (sc index)
  (cond
    ((%seq-indexable-fully-counted sc)
     (<= index (%seq-indexable-ubound sc)))
    ((<= index (%seq-indexable-ubound sc)) t)
    ((%seq-indexable-by (%seq-indexable-seq sc) index)
     (prog1 t (setf (%seq-indexable-ubound sc) index)))))


(defun %seq-indexable-count (sc)
  (if (%seq-indexable-fully-counted sc)
      (1+ (%seq-indexable-ubound sc))
      (let ((c (%count-seq (%seq-indexable-seq sc))))
	(prog1 c
	  (setf (%seq-indexable-ubound sc) (1- c)
		(%seq-indexable-fully-counted sc) t)))))


(defun %seq-to-indexable (seq)
  (when seq
    (make-%seq-indexable
     :seq seq
     :ubound -1
     :fully-counted nil)))

(extend-type %seq-indexable
  collection
  (empty (o) (%seq-to-indexable (empty (%seq-indexable-seq o))))
  (empty-p (o) (empty-p (%seq-indexable-seq o)))
  (in (o v) (and (non-negative-integer-p v)
                 (%seq-indexable-contains-index o v))) 
  
  counted-collection
  (counted-p (o) (declare (ignore o)) nil)
  (len (o) (%seq-indexable-count o))

  indexed-collection
  (idx (o index) (%seq-nth-or-error (%seq-indexable-seq o) index)))



;;;;; convert a seq to an associative:


(defun %seq-to-associative (seq)
  (%indexable-to-associative (%seq-to-indexable seq)))

;;;; convert associative to indexable:
(defun %associative-to-indexable (assoc)
  (%seq-to-indexable (all-keys-and-values assoc)))
