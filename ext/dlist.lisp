(ql:quickload :dlist)

(error "the gci.dlist code is completely broken")

(defpackage :com.clearly-useful.gci.dlist
  (:use :cl
        :alexandria
        :com.clearly-useful.generic-collection-interface
        :com.clearly-useful.protocols))

(in-package :com.clearly-useful.gci.dlist)

(defun %seq-find (o a)
  (doseq (v o nil)
    (when (equalp v a)
      (return-from %seq-find t))))


;;;;;;; problems: dlist does not define
;;;;;;; an empty object. we define our own here,
;;;;;;; but this could well break existing dlist
;;;;;;; (i.e.) not used via gci code.

(defvar *empty* (dlist:dlist '*empty*))

;;;;;;the below is incomplete/incorrect

(extend-type dlist:dcons
  collection
  (empty (o) *empty*)
  (empty-p (o) (eq o *empty*))
  (in (o a) (unless (eq o *empty*)
              (%seq-find o a)))

  counted-collection
  (counted-p (o) nil)
  (len (o) (dlist:dlist-length o))

  seqable
  (seq (o) (unless (empty-p o)
             o))
  seq
  (fst (o) (unless (empty-p o)
             (dlist::dcons-data o)))
  (rst (o) (let ((r (dlist:next o)))
             (unless (eq r *empty*)
               r))))

(extend-type dlist:dlist
  collection
  (empty (o) *empty*)
  (empty-p (o) (eq o *empty*))
  (in (o a) (unless (eq o *empty*)
              (%seq-find o a)))

  counted-collection
  (counted-p (o) nil)
  (len (o) (dlist:dlist-length o))

  seqable
  (seq (o) (unless (empty-p o)
             o))
  seq
  (fst (o) (unless (empty-p o)
             (dlist:dlist-first o)))
  (rst (o) (let ((r (dlist:next o)))
             (unless (eq r *empty*)
               r))))


(defmethod conj ((a dlist:dlist) val)
  (dlist:dcons nil val a))
