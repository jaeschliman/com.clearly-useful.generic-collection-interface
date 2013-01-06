(in-package :com.clearly-useful.generic-collection-interface)


(defun %vector-conj! (adjustable-vector val)
  (vector-push-extend val adjustable-vector (length adjustable-vector))
  adjustable-vector)

(defun %hash-conj! (ht seq)
  (setf (gethash (fst seq) ht) (fst (rst seq)))
  ht)

(defgeneric conj (a val)
  (:method ((a null) v) (list v))
  (:method ((a cons) v) (cons v a))
  ;;should be non-destructive
  (:method ((a vector) v)
    ;;;horrible performance, but anyway.
    (let ((vec (%adjustable a)))
      (vector-push-extend v vec)
      vec))
  (:method ((a hash-table) v)
    ;;same
    (%hash-conj! (%dup-hash a) v)))


;;better here.

(defun into (coll reduceable)
  (etypecase coll
    (vector
     (fold-left #'%vector-conj! reduceable :initial-value (%adjustable coll)))
    (hash-table
     (fold-left #'%hash-conj! reduceable :initial-value (%dup-hash coll)))
    (t
     (fold-left #'conj reduceable :initial-value coll))))

