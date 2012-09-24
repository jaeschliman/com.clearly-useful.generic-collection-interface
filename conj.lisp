(in-package :com.clearly-useful.generic-collection-interface)


(defgeneric conj (a val)
  (:method ((a null) v) (list v))
  (:method ((a cons) v) (cons v a))
  ;;should be non-destructive
  (:method ((a vector) v)
    ;;;horrible performance, but anyway.
    (let ((vec (%adjustable a)))
      (vector-push-extend v vec)
      vec)))


(defun %vector-conj! (adjustable-vector val)
  (vector-push-extend val adjustable-vector (length adjustable-vector))
  adjustable-vector)

(defun into (coll reduceable)
  (etypecase coll
    (vector
     (fold-left #'%vector-conj! reduceable :initial-value (%adjustable coll)))
    (t
     (fold-left #'conj reduceable :initial-value coll))))

