(in-package #:com.clearly-useful.generic-collection-interface)


(defmacro doseq ((var form &optional return-clause) &body body)
  "   bind var to each item in (seq form) and execute body.
similar to dolist."
  (with-gensyms (seq tail)
    `(when-let (,seq (seq ,form))
       (do* ((,tail ,seq (rst ,tail))
             (,var (fst ,tail)
                   (fst ,tail)))
            ((null ,tail)
             ,@(when return-clause
                     (list return-clause)))
         ,@body))))


(defmacro doindexable ((var form &optional return-clause) &body body)
  "   bind var to each item in (element-at indexed n)
and execute body.
where n is 0..(count-elements indexed)
      and indexed is (indexable form)
similar to dolist."
  (with-gensyms (idx i count)
    `(let* ((,idx (indexed-collection ,form))
            (,count (len ,idx)))
       (when (plusp ,count)
         (do* ((,i 0 (1+ ,i))
               (,var (idx ,idx ,i)
                     (if (= ,i ,count) nil ;;like dolist
                         (idx ,idx ,i))))
              ((= ,i ,count)
               ,@(when return-clause
                       (list return-clause)))
           ,@body)))))



