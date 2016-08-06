(in-package :kos)

(defmacro doitimes (count &body body)
  `(dotimes (,(gensym) ,(first count))
	    ,@body))

(defmacro while (test &body body)
  `(loop
      (unless ,test
	(return))
      ,@body))

(defmacro fn-case (keyform test &body clauses)
  (let ((kf (gensym))
	(tst (gensym)))
    `(let ((,kf ,keyform)
	   (,tst ,test))
       (cond
	 ,@ (mapcar (lambda (clause)
		      (if (eq (car clause) t)
			  `(t
			    ,@ (cdr clause))
			  (let ((key (car clause))
				(code (cdr clause)))
			    `((funcall ,tst ,kf ,key)
			      ,@code)))) clauses)))))
