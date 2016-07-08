(in-package :kos)

(defmacro doitimes (count &body body)
  `(dotimes (,(gensym) ,(first count))
	    ,@body))

(defmacro while (test &body body)
  `(loop
      (unless ,test
	(return))
      ,@body))
