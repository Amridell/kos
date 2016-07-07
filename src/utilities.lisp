(in-package :kos)

(defmacro doitimes (count &body body)
  `(dotimes (,(gensym) ,(first count))
	    ,@body))
