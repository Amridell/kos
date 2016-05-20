(in-package :kos)

(defmacro doitimes (count &body body)
  `(dotimes (,(gensym) ,count)
	    ,@body))

(defun make-map (x y)
  (check-type x (integer 2 *))
  (check-type x (integer 2 *))
  (let (map)
    (let (bottom-wall)
      (doitimes y
	(push :wall bottom-wall))
      (push bottom-wall map))
    (doitimes (- x 2)
      (let (mid-wall)
	(push :wall mid-wall)
	(doitimes (- y 2)
	  (push :floor mid-wall))
	(push mid-wall map)))
    (let (top-wall)
      (doitimes y
	(push :wall top-wall))
      (push top-wall map))))

(defun kos ()
  (princ "Welcome to King of Shadows"))
