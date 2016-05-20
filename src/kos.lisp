(in-package :kos)

(defmacro doitimes (count &body body)
  `(dotimes (,(gensym) ,count)
	    ,@body))

(defun make-map (x y)
  (check-type x (integer 2 *))
  (check-type x (integer 2 *))
  (let (map)
    ;; make the bottom wall
    (let (bottom-wall)
      (doitimes y
	(push :wall bottom-wall))
      (push bottom-wall map))
    ;; make all of the "sandwich" rows
    (doitimes (- x 2)
      (let (mid-wall)
	(push :wall mid-wall)
	(doitimes (- y 2)
	  (push :floor mid-wall))
	(push :wall mid-wall)
	(push mid-wall map)))
    ;; make the top wall
    (let (top-wall)
      (doitimes y
	(push :wall top-wall))
      (push top-wall map))))

(defun print-map (map width height)
  (dotimes (y height)
    (dotimes (x width)
      (princ (case (nth x (nth y map))
	       (:wall #\#)
	       (:floor #\.))))
    (princ #\Newline)))

(defun kos ()
  (princ "Welcome to King of Shadows"))
