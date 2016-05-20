(in-package :kos)

(defmacro doitimes (count &body body)
  `(dotimes (,(gensym) ,count)
	    ,@body))

(defstruct map-floor
  (tiles nil :type list)
  (width nil :type integer)
  (height nil :type integer))

(defun make-map-floor-tiles (x y)
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

(defun print-map-floor (map width height)
  (declare (ignore height))
  (dolist (row map)
    (dotimes (x width)
      (princ (case (nth x row)
	       (:wall #\#)
	       (:floor #\.))))
    (princ #\Newline)))

(defun kos ()
  (format t "Welcome to King of Shadows!~%")
  (let ((map (make-map-floor :tiles (make-map-floor-tiles 5 5)
			     :width 5
			     :height 5)))
    (print-map-floor (map-floor-tiles map) 5 5)))
