(in-package :kos)

(defmacro doitimes (count &body body)
  `(dotimes (,(gensym) ,count)
	    ,@body))

(defun make-map-floor (width height)
  (check-type width (integer 2 *))
  (check-type height (integer 2 *))
  (let ((tiles (let (map)
		 ;; make the bottom wall
		 (let (bottom-wall)
		   (doitimes width
		     (push :wall bottom-wall))
		   (push bottom-wall map))
		 ;; make all of the "sandwich" rows
		 (dotimes (y (- height 2))
		   (let (mid-wall)
		     (push :wall mid-wall)
		     (dotimes (x (- width 2))
		       (push :floor mid-wall))
		     (push :wall mid-wall)
		     (push mid-wall map)))
		 ;; make the top wall
		 (let (top-wall)
		   (doitimes width
		     (push :wall top-wall))
		   (push top-wall map))
		 map)))
    (lambda (name &rest args)
      (declare (ignore args))
      (case name
	(:print
	 (dolist (row tiles)
	   (dotimes (x width)
	     (princ (case (nth x row)
		      (:wall #\#)
		      (:floor #\.))))
	   (princ #\Newline)))))))

(defun make-player (x y)
  (check-type x (integer 0 *))
  (check-type y (integer 0 *))
  (let ((character #\@))
    (lambda (name &rest args)
      (case name
	(:draw
	 (let ((floor (first args)))
	   ))))))

(defun kos ()
  (format t "Welcome to King of Shadows!~%")
  (let ((map-floor (make-map-floor 10 6)))
    (funcall map-floor :print)))
