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
		   (doitimes height
		     (push :wall bottom-wall))
		   (push bottom-wall map))
		 ;; make all of the "sandwich" rows
		 (doitimes (- width 2)
		   (let (mid-wall)
		     (push :wall mid-wall)
		     (doitimes (- height 2)
		       (push :floor mid-wall))
		     (push :wall mid-wall)
		     (push mid-wall map)))
		 ;; make the top wall
		 (let (top-wall)
		   (doitimes height
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

(defun kos ()
  (format t "Welcome to King of Shadows!~%")
  (let ((map-floor (make-map-floor 5 5)))
    (funcall map-floor :print)))
