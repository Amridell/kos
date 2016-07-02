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
    (flet ((wallsym-to-char (wallsym)
	     (case wallsym
	       (:wall #\#)
	       (:floor #\.)))
	   (tile-at (x y)
	     (nth x (nth y tiles))))
      (lambda (name &rest args)
	(flet ((draw-it (function)
		 (dotimes (y height)
		   (let ((row (nth y tiles)))
		     (dotimes (x width)
		       (let ((char (wallsym-to-char (nth x row))))
			 (funcall function char x y))))
		   (princ #\Newline))))
	  (case name
	    ;; print to the REPL, as opposed to termbox render
	    (:print
	     (draw-it (lambda (char x y)
			(declare (ignore x))
			(declare (ignore y))
			(princ char))))
	    ;; termbox render, as opposed to print to REPL
	    (:render
	     (draw-it (lambda (char x y)
			(tb:change-cell x y char termbox:+default+ termbox:+default+) x y)))
	    ;; can we enter a certain tile?
	    (:passable?
	     (let ((tile (tile-at (pop args) (pop args))))
	       (eq tile :floor)))
	    (:tile-at
	     (tile-at (pop args) (pop args)))))))))

(defun make-player (x y)
  (check-type x (integer 0 *))
  (check-type y (integer 0 *))
  (let ((character #\@))
    (lambda (name &rest args)
      (case name
	(:draw
	 (let ((floor (first args)))
	   ))))))

(defun make-row (width)
  (let (row)
    (doitimes width
      (push nil row))
    row))

(defun make-list-array (width height)
  (let (list-array)
    (dotimes (y height)
      (push (make-row width) list-array))
    list-array))

(defun make-renderer (&optional false-mode)
  (let (cells
	width
	height)
    (lambda (name &rest args)
      (if false-mode
	  (case name
	    (:init
	     (setf width 50
		   height 20)
	     (setf cells (make-list-array width height)))
	    (:clear
	     (dotimes (y height)
	       (dotimes (x width)
		 (setf (nth x (nth y cells)) #\Space))))
	    (:change-cell
	     (let ((x (pop args))
		   (y (pop args))
		   (char (pop args))
		   (fg (pop args))
		   (bg (pop args)))
	       )))
	  (case name
	    (:init
	     (tb:init))
	    (:clear
	     (tb:clear)))
	  ))))

(defun kos (&optional (slime nil))
  ;(format t "Welcome to King of Shadows!~%")
  (let ((renderer (make-renderer slime)))
    (funcall renderer :init)
    (funcall renderer :clear)
    (let ((map-floor (make-map-floor 10 6)))
      (funcall map-floor :render))
    (funcall renderer :present)
    (unless slime
      (sleep 1))
    (funcall renderer :shutdown)))
