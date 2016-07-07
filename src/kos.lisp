(in-package :kos)

(defun make-player (renderer x y)
  (check-type x (integer 0 *))
  (check-type y (integer 0 *))
  (let ((character #\@))
    (lambda (name &rest args)
      (case name
	(:render
	 (funcall renderer :change-cell x y character))
	(:move
	 (let ((floor (pop args))
	       (new-x (pop args))
	       (new-y (pop args)))
	   (when (funcall floor :passable? new-x new-y)
	     (setf x new-x
		   y new-y))))))))

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
	     (setf cells (make-grid width height)))
	    (:clear
	     (dotimes (y height)
	       (dotimes (x width)
		 (setf (tile-at cells x y) #\Space))))
	    (:change-cell
	     (let ((x (pop args))
		   (y (pop args))
		   (char (pop args)))
	       (setf (tile-at cells x y) char)))
	    (:present
	     (dotimes (y height)
	       (let ((row (nth y cells)))
		 (dotimes (x width)
		   (princ (nth x row)))
		 (princ #\Newline)))))
	  (case name
	    (:init
	     (tb:init))
	    (:clear
	     (tb:clear))
	    (:change-cell
	     (let ((x (pop args))
		   (y (pop args))
		   (char (pop args))
		   (fg (pop args))
		   (bg (pop args)))
	       (tb:change-cell x y char (or fg termbox:+default+) (or bg termbox:+default+))))
	    (:present
	     (tb:present))
	    (:shutdown
	     (tb:shutdown)))))))

(defun kos (&optional (slime? nil))
  ;(format t "Welcome to King of Shadows!~%")
  (let ((renderer (make-renderer slime?)))
    (funcall renderer :init)
    (funcall renderer :clear)
    (let ((map-floor (make-floor renderer 10 6)))
      (funcall map-floor :render))
    (funcall renderer :present)
    (unless slime?
      (sleep 1))
    (funcall renderer :shutdown)))
