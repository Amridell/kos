(in-package :kos)

(defun make-player (io x y)
  (check-type x (integer 0 *))
  (check-type y (integer 0 *))
  (let ((character #\@))
    (lambda (name &rest args)
      (case name
	(:render
	 (funcall io :change-cell x y character))
	(:move
	 (let ((floor (pop args))
	       (new-x (pop args))
	       (new-y (pop args)))
	   (when (funcall floor :passable? new-x new-y)
	     (setf x new-x
		   y new-y))))))))

(defun make-io (&optional false-mode)
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
		 (princ #\Newline))))
	    (:poll-event
	     (case (read)
	       (q
		:quit))))
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
	     (tb:shutdown))
	    (:poll-event
	     (let ((event (termbox:event-plist (tb:poll-event))))
	       (cond
		 ((= (getf event :ch) (char-code #\q))
		  :quit)))))))))

(defun kos (&optional (slime? nil))
  ;(format t "Welcome to King of Shadows!~%")
  (let ((io (make-io slime?)))
    (funcall io :init)
    (funcall io :clear)
    (let ((map-floor (make-floor io 10 6))
	  (player (make-player io 1 1))
	  (running t))
      (funcall map-floor :render)
      (funcall player :render)
      (funcall io :present)
      (while running
	(if (eq (funcall io :poll-event) :quit)
	    (setf running nil))
	(funcall map-floor :render)
	(funcall player :render)
	(funcall io :present)))
    (funcall io :shutdown)))
