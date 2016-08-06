(in-package :kos)

(declaim (optimize (debug 3)))

(defun make-player (floor x y)
  (check-type x (integer 0 *))
  (check-type y (integer 0 *))
  (let ((character #\@))
    (lambda (name &rest args)
      (case name
	(:render
	 (tb:change-cell x y character))
	(:move
	 (let ((new-x (pop args))
	       (new-y (pop args)))
	   (when (funcall floor :passable? new-x new-y)
	     (setf x new-x
		   y new-y))))
	(:move-relative
	 (let ((x-offset (pop args))
	       (y-offset  (pop args)))
	   (when (funcall floor :passable? (+ x x-offset) (+ y y-offset))
	     (incf x x-offset)
	     (incf y y-offset))))))))

(defun kos ()
  ;(format t "Welcome to King of Shadows!~%")
  (tb:init)
  (tb:clear)
  (let* ((map-floor (make-floor 36 5)) ;; "room.txt"))
	 (player (make-player map-floor 1 1))
	 (running t))
    (funcall map-floor :render)
    (funcall player :render)
    ;;(print (convert-room "room.txt"))
    (print (make-floor 2 3 "room.txt"))
    (tb:present)
    (while running
      (let* ((event (tb:poll-event))
	     (plist (termbox:event-plist event))
	     (char (getf plist :ch)))
	(tb:write-text 0 20 (format nil "~A" plist))
	(when char
	  (fn-case char (lambda (char-int char-char) (= char-int (char-code char-char)))
	    (#\q
	     (setf running nil))
	    (#\j
	     (funcall player :move-relative 0 1))
	    (#\k
	     (funcall player :move-relative 0 -1))
	    (#\h
	     (funcall player :move-relative -1 0))
	    (#\l
	     (funcall player :move-relative 1 0)))))
      (funcall map-floor :render)
      (funcall player :render)
      (tb:present)))
  (tb:shutdown))

;; this is just a convenience function so that we can more easily exit a broken termbox session
(defun common-lisp-user::s ()
  (tb:shutdown))
