;;;; Alysaur trying to make a room.

;;; Return random number between the specified low and high value.
(defun random-from (low high)
  (+ low (random (- high low -1))))

;;; Make horizontal wall.
;;;  e.g. "######"
(defun make-wall (width)
  (loop repeat width
     collect "#"))

;;; Make room space.
;;;  e.g. "...."
(defun make-space-slice (width)
  (loop repeat width
     collect "."))

;;; Make room space with walls.
;;;  e.g. "#....#"
(defun make-walled-slice (width)
  (append (cons "#" (make-space-slice (- width 2)))
	  (list "#")))

;;; Make all room spaces with walls.
;;;  e.g. "#....#"
;;;       "#....#"
;;;       "#....#"
(defun make-walled-slices (width height)
  (loop repeat height
     collect (make-walled-slice width)))

;;; Make complete room.
;;;  e.g. "######"
;;;       "#....#"
;;;       "#....#"
;;;       "#....#"
;;;       "######"
(defun make-room (width height)
  (append (cons (make-wall width)
		(make-walled-slices width (- height 2)))
	  (list (make-wall width))))

(defun make-random-room ()
    (let ((width (random-from 3 50))
	   (height (random-from 3 10)))
      (make-room width height)))

;;; Pretty print a room.
(defun print-room (room)
  (format t "~{~{~a~}~%~}" room))

;;; Test generation of a random room.
(setf *random-state* (make-random-state t))
(print-room (make-random-room))
