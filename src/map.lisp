(In-package :kos)

(defun make-row (width)
  (let (row)
    (doitimes (width)
      (push nil row))
    row))

(defun make-grid (width height)
  "makes a list of lists in row-major order"
  ;; row-major means that the each element of the outer list is a row
  (let (list-array)
    (dotimes (y height)
      (push (make-row width) list-array))
    list-array))

(defun tile-at (map x y)
  (nth x (nth y map)))

(defun (setf tile-at) (val map x y)
  (setf (nth x (nth y map)) val))

(defun make-rectangular-room (width height)
  (let ((map (make-grid width height)))
    ;;set the floor space
    (dotimes (x width)
      (dotimes (y height)
	(setf (tile-at map x y) :floor)))
    ;; make the top and bottom walls
    (dotimes (x width)
      (setf (tile-at map x 0) :wall
	    (tile-at map x (1- height)) :wall))
    (dotimes (y height)
      (setf (tile-at map 0 y) :wall
	    (tile-at map (1- width) y) :wall))
    map))

(defun wallsym-to-char (wallsym)
  (case wallsym
    (:wall #\#)
    (:floor #\.)))

(defun print-floor (floor width height)
  (dotimes (y height)
    (let ((row (nth y floor)))
      (dotimes (x width)
	(let ((char (wallsym-to-char (nth x row))))
	  (princ char))))
    (princ #\Newline)))

(defun make-floor (io width height)
  (check-type width (integer 2 *))
  (check-type height (integer 2 *))
  (let ((tiles (make-rectangular-room width height)))
    (lambda (name &rest args)
      ;; this is a clever little factorization of code that eliminates some redundancy between :print and :render
      (case name
	;; print to the REPL, as opposed to termbox render
	(:render
	 (dotimes (y height)
	   (let ((row (nth y tiles)))
	     (dotimes (x width)
	       (let ((char (wallsym-to-char (nth x row))))
		 (funcall io :change-cell x y char))))))
	;; can we enter a certain tile?
	(:passable?
	 (let ((tile (tile-at tiles (pop args) (pop args))))
	   (eq tile :floor)))
	(:tile-at
	 (tile-at tiles (pop args) (pop args)))))))
