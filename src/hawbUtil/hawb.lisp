;;;this is the file that Hawbs will write for things

;;converts a file into a list of lines ("strings")
(defun get-file (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line collect line)))

;;converts a list of strings into a list of characters
(defun convert-room (filename) 
	(let ((output)) 
		(setq output (get-file filename))
		(loop for line in output 
			collect (concatenate 'list line))))

;;checks the inputted map to see if all of the 
;;inner lists are equi-length, if so, makes a 2d array
(defun make-map-array (map) 
  (if (apply #'= (mapcar #'length map))
      (make-array (list (length map) (length (first map)))
      :initial-contents map)
      nil))

;;;prints out a 2d array for the thing that is room.txt
;;;(print (make-map-array (convert-room "room.txt")))

(defun print-room (array)
	if ()
	()
	(format t "error in reading the '@'"))

;;;fix this thing
;;;(position #\t (make-array '(2 2) :initial-contents '((#\a #\h) (#\g #\t))))


;;;(tcod:console-print tcod:*root* 1 1 "thing")