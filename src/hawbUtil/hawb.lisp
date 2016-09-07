;;;this is the file that Hawbs will write for things

(ql:quickload :tcod)

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

(defun print-room (given-array)
	;;if ()
	;;()
	;;(format t "error in reading the '@'"))

	(destructuring-bind (height width) (array-dimensions given-array)

		(loop for y from 0 to (- height 1)
			do (loop for x from 0 to (- width 1)
				;do (write-char (aref given-array y x))
				do (tcod::console-put-char tcod:*root* x y (char-code (aref given-array y x)) :set)))))
				;do (tcod:console-print tcod:*root* x y (write-to-string (aref given-array y x))))
			;do (format t "~%"))))

				;;do (tcod:console-print tcod:*root* ))))

;;;fix this thing
;;;(position #\t (make-array '(2 2) :initial-contents '((#\a #\h) (#\g #\t))))


;;;(tcod:console-print tcod:*root* 1 1 "thing")


(tcod:console-set-custom-font
   (merge-pathnames #p"data/fonts/arial10x10.png"
                    (asdf/system:system-source-directory
                     (asdf:find-system "atemzug-rl")))
   '(:FONT-TYPE-GREYSCALE :FONT-LAYOUT-TCOD))

(defparameter *x* 45)
(defparameter *y* 20)
(defparameter *running* t)

(defun main-run ()
	(tcod:console-init-root 80 50 :title "Libtcod Lisp Tutorial" :fullscreen? nil :renderer :renderer-sdl)
	(loop
	   while *running*
	   do 
		 (tcod::console-clear tcod:*root*)

		 (print-room (make-map-array (convert-room "room.txt")))

		 (tcod::console-put-char tcod:*root* *x* *y* (char-code #\@) :set)		  
		 (tcod::console-flush)
		
		 (let ((events (tcod:sys-get-events)))
		 	(if (tcod:is-key-pressed? :UP) (decf *y*))
				  	
		   (loop
			  for e in events
			  do 
				(format t "~A~%" e)
				(case (car e)
					(:event-key-release (incf *x*))
				  	(:event-key (incf *x*))
				  	(:event-mouse-press (setf *running* nil)))))
		 (if (not (tcod:legal-console-coordinates? tcod:*root* *x* *y*)) 
		 	(if (< *y* 0) (setf *y* 10))
		 	(if (> *y* 79) (setf *y* 49)))))


(defun rerun ()
  (setf *running* t)
  (main-run))

(main-run)