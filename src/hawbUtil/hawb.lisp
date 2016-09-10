;;;this is the file that Hawbs will write for things

(ql:quickload :tcod)

(defparameter *x* 1)
(defparameter *y* 1)

(defparameter *running* t)

(defparameter *map-width* 80)
(defparameter *map-height* 50)

(defparameter *window-width* 80)
(defparameter *window-height* 50)

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

	;;sets the variables to the dimesions of the 
	;;of the given array
	(destructuring-bind (height width) (array-dimensions given-array)
		(setf *map-height* height)
		(setf *map-width* width)

		;;loops through the array to print all of
		;;the characters inside of it
		(loop for y from 0 to (- height 1)
			do (loop for x from 0 to (- width 1)
				;do (write-char (aref given-array y x))
				do (tcod::console-put-char tcod:*root* x y (char-code (aref given-array y x)) :set)))))


				;do (tcod:console-print tcod:*root* x y (write-to-string (aref given-array y x))))
;;;(position #\t (make-array '(2 2) :initial-contents '((#\a #\h) (#\g #\t))))

;;;(tcod:console-print tcod:*root* 1 1 "thing")

(defun is-walkable (y x map) 

	;;if the x or y is out of the 
	;;array, dont move
	(if (>= x *map-width*) nil)
	(if (>= y *map-height*) nil)

	(let ((map-char (aref map x y)))
		(print map-char)

		;;the list of available 
		;;chars in the array
		(cond
		 	((char= map-char #\#) nil)
			((char= map-char #\.) t)
			((char= map-char #\|) t)
			((char= map-char #\-) t)
			((char= map-char #\?) nil))))


(tcod:console-set-custom-font
   (merge-pathnames #p"data/fonts/arial10x10.png"
                    (asdf/system:system-source-directory
                     (asdf:find-system "atemzug-rl")))

   '(:FONT-TYPE-GREYSCALE :FONT-LAYOUT-TCOD))

(defun main-run ()
	(tcod:console-init-root *window-width* *window-height* 
		:title "Libtcod Lisp Tutorial" 
		:fullscreen? nil :renderer 
		:renderer-sdl)

	(defparameter *map* (make-map-array (convert-room "room.txt")))

	(loop
	   while *running*
	   do 

	   	(if (<= *y* 0) (setf *y* 0))

		(if (>= *y* (- *map-height* 1)) 
			(setf *y* (- *map-height* 1)))

	    (if (<= *x* 0) (setf *x* 0))

		(if (>= *x* (- *map-width* 1)) 
			(setf *x* (- *map-width* 1)))

		 (tcod::console-clear tcod:*root*)
		 (print-room *map*)
		 (tcod::console-put-char tcod:*root* *x* *y* (char-code #\@) :set)	
		 (tcod::console-flush)

		;(format t "~a:~a~%" *x* *y*)
		
		 (let ((events (tcod:sys-get-events)))

		 	(if (tcod:is-key-pressed? :UP) 
		 		(if (is-walkable *x* (- *y* 1) *map*) 
		 			(decf *y*)))
		 	
		 	(if (tcod:is-key-pressed? :DOWN) 
		 		(if (is-walkable *x* (+ *y* 1) *map*) 
		 			(incf *y*)))
		 	
		 	(if (tcod:is-key-pressed? :LEFT) 
		 		(if (is-walkable (- *x* 1) *y* *map*) 
		 			(decf *x*)))

			(if (tcod:is-key-pressed? :RIGHT) 
				(if (is-walkable (+ *x* 1) *y* *map*) 
					(incf *x*)))

				  	
		   (loop
			  for e in events
			  do 
				(format t "~A~%" e)
				(case (car e)
					;(:event-key-release (incf *x*))
				  	;(:event-key (incf *x*))
				  	(:event-mouse-press (setf *running* nil)))))))



(defun rerun ()
  (setf *running* t)
  (main-run))

(main-run)