
;;;function that takes in a file name, then
;;;loops through the lines to print them out
(defun print-room (filename)
  (let ((in (open filename :if-does-not-exist nil)))
      (when in            
            (loop for line = (read-line in nil)
                         while line do (format t "~a~%" line))
            
                (close in))))


;;;trying to make the function above read in the lines,
;;;but also make them into a list

;;(setq newlist (list nil))


#||

(defun read-room (filename)

  ;;;open a file, and ensure that the file exists
  (let ((in (open filename :if-does-not-exist nil)))
      
      ;;(defparameter *newlist* (list (read-line in nil)))
      
      (when in
            ;;;loop through the lines, and print them
            ;;;then append them to a new list and set
            ;;;newList to that value

            (defparameter newlist (loop-line in))

            (close in)
  ;;;close the file then return the list obj
      (list newlist)))



;;;made a new unct that tries to loop given
;;;some stream and put all of the lines
;;; it finds into a file
(defun loop-line (in) 
  (loop for line = (read-line in nil)

                      while line 

                         do (format t "~a~%" line)
                          
                         ;;do (setq newlist (list newlist line)))
                        ;;do (setq newlist (list line newlist)))
                        (collect line)))
  
;;(defvar listy (cons 9 98))

;;opens a file
;;(open "room.txt")

  ;;(format t "~a~%" (read-line in))
||#



;;(let ((stream (open "room.txt")))
    ;; do stuff with stream
      ;;(close stream))






(defun get-room (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun write-room (filename) 
  (open filename :direction :output :if-exists :supersede))

(defvar thing (get-room "room.txt"))

;;(get-room "room.txt")

(print-room "room.txt")

