

(defun position-each (item sequence)
    (loop
        for x in sequence
        for index from 0
            if (eq x item)
                collect index))


(defun check-string (item sequence)
      (let ((result (position-each item sequence)))
             (if (not (null result))
                    (list result))))
                    ;;(print result))))


(defun check-map (item map)
    (loop for x in map collect (check-string item x)))
        
        
(defun print-list (x) 
	(loop for i in x 
		do (format t "~a~a" i #\newline)))
                                        
        
(let ((map))
    (setq map (list (list #\#  #\#  #\#  #\#  #\#  #\#) 
    				(list #\#  #\.  #\@  #\.  #\.  #\#) 
                    (list #\#  #\.  #\.  #\@  #\@  #\#)  
                    (list #\#  #\@  #\.  #\.  #\.  #\#) 
                    (list #\#  #\#  #\#  #\#  #\#  #\#)))
                        
        (print-list map)
        
        (format t "checkMap: ~a~%" (check-map #\@ map)))
        
