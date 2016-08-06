;;checks the inputted map to see if all of the 
;;inner lists are equi-length, if so, makes a 2d array
(defun make-map-array (map) 
  (if (apply #'= (mapcar #'length map))
      (make-array (list (length map) (length (first map)))
		  :initial-contents map)
      nil))


(defun make-array-test ()
  (let ((map) (thing))
    (setq map (list (list #\#  #\#  #\#  #\#  #\#  #\#) 
		    (list #\#  #\.  #\@  #\.  #\.  #\#) 
		    (list #\#  #\.  #\.  #\@  #\@  #\#)  
		    (list #\#  #\@  #\.  #\.  #\.  #\#) 
		    (list #\#  #\#  #\#  #\#  #\#  #\#)))

    (print (make-map-array map))))
