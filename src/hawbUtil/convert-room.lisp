
(defun get-file (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line collect line)))

(defun convert-room (filename) 
	(let ((output)) 
		(setq output (get-file filename))
		(loop for line in output 
			;;do (print (coerce (aref line 0) 'character)))))
			collect (concatenate 'list line))))
				;;loop for char in line
				;;do (print (coerce char 'character))))))
				;collect (coerce char 'character)))))


;;changes the room, from looking like
#| 
####################################
#.@................................#
#..................................|
#..................................|
##--################################
|# 
;; to lists of chars, ie
;; ((#\#) (#\@) (#\#))


(print (convert-room "room.txt"))