
(defun get-intersect (x1 y1 x2 y2 x3 y3 x4 y4)

	"hopefully returns two integers i guess"

	;;(if ( < (abs (- (/ (- y2 y1) (- x2 x1)) 
		;;	(/ (- y4 y3) (- x4 x3)))) .01) 
		;;(break))

	(let 
		((numx 
			(- 
				(* 
					(- 
						(* x1 y2)
					  	(* y1 x2)
					) 
				   
				    (- x3 x4)
				)	

				(*
				    (- x1 x2)	

					(-
						(* x3 y4)
						(* y3 x4)
					)
				)
			)
		)

		(demx 
			(-
				(* 
					(- x1 x2)
					(- y3 y4)
				)
				
				(* 
					(- y1 y2)
					(- x3 x4)
				)
			)
		)

		(numy 
			(- 
				(* 
					(- 
						(* x1 y2)
					  	(* y1 x2)
					) 
			  
				    (- y3 y4)
				)

				(*
					(- y1 y2)

					(-
						(* x3 y4)
						(* y3 x4)
					)
				)
			)
		)

		(demy 
			(-
				(* 
					(- x1 x2)
					(- y3 y4)
				)
			
				(* 
					(- y1 y2)
					(- x3 x4)
				)
			)
		))


	(if (< demx .0001)   
			(return-from get-intersect nil))

	(if (< demy .0001)   
			(return-from get-intersect nil))

	;(print (list (/ numx demx) (/ numy demy))))
	(list (/ numx demx) (/ numy demy))))


(defclass line-seg () ( (sx :initarg :sx :initform 0 :accessor sx) 
						(sy :initarg :sy :initform 0 :accessor sy) 
						(fx :initarg :fx :initform 0 :accessor fx) 
						(fy :initarg :fy :initform 0 :accessor fy)))
;(slot-value thing 'sx)



(defun line-length (in-line)
    (sqrt (+
    		(exp (- (slot-value in-line 'fx) (slot-value in-line 'sx)) 2) 
    		(exp (- (slot-value in-line 'fy) (slot-value in-line 'sy)) 2))))


;(print (get-intersect 0 0 3 2 1 0 1 1))


;1st item (car '(9 8 7))
;2nd item (cadr '(9 8 7))


(defun tunnel-rec (array in-line hdir vdir) 

	(let ((xinter) 
			(yinter) 
			(h-intersect (list 10 10)) 
			(v-intersect (list 10 10)))

		;;THIS DOESNT WORK AS INTENDED, WILL JUST PICK ONE DIRECTION TO GO IN

		(if (string= hdir "right")
			(print "right")
			(print "left"))

		(if (string= vdir "down") 
			(print "down")
			(print "up"))
		
		;;THIS DOESNT WORK AS INTENDED, WILL JUST PICK ONE DIRECTION TO GO IN
		
			(let 
				((v-line (make-instance 'line-seg 
						:sx (slot-value in-line 'sx) :sy (slot-value in-line 'sy) 
						:fx (car v-intersect) 		:fy (cadr v-intersect)))

			     (h-line (make-instance 'line-seg 
			     		:sx (slot-value in-line 'sx) :sy (slot-value in-line 'sy) 
			     		:fx (car h-intersect) :fy (cadr h-intersect))))
			
				(if (< (line-length h-line) (line-length v-line))
					((setf xinter (car h-intersect)) 
						(setf yinter (cadr	h-intersect)))

					((setf xinter (car v-intersect)) 
						(setf yinter (cadr v-intersect))))

		(print xinter) 
		(print yinter) 
		(print h-intersect) 
		(print array)

		;(tunnel-rec array in-line hdir vdir))
			)))

	


(defun tunnel (array in-line)

    ;"1 is up, 2 is down, 3 is left, 4 is right" 
    ;u d l r

    ;(if (< startx 0) (return-from tunnel nil))
    ;(if (< starty 0) (return-from tunnel nil))
    ;(if (< finalx 0) (return-from tunnel nil))
    ;(if (< finaly 0) (return-from tunnel nil))
    
    (let 
    	((hdir "left") 
    	(vdir "up")) 

        (if (> (- (slot-value in-line 'fx) (slot-value in-line 'sx)) 0) 	
        		(setf hdir "right"))

        (if (> (- (slot-value in-line 'fy) (slot-value in-line 'sy)) 0) 
        		(setf vdir "down"))
        
        ;(pprint "1 is up, 2 is down, 3 is left, 4 is right")
        ;(format t "~%hdir: ~a vdir: ~a ~%" hdir vdir)
        ;(print (string= "hello" "world"))
        
        (tunnel-rec array in-line hdir vdir)))





;; source 
;; https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line

;(print (get-intersect 0 0 3 2 1 0 1 1))
	
;(print (make-array '(17 20) :initial-element #\#))

(let (( map-thing ) 
	  ( line-new (make-instance 'line-seg :sx 0 :sy 0 :fx 13 :fy 17 )))

    (setf map-thing (make-array '(17 20) :initial-element '#\#))
    
    ;;do things to tunnel through this shit
    (tunnel map-thing line-new)
    
	;(defun things (x1) (x1 2 3))

	;(things #'-)
    
    (print map-thing))



 
