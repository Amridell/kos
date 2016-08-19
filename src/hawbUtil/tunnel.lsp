
(defun line-length (x1 y1 x2 y2) 
    (sqrt (+ (exp (- x2 x1) 2) (- y2 y1))))

;(print (get-intersect 0 0 3 2 1 0 1 1))


(defun get-next-intersect () )


(defun tunnel (startx starty finalx finaly) 

    "1 is up, 2 is down, 3 is left, 4 is right" 
    ;u d l r
    
    (let ((xdiff 3) (ydiff 1)) 
        (if (> (- finalx startx) 0) (setq xdiff 4))
        (if (> (- finaly starty) 0) (setq ydiff 2))
        
        (pprint "1 is up, 2 is down, 3 is left, 4 is right")
        
        (format t "~%xdiff: ~a ydiff: ~a ~%" xdiff ydiff)
        
        (if (= xdiff 2)
            ;going to be going down
            (if (= ydiff 4)
                ;going right
                ()
                ;going left
                ())
                
            ;going to be going up
            (if (= ydiff 4)
                ;going right
                ()
                ;going left
                ())
        )
    ))



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

;; source 
;; https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line

;(print (get-intersect 0 0 3 2 1 0 1 1))
	
;(print (make-array '(17 20) :initial-element #\#))

(let (map-thing)
    (setq map-thing (make-array '(17 20) :initial-element '#\#))
    
    ;;do things to tunnel through this shit
    (tunnel 1 1 13 15)
    
(defun things (x1) (x1 2 3))

(things #'-)
    
    (print map-thing))



 