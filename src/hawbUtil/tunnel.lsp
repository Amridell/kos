
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


(defun line-length (x1 y1 x2 y2) 
    (sqrt (+ (exp (- x2 x1) 2) (- y2 y1))))


;(print (get-intersect 0 0 3 2 1 0 1 1))


;1st item (car '(9 8 7))
;2nd item (cadr '(9 8 7))


(defun tunnel (array startx starty finalx finaly hdir vdir) 

	(let ((xinter nil) (yinter nil) (h-intersect nil)) 

		(if (=string hdir "right")
			(setq h-intersect (get-intersect startx starty finalx finaly 
											(+ startx 1) starty  (+ startx 1) (+ starty 1)))
			(setq h-intersect (get-intersect startx starty finalx finaly 
											startx (+ starty 1) (+ startx 1) (+ starty 1))))

		(if (=string vdir "down") 
			(setq v-intersect (get-intersect startx starty finalx finaly 
											startx starty startx (+ 1 starty)))
			(setq v-intersect (get-intersect startx starty finalx finaly 
											startx starty (+ startx 1) starty)))

		(if (< (line-length startx starty (car h-intersect) (cadr h-intersect)) 
				(line-length startx starty (car v-intersect) (cadr v-intersect)))

			((setq xinter (car h-intersect)) (setq yinter (cadr	h-intersect)))
			((setq xinter (car v-intersect)) (setq yinter (cadr v-intersect))))

		(tunnel array xinter yinter finalx finaly hdir vdir))))

	


(defun tunnel (array startx starty finalx finaly) 

    ;"1 is up, 2 is down, 3 is left, 4 is right" 
    ;u d l r

    ;(if (< startx 0) (return-from tunnel nil))
    ;(if (< starty 0) (return-from tunnel nil))
    ;(if (< finalx 0) (return-from tunnel nil))
    ;(if (< finaly 0) (return-from tunnel nil))
    
    (let ((hdir "left") (vdir "up")) 
        (if (> (- finalx startx) 0) (setq hdir "right"))
        (if (> (- finaly starty) 0) (setq vdir "down"))
        
        ;(pprint "1 is up, 2 is down, 3 is left, 4 is right")
        
        ;(format t "~%hdir: ~a vdir: ~a ~%" hdir vdir)

        (print (string= "hello" "world"))
        
        (tunnel array startx starty finalx finaly hdir vdir)
    ))





;; source 
;; https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line

;(print (get-intersect 0 0 3 2 1 0 1 1))
	
;(print (make-array '(17 20) :initial-element #\#))

(let (map-thing)

    (setq map-thing (make-array '(17 20) :initial-element '#\#))
    
    ;;do things to tunnel through this shit
    (tunnel map-thing 1 1 13 15)
    
	;(defun things (x1) (x1 2 3))

	;(things #'-)
    
    (print map-thing))



 
