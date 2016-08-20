
(defvar *somewhat-zero* 0.0001)
;;small number to check the closeness 
;;of the slopes of two lines

(defun random-from (low high)
  "Return a random number between the specified low and high limits."
(+ low (random (- high low -1))))

(defun get-intersect (x1 y1 x2 y2 x3 y3 x4 y4)

	"hopefully returns two integers i guess"

	;;does a bunch of jank shit to get some
	;;numbers of things
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


(defclass line-seg () (
		;;make a class that has 4 values that will 
		;;symbolize the coords of the start
		;;and finish points
		(sx :initarg :sx :initform 0 :accessor sx) 
		(sy :initarg :sy :initform 0 :accessor sy) 
		(fx :initarg :fx :initform 0 :accessor fx) 
		(fy :initarg :fy :initform 0 :accessor fy)))




(defun line-length (in-line)
	;;returns the sqrt of the sum of the 
	;;squares of the delta x's and y's
    (sqrt (+
    		(expt (- (slot-value in-line 'fx) (slot-value in-line 'sx)) 2) 
    		(expt (- (slot-value in-line 'fy) (slot-value in-line 'sy)) 2))))


(defun get-segment-intersection (px py ux uy qx qy vx vy)
  "Get intersection point of two line segments."
  (let* ((wx (- px qx))
	 (wy (- py qy))
	 (s_numerator (- (* vy wx) (* vx wy)))
	 (t_numerator (- (* ux wy) (* uy wx)))
	 (s_denominator (- (* vx uy) (* vy ux)))
	 (t_denominator (- s_denominator)))
    (cond ((< (abs s_denominator) *somewhat-zero*) nil)
	  (t (let ((s_intersection (/ s_numerator s_denominator))
		   (t_intersection (/ t_numerator t_denominator)))
	       (cond ((and (<= 0 s_intersection 1) (<= 0 t_intersection 1))
		      (list (+ px (* s_intersection ux)) (+ py (* s_intersection uy))))
(t nil)))))))

(defun project-ray-to-grid (x y rx ry)
  "Get next grid line intersection along the ray."
  ;; TODO: implement.
  (let ((i1
	 (cond ((> rx 0)
		(get-segment-intersection x y rx ry (floor (+ x 1)) (floor y) 0 1))
	       ((< rx 0)
		(get-segment-intersection x y rx ry (floor x) (floor y) 0 1))
	       (t nil)))
	(i2
	 (cond ((> ry 0)
		(get-segment-intersection x y rx ry (floor x) (floor (+ y 1)) 1 0))
	       ((< ry 0)
		(get-segment-intersection x y rx ry (floor x) (floor y) 1 0))
	       (t nil))))
(if (null i2) i1 i2)))


(defun tunnel-rec (in-line hdir vdir) 
			
	(let ( 	
			;;saves the x and y coords for the
			;;ending and starting points
			(new-x (slot-value in-line 'sx)) (new-y (slot-value in-line 'sy))
			(fin-x (slot-value in-line 'fx)) (fin-y (slot-value in-line 'fy))
			;;set up the vars dir, for making
			;;a list of directions
			(dir) 
			;;and coords for saving the values
			;;of the next intersection
			(coord))
		
		;;loop while inbetween start
		;;and final points
		(loop while (and (< new-x fin-x) (< new-y fin-y)) 

			;;set coord to the next intersection
			do (setf coord (project-ray-to-grid new-x new-y fin-x fin-y))

			;;if the first part of coord
			;;is an integer, it means
			;;that its hit a horizantal
			;;wall, else, its a vert wall
			do (if (integerp (car coord))
				(setf dir hdir)
				(setf dir vdir))				
				
			;;set the new-x and new-y to 
			;;the intersection that was just
			;;found
			do (setf new-x (car coord))
			do (setf new-y (cadr coord))
		
			;;collect the direction that was 
			;;found using the project-ray
			;function
			collect dir)))



(defun tunnel (array in-line)
    
    (let 
    	;;set up the directions var for later
    	((directions)

    	;;save all the coords starting and ending
    	;;x and y values
    	(fn-x (slot-value in-line 'fx))
    	(fn-y (slot-value in-line 'fy))
    	(pt-x (slot-value in-line 'sx))
    	(pt-y (slot-value in-line 'sy))

    	(hdir "left") 
    	(vdir "up")) 

    		;;set hdir to right, if its not going to 
    		;;be going to the left
        	(if (> (- (slot-value in-line 'fx) (slot-value in-line 'sx)) 0) 	
        		(setf hdir "right"))

        	;;same thing but for the vertical dir
        	(if (> (- (slot-value in-line 'fy) (slot-value in-line 'sy)) 0) 
        		(setf vdir "down"))

        	;;
        	(setf directions (tunnel-rec in-line hdir vdir))
        
        	;;go through the directions, and for 
        	;;each one, if still in range, loop 
        	;;through changing chars
    		(loop for command in directions 
    			while (and (< pt-x fn-x) (< pt-y fn-y))

    			;;set the space to a period
    			do (setf (aref array pt-x pt-y) #\.)

    			;;go through the directions 
    			;;if the strings fit, incriment
    			;;or decriment accordingly
    			(if (string= command "left") 	(setf pt-x (- pt-x 1)))
    			(if (string= command "right") 	(setf pt-x (+ pt-x 1)))
    			(if (string= command "up") 		(setf pt-y (- pt-y 1)))
    			(if (string= command "down") 	(setf pt-y (+ pt-y 1))))))


;; source 
;; https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line

(let* ( (map-thing)
		;;sets the bounds for the 2d array
		(array-l 20)
		(array-h 17)

		;;sets the points for the line
		(stx (random-from 1 (- array-l 1)))
		(sty (random-from 1 (- array-h 1)))
		(fiy (random-from 1 (- array-h 1)))
		(fix (random-from 1 (- array-l 1)))

		;;makes a line obj that will
		(line-new (make-instance 'line-seg 
	  							:sx stx :sy sty 
	  							:fx fix :fy fiy)))
	  	;;(line-new (make-instance 'line-seg 
	  	;;						:sx 1 :sy 1 
	  	;;						:fx 4 :fy 10)))
	
	;;print out the coords for the line
	(format t "(~a ~a) to (~a ~a)~%" stx sty fix fiy)

	;;make the array, init it to spaces
    (setf map-thing (make-array (list array-l array-h) :initial-element '#\ ))
    
    ;;call the tunnel function that will
    ;;make periods appear in the 2d array
    (tunnel map-thing line-new)
    
    ;;print map-thing
    (format t "~a~%" map-thing))
   