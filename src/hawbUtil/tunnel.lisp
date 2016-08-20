
(defvar *somewhat-zero* 0.0001)

(defun random-from (low high)
  "Return a random number between the specified low and high limits."
(+ low (random (- high low -1))))

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
    		(expt (- (slot-value in-line 'fx) (slot-value in-line 'sx)) 2) 
    		(expt (- (slot-value in-line 'fy) (slot-value in-line 'sy)) 2))))


;(print (get-intersect 0 0 3 2 1 0 1 1))


;1st item (car '(9 8 7))
;2nd item (cadr '(9 8 7))

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


(defun tunnel-rec (array in-line hdir vdir) 

	(let  	;(xinter) 
			;(yinter) 
			(intersect)

			(list array in-line hdir vdir intersect)

		;;THIS DOESNT WORK AS INTENDED, WILL JUST PICK ONE DIRECTION TO GO IN

		#|
		(if (string= hdir "right")
			(print "right")
			(print "left"))

		(if (string= vdir "down")
			(print "down")
			(print "up"))
		|#

		(let ( 	(new-x (slot-value in-line 'sx)) (new-y (slot-value in-line 'sy))
				(fin-x (slot-value in-line 'fx)) (fin-y (slot-value in-line 'fy))
				(dir) (coord))
		
			(loop while (and (< new-x fin-x) (< new-y fin-y)) 

				do (setf coord (project-ray-to-grid new-x new-y fin-x fin-y))

				do (if (integerp (car coord))
					(setf dir hdir)
					(setf dir vdir))

				;do (print "loop")
				
				
				do (setf new-x (car coord))
				do (setf new-y (cadr coord))
		
				collect dir))))
		
		;;THIS DOESNT WORK AS INTENDED, WILL JUST PICK ONE DIRECTION TO GO IN

		;(print xinter) 
		;(print yinter) 
		;(print h-intersect) 
		;(print array)

		;(tunnel-rec array in-line hdir vdir))


(defun tunnel (array in-line)

    ;"1 is up, 2 is down, 3 is left, 4 is right" 
    ;u d l r

    ;(if (< startx 0) (return-from tunnel nil))
    ;(if (< starty 0) (return-from tunnel nil))
    ;(if (< finalx 0) (return-from tunnel nil))
    ;(if (< finaly 0) (return-from tunnel nil))
    
    (let 
    	((directions)
    	(fn-x (slot-value in-line 'fx))
    	(fn-y (slot-value in-line 'fy))
    	(pt-x (slot-value in-line 'sx))
    	(pt-y (slot-value in-line 'sy))
    	(hdir "left") 
    	(vdir "up")) 

        	(if (> (- (slot-value in-line 'fx) (slot-value in-line 'sx)) 0) 	
        		(setf hdir "right"))

        	(if (> (- (slot-value in-line 'fy) (slot-value in-line 'sy)) 0) 
        		(setf vdir "down"))
        
        ;(pprint "1 is up, 2 is down, 3 is left, 4 is right")
        ;(format t "~%hdir: ~a vdir: ~a ~%" hdir vdir)
        ;(print (string= "hello" "world"))
        
        	(setf directions (tunnel-rec array in-line hdir vdir))
        
    		(loop for command in directions 
    			while (and (< pt-x fn-x) (< pt-y fn-y))

    			;(setf (aref x 2 1) "blue
    			;do (print 9)
    			
    			do (setf (aref array pt-x pt-y) #\.)

    			(if (string= command "left") 	(setf pt-x (- pt-x 1)))
    			(if (string= command "right") 	(setf pt-x (+ pt-x 1)))
    			(if (string= command "up") 		(setf pt-y (- pt-y 1)))
    			(if (string= command "down") 	(setf pt-y (+ pt-y 1)))))

    ;(loop for char in array 
    ;	do (print char))
    
    (format t "~a~%" array))


;; source 
;; https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line

;(print (get-intersect 0 0 3 2 1 0 1 1))
	
;(print (make-array '(17 20) :initial-element #\#))

(let* ( (map-thing)
		(array-h 20)
		(array-l 17)
		(stx (random-from 1 (- 20 1)))
		(sty (random-from 1 (- 17 1)))
		(fiy (random-from 1 (- 17 1)))
		(fix (random-from 1 (- 20 1)))

		;(print st-x)

	  	(line-new (make-instance 'line-seg 
	  							:sx stx :sy sty 
	  							:fx fix :fy fiy)))

	  	;(line-new (make-instance 'line-seg 
	  	;						:sx 1 :sy 1 
	  	;						:fx 3 :fy 7)))
	
	(format t "(~a ~a) to (~a ~a)~%" stx sty fix fiy)

    (setf map-thing (make-array (list array-h array-l) :initial-element '#\ ))
    
    ;;do things to tunnel through this shit
    
    (tunnel map-thing line-new))
    
	;(defun things (x1) (x1 2 3))

	;(things #'-)
    
    ;(print map-thing))
    



 
