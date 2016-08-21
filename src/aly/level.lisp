;;;; Alysaur trying to make a level.

(defvar *somewhat-zero* 0.0001)

(defun random-from (low high)
  "Return a random number between the specified low and high limits."
  (+ low (random (- high low -1))))

(defun random-until (low exclusive-high)
  "Return a random number between the low (inclusive) and
   high (exclusive) limits."
  (+ low (random (- exclusive-high low))))

(defun make-wall (width)
  "Make a horizontal wall of the specified width.
    e.g. '######'"
  (make-list width :initial-element #\#))

(defun make-space-slice (width)
  "Make a slice of empty space of the specified width.
    e.g. '....'"
  (make-list width :initial-element #\.))

(defun make-walled-slice (width)
  "Make a slice of walled space of the specified width.
    e.g. '#....#'"
  (cons #\#
	(nconc (make-space-slice (- width 2))
	       (list #\#))))

(defun make-walled-slices (width height)
  "Make slices of walled space of the specified width and height.
    e.g. '#....#'
         '#....#'
         '#....#'"
  (loop repeat height
     collect (make-walled-slice width)))

(defun make-area (width height)
  "Make an area of the specifed width and height.
    e.g. '######'
         '#....#'
         '#....#'
         '#....#'
         '######'"
  (cons (make-wall width)
	(nconc (make-walled-slices width (- height 2))
	       (list (make-wall width)))))

(defun make-random-area ()
  "Make an area of random width and height."
  (let ((width (random-from 3 50))
	(height (random-from 3 10)))
    (make-area width height)))

(defun draw-area (area)
  "Draw an area to the screen."
  (format t "~{~{~a~}~%~}" area))

(defgeneric draw (shape)
  (:documentation "Draw a shape to the screen."))

(defclass shape ()
  ((x-position :initarg :x
	       :initform 0
	       :accessor x
	       :documentation "Position of the shape's x-coordinate.")
   (y-position :initarg :y
	       :initform 0
	       :accessor y
	       :documentation "Position of the shape's y-coordinate.")))

(defmethod translate ((shape shape) dx dy)
  "Translate a shape by the specified offsets."
  (with-accessors ((x x) (y y)) shape
    (setq x (+ x dx))
    (setq y (+ y dy))))

(defclass area (shape)
  ((width :initarg :width
	  :initform (random-from 4 71)
	  :accessor width
	  :documentation "Width of the area.")
   (height :initarg :height
	   :initform (random-from 4 13)
	   :accessor height
	   :documentation "Height of the area.")
   (data :accessor data
	 :documentation "A list of lists for the tiles in an area.")))

(defmethod initialize-instance :after ((area area) &key)
  "Initialize data with an area of the specified width and height."
  (with-slots (width height data) area
    (setf data (make-area width height))))

(defmethod draw ((shape area))
  "Draw an area to the screen."
  (draw-area (slot-value shape 'data)))

(defun has-overlap (area area-list)
  "Check if an area overlaps with a list of areas."
  (with-accessors ((l1 x) (t1 y) (w1 width) (h1 height)) area
    (let ((r1 (+ l1 w1 -1)) (b1 (+ t1 h1 -1)))
      (dolist (n area-list)
	(with-accessors ((l2 x) (t2 y) (w2 width) (h2 height)) n
	  (let ((r2 (+ l2 w2 -1)) (b2 (+ t2 h2 -1)))
	    (if (and (>= r1 l2) (<= l1 r2) (>= b1 t2) (<= t1 b2))
		(return-from has-overlap T)))))))
  nil)

(defclass level (shape)
  ((top :initform 0
	:accessor top
	:documentation "First y-coordinate of the level.")
   (left :initform 0
	 :accessor left
	 :documentation "First x-coordinate of the level.")
   (bottom :initform 0
	   :accessor bottom
	   :documentation "Last y-coordinate of the level.")
   (right :initform 0
	  :accessor right
	  :documentation "Last x-coordinate of the level.")
   (rooms :initform '()
	  :accessor data
	  :documentation "List of areas on the level.")))

(defmethod add ((new-area area) (level level))
  "Add a non-overlapping area to a level."
  (with-accessors ((top top)
		   (left left)
		   (bottom bottom)
		   (right right)
		   (current-areas data)) level
    (with-accessors ((x x)
		     (y y)
		     (width width)
		     (height height)) new-area
      (unless (has-overlap new-area current-areas)
	(setf top (min y top))
	(setf left (min x left))
	(setf bottom (max (+ y height) bottom))
	(setf right (max (+ x width) right))
	(push new-area current-areas)))))

(defun list-from-level (level)
  "Create a list of lists of tiles from a level."
  (with-accessors ((x1 left)
		   (y1 top)
		   (x2 right)
		   (y2 bottom)
		   (areas data)) level
    (let ((tiles
      ;; Create list of lists large enough to fit all level tiles.
      (loop
	 repeat (- y2 y1)
	 collect (make-list (- x2 x1) :initial-element #\Space))))
      ;; Copy areas into list.
      (dolist (area areas)
	(loop
	   for n from (- (y area) y1)
	   for row in (data area)
	   do (setf (subseq (nth n tiles) (- (x area) x1)) row)))
      tiles)))

(defun array-from-list (list-of-lists)
  "Create a 2d array from a list of lists."
  (make-array (list (length list-of-lists)
		    (length (first list-of-lists)))
	      :initial-contents list-of-lists))

(defun array-from-level (level)
  (array-from-list (list-from-level level)))

(defun wallp (tile)
  "Check if a tile is a wall."
  (equalp tile #\#))

(defun withinp (array x y)
  (destructuring-bind (h w) (array-dimensions array)
    (and (>= x 0) (>= y 0) (< x w) (< y h))))

(defun visiblep (level-array x-position y-position)
  ;; TODO: implement
  (destructuring-bind (height width) (array-dimensions level-array)
    (loop for (dx  dy) in '((-1 -1) (-1 0) (-1 1) (0 1)
			    (1 1) (1 0) (1 -1) (0 -1))
	 do (print (list (+ x-position dx) (+ y-position dy))))))

(defun draw-array (level-array)
  (destructuring-bind (height width) (array-dimensions level-array)
    (loop for y from 0 to (- height 1)
       do (loop for x from 0 to (- width 1)
	     do (write-char (aref level-array y x)))
       do (format t "~%"))))

(defmethod draw ((shape level))
  "Draw a level to the screen."
  (draw-area (list-from-level shape)))

(defclass corridor (shape)
  ((direction :initform nil
	      :accessor direction
	      :documentation "Direction of the corridor. (N, S, E, W)")
   (length :initform 1
	   :accessor size
	   :documentation "Length of the corridor along its direction.")))

(defun random-edge-position (width height)
  (let ((n (random-from 0 3)))
    (cond ((= n 0) (list 0 (random-until 0 height)))
	  ((= n 1) (list (- width 1) (random-until 0 height)))
	  ((= n 2) (list (random-until 0 width) 0))
	  (t (list (random-until 0 width) (- height 1))))))

(defun make-dorf (x y)
  (list :x x :y y :vx 0 :vy 0 :m 1))

(defun apply-force (entity fx fy &optional (dt 1))
  (incf (getf entity :vx) (* dt (/ fx (getf entity :m))))
  (incf (getf entity :vy) (* dt (/ fy (getf entity :m))))
  entity)

(defun step-time (entity &optional (dt 1))
  (incf (getf entity :x) (* dt (getf entity :vx)))
  (incf (getf entity :y) (* dt (getf entity :vy)))
  entity)

(defun ray-traverses-cells-p (x y dx dy)
  "Test if ray traverses between multiple cells."
  (not (and (= (floor x) (floor (+ x dx)))
	    (= (floor y) (floor (+ y dy))))))

(defun is-parallel (ux uy vx vy)
  "Test if two vectors are parallel."
  (< (abs (- (* ux vy) (* vx uy))) *somewhat-zero*))

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

(defun segments-intersect-p (px py ux uy qx qy vx vy)
  "Test if two line segments intersect."
  (= (length (get-segment-intersection px py ux uy qx qy vx vy)) 2))

(defun vector-length (&rest components)
  "Get vector magnitude."
  (loop for x in components
     when (numberp x) summing (* x x) into total
     finally (return (sqrt total))))

(defun make-unit-vector (&rest components)
  (let ((divisor (apply #'vector-length components)))
    (mapcar #'(lambda (x) (/ x divisor)) components)))

(defun project-ray-to-grid (x y rx ry)
  "Get next grid line intersection along the ray."
  ;; TODO: fix
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

(defun get-direction (ax ay bx by)
  (let ((vertical-direction (cond
			      ((= (+ 1 (floor ay)) by) "S")
			      ((= (- 1 (ceiling 1 ay)) by) "N")
			      (t "")))
	(horizontal-direction (cond
				((= (+ 1 (floor ax)) bx) "E")
				((= (- 1 (ceiling ax)) bx) "W")
				(t ""))))
    (concatenate 'string vertical-direction horizontal-direction)))

(defun get-grid-projection-direction (x y rx ry)
  (destructuring-bind (ix iy) (project-ray-to-grid x y rx ry)
    (get-direction x y ix iy)))

(defun ray-intersects-cell-p (x y rx1 ry1 rx2 ry2)
  ;; TODO: implement using segments-intersect-p
  t)

(defun tunnel (array x y)
  (setf (aref array x y) #\Space)
  array)

(defun make-terrain (width height)
  (let ((terrain (make-array (list height width) :initial-element #\#)))
    (dotimes (n 10) (tunnel terrain n n))
    terrain))

;;; Test generation of a level.
;(setf *random-state* (make-random-state t))
;(defparameter shabam (make-instance 'level))
;(add (make-instance 'area :height 5 :width 8 :x -2 :y 1) shabam)
;(add (make-instance 'area :height 6 :width 10 :x 12 :y -1) shabam)
;(add (make-instance 'area :height 4 :width 14 :x 2 :y 7) shabam)
;(draw-array (array-from-level shabam))
