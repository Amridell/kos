#!/usr/bin/sbcl --script

(setf *random-state* (make-random-state t))

(defun random-from (low high)

"Return a random number between the specified low and high limits."

    (+ low (random (- high low -1))))

;;checks the inputted map to see if all of the 
;;inner lists are equi-length, if so, makes a 2d array
(defun make-map-array (map) 
  (if (apply #'= (mapcar #'length map))
      (make-array (list (length map) (length (first map)))
      :initial-contents map)
      nil))


(defun make-array-test ()
  (let (map)
    (setq map (list (list #\#  #\#  #\#  #\#  #\#  #\#) 
        (list #\#  #\.  #\@  #\.  #\.  #\#) 
        (list #\#  #\.  #\.  #\@  #\@  #\#)  
        (list #\#  #\.  #\.  #\.  #\.  #\#) 
        (list #\#  #\#  #\#  #\#  #\#  #\#)))

    (format t "~a~%" (make-map-array map))))
;(make-array-test)

(defun in-range ( a b c ) 
  (if (and (<= a b) (<= b c))
    t
    nil))

(defun tunnel-from (map side-length x y) 
  (let* ((nx (random-from 1 (- side-length 2)))
        (ny (random-from 1 (- side-length 2)))
        (dx (- nx x))
        (dy (- ny y)))

    (print (list x y nx ny))

    (setf (aref map x y) #\.)

    (loop while (and (not (= x nx)) 
                     (not (= y ny)) 
                     (in-range 1 x (- side-length 1)) 
                     (in-range 1 y (- side-length 1)))

      ;do (print "thing")
      
      ;get difference of nx ny y x and decide path
      
      do (if (= (random 1) 0)

            (if (not (= (random 8) 1)) 
               (if (plusp dx) (incf x) (decf x))              
               (if (plusp dx) (decf x) (incf x)))

            (if (not (= (random 8) 1))
               (if (plusp dy) (decf y) (incf y))
               (if (plusp dy) (incf y) (decf y))))

      do (print (list x y))
      do (setf  (aref map y x) #\.
                dx (- nx x)
                dy (- ny y)))

    (setf (aref map nx ny) #\.)))

(defun tunnel (map side-length) 
  "creates a map and a starting point for the 
  tunnel-from function to start"

  (let ((x (random-from 1 (- side-length 2)))
        (y (random-from 1 (- side-length 2))))
    ;(print (list x y))
    ;(setf (aref map x y) #\.)
    (tunnel-from map side-length x y)))

(defun drunk-tunnel (map length &optional x y amount) 
  
  (if amount
      (let ((amount (random (/ length 5)))
          (x (random-from 1 (- length 1)))
          (y (random-from 1 (- length 1)))) 
          
          (setf (aref map x y) #\.)
          (drunk-tunnel map length x y amount))

      (let ((r (random 4)))
          (if (<= amount 0) (return-from drunk-tunnel nil))
          (cond
            ((= r 0) (incf x))
            ((= r 1) (incf y))
            ((= r 2) (decf x))
            ((= r 3) (decf y)))
          
          (if (< x 0) (setf x 0))
          (if (< (- length 1) x) (setf x (- length 1)))
          (if (< y 0) (setf y 0))
          (if (< (- length 1) y) (setf y (- length 1)))
         
          (setf (aref map y x) #\.)          
          (drunk-tunnel map length x y (- amount 1)))))


(defun make-test-map ()
  (let* ((map-length 10)
        (map (make-array (list map-length map-length)
                           :initial-element #\#)))
    
      (drunk-tunnel map map-length)
      map))

;(make-test-map)
(print (make-test-map))

