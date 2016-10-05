
;;fork dump on clozure
(defparameter number-2 10)

(defparameter foo (let ((a 10) (b number-2))
						(lambda (x) (+ x a b number-2))))

(format t "test1 : ~a~%" (funcall foo 10))

(setf number-2 20)

(format t "test2 : ~a~%" (funcall foo 10))

;;fork on let vs let*

(let ((x 10) (y 20))
	(format t "LET : ~a~%" (+ x y)))

(let* ((x 10) (y (+ x 10)))
	(format t "LET* : ~a :: ~a~%" x y))


;plusp is true if its positive

(if (not (plusp 0)) (print (plusp 15))))