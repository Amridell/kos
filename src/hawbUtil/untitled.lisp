#!/usr/bin/sbcl --script

(setf *random-state* (make-random-state t))

(defun random-from (low high)

"Return a random number between the specified low and high limits."

    (+ low (random (- high low -1))))


(defun get-percentile (number percent) 
	(round (* number percent)))


(defun weighted-random-from (low mid high)
"Return a random number between the specified low and high limits."
	(let ((random-num (random-from 0 100)) 
			(low-range (- mid low))
			(high-range (- high mid)))

		(cond 
			;; .o0o. 
			;; .=7, o=18, 0=25

			((<  random-num 7) 	
				(random-from low (+ low (get-percentile low-range .14))))

			((< random-num 25)	
				(random-from (+ low (get-percentile low-range .14)) 
					(+ low (get-percentile low-range .50))))

			((< random-num 50) 
				(random-from (+ low (get-percentile low-range .50)) mid))

			((< random-num 75) 	
				(random-from mid (+ (get-percentile high-range .50) mid)))

			((< random-num 93) 	
				(random-from (+ (get-percentile high-range .50) mid)
								(+ (get-percentile high-range .86) mid)))

			(t
				(random-from (+ (get-percentile high-range .86) mid) high)))))

(defun test-weight-random () 
	(loop for i from 0 to 10000
		summing (weighted-random-from 0 150 300) into total
		finally (return (/ total (float 10000)))))

;(print (test-weight-random))


(defun file-to-list (file-name) 
"makes a list of all the lines in a file"
    (with-open-file (stream file-name)
        (loop for line = (read-line stream nil)
            while line collect line)))


(defun list-to-array (in-list) 
"forms an array given a list of items"
    (make-array (length in-list) :initial-contents in-list))

(defun file-to-array (file-name) 
"uses list-to-array and file-to-list to get an array from file"
	(list-to-array (file-to-list file-name)))


;makes a struck for the node that will
;contain a single character and a list
(defstruct node
  (data nil)
  (children nil))


(defun get-rest-of-chars (word)
	"returns the 2nd-final characters, 
	if the string has more that 1 char"
  (let ((length (length word)))
    (if (> length 1)
    	;if length > 1, return chars
		(subseq word 1 length)
		;else return empty string
		"")))

(defun char-in-tree? (char tree)
	"checks the first member of a branch,
	and does a char comparison on each node
	for some given character"
  (first (member char (node-children tree)

  			;maps a lambda function to the list, to
  			;verify if the node is equal to the given char
  			:test (lambda (char node) 
  				(char= char (node-data node))))))


(defun add-word-to-tree (word tree)
	"takes a word and tree, verifies (member tree first-char)
	and adds a node, or further recurses"  
  ;while the length of the word is <= 0
  (when (plusp (length word))

  	;gather the first and rest characters,
  	;and search the branch of nodes for the 
  	;one containing the first char
   (let* ((first-char (aref word 0))
	  	(rest-of-chars (get-rest-of-chars word))
	  	(containing-tree (char-in-tree? first-char tree)))

     (if containing-tree

     	;if the char is in the current branch,
     	;and the char isnt empty, call add-word
     	;on the matching member
	 	(unless (string= rest-of-chars "")
	  		(add-word-to-tree rest-of-chars containing-tree))

	 	;push the first character into a new node, and
	 	;call the call the function again, if rest-of-chars
	 (progn
	   (push (make-node :data first-char) (node-children tree))
	   (add-word-to-tree rest-of-chars (first (node-children tree)))))))
  
  ;return the tree
  tree)

(defun make-tree (list-of-words)
  (let ((tree (make-node)))
  	;for each word in the list, add
  	;a branch, or modify one
    (dolist (word list-of-words)
      (add-word-to-tree word tree))
    ;return the tree
    tree))

;;creates the variables *vowels* and *consonants* they
;;will hold the trees that will be used for words
(defparameter *consonants* (make-tree (file-to-list "consonant.txt")))
;(setf *consonants* (make-tree *consonants*))
;(print (length *consonants*))

(defparameter *vowels* (make-tree (file-to-list "vowel.txt")))
;(setf *vowels* (make-tree *vowels*))
;(print (length *vowels*))


;(print *vowels*)
;(print *consonants*)



(defun get-vowel (&optional branch)
	(if (null branch) 
		(let ((branch (nth (random-from 0 (- (length (node-children *vowels*)) 1)) (node-children *vowels*))))
			(format nil "~a~a" 
				(node-data branch)
				(get-vowel branch))
		(progn
			(if (null (node-children branch)) (return-from get-vowel nil))
			(node-data (nth (random-from 0 (- (length (node-children branch)) 1)) (node-children branch)))))))



(defun get-conson (&optional branch)
	(if (null branch)
		(let ((branch (nth (random-from 0 (- (length (node-children *consonants*)) 1)) (node-children *consonants*))))
			(format nil "~a~a" 
				(node-data branch)
				(get-conson branch))

		(if (null (node-children branch)) (return-from get-conson "")
			(node-data (nth (random-from 0 (- (length (node-children branch)) 1)) (node-children branch)))))))
;(print (nth (random-from 0 (length (node-children *consonants*))) (node-children *consonants*)))



(defun get-chunk ()
	;(nth (random 7) 
		(nth (random 7) 
				(list (list (get-vowel)) 
					(list (get-vowel) (get-conson))
					(list (get-vowel) (get-conson) (get-conson))
					(list (get-conson) (get-conson) (get-vowel))
					(list (get-vowel) (get-conson) (get-conson)))))
					;(list (get-conson) (get-vowel)))))



(defun gen-syllable ()
	(loop for i from 0 to (weighted-random-from 1 2 7)
		collect (get-chunk)))

(format nil "~{~{~a~}~}" (gen-syllable))



;(lambda (x) (* x 2))

;(list #'+ (function -) (symbol-function '*) (lambda (x) (* x 2)))
	

;(word-gen)


;(print nil)

