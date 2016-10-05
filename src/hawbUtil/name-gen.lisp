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




(defparameter consonants (file-to-array "consonant.txt"))
;(print (length consonants))

(defparameter vowels (file-to-list "vowel.txt"))
;(print (length vowels))

(defstruct node (data "start" :type string) next)


 
;(defparameter thing (make-node :data "f" :children (list 1 2 3)))

(defun node-string= (node-in string-in)

	(if (string= (node-data node-in) "start") 
	t
	(string= (node-data node-in) string-in)))


;(defparameter tree (list (make-node :data "start" :children (list nil))))
;(defparameter tree (list (make-node :data "start" :next (list))))

(defun get-word-tree (word-list)
	"loops through all the strings in a list, appends to a list"
	;set the tree to just being a a starting list
	(let ((tree (list (make-node :data "start" :next (list)))))
		;for each word in the list, call branch on it
		(loop for word in word-list
			do (get-word-branch word tree))
		;output the tree
		tree))

(defun get-word-branch (word curr-tree)
	"loop through members of curr-tree, if match, recurse on that node, 
	else add a the first char to the list, continue recursing"
	
	;initialize variables, first
	;and rest are the first char
	;and rest of the string resepectively
	
	(if (null word) (return-from get-word-branch nil))

	(let* ((first (if (< 0 (length word))
						(coerce (list (aref word 0)) 'string) 
						nil))

		(rest (if (not (null first)) (subseq word 1) nil)))
	
		;for the nodes in the current tree,
		(loop for i in curr-tree
			
			;if the nodes data member is start, 
			;set its member as the first char
			do (if (string= (node-data i) "start")
				(setf (node-data i) first)
				())
			
			do (if (node-string= i first)
			 	;if the node and first match call get-branch again
				(get-word-branch rest (node-next i))

				;else, append a new node into curr-tree
				;and call the get-branch function again
				(progn (setf curr-tree
							(append curr-tree
								(list (make-node :data first :next (list nil)))))
						(get-word-branch rest curr-tree))))
					
		;(print curr-tree))
		curr-tree))
							


(print (get-word-tree vowels))




(defun get-vowel () 
	"returns a random string made of a vowel"
	(nth (random-from 0 (- (length vowels) 1)) vowels))

(defun get-conson () 
	"returns a random string made of a consonant"
	(aref consonants (random-from 0 (- (length consonants) 1))))

(defun syllable-gen () 
	"generates a syllable based off vowels and consonants"
	(concatenate 'string (get-vowel) (get-vowel)))


(defun word-gen ()
	"randomly generates a number of syllables and produces a string"
	(let (word) 
		(loop for syl-count from 0 to (weighted-random-from 1 3 6)

	;add some junk in here
			do (print syl-count)
			do (print (setf word 
				(concatenate 'string word (syllable-gen)))))))
	;herehrehrherherhehre
		

;(lambda (x) (* x 2))

;(list #'+ (function -) (symbol-function '*) (lambda (x) (* x 2)))
	

(word-gen)


(print "")