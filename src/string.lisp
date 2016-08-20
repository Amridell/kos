(defun chars-to-string (char-list)
"This takes a list of chars (and maybe NILs?!?!?!?) and returns a string B)"
	(format nil "~{~@[~a~]~}" char-list))
