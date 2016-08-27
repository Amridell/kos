; Source: 
;https://gist.github.com/johnfredcee/8243349

;;;; package.lisp

(defpackage #:atemzug-rl
  (:use #:tcod)
  (:export init-actors init-map main))

(tcod:console-set-custom-font
   (merge-pathnames #p"data/fonts/arial10x10.png"
                    (asdf/system:system-source-directory
                     (asdf:find-system "atemzug-rl")))
   '(:FONT-TYPE-GREYSCALE :FONT-LAYOUT-TCOD))

(defparameter *x* 45)
(defparameter *y* 20)
(defparameter *running* t)

(defun main-run ()
	(tcod:console-init-root 80 50 :title "Libtcod Lisp Tutorial" :fullscreen? nil :renderer :renderer-sdl)
	(loop
	   while *running*
	   do 
		 (tcod::console-clear tcod:*root*)
		 (tcod::console-put-char tcod:*root* *x* *y* (char-code #\@) :set)		  
		 (tcod::console-flush)
		 (let ((events (tcod:sys-get-events)))
		   (loop
			  for e in events
			  do 
				(format t "~A~%" e)
				(case (car e)
				  (:event-key-release (incf *x*))
				  (:event-key (incf *x*))
				  (:event-mouse-press (setf *running* nil)))))))


(defun rerun ()
  (setf *running* t)
  (main-run))