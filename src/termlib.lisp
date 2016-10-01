(defmacro with-ttf-init (&body body)
  `(progn
     (sdl2-ttf:init)
     ,@body
     (sdl2-ttf:quit)))

(defun resource (path)
  (asdf:system-relative-pathname 'gooey path))

(defun font (name size)
  (sdl2-ttf:open-font (resource (concatenate 'string "fonts/" name)) size))

#|
(when surface (sdl2:free-surface surface))
(when texture (sdl2:destroy-texture texture))
(when rect (sdl2:free-rect rect))
|# 

(defparameter *character-width* nil)
(defparameter *character-height* nil)

(defun col>pix (column-index)
  (* column-index *character-width*))
(defun row>pix (row-index)
  (* row-index *character-height*))

(defun test ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (window)
      (sdl2:with-renderer (renderer window :flags '(:accelerated))
	(with-ttf-init
	  (multiple-value-bind (window-width window-height)
	      (sdl2:get-window-size window)
	    (format t "~s~%" (cons window-width window-height))
	    (let* ((font (font "UbuntuMono-R.ttf" 36))
		   (surface (sdl2-ttf:render-text-blended font " " 200 0 200 255))
		   (texture (sdl2:create-texture-from-surface renderer surface)))
	      (setf *character-width* (sdl2:texture-width texture)
		    *character-height* (sdl2:texture-height texture))
	      (let ((rect (sdl2:make-rect (col>pix 0) (row>pix 0) *character-width* *character-height*)))
		(sdl2:with-event-loop (:method :poll)
		  (:keyup (:keysym keysym)
			  (let ((scancode-value (sdl2:scancode-value keysym)))
			    (cond
			      ((sdl2:scancode= scancode-value :scancode-escape) (sdl2:push-event :quit)))))
		  (:idle ()
			 (sdl2:set-render-draw-color renderer 0 0 0 255)
			 (sdl2:render-clear renderer)
			 
			 (sdl2:set-render-draw-color renderer 255 255 255 255)
			 (sdl2:render-draw-rect renderer rect)
			 
			 (sdl2:render-copy renderer texture :source-rect (cffi:null-pointer) :dest-rect rect)
			 
			 (sdl2:render-present renderer))
		  (:quit () t))))))))))
