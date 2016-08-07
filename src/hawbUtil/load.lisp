;;(let (q (quit))

;;inorder to get cffi all 
;;you need to do is quickload it
(ql:quickload :cffi)

;;loading cl-tcod into lisp
;;(ql:quickload :cl-tcod)

;;dont think defstar is used in this
;;or maybe you dooo...
(ql:quickload :defstar)

;;this pushes the location of the 
;;of the tcod files into the somplace land
(push #P"/home/test/quicklisp/local-projects/tcod/"
   cffi:*foreign-library-directories*)

;;loads the tcod lib.
(ql:quickload :tcod)


;;tests to see if you can run
;;the hello world sample
;;(tcod:hello-world)

;;)
