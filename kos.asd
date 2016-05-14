;;; -*- mode: lisp; syntax: common-lisp; package: cl-user; base: 10 -*-

;;; copyright (c) 2014-2015, Grant A. Vesely. all rights reserved.
;;; TODO: add more and proper legal thtuff

(in-package :cl-user)

(defpackage :kos-asd
  (:use :cl :asdf))

(in-package :kos-asd)

(defsystem :kos
  :description "King of Shadows: A Lichlike"
  :version "0.0.0"
  :pathname "src"
  :components ((:file "package")
	       (:file "kos"
		      :depends-on ("package")))
  :depends-on (:cl-termbox))
