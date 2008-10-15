;;; -*- Mode: Lisp -*-

(defpackage :cl-io-mod.system
  (:use :cl :asdf))

(in-package :cl-io-mod.system)

(defsystem :cl-io-mod
  :depends-on (:bintype :iterate)
  :components
  ((:file "packages")
   (:file "mod" :depends-on ("packages"))))
