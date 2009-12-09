;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :cl-io-mod
  :depends-on (:bintype :iterate)
  :components
  ((:file "packages")
   (:file "mod" :depends-on ("packages"))))
