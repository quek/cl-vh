;;;; -*- Mode: LISP; -*-
(asdf:defsystem :cl-vh
  :version "0.0.0"
  :serial t
  :components ((:file "package")
               (:file "cl-vh"))
  :depends-on (:mcclim-uim
               :mcclim-freetype
               :anaphora
               :cl-fad))

