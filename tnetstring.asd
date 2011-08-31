;;;; tnetstring.asd

(asdf:defsystem #:tnetstring
  :depends-on (#:alexandria
               #:flexi-streams)
  :serial t
  :components ((:file "package")
               (:file "camel-case")
               (:file "tnetstring")))
