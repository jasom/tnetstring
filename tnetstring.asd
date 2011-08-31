;;;; tnetstring.asd

(asdf:defsystem #:tnetstring
  :depends-on (#:alexandria
               #:babel)
  :serial t
  :components ((:file "package")
               (:file "camel-case")
               (:file "tnetstring")))
