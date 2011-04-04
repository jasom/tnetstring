;;;; package.lisp

(defpackage #:tnetstring
  (:use #:cl)
  (:export :parse-tnetstring
           :parse-tnetbytes
           :dump-tnetstring
	   :*dict-decode-type*
	   :*false*
	   :*empty-list*
	   :*null*
	   :*make-empty-dict*
	   :*nil-encode*
           :*decode-table*
           :*translate-read-key*
           :*translate-write-symbol*
	   ))

