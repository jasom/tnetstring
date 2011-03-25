(asdf:load-system :cl-json)
(asdf:load-system :tnetstring)

(defparameter *json-tests* (loop for (k v) in tnetstring::*tests* 
				collect (list (json:encode-json-to-string v) v)))


(defun thrash-tnetstrings (count)
  (dotimes (i count)
    (loop for (data expect) in tnetstring::*tests*
	    do (let* ((payload (tnetstring:parse-tnetstring data))
		   (again (tnetstring:dump-tnetstring-to-string payload))
		   (back (tnetstring:parse-tnetstring again)))
	      back))))

(defun thrash-json (count)
    (dotimes (i count)
      (loop for (data expect) in *json-tests*
	 do (let* ((payload (json:decode-json-from-string data))
		(again (json:encode-json-to-string payload))
		(back (json:decode-json-from-string again)))
	   back))))

(time (thrash-tnetstrings 10000))
(time (thrash-json 10000))
