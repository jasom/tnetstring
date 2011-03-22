;;;; tnetstring.lisp

(in-package #:tnetstring)

(declaim (optimize (speed 3)))

(defun make-keyword (key)
  (intern (camel-case-to-lisp key)
          (find-package 'keyword)))

(defun slice (vector &key (start 0) (end -1))
  (declare (type fixnum start end)
	   (type string vector))
  (let ((start (if (< start 0) (+ 1 (length vector) start) start))
        (end (if (< end 0) (+ 1 (length vector) end) end)))
    (declare (type fixnum start end))
    (make-array (list (- end start)) :displaced-to vector :displaced-index-offset start
                :element-type (array-element-type vector))))

(defun find-and-split (vector value)
  (declare (type string vector)
	   (type character value))
  (let ((idx (position value vector)))
    (if idx
      (values (slice vector :end idx)
              (slice vector :start (1+ idx)))
      (values vector #()))))

;Could switch code-char to an ASCII digit table since there are only 10 values
;we care about.  That would also fix the parse-integer ignores whitspace
;incompatibility.
(defun parse-bin-integer (vector)
  (parse-integer (map 'string #'code-char vector)))

(defun parse-payload (stream)
  (declare (type stream stream))
  (let* 
      ((length (parse-integer (with-output-to-string (s)
				(loop for c = (read-char stream)
				   while (not (eq c #\:))
				   do (write-char c s)))))
       (payload (make-string length :initial-element #\Nul))
       (position (file-position stream))
       (seek (file-position stream (+ position length)))
       (payload-type (read-char stream)))
    (file-position stream position)
    (assert seek)
    (values length payload-type)))

(defun parse-tnetstream (stream)
  "Parse netstring in seekable stream"
  (multiple-value-bind (length payload-type) (parse-payload stream)
    (let ((returnme
	   (ecase payload-type
	     (#\# (let ((str (make-string length)))
		    (read-sequence str stream)
		    (parse-integer str)))
	     (#\" (let ((str (make-string length)))
		    (read-sequence str stream)
		    str))
	     (#\} (parse-dict stream length))
	     (#\] (parse-list stream length))
	     (#\! (progn
		    (file-position stream (+ (file-position stream) length))
		    (= length 4)))
	     (#\~ nil)
	     (#\, (let ((str (make-string length)))
		    (read-sequence str stream)
		    str)))))
      (read-char stream)
      returnme)))

(defun parse-tnetstring (string)
  (with-input-from-string (s string)
    (parse-tnetstream s)))

(defun parse-list (stream length)
  (if (= length 0)
    nil
    (let ((end (+ (file-position stream) length)))
      (loop for value = (parse-tnetstream stream)
       collect value
       while (< (file-position stream) end)))))

(defun parse-pair (stream)
  (let* ((key (parse-tnetstream stream))
	 (value (parse-tnetstream stream)))
      (values key value)))


(defun parse-dict (stream length)
  (let ((new-hash (make-hash-table)))
    (if (= length 0)
      new-hash
      (let ((end (+ (file-position stream) length)))
	(loop for (key value) = (multiple-value-list (parse-pair stream))
	   do (setf (gethash (make-keyword key) new-hash) value)
	   when (>= (file-position stream) end) return (values new-hash))))))

(defgeneric dump-tnetstring (data &optional stream))

(defun output-netstring (data identifier &optional stream)
  "Internal function used by dump-tnetstring"
  (declare (type string data))
  (format stream "~D:~A~C" (length data) data identifier))

(defmethod dump-tnetstring ((n integer) &optional stream)
  (output-netstring (format nil "~D" n) #\# stream))

(defmethod dump-tnetstring ((n real) &optional stream)
  (format stream "~f" (float n 1l0)))

(defmethod dump-tnetstring ((string string) &optional stream)
  (format stream "~D:~A," (length string) string))

(defmethod dump-tnetstring ((h hash-table) &optional stream)
  (output-netstring
    (with-output-to-string (s)
      (loop for k being the hash-key of h
            do (dump-tnetstring k s)
            do (dump-tnetstring (gethash k h) s)))
    #\} stream))

(defmethod dump-tnetstring ((l list) &optional stream)
  (output-netstring
    (with-output-to-string (s)
      (loop for item in l do (dump-tnetstring item s)))
    #\] stream))

(defmethod dump-tnetstring ((n null) &optional stream)
  (declare (ignore n))
  (format stream "0:~~"))

(defmethod dump-tnetstring ((s symbol) &optional stream)
  (if (eq s t) (output-netstring "true" #\! stream)
    (dump-tnetstring (lisp-to-camel-case (symbol-name s)) stream)))

(defparameter *tests* 
  (list (list "0:}" (make-hash-table))
        (list "0:]" nil)
        (list "51:5:hello,39:11:12345678901#4:this,4:true!0:~4:,]}" 
              (let ((x (make-hash-table)))
                (setf (gethash :hello x) (list 12345678901 "this" t nil ""))
                x))
        (list "5:12345#" 012345)
        (list "12:this is cool," "this is cool")
        (list "0:," "")
        (list "0:~" nil)
        (list "4:true!" t)
        (list "5:false!" nil)
        (list "10:," "")
        (list "24:5:12345#5:67890#5:xxxxx,]" (list 12345 67890 "xxxxx"))))

(defun myshow (x)
  (if (typep x 'hash-table)
    (format nil "{~{~S: ~S~}}"
            (loop for k being the hash-key of x
                  collect k
                  collect (gethash k x)))
    (format nil "~S" x)))

(defun test ()
  (loop for (data expect) in *tests*
     do (let ((payload (parse-tnetstring data)))
	  (assert (or payload (null expect)))

	  (format t "~&~A~&" (equalp payload expect))
	  (format t "EXPECT: ~A GOT: ~A"
		  (myshow expect)
		  (myshow payload)))))

(defun test-dump ()
  (loop for (data expect) in *tests*
        do (let ((string (dump-tnetstring expect)))
             (format t "~&~A~&" (equal string data))
             (format t "EXPECT: ~S GOT: ~S" data string))))
