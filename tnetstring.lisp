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

(defun get-ns-length (stream)
  (loop
     with total = 0
     for c = (read-char stream)
     when (eq c #\:) return total
       do (setq total (+ (- (char-code c) (char-code #\0)) (* total 10)))))

(defun parse-payload (stream)
  (declare (type stream stream))
  (let* 
      ((length (get-ns-length stream))
       (position (file-position stream))
       (seek (file-position stream (+ position length)))
       (payload-type (read-char stream)))
    (declare (type fixnum position length))
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
  (declare (type unsigned-byte length))
  (if (= length 0)
    nil
    (let ((end (+ (the unsigned-byte (file-position stream)) length)))
      (loop for value = (parse-tnetstream stream)
       collect value
       while (< (the unsigned-byte (file-position stream)) end)))))

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

(defun dump-tnetstring-to-string (data)
  (with-output-to-string (s)
    (dump-tnetstring data s)))

(defgeneric dump-tnetstring (data stream))

(defun output-netstring (data identifier  stream)
  "Internal function used by dump-tnetstring"
  (declare (type string data)
	   (type stream stream)
	   (type character identifier))
  (format stream "~D" (length data))
  (write-char #\: stream)
  (write-sequence data stream)
  (write-char identifier stream))


(defmethod dump-tnetstring ((n integer)  stream)
  (declare (type stream stream))
  (output-netstring (format nil "~D" n) #\# stream))

(defmethod dump-tnetstring ((n real)  stream)
  (declare (type stream stream))
  (format stream "~f" (float n 1l0)))

(defmethod dump-tnetstring ((string string)  stream)
  (output-netstring string #\, stream))

(defmethod dump-tnetstring ((h hash-table)  stream)
  (declare (type stream stream))
  (output-netstring
    (with-output-to-string (s)
      (loop for k being the hash-key of h
            do (dump-tnetstring k s)
            do (dump-tnetstring (gethash k h) s)))
    #\} stream))

(defmethod dump-tnetstring ((l list)  stream)
  (declare (type stream stream))
  (output-netstring
    (with-output-to-string (s)
      (loop for item in l do (dump-tnetstring item s)))
    #\] stream))

(defmethod dump-tnetstring ((n null)  stream)
  (declare (type stream stream))
  (declare (ignore n))
  (write-sequence "0:~" stream))

(defmethod dump-tnetstring ((s symbol)  stream)
  (declare (type stream stream))
  (if (eq s t) (write-sequence "4:true!" stream)
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
