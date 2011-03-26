;;;; tnetstring.lisp
;;;; Copyright (c) 2011 Jason Miller

(in-package #:tnetstring)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (with-input-from-string (s "hello") (file-position s 2))
    (push :string-seek *features*)))

(declaim (optimize (speed 3) (safety 0)))

(defun make-keyword (key)
  (intern (camel-case-to-lisp key)
          (find-package 'keyword)))


(defun get-ns-length (string start end)
  (declare (ignorable end))
  (loop
	     with total fixnum = 0
	     for index = start then (1+ index)
	     for c = (aref string index)
	     when (eq c #\:) return (values total (1+ index))
	     do (setq total (+ (- (char-code c) (char-code #\0)) (* total 10)))))

(defun parse-payload (string start end)
	(declare (type string string)
		 (type fixnum start end))
	(multiple-value-bind (length start)
	    (get-ns-length string start end)
	  (let ((payload-type (aref string (+ start length))))
	  (values start length payload-type))))


(defun parse-tnetstring (string &optional (start 0) (end (1- (length string))))
  "Parse netstring"
  (declare (type fixnum start end)
	   (type string string))
  (multiple-value-bind (start length payload-type)
      (parse-payload string start end)
    (declare (type fixnum start length))
    (values
     (ecase payload-type
       (#\, (make-array (list (- end start)) 
			:element-type 'character
			:displaced-to string
			:displaced-index-offset start))
       (#\# (parse-integer string :start start :end (+ start length)))
       (#\} (parse-dict string start (+ start length)))
       (#\] (parse-list string start (+ start length)))
       (#\! (= length 4))
       (#\~ nil))
     (+ 1 start length))))




(defun parse-list (string start end)
  (declare (type fixnum start end))
  (if (= start end)
    nil
    (loop for (value next) =
	 (multiple-value-list (parse-tnetstring string start end))
	 then (multiple-value-list (parse-tnetstring string next end))
       collect value
       while (< next end))))

(defun parse-pair (string start end)
  (multiple-value-bind (key start)
      (parse-tnetstring string start end)
    (multiple-value-bind (value start)
	(parse-tnetstring string start end)
    (values key value start))))



(defun parse-dict (string start end)
  (declare (type fixnum start end)
	   (string string))
  (let ((new-hash (make-hash-table)))
    (if (= start end)
	new-hash
	(loop for (key value next) = (multiple-value-list (parse-pair string start end))
	     then (multiple-value-list (parse-pair string next end))
	   do (setf (gethash (make-keyword key) new-hash) value)
	   when (>= next end) return (values new-hash)))))


(defun dump-tnetstring (data &optional stream)
  (if (null stream)
      (with-output-to-string (s)
	(dump-tnetstring-internal data s))
      (dump-tnetstring-internal data stream)))

(defun output-netstring (data identifier  stream)
  "Internal function used by dump-tnetstring"
  (declare (type string data)
	   (type stream stream)
	   (type character identifier))
  (format stream "~D" (length data))
  (write-char #\: stream)
  (write-sequence data stream)
  (write-char identifier stream))

(defun dump-tnetstring-fixnum (n stream)
  (declare (type fixnum n)
	   (type stream stream))
  (let ((length 
	 (loop with length of-type fixnum = (if (< n 0) 2 1)
	    with mult of-type integer = 10
	    with n of-type fixnum = (if (= n most-negative-fixnum) most-positive-fixnum (abs n))
	    while (<= mult n)
	    do (setq mult (* 10 mult) length (+ 1 length))
	    finally (return length))))
    (format stream "~D" length)
    (write-char #\: stream)
    (format stream "~D" n)
    (write-char #\# stream)))

(defun dump-tnetstring-int (n stream)
  (declare (type stream stream)
	   (type integer n))
  (if (typep n 'fixnum)
      (dump-tnetstring-fixnum n stream)
      (output-netstring (format nil "~D" n) #\# stream)))

(defun dump-tnetstring-hash (h stream)
  (declare (type stream stream)
	   (type hash-table h))
  (output-netstring
   (with-output-to-string (s)
     (loop for k being the hash-key of h
	for v being the hash-value of h
	do (dump-tnetstring-internal k s)
	do (dump-tnetstring-internal v s)))
   #\} stream))


(defun dump-tnetstring-list (l stream)
  (declare (type list l)
	   (type stream stream))
  (output-netstring
   (with-output-to-string (s)
     (loop for item in l do (dump-tnetstring-internal item s)))
   #\] stream))

(defun dump-tnetstring-symbol (s stream)
  (declare (type symbol s)
	   (type stream stream))
  (output-netstring (lisp-to-camel-case (symbol-name s)) #\, stream))

(defun dump-tnetstring-internal (data stream)
  (declare (type stream stream))
  (cond
    ((typep data 'string)
     (output-netstring data #\, stream))
    ((typep data 'integer)
     (dump-tnetstring-int data stream))
    ((typep data 'hash-table)
     (dump-tnetstring-hash data stream))
    ((typep data 'real)
     (dump-tnetstring-float data stream))
    ((null data)
     (write-sequence "0:~" stream))
    ((typep data 'list)
     (dump-tnetstring-list data stream))
    ((eq t data)
     (write-sequence "4:true!" stream))
    ((typep data 'symbol)
     (dump-tnetstring-symbol data stream))
    (t (assert nil))))


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
