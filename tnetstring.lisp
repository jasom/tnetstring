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

#|
(defun make-keyword (key)
  (intern key 
          (find-package 'keyword)))
|#

(defun get-ns-length (stream)
  (loop
     with total fixnum = 0
     for c = (read-char stream)
     when (eq c #\:) return total
       do (setq total (+ (- (char-code c) (char-code #\0)) (the fixnum (* total 10)))))) 

#+string-seek(defun parse-payload (stream)
	(declare (type stream stream))
	(let* 
	    ((length (get-ns-length stream))
	     (position (the fixnum (file-position stream)))
	     (seek (file-position stream (+ position length)))
	     (payload-type (read-char stream)))
	  (declare (type fixnum position length))
	  (file-position stream position)
	  (assert seek)
	  (values length payload-type)))

#-string-seek(defun parse-payload (stream)
	(declare (type stream stream))
	(let* 
	    ((length (get-ns-length stream))
	     (data (make-string length))
	     (_ (read-sequence data stream))
	     (payload-type (read-char stream)))
	  (declare (type fixnum length))
	  (values data payload-type)))

#+string-seek(defun parse-tnetstream (stream)
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
	     (#\} (parse-dict-to-alist stream length))
	     (#\] (parse-list stream length))
	     (#\! (progn
		    (file-position stream (+ (the fixnum (file-position stream)) length))
		    (= length 4)))
	     (#\~ nil)
	     (#\, (let ((str (make-string length)))
		    (read-sequence str stream)
		    str)))))
      (read-char stream)
      returnme)))

#-string-seek(defun parse-tnetstream (stream)
	"Parse netstring in seekable stream"
	(multiple-value-bind (data payload-type) (parse-payload stream)
	  (with-input-from-string (stream data)
	    (let* ((length (length data))
		   (returnme
		    (ecase payload-type
		      (#\# (parse-integer data))
		      (#\" data)
		      (#\} (parse-dict stream length))
		      (#\] (parse-list stream length))
		      (#\! (= length 4))
		      (#\~ nil)
		      (#\, data))))
	      returnme))))

(defun parse-tnetstring (string)
	(with-input-from-string (s string)
	  (parse-tnetstream s)))


#+string-seek(defmacro with-partial-file ((stream length) &body b)
	       (let ((end (gensym)))
		 `(let ((,end (+ (the fixnum (file-position ,stream)) ,length)))
		    (flet ((eof-p () (>= (the fixnum (file-position ,stream)) ,end)))
		      ,@b))))
#-string-seek(defmacro with-partial-file ((stream length) &body b)
	(declare (ignore length))
	`(flet ((eof-p () (null (peek-char nil ,stream nil nil))))
	  ,@b))

(defun parse-list (stream length)
  (declare (type fixnum length))
  (if (= length 0)
    nil
    (with-partial-file (stream length)
      (loop for value = (parse-tnetstream stream)
       collect value
       while (not (eof-p))))))

(defun parse-pair (stream)
  (let* ((key (parse-tnetstream stream))
	 (value (parse-tnetstream stream)))
      (values key value)))


(defun parse-dict-to-alist (stream length)
  (declare (type fixnum length)
	   (stream stream))
  (let ((new-hash nil))
    (if (= length 0)
      new-hash
      (with-partial-file (stream length)
	(loop for (key value) = (multiple-value-list (parse-pair stream))
	   do (push (cons key value) new-hash) 
	   when (eof-p) return (values new-hash))))))

(defun parse-dict (stream length)
  (declare (type fixnum length)
	   (stream stream))
  (let ((new-hash (make-hash-table)))
    (if (= length 0)
      new-hash
      (with-partial-file (stream length)
	(loop for (key value) = (multiple-value-list (parse-pair stream))
	   do (setf (gethash (make-keyword key) new-hash) value)
	   when (eof-p) return (values new-hash))))))

#+nil(defun parse-dict (stream length)
       (let ((new-hash (make-hash-table)))
	 (if (= length 0)
	     new-hash
	     (let ((end (+ (file-position stream) length)))
	       (loop for (key value) = (multiple-value-list (parse-pair stream))
		  do (setf (gethash (make-keyword key) new-hash) value)
		  when (>= (file-position stream) end) return (values new-hash))))))

(defun dump-tnetstring (data &optional stream)
  (if (null stream)
      (with-output-to-string (s)
	(dump-tnetstring-internal data s))
      (dump-tnetstring-internal data stream)))

(defun digit-to-char (x) (code-char (+ (char-code #\0) x)))

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

(defun dump-tnetstring-alist (alist stream)
  (declare (type stream stream)
	   (type list alist))
  (output-netstring
   (with-output-to-string (s)
     (loop for pair in alist
	do (dump-tnetstring-internal (car pair) s)
	do (dump-tnetstring-internal (cdr pair) s)))
   #\} stream))

(defun dump-tnetstring-list (l stream)
  (declare (type list l)
	   (type stream stream))
  (if (and (consp (car l)) (keywordp (caar l)))
      (dump-tnetstring-alist l stream)
      (output-netstring
       (with-output-to-string (s)
	 (loop for item in l do (dump-tnetstring-internal item s)))
       #\] stream)))

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
