;;;; tnetstring.lisp
;;;; Copyright (c) 2011 Jason Miller

(in-package #:tnetstring)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (with-input-from-string (s "hello") (file-position s 2))
    (push :string-seek *features*)))

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

#+string-seek(defun parse-payload (stream)
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
		 `(let ((,end (+ (file-position ,stream) ,length)))
		    (flet ((eof-p () (>= (file-position ,stream) ,end)))
		      ,@b))))
#-string-seek(defmacro with-partial-file ((stream length) &body b)
	(declare (ignore length))
	`(flet ((eof-p () (null (peek-char nil ,stream nil nil))))
	  ,@b))

(defun parse-list (stream length)
  (declare (type unsigned-byte length))
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



(defun parse-dict (stream length)
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

(defun dump-tnetstring-to-string (data)
  (with-output-to-string (s)
    (dump-tnetstring data s)))

(defun output-netstring (data identifier  stream)
  "Internal function used by dump-tnetstring"
  (declare (type string data)
	   (type stream stream)
	   (type character identifier))
  (format stream "~D" (length data))
  (write-char #\: stream)
  (write-sequence data stream)
  (write-char identifier stream))



(defun dump-tnetstring-int (n stream)
  (declare (type stream stream)
	   (type integer n))
  (output-netstring (format nil "~D" n) #\# stream))


(defun dump-tnetstring-float (n stream)
  (declare (type stream stream)
	   (type real n))
  (format stream "~f" (float n 1l0)))



(defun dump-tnetstring-hash (h stream)
  (declare (type stream stream)
	   (type hash-table h))
  (output-netstring
   (with-output-to-string (s)
     (loop for k being the hash-key of h
	for v being the hash-value of h
	do (dump-tnetstring k s)
	do (dump-tnetstring v s)))
   #\} stream))


(defun dump-tnetstring-list (l stream)
  (declare (type list l)
	   (type stream stream))
  (output-netstring
   (with-output-to-string (s)
     (loop for item in l do (dump-tnetstring item s)))
   #\] stream))

(defun dump-tnetstring-symbol (s stream)
  (declare (type symbol s)
	   (type stream stream))
  (output-netstring (lisp-to-camel-case (symbol-name s)) #\, stream))

(defun dump-tnetstring (data stream)
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
        do (let ((string (dump-tnetstring-to-string expect)))
             (format t "~&~A~&" (equal string data))
             (format t "EXPECT: ~S GOT: ~S" data string))))
