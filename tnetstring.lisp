;;;; tnetstring.lisp
;;;; Copyright (c) 2011 Jason Miller

(in-package #:tnetstring)
(declaim (optimize (speed 3) ))

(defparameter *dict-decode-type* :alist
  "What to encode tnetstring 'dictionaries' into.
   Valid values are :alist and :hash-table")

(defparameter *false* nil
  "What to decode tnetstring boolean 'false' into.")

(defparameter *empty-list* nil
  "What to decode tnetstring empty-list into")

(defparameter *null* :null
  "What to decode tnetstring null-object into")

(defparameter *make-empty-dict* (lambda () (if (eq *dict-decode-type* :hash-table)
                                          (make-hash-table)
                                          nil))
  "Function to create object when reading an empty-dictionary.
   Unlike *empty-list* this is a function, since a fresh, empty
   hash-table is something you might reasonably want (and is in fact
   the default when *dict-decode-type* is :hash-table")

(defparameter *nil-encode* "0:~"
  "What nil should encode as")

(defparameter *encode-table* '((false . "5:false!")
			       (null . "0:~")
			       (empty . "0:]"))
  "An alist of symbols that should decode to particular netstrings.")



(defparameter *translate-read-key* nil
  "Function to translate names of keys when reading dictionaries.
Defaults to the identity")

(defparameter *translate-write-symbol* nil
  "Function to translate names of symbols when writing.
Should probably be inverse of *translate-read-key*
Defaults to the identity")

(defconstant +key-package+ (find-package :keyword)
  "Package to intern dictionary keys in")


(defstruct (fake-string-stream (:conc-name fss-))
  (data "" :type (or simple-string (simple-array (unsigned-byte 8) (*))))
  (pos -1 :type fixnum)
  (length 0 :type fixnum))


(defun fss-read-char (fake-stream)
  (declare (type fake-string-stream fake-stream))
  (progn
    (incf (fss-pos fake-stream))
    (when (>= (fss-pos fake-stream) (fss-length fake-stream))
      (error (make-condition 'end-of-file :stream fake-stream)))
    (aref (fss-data fake-stream) (fss-pos fake-stream))))

(defun fss-read-offset (fake-stream offset)
  (declare (type fake-string-stream fake-stream)
	   (type fixnum offset))
  (aref (fss-data fake-stream) (+ (fss-pos fake-stream) offset 1)))

(defun fss-consume (fake-stream count)
  (declare (type fixnum count))
  (setf (fss-pos fake-stream) (+ count (fss-pos fake-stream))))

(defun fss-read-sequence (seq str)
  (declare (type array seq))
  (let ((l (length seq)))
    (dotimes (i l)
      (setf (aref seq i) (fss-read-char str)))))


(defun dumb-convert (v)
  (declare (optimize (speed 3))
	   (type (simple-array (unsigned-byte 8) (*)) v))
  "This is a dumb way to convert a byte-vector to a string.
However, it is 100% reversible, so you can re-encode correctly if needed.
Also it is significantly faster than (map 'string #code-char v) on sbcl"
  (let ((s (make-string (length v))))
    (dotimes (i (length v))
      (setf (aref s i) (code-char (aref v i)))) s))

#+nil
(defun make-keyword (key)
  (declare (values symbol))
  (let ((key (if (not (typep key 'string))
		 (dumb-convert key)
		 key)))
    (intern (if *translate-read-key*
                (funcall *translate-read-key* key)
                key)
            +key-package+)))

(defun get-ns-length (fake-stream)
  (loop
     with total fixnum = 0
     for a = (fss-read-char fake-stream)
     for c = (if (typep a 'character) (char-code a) a)
     when (eq c (char-code #\:)) return total
     do (setq total (+ (- c (char-code #\0)) (the fixnum (* total 10)))))) 

(declaim (inline alloc-string))

(defun alloc-string (marker length)
  (if (typep marker 'character)
      (make-string length)
      (make-array (list length) :element-type '(unsigned-byte 8))))

(defun read-integer (sequence start end)
  (loop
     for i from start below end
     with result = 0
     do (setf result
              (+ (* result 10)
                 (- (aref sequence i) (char-code #\0))))
     finally (return result)))

(defun read-length (sequence start end)
  (loop
     for i from start below end
     for b = (aref sequence i)
     with length = 0
     when (eq b (char-code #\:)) return (list length (1+ i))
     do (setf length
              (+ (* length 10)
                 (- b (char-code #\0))))
     finally (return (list nil nil))))

(defun parse-tnetstring (bytes &optional (start 0) (end (length bytes)))
  (destructuring-bind (length start)
      (read-length bytes start end)
    (if (or (null length)
            (> (+ start length) end))
        'eof
        (let* ((end (+ start length))
               (next (1+ end))
               (payload-type (aref bytes end)))
          (let ((result
                 (ecase payload-type
                   ;; "string", which for tnetstrings is an uninterpreted
                   ;; sequence of bytes
                   (#.(char-code #\,)
                      (make-array length :element-type '(unsigned-byte 8) :displaced-to bytes :displaced-index-offset start))
                   ;; integer
                   (#.(char-code #\#)
                      (read-integer bytes start (+ start length)))
                   ;; dictionary
                   (#.(char-code #\})
                      (let ((alist
                             (let ((start start))
                               (loop
                                  for (key pos1) = (multiple-value-list (parse-tnetstring bytes start end))
                                  until (eq key 'eof)
                                  for (val pos2) = (multiple-value-list (parse-tnetstring bytes pos1 end))
                                  do (setf start pos2)
                                  collect (cons key val)))))
                        (if (eq *dict-decode-type* :hash-table)
                            (alist-hash-table alist)
                            alist)))
                   ;; list
                   (#.(char-code #\])
                      (or
                       (let ((start start))
                         (loop
                            for (val pos) = (multiple-value-list (parse-tnetstring bytes start end))
                            until (eq val 'eof)
                            do (setf start pos)
                            collect val))
                       *empty-list*))
                   ;; boolean
                   (#.(char-code #\!)
                      (or (equalp (subseq bytes start end) #.(string-to-octets "true"))
                          *false*))
                   ;; float
                   (#.(char-code #\^)
                      (read-from-string (octets-to-string (subseq bytes start end))))
                   ;; null
                   (#.(char-code #\~)
                      *null*))))
            (values result next))))))

(defmacro with-partial-file ((stream length) &body b)
  (let ((end (gensym)))
    `(let ((,end (+ (fss-pos ,stream) ,length)))
       (flet ((eof-p () (>= (fss-pos ,stream) ,end)))
	 ,@b))))

(defun dump-tnetstring (data &optional stream)
  "Serialize data to a tnetstring.  If stream is missing or nil,
then outputs to a string.  Otherwise outputs to stream"
  (if (null stream)
      (with-output-to-string (s)
	(dump-tnetstring-internal data s))
      (dump-tnetstring-internal data stream)))

(let ((length-print (pprint-dispatch most-positive-fixnum)))
  (defun output-netstring (data identifier  stream)
    (declare (type string data)
             (type stream stream)
             (type character identifier))
    
    #+sbcl(funcall length-print stream (length data))
    #-sbcl(write (length data) :stream stream)
                                        ;(format stream "~D" (length data))
    (write-char #\: stream)
    (write-sequence data stream)
    (write-char identifier stream)))

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
        when (not (typep k '(or string symbol)))
        do (error 'type-error :expected-type '(or string symbol)
                  :datum k)
	do (dump-tnetstring-internal k s)
	do (dump-tnetstring-internal v s)))
   #\} stream))

(defun dump-tnetstring-alist (alist stream)
  (declare (type stream stream)
	   (type list alist))
  (output-netstring
   (with-output-to-string (s)
     (loop for pair in alist
        for k = (car pair)
        when (not (typep k '(or string symbol)))
	do (error 'type-error :expected-type '(or string symbol)
                  :datum k)
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
  (if (and
       (not (null *encode-table*))
       (assoc s *encode-table*))
      (let ((x (cdr (assoc s *encode-table*))))
	(write-sequence (cdr x) stream))
      (output-netstring (if *translate-write-symbol*
			    (funcall *translate-write-symbol* (symbol-name s))
			    (symbol-name s))
			#\, stream)))

(defun dump-tnetstring-float (f stream)
  (let ((f (float f 1d0))
	(*read-default-float-format* 'double-float))
    (output-netstring (format nil "~,6F" f) #\^ stream)))

(defun dump-tnetstring-internal (data stream)
  (declare (type stream stream))
  (cond
    ((typep data 'string)
     (output-netstring data #\, stream))
    ((typep data 'integer)
     (dump-tnetstring-int data stream))
    ((typep data 'hash-table)
     (dump-tnetstring-hash data stream))
                                        ;((typep data 'real)
                                        ;(dump-tnetstring-float data stream))
    ((null data)
     (write-sequence *nil-encode* stream))
    ((typep data 'list)
     (dump-tnetstring-list data stream))
    ((eq t data)
     (write-sequence "4:true!" stream))
    ((typep data 'symbol)
     (dump-tnetstring-symbol data stream))
    ((typep data 'float)
     (dump-tnetstring-float data stream))
    (t 
     (error 'type-error :expected-type
	    '(or string integer hash-table null list symbol)))))


(defparameter *tests* 
  (list (list (string-to-octets "0:}") nil)
        (list (string-to-octets "0:]") nil)
        (list (string-to-octets "51:5:hello,39:11:12345678901#4:this,4:true!0:~4:,]}") 
	      (list :|hello| 12345678901 (string-to-octets "this") t nil (string-to-octets "")))
        (list (string-to-octets "5:12345#") 012345)
        (list (string-to-octets "12:this is cool,") (string-to-octets "this is cool"))
        (list (string-to-octets "0:,") (string-to-octets ""))
        (list (string-to-octets "0:~") :null)
        (list (string-to-octets "4:true!") t)
        (list (string-to-octets "5:false!") nil)
        (list (string-to-octets "10:,") (string-to-octets ""))
        (list (string-to-octets "24:5:12345#5:67890#5:xxxxx,]") (list 12345 67890 (string-to-octets "xxxxx")))))

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
