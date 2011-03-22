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

(defun parse-payload (data)
  (declare (type string data)
	   (inline slice))
  (multiple-value-bind (length extra) (find-and-split data #\:)
    (let* ((length (parse-integer length))
           (payload (slice extra :end length))
           (extra (slice extra :start length))
           (payload-type (aref extra 0))
           (remain (slice extra :start 1)))
      (values payload payload-type remain))))

(defun parse-tnetstring (data)
  "Parse a string as netstring"
  (multiple-value-bind (payload payload-type remain) (parse-payload data)
    (values (ecase payload-type
              (#\# (parse-integer payload))
              (#\" payload)
              (#\} (parse-dict payload))
              (#\] (parse-list payload))
              (#\! (equal payload "true"))
              (#\~ nil)
              (#\, payload))
            remain)))

(defun parse-bytes-as-tnetstring (string)
  "Parse an array of (unsigned-byte 255) as netstring."
  (parse-tnetstring (map 'string  #'code-char string)))

(defun parse-list (data)
  (declare (type string data))
  (if (= (length data) 0)
    nil
    (loop for (value extra) = (multiple-value-list (parse-tnetstring data))
          then (multiple-value-list (parse-tnetstring extra))
          collect value
          while (> (length extra) 0))))

(defun parse-pair (data)
  (multiple-value-bind (key extra) (parse-tnetstring data)
    (multiple-value-bind (value extra) (parse-tnetstring extra)
      (values key value extra))))

(defun parse-dict (data)
  (declare (type string data))
  (let ((new-hash (make-hash-table)))
    (if (= (length data) 0)
      (values new-hash data)
      (loop for (key value extra) = (multiple-value-list (parse-pair data))
            then (multiple-value-list (parse-pair extra))
            do (setf (gethash (make-keyword key) new-hash) value)
            when (<= (length extra) 0) return (values new-hash extra)))))

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
        do (multiple-value-bind (payload remain) (parse-tnetstring data)
             (assert (= (length remain) 0))
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
