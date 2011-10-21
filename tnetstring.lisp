;;;; tnetstring.lisp
;;;; Copyright (c) 2011 Jason Miller

(in-package #:tnetstring)
(declaim (optimize (speed 3) (safety 0)))

(defparameter *dict-decode-type* :alist
  "What to encode tnetstring 'dictionaries' into.
   Valid values are :alist and :hash-table")

(defparameter *false* nil
  "What to decode tnetstring boolean 'false' into.")

(defparameter *empty-list* nil
  "What to decode tnetstring empty-list into")

(defparameter *null* :null
  "What to decode tnetstring null-object into")

(declaim (inline dumb-byte-char
		 dumb-char-byte))
(defun dumb-byte-char (v)
  (declare (type (simple-array (unsigned-byte 8) (*)) v))
  "This is a dumb way to convert a byte-vector to a string.
However, it is 100% reversible, so you can re-encode correctly if needed.
Also it is significantly faster than (map 'string #code-char v) on sbcl"
(let ((s (make-string (length v))))
  (dotimes (i (length v) s)
    (setf (aref s i) (code-char (aref v i))))))

#-sbcl(defun dumb-char-byte (s)
        (declare (type simple-string s))
        (let ((v (make-array (list (length s)) :element-type '(unsigned-byte 8)
                             :initial-element 0)))
          (dotimes (i (length s) v)
            (setf (aref v i) (char-code (aref s i))))))
#+sbcl(defun dumb-char-byte (s)
        (babel:string-to-octets s))


(defun read-integer (sequence start end)
  "Interprets the given subsequence as a base-10 integer in a 7-bit ASCII
   compatible encoding.  (So, UTC-8 will do fine.)  An invalid input yields
   garbage."  
  (declare ((simple-array (unsigned-byte 8)) sequence)
           (fixnum start end))
  (let ((sign 1))
    (declare (integer sign))
    (when (= (aref sequence start) #.(char-code #\-))
      (setf sign -1
            start (1+ start)))
    (the integer
      (loop
         for i of-type fixnum from start below end
         with result of-type integer = 0
         do (setf result
                  (+ (* result 10)
                     (- (aref sequence i) #.(char-code #\0))))
         finally (return (* sign result))))))

(defun read-length (sequence start end)
  "Returns as multiple values a small unsigned integer terminated by #\: and
   the position in the sequence immediately following the terminator.  Assumes
   a 7-bit ASCII compatible encoding, and produces garbage when given input that
   does not meet these assumptions."
  (declare ((simple-array (unsigned-byte 8)) sequence)
           (fixnum start end))
  (loop
     for i of-type fixnum from start below end
     for b = (aref sequence i)
     with length of-type fixnum = 0
     when (eq b #.(char-code #\:)) return (values length (1+ i))
     do (setf length
              (+ (the fixnum (ash length 3)) (ash length 1)
                 (- b #.(char-code #\0))))
     finally (values nil nil)))

(defmacro parsing ((bytes start end &key shared-p) &body body)
  "Convenience macro for parsing successive tnetstrings from the same
   input.  Keeps track of the necessary bookkeeping."
  (once-only (bytes start end shared-p)
    (with-unique-names (value position pos)
      `(let ((,position ,start))
         (flet ((next-tnetstring ()
                  (multiple-value-bind (,value ,pos)
                      (parse-tnetstring ,bytes ,position ,end :shared-p ,shared-p)
                    (setf ,position ,pos)
                    ,value)))
           ,@body)))))

;; Netstrings are defined as sequences of 8-bit bytes.
;; See: http://tnetstrings.org/
;; Also: http://cr.yp.to/proto/netstrings.txt
(defun parse-tnetstring (bytes &optional (start 0) (end (length bytes)) &key shared-p)
  "Reads one tnetstring from the given subsequence and returns as multiple
   values the data and the position in the sequence immediately following
   the data."
  (declare ((simple-array (unsigned-byte 8)) bytes)
           (fixnum start end))
  (multiple-value-bind (length start)
      (read-length bytes start end)
    (declare ((or null fixnum) length start))
    (if (or (null length)
            (> (+ start length) end))
        (values 'eof nil)
        (let* ((end (+ start length))
               (next (1+ end))
               (payload-type (aref bytes end)))
          (declare (fixnum end next)
                   ((unsigned-byte 8) payload-type))
          (let ((result
                 (ecase payload-type
                   ;; "string", which for tnetstrings is an uninterpreted
                   ;; sequence of bytes
                   (#.(char-code #\,)
                      ;; Sharing is a useful optimization for parsing
                      ;; because it avoids allocating memory and
                      ;; copying, but the result is no longer a
                      ;; simple-array and can't easily be passed back
                      ;; into dump-tnetstring.
                      (if shared-p
                          (make-array length :displaced-to bytes :displaced-index-offset start)
                          (subseq bytes start end)))
                   ;; integer
                   (#.(char-code #\#)
                      (read-integer bytes start end))
                   ;; dictionary
                   (#.(char-code #\})
                      (let ((alist
                             (parsing (bytes start end)
                               (loop
                                  for key = (next-tnetstring)
                                  until (eq key 'eof)
                                  for val = (next-tnetstring)
                                  collect (cons (make-keyword (dumb-byte-char key)) val)))))
                        (if (eq *dict-decode-type* :hash-table)
                            (alist-hash-table alist)
                            alist)))
                   ;; list
                   (#.(char-code #\])
                      (or
                       (parsing (bytes start end)
                         (loop
                            for val = (next-tnetstring)
                            until (eq val 'eof)
                            collect val))
                       *empty-list*))
                   ;; boolean
                   (#.(char-code #\!)
                      (or (equalp (subseq bytes start end) #.(babel:string-to-octets "true"))
                          *false*))
                   ;; float
                   (#.(char-code #\^)
                      (read-from-string (babel:octets-to-string (subseq bytes start end))))
                   ;; null
                   (#.(char-code #\~)
                      *null*))))
            (values result next))))))

(defun print-integer (n)
  "Renders an integer into an vector of octets.  As quickly as possible."
  (declare (integer n))
  (macrolet ((specialized-integer (type)
               `(let ((sign nil))
                  (declare (,type n))
                  (when (minusp n)
                    (setf sign t
                          n (- n)))
                  (let ((length
                         (loop for i of-type ,type = n then (truncate i 10) while (plusp i) count t))
                        bytes)
                    (declare (,type length))
                    (when sign
                      (incf length))
                    (setf bytes (make-array length :element-type '(unsigned-byte 8)))
                    (do ((p (1- length) (1- p)))
                        ((zerop n))
                      (multiple-value-bind (div mod)
                          (truncate n 10)
                        (setf (aref bytes p) (+ mod #.(char-code #\0))
                              n div)))
                    (when sign
                      (setf (aref bytes 0) #.(char-code #\-)))
                    bytes))))
    (cond
      ((zerop n)
       #.(make-array 1 :element-type '(unsigned-byte 8) :initial-element (char-code #\0)))
      ((typep n 'fixnum)
       (specialized-integer fixnum))
      ((typep n '(signed-byte 64))
       (specialized-integer (signed-byte 64)))
      (t
       (specialized-integer integer)))))

(defun make-tnetstring (type args)
  "Contructs a tnetstring from the available data by prepending a length
   and appending a type signature.  Optimized for fast concatenation."
  (declare ((unsigned-byte 8) type)
           (list args))
  (let* ((length (the fixnum (loop for arg in args sum (length arg))))
         (prefix (print-integer length))
         (total (+ length (length prefix) 2))
         (bytes (make-array total :element-type '(unsigned-byte 8)))
         (pos 0))
    (declare (fixnum pos))
    (loop
       for byte across prefix
       do (setf (aref bytes pos) byte
                pos (1+ pos)))
    (setf (aref bytes pos) (char-code #\:)
          pos (1+ pos))
    (dolist (arg args)
      (declare ((simple-array (unsigned-byte 8)) arg))
      (loop
         for byte across arg
         do (setf (aref bytes pos) byte
                  pos (1+ pos))))
    (setf (aref bytes pos) type)
    bytes))

(defun dump-tnetstring (data)
  "Render the given item as a sequence of octets."
  (cond
    ((null data)
     #.(babel:string-to-octets "0:~"))
    ((eq data t)
     #.(babel:string-to-octets "4:true!"))
    ((integerp data)
     (make-tnetstring
      #.(char-code #\#)
      (list (print-integer data))))
    ((floatp data)
     (make-tnetstring
      #.(char-code #\^)
      (list (babel:string-to-octets (princ-to-string data)))))
    ((stringp data)
     (make-tnetstring #.(char-code #\,) (list (babel:string-to-octets data))))
    ((vectorp data)
     (make-tnetstring #.(char-code #\,) (list data)))
    ((symbolp data)
     (make-tnetstring #.(char-code #\,) (list (dumb-char-byte (symbol-name data)))))
    ((and (listp data)
          (consp (car data))
          (symbolp (caar data)))
     (make-tnetstring
      #.(char-code #\})
      (loop
         for (key . val) in data
         collect (dump-tnetstring key)
         collect (dump-tnetstring val))))
    ((listp data)
     (make-tnetstring
      #.(char-code #\])
      (mapcar #'dump-tnetstring data)))
    ((hash-table-p data)
     (make-tnetstring
      #.(char-code #\})
      (loop
         for key being the hash-keys of data using (hash-value val)
         collect (dump-tnetstring key)
         collect (dump-tnetstring val))))))

(defparameter *tests* 
  (list (list (babel:string-to-octets "0:}") nil)
        (list (babel:string-to-octets "0:]") nil)
        (list (babel:string-to-octets "51:5:hello,39:11:12345678901#4:this,4:true!0:~4:,]}") 
	      (list :|hello| 12345678901 (babel:string-to-octets "this") t nil (babel:string-to-octets "")))
        (list (babel:string-to-octets "5:12345#") 012345)
        (list (babel:string-to-octets "12:this is cool,") (babel:string-to-octets "this is cool"))
        (list (babel:string-to-octets "0:,") (babel:string-to-octets ""))
        (list (babel:string-to-octets "0:~") :null)
        (list (babel:string-to-octets "4:true!") t)
        (list (babel:string-to-octets "5:false!") nil)
        (list (babel:string-to-octets "10:,") (babel:string-to-octets ""))
        (list (babel:string-to-octets "24:5:12345#5:67890#5:xxxxx,]") (list 12345 67890 (babel:string-to-octets "xxxxx"))))
  "Zed's compatibility suite.")

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
