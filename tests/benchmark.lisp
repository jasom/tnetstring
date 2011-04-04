(ql:quickload :tnetstring)
(ql:quickload :cl-json)

(defun compare-alists (a b)
  (and (= (length a) (length b))
       (every (lambda (item) 
             (my-compare (cdr item) (cdr (assoc (car item) b)))) a)))

(defun compare-alist-hash (al ht)
  (and (= (length al) (hash-table-count ht))
       (every (lambda (item)
                (my-compare (cdr item) (gethash (car item) ht))) al)))

(defun my-compare (a b)
  (or (equalp a b)
      (and (consp a) (consp b)
           (consp (car a)) (consp (car b))
           (keywordp (caar a))
           (keywordp (caar b))
           (compare-alists a b))
      (and (consp a) (hash-table-p b)
           (compare-alist-hash a b))
      (and (consp a) (consp b)
           (every (lambda (x) (my-compare (car x) (cdr x)))
                  (pairlis a b)))
      (and (stringp a) (vectorp b)
           (string= a (map 'string #'code-char b)))))

(defun a-test (a b)
  (if (my-compare a b)
    (progn (format t "PASSED~&") t)
    (progn (format t "FAILED:~&")
           (print a)
           (print b) nil)))

(defun test-tnet-decode (&optional bytes)
  (with-open-file (s "tests/srecs.txt")
  (with-open-file (tnet "tests/tnets.txt")
    (let ((done (gensym)))
      (loop for item = (read s nil done)
            for titem = (when (not (eq item done))
                         (if bytes
                           (tnetstring:parse-tnetbytes
                             (map '(simple-array (unsigned-byte 8) (*)) #'char-code (read-line tnet)))
                           (tnetstring:parse-tnetstring (read-line tnet))))
            until (eq item done)
            sum (if (a-test item titem) 1 0) into passed
            sum 1 into total
            finally (progn
                      (format t "~D of ~D tests passed~&" passed total)
                      (return (values passed total))))))))

(defun test-tnet-encode-decode ()
  (let ((tests
          (with-open-file (s "tests/srecs.txt")
            (loop for item = (read s nil 'done)
                  until (eq item 'done)
                  collect item))))
    (loop for item in tests
          sum (if (a-test item (tnetstring:parse-tnetstring 
                                 (tnetstring:dump-tnetstring item)))
                1 0) into passed
          sum 1 into total
          finally (progn
                    (format t "~D of ~D tests passed~&" passed total)
                    (return (values passed total))))))

(defun benchmark-encode (iters)
  (let ((tests (with-open-file (s "tests/srecs.txt")
                 (let ((done (gensym)))
                   (loop for item = (read s nil done)
                         until (eq item done)
                         collect item)))
               ))
    (time (dotimes (_ iters) (loop for item in tests
                                   do (tnetstring:dump-tnetstring item))))))

(defun benchmark-decode (iters &optional bytes)
  (let* ((tests (with-open-file (s "tests/srecs.txt")
                  (let ((done (gensym)))
                    (loop for item = (read s nil done)
                          until (eq item done)
                          collect item)))
                )
         (test-strings (mapcar #'tnetstring:dump-tnetstring tests))
         (test-strings (if bytes
                         (mapcar
                           (lambda (x)
                             (map 
                               '(simple-array (unsigned-byte 8) (*))
                               #'char-code x))
                           test-strings)
                         test-strings)))
    ;(print test-strings)
    (time (dotimes (_ iters)
            (loop for item in test-strings
                  do (if bytes
                       (tnetstring:parse-tnetbytes item)
                       (tnetstring:parse-tnetstring item)))))))

(defun benchmark-json-decode (iters )
  (let ((json:*json-identifier-name-to-lisp* (lambda (x) x))
        (json:*lisp-identifier-name-to-json* (lambda (x) x)))
    (let ((tests (with-open-file (j "tests/json.txt")
                   (let ((done (gensym)))
                     (loop for item = (read-line j nil done)
                           until (eq item done)
                           collect item)))))
      (time (dotimes (_ iters)
              (loop for item in tests do (json:decode-json-from-string item)))))))

(defmacro run-tests (form)
  `(multiple-value-bind (a b) ,form
    (setq passed (+ passed a))
    (setq total (+ total b))))

(let ((passed 0)
      (total 0)
      (a) (b))
  (run-tests (test-tnet-decode))
  (run-tests (test-tnet-decode t))
  (run-tests (test-tnet-encode-decode))

  (let ((tnetstring:*dict-decode-type* :hash-table))
    (run-tests (test-tnet-decode))
    (run-tests (test-tnet-encode-decode)))
  (format t "~&~A of ~A tests passed~&" passed total))
 
(format t "~&TNET encode~&")
(benchmark-encode 1000)
(format t "~&TNET decode~&")
(benchmark-decode 1000)
(format t "~&TNET decode (bytes)~&")
(benchmark-decode 1000 t)
(format t "~&TNET decode (hash-table dictionaries)~&")
(let ((tnetstring:*dict-decode-type* :hash-table))
  (benchmark-decode 1000))
(format t "~&JSON decode~&")
(benchmark-json-decode 1000)
