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
      (and (listp a) (listp b)
           (consp (car a)) (consp (car b))
           (keywordp (caar a))
           (keywordp (caar b))
           (compare-alists a b))
      (and (listp a) (hash-table-p b)
           (compare-alist-hash a b))
      (and (listp a) (listp b)
           (every #'my-compare a b))
      (and (stringp a) (vectorp b)
           (string= a (map 'string #'code-char b)))))

(defun a-test (a b)
  (if (my-compare a b)
      (progn (format t "PASSED~&") t)
      (progn (format t "FAILED:~&")
             (print a)
             (print b) nil)))

(defun test-tnet-decode ()
  (with-open-file (s "tests/srecs.txt")
    (with-open-file (tnet "tests/tnets.txt")
      (let ((done (gensym)))
        (loop
           for item = (read s nil done)
           until (eq item done)
           for titem = (tnetstring:parse-tnetstring (babel:string-to-octets (read-line tnet)))
           count (a-test item titem) into passed
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
                      collect item)))))
    (time (dotimes (_ iters)
            (mapc #'tnetstring:dump-tnetstring tests)))))

(defun benchmark-decode (iters)
  (let* ((tests (with-open-file (s "tests/srecs.txt")
                  (let ((done (gensym)))
                    (loop for item = (read s nil done)
                       until (eq item done)
                       collect item))))
         (test-strings (mapcar #'tnetstring:dump-tnetstring tests)))
    (time (dotimes (_ iters)
            (mapc #'tnetstring:parse-tnetstring test-strings)))))

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
  (run-tests (test-tnet-encode-decode))

  (let ((tnetstring:*dict-decode-type* :hash-table))
    (run-tests (test-tnet-decode))
    (run-tests (test-tnet-encode-decode)))
  (format t "~&~A of ~A tests passed~&" passed total))

(format t "~&TNET encode~&")
(benchmark-encode 1000)
(format t "~&TNET decode~&")
(benchmark-decode 1000)
(format t "~&TNET decode (hash-table dictionaries)~&")
(let ((tnetstring:*dict-decode-type* :hash-table))
  (benchmark-decode 1000))
(format t "~&JSON decode~&")
(benchmark-json-decode 1000)
