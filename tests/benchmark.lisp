(ql:quickload :tnetstring)
(ql:quickload :cl-json)

(defun compare-alists (a b)
  (and (= (length a) (length b))
       (every (lambda (item) 
             (equalp item (assoc (car item) b))) a)))

(defun compare-alist-hash (al ht)
  (and (= (length al) (hash-table-count ht))
       (every (lambda (item)
                (equalp (cdr item) (gethash (car item) ht))) al)))

(defun my-compare (a b)
  (or (equalp a b)
      (and (consp a) (consp b)
           (consp (car a)) (consp (car b))
           (keywordp (caar a))
           (keywordp (caar b))
           (compare-alists a b))
      (and (consp a) (hash-table-p b)
           (compare-alist-hash a b))))

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
      (loop for item = (read s nil done)
            for titem = (when (not (eq item done))
                         (tnetstring:parse-tnetstring (read-line tnet)))
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

(defun benchmark-decode (iters)
  (let* ((tests (with-open-file (s "tests/srecs.txt")
                  (let ((done (gensym)))
                    (loop for item = (read s nil done)
                          until (eq item done)
                          collect item)))
                )
         (test-strings (mapcar #'tnetstring:dump-tnetstring tests)))
    ;(print test-strings)
    (time (dotimes (_ iters)
            (loop for item in test-strings
                  do (tnetstring:parse-tnetstring item))))))

(defun benchmark-json-decode (iters)
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
 
(benchmark-encode 1000)
(benchmark-decode 1000)
(let ((tnetstring:*dict-decode-type* :hash-table))
(benchmark-encode 1000)
(benchmark-decode 1000))
(benchmark-json-decode 1000)
