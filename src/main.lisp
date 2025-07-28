(in-package #:lem-ulisp)

;; Device to read from
(defparameter *tty* #P"/dev/tty.usbserial-210")

;; Timeout when reading from serial 
;; It does not apply to the waiting for a response, only for echo and clear
(defparameter *serial-read-timeout-ms* 100)

;; testing editor

(defun but-last (seq)
  (s:slice seq 0 -1))

(defun run-op (ops sexp)
  (a:switch ((car ops))
    (#\c (cons (cdr ops)
               (car sexp)))
    (#\d (cons (cdr ops)
               (cdr sexp)))
    (#\b (cons (cdr ops)
               sexp))))

(defun run-ops (ops sexp)
  (if (null ops)
      (cons ops sexp)
      (let ((res (run-op ops sexp)))
        (run-ops (car res) (cdr res)))))

(defun editor (sexp)
  (loop :for ops := (list (read-char)) 
        :then (cons (read-char) ops)
        :until (eq (car ops) #\q)
        :do (format t "$ ~a~%" ops)))

(defun eval-print (sexp)
  "eval a string in the picocalc and return the results"
  (c:with-serial (serial *tty*)
    (clean-serial serial)
    (write-serial serial sexp)
    (read-serial-response serial)))

(defmacro read-eval-print (sexp)
  "convert the sexp into a string, eval and print the results"
  `(eval-print (prin1-to-string ',sexp)))
