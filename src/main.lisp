(defpackage pico-repl
  (:use #:cl)
  (:local-nicknames 
   (:a :alexandria)
   (:s :serapeum)
   (:c :cserial-port))
  (:export :eval-read))
(in-package #:pico-repl)

(defparameter *tty* #P"/dev/ttyUSB0")

(defun carriage-p (n) (= n 13))

(defun try-read-serial (serial)
  "reads from tty a string or nil if finished"
  (let ((res (make-array 32 :element-type '(unsigned-byte 8))))
    (let ((total-read (handler-case
                          (c:read-serial-byte-vector
                           res serial :timeout-ms 500)
                        (error () 0))))
      (when (plusp total-read)
        (babel:octets-to-string res :end total-read)))))

(defun read-serial-raw ()
  "reads from tty and return the string"
  (c:with-serial (rs *tty*)
    (let ((res (loop :for res := (try-read-serial rs)
                     :while res 
                     :collect res)))
      (when res
        (s:~>> res
               (format nil "~{~a~}")
               (str:split #\Return)
               (mapcar #'str:trim))))))

(defun read-serial ()
  "reads from tty and return the result, echo and memory left"
  (a:when-let ((output (read-serial-raw)))
    (let* ((len (length output))
           (echo (car output))
           (results (subseq output 1 (1- len)))
           (memory (s:slice (last output) 0 -1)))
      (values (str:trim-right (str:join #\Newline results))
              echo
              memory))))

(defun write-serial (sexp)
  "send a string to the tty"
  (c:with-serial (ws *tty*)
    (c:write-serial-string sexp ws)))

(defun eval-read (sexp)
  "eval a string in the picocalc"
  (read-serial)
  (write-serial sexp)
  (with-input-from-string (s (read-serial))
    (read s)))

(defmacro eval-read-exp (sexp)
  `(eval-read (prin1-to-string ',sexp)))
