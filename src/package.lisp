(defpackage lem-ulisp
  (:use #:cl)
  (:local-nicknames 
   (:a #:alexandria)
   (:s #:serapeum)
   (:c #:cserial-port))
  (:export :*tty*
           :*serial-read-timeout-ms*
           :eval-print
           :read-eval-print))
