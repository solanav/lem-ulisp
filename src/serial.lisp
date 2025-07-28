(in-package :lem-ulisp)

(defun serial-finished-p (arr) 
  "check that the two last bytes of the array are CR and LF "
  (let ((len (length arr)))
    (when (>= len 2)
      (let ((last-byte (code-char (aref arr (- len 1))))
            (second-to-last-byte (code-char (aref arr (- len 2)))))
        (cond ((and (eq second-to-last-byte #\Return)
                    (eq last-byte #\Newline))
               :crlf)
              ((and (eq second-to-last-byte #\>)
                    (eq last-byte #\Space))
               :repl)
              (t nil))))))

(defun parse-line (buff)
  "convert the bytes into a line without CRLF and without the `> ` of the REPL"
  (when (plusp (length buff))
    (babel:octets-to-string
     buff :end (- (length buff) 2))))

(defun read-serial-line (serial &key (timeout-ms *serial-read-timeout-ms*))
  "read until CRLF/REPL and return as a string"
  (loop :with buff := (make-array 0 :element-type '(unsigned-byte 8) 
                                  :adjustable t
                                  :fill-pointer t)
        :for c := (ignore-errors
                    (c:read-serial-byte
                     serial :timeout-ms timeout-ms))
        ;; :do (format t "Read: ~a (~a : \"~a\")~%" c buff (ignore-errors (babel:octets-to-string buff)))
        :while c
        :do (vector-push-extend c buff)
        :until (serial-finished-p buff)
        :finally (return (parse-line buff))))

(defun read-serial-until-repl (serial)
  "read until we hit the REPL"
  (loop :with buff := (make-array 0 :element-type '(unsigned-byte 8) 
                                  :adjustable t
                                  :fill-pointer t)
        :for c := (c:read-serial-byte serial)
        :do (vector-push-extend c buff)
        :until (eq (serial-finished-p buff) :repl)
        :finally (return (str:split (s:fmt "~a~a" #\Return #\Newline)
                                    (babel:octets-to-string buff)))))

(defun clean-serial (serial)
  "read bytes from serial until it times out"
  (loop :while (ignore-errors
                 (c:read-serial-byte
                  serial :timeout-ms *serial-read-timeout-ms*))))

(defun read-serial-response (serial)
  "read from serial and parse into output, echo and memory left"
  (flet ((stdout (rest) (str:join #\Newline (s:slice rest 0 -1)))
         (memory (rest) (parse-integer (s:slice (car (last rest)) 0 -2))))
    (let ((echo (read-serial-line serial :timeout-ms nil))
          (rest (read-serial-until-repl serial)))
      (values (stdout rest)
              echo
              (memory rest)))))

(defun write-serial (serial sexp &optional timeout-ms)
  "write the string to the device"
  (c:write-serial-string
   sexp serial :timeout-ms timeout-ms))