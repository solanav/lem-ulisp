(in-package :lem-ulisp)

(defun list-tty-devices ()
  (directory #P"/dev/tty.*"))