(defsystem "lem-ulisp"
  :version "0.0.1"
  :author "Antonio Solana"
  :license "GPLv3"
  :depends-on ("alexandria"
               "serapeum"
               "cserial-port"
               "str")
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 
                 (:file "utils")
                 (:file "serial")
                 (:file "main"))))
  :description "Communicate with a uLisp device through the serial port")
