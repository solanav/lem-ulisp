(defsystem "lem-ulisp""
  :version "0.0.1"
  :author "Antonio Solana"
  :license ""
  :depends-on (:alexandria
               :serapeum
               :cserial-port
               :str)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Communicate with the serial port")
