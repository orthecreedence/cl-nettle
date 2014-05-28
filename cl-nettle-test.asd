(asdf:defsystem cl-nettle-test
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :description "Low-level Nettle bindings for Common Lisp."
  :depends-on (#:fiveam #:cl-nettle #:babel #:yason)
  :components
  ((:module test
    :serial t
    :components
    ((:file "main")
     (:file "hash")
     (:file "random")
     (:file "crypto")))))

