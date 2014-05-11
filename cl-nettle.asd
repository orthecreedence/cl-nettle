(asdf:defsystem cl-nettle
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.5"
  :description "Low-level Nettle bindings for Common Lisp."
  :depends-on (#:cffi)
  :components ((:file "nettle")
               (:file "wrapper" :depends-on ("nettle"))
               (:file "bindings" :depends-on ("wrapper"))
               (:file "exports" :depends-on ("bindings"))
               (:file "accessors" :depends-on ("exports"))))

