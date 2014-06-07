(asdf:defsystem cl-nettle
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.2.0"
  :description "Low-level Nettle bindings for Common Lisp."
  :depends-on (#:cffi
               #:static-vectors
               #:babel
               #:defstar)
  :components
  ((:file "nettle")
   (:file "wrapper" :depends-on ("nettle"))
   (:file "bindings" :depends-on ("wrapper"))
   (:file "exports" :depends-on ("bindings"))
   (:file "accessors" :depends-on ("exports"))
   (:module high-level
    :depends-on ("nettle" "bindings" "exports" "accessors")
    :serial t
    :components
    ((:file "package")
     (:file "base")
     (:file "random")
     (:file "hash")
     (:file "key")
     (:file "pad")
     (:file "aes-cbc")
     (:file "aes-gcm")
     (:file "rsa")
     (:file "ecc")))))

