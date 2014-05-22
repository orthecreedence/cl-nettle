(defpackage :nettle-highlevel
  (:use :cl :static-vectors)
  (:nicknames :nec)
  (:export #:nettle-error
           #:nettle-auth-error
           #:octet
           #:octet-array
           #:with-crypto-object
           #:with-static-vectors

           #:random-init
           #:random-close
           #:random-bytes
           #:random-byte
           #:random-float

           #:sha1
           #:sha256
           #:sha512
           #:md5

           #:encrypt-aes-gcm
           #:decrypt-aes-gcm))

