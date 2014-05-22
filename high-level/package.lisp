(defpackage :nettle-highlevel
  (:use :cl :static-vectors)
  (:nicknames :nec)
  (:export #:nettle-error
           #:nettle-auth-error

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

