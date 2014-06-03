(defpackage :nettle-highlevel
  (:use :cl :static-vectors)
  (:nicknames :nec)
  (:export #:nettle-error
           #:nettle-auth-error
           #:octet
           #:octet-array
           #:with-crypto-object
           #:with-static-vectors

           #:nettle-random-error
           #:nettle-random-init-fail
           #:nettle-random-already-open
           #:nettle-random-closed
           #:random-init
           #:random-close
           #:random-bytes
           #:random-byte
           #:random-float

           #:sha1
           #:sha256
           #:sha512
           #:md5
           #:hmac-sha1
           #:hmac-sha256
           #:hmac-sha512
           #:hmac-md5

           #:pbkdf2

           #:pad-ansix923
           #:pad-pkcs7
           #:unpad
           
           #:encrypt-aes-cbc
           #:decrypt-aes-cbc
           
           #:nettle-crypto-auth-error
           #:encrypt-aes-gcm
           #:decrypt-aes-gcm))

