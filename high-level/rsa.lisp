(in-package :nettle-highlevel)

(define-condition rsa-error (nettle-error) ()
  (:documentation "Defines a general RSA error."))

(defun rsa-serialize-keys (public-pt private-pt)
  "Given public/private key *pointers*, serialize them to a byte array."
  (cffi:with-foreign-object (buffer '(:pointer (:struct ne:buffer)))
    (ne:rsa-keypair-to-sexp buffer "rsa-pkcs1-sha1" public-pt private-pt)
    (let* ((size (ne-a:buffer-size buffer))
           (res (make-array size :element-type 'octet))
           (buf-inner (ne-a:buffer-contents buffer)))
      (dotimes (i size)
        (setf (aref res i) (cffi:mem-aref buf-inner :unsigned-char i)))
      res)))

(defun rsa-generate-keypair (bits &key (exponent 257))
  "Generate a random keypair. The random system must be init (see random-init)."
  (random-check-open)
  (with-crypto-object (public :rsa-public-key)
    (with-crypto-object (private :rsa-private-key)
      (ne:rsa-public-key-init public)
      (ne:rsa-private-key-init private)
      (let ((success (ne:rsa-generate-keypair
                       public private
                       *yarrow-ctx* (cffi:foreign-symbol-pointer "nettle_yarrow256_random")
                       (cffi:null-pointer) (cffi:null-pointer)
                       bits
                       exponent)))
        (when (zerop success)
          (error 'rsa-error :msg "problem generating RSA key"))
        (rsa-serialize-keys public private)
        ))))

;(random-init)
;(format t "res: ~a~%" (rsa-generate-keypair 4096))
