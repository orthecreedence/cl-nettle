(in-package :nettle-highlevel)

(defun* (encrypt-aes-cbc -> octet-array)
          ((key octet-array-32)
           (plaintext octet-array)
           (iv octet-array-16)
           &key ((padding symbol) 'pad-ansix923))
  "Encrypt plaintext via AES-CBC. :start/:end keywords can be used to specify
   the position of the plaintext in the given plaintext array, which can be
   useful for minimizing copy if your plaintext has "
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (assert (or (= (length key) ne:+aes-min-key-size+)
              (= (length key) ne:+aes-max-key-size+)))
  (assert (= (length iv) ne:+aes-block-size+))
  (with-crypto-object (ctx :aes)
    (let ((plaintext (funcall padding plaintext))
          (enc-fn (cffi:foreign-symbol-pointer "nettle_aes_encrypt")))
      (with-static-vectors ((key-s key)
                            (plaintext-s plaintext)
                            (iv-s iv)
                            (ciphertext-s (length plaintext)))
        (ne:aes-set-encrypt-key ctx (length key) (static-vector-pointer key-s))
        (ne:cbc-encrypt ctx
                        enc-fn
                        ne:+aes-block-size+
                        (static-vector-pointer iv-s)
                        (length plaintext)
                        (static-vector-pointer ciphertext-s)
                        (static-vector-pointer plaintext-s))
        (copy-seq ciphertext-s)))))

(defun* (decrypt-aes-cbc -> octet-array)
          ((key octet-array-32)
           (ciphertext octet-array)
           (iv octet-array)
          &key ((start fixnum) 0)
               ((end (or null fixnum)) nil))
  "Decrypt ciphertext via AES-CBC.
   
   You can optionally specify the :start/:end of the ciphertext which is useful
   in cases where you are decrypting a large block of data that has auth info
   encoded in the array and you don't want to copy the ciphertext out."
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (assert (or (= (length key) ne:+aes-min-key-size+)
              (= (length key) ne:+aes-max-key-size+)))
  (assert (= (length iv) ne:+aes-block-size+))
  (with-crypto-object (ctx :aes)
    (let ((enc-fn (cffi:foreign-symbol-pointer "nettle_aes_decrypt")))
      (with-static-vectors ((key-s key)
                            (ciphertext-s ciphertext :start start :end end)
                            (iv-s iv)
                            (plaintext-s (length ciphertext)))
        (ne:aes-set-decrypt-key ctx (length key) (static-vector-pointer key-s))
        (ne:cbc-decrypt ctx
                        enc-fn
                        ne:+aes-block-size+
                        (static-vector-pointer iv-s)
                        (length ciphertext)
                        (static-vector-pointer plaintext-s)
                        (static-vector-pointer ciphertext-s))
        (unpad plaintext-s)))))

