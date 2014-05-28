(in-package :nettle-highlevel)

(defun encrypt-aes-cbc (key plaintext iv &key (padding 'pad-ansix923))
  "Encrypt plaintext via AES-CBC."
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

(defun decrypt-aes-cbc (key ciphertext iv)
  "Decrypt ciphertext via AES-CBC."
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (assert (or (= (length key) ne:+aes-min-key-size+)
              (= (length key) ne:+aes-max-key-size+)))
  (assert (= (length iv) ne:+aes-block-size+))
  (with-crypto-object (ctx :aes)
    (let ((enc-fn (cffi:foreign-symbol-pointer "nettle_aes_decrypt")))
      (with-static-vectors ((key-s key)
                            (ciphertext-s ciphertext)
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

