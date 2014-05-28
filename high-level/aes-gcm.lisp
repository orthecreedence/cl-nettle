(in-package :nettle-highlevel)

(define-condition nettle-crypto-auth-error (nettle-error) ()
  (:documentation "Thrown when message authentication fails."))

(defun encrypt-aes-gcm (key plaintext iv auth)
  "Encrypt the given plaintext with the key/IV/auth data. All arguments must be
   in binary form '(vector (unsigned-byte 8)). As the name implies, data is
   encrypted using AES/GCM. Appends the auth tag to the end of the returned
   ciphertext."
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (assert (or (= (length key) ne:+aes-min-key-size+)
              (= (length key) ne:+aes-max-key-size+)))
  (with-crypto-object (ctx :gcm-aes)
    (with-static-vectors ((key-s key)
                          (plaintext-s plaintext)
                          (iv-s iv)
                          (auth-s auth)
                          (ciphertext-s (length plaintext))
                          (tag-s ne:+gcm-block-size+))
      (ne:gcm-aes-set-key ctx (length key) (static-vector-pointer key-s))
      (ne:gcm-aes-set-iv ctx (length iv) (static-vector-pointer iv-s))
      (when (and auth (not (zerop (length auth-s))))
        (ne:gcm-aes-update ctx (length auth) (static-vector-pointer auth-s)))
      (ne:gcm-aes-encrypt ctx
                          (length plaintext-s)
                          (static-vector-pointer ciphertext-s)
                          (static-vector-pointer plaintext-s))
      (ne:gcm-aes-digest ctx ne:+gcm-block-size+ (static-vector-pointer tag-s))
      (concatenate 'octet-array
                   ciphertext-s
                   tag-s))))

(defun decrypt-aes-gcm (key ciphertext iv auth)
  "Decrypt the given ciphertext with the key/IV/auth data. All arguments must be
   in binary form '(vector (unsigned-byte 8)). As the name implies, data is
   decrypted using AES/GCM. Assumes the auth tag is appended at the end of the
   ciphertext."
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (assert (or (= (length key) ne:+aes-min-key-size+)
              (= (length key) ne:+aes-max-key-size+)))
  (with-crypto-object (ctx :gcm-aes)
    (let* ((tagsplit ne:+gcm-block-size+)
           (tag (subseq ciphertext (- (length ciphertext) tagsplit)))
           (ciphertext-length (- (length ciphertext) tagsplit)))
      (with-static-vectors ((key-s key)
                            (ciphertext-s ciphertext)
                            (iv-s iv)
                            (auth-s auth)
                            (plaintext-s ciphertext-length)
                            (tag-s ne:+gcm-block-size+))
        (ne:gcm-aes-set-key ctx (length key) (static-vector-pointer key-s))
        (ne:gcm-aes-set-iv ctx (length iv) (static-vector-pointer iv-s))
        (when (and auth (not (zerop (length auth-s))))
          (ne:gcm-aes-update ctx (length auth) (static-vector-pointer auth-s)))
        (ne:gcm-aes-decrypt ctx
                            ciphertext-length
                            (static-vector-pointer plaintext-s)
                            (static-vector-pointer ciphertext-s))
        (ne:gcm-aes-digest ctx ne:+gcm-block-size+ (static-vector-pointer tag-s))
        (let ((authtag (copy-seq tag-s)))
          (unless (equalp authtag tag)
            (error 'nettle-crypto-auth-error :msg "Authentication failed."))
          (copy-seq plaintext-s))))))

