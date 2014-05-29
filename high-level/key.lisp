(in-package :nettle-highlevel)

(defun pbkdf2 (pass salt iterations key-size &key (hasher :sha256))
  "PBKDF2 the given password/salt combo. Spits out a key of key-size bytes. 
   Optionally specify the hasher as a keyword (defaults to :sha256, currently
   the only other alternative is :sha1)."
  (with-static-vectors ((pass-s pass)
                        (salt-s salt)
                        (key-s key-size))
    (let ((fn (case hasher
                (:sha1 'ne:pbkdf2-hmac-sha1)
                (:sha256 'ne:pbkdf2-hmac-sha256)
                (t (error "bad :hasher passed (must be :sha1 :sha256)")))))
      (funcall fn
               (length pass-s)
               (static-vector-pointer pass-s)
               iterations
               (length salt-s)
               (static-vector-pointer salt-s)
               key-size
               (static-vector-pointer key-s))
      (copy-seq key-s))))

