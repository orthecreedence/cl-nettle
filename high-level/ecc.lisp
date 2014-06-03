(in-package :nettle-highlevel)

;; NOTE: this is highly unfinished, and may remain that way for a while. Nettle
;; doesn't expose (or at least doesn't document) a good way to do ECC shared-key
;; crypto (only signing) so this may be a wash for now. I'd rather not muck
;; around in lower-level functions because there's so much that one can get
;; wrong. For now, use RSA with large keys for asym crypto.

(defun ecc ()
  (cffi:with-foreign-objects ((point '(:pointer (:struct ne:ecc-point)))
                              (scalar '(:pointer (:struct ne:ecc-scalar))))
    (let ((curve ne:*secp-384r1*)
          (rand-fn (cffi:foreign-symbol-pointer "nettle_yarrow256_random")))
      (ne:ecc-point-init point curve)
      (ne:ecc-scalar-init scalar curve)
      (ne:ecc-scalar-random scalar *yarrow-ctx* rand-fn))))
