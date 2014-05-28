(in-package :nettle-highlevel)

(defun run-hasher (hash-type input)
  "Run a hasher of the given type. Since all hashers follow the same basic
   patterns, all we have to do is import some symbols and we're pretty much
   done."
  (flet ((import-sym (suffix)
           (intern (format nil "~a-~a" hash-type (string suffix)) :nettle)))
    (let ((ctx-struct (intern (string hash-type) :keyword))
          (digest-size (symbol-value (intern (format nil "+~a-DIGEST-SIZE+" hash-type) :nettle)))
          (init-fn (import-sym 'init))
          (update-fn (import-sym 'update))
          (digest-fn (import-sym 'digest)))
      (with-crypto-object (ctx ctx-struct)
        (with-static-vectors ((input-s input)
                              (digest-s digest-size))
          (funcall init-fn ctx)
          (funcall update-fn ctx (length input) (static-vector-pointer input-s))
          (funcall digest-fn ctx digest-size (static-vector-pointer digest-s))
          (copy-seq digest-s))))))

(defun run-auth-hasher (hash-type key input)
  "Run an HMAC hasher against the given key."
  (flet ((import-sym (suffix)
           (intern (format nil "HMAC-~a-~a" hash-type (string suffix)) :nettle)))
    (let ((ctx-struct (intern (format nil "HMAC-~a" hash-type) :keyword))
          (digest-size (symbol-value (intern (format nil "+~a-DIGEST-SIZE+" hash-type) :nettle)))
          (init-fn (import-sym 'set-key))
          (update-fn (import-sym 'update))
          (digest-fn (import-sym 'digest)))
      (with-crypto-object (ctx ctx-struct)
        (with-static-vectors ((input-s input)
                              (key-s key)
                              (digest-s digest-size))
          (funcall init-fn ctx (length key-s) (static-vector-pointer key-s))
          (funcall update-fn ctx (length input) (static-vector-pointer input))
          (funcall digest-fn ctx digest-size (static-vector-pointer digest-s))
          (copy-seq digest-s))))))

(defmacro make-hasher (hash-type)
  "Defines a hasher function for us. Simple wrapper around run-hasher."
  `(defun ,hash-type (input)
     ,(format nil "~a the input vector (octet vectors only, please)." hash-type)
     (run-hasher ',hash-type input)))
    
(defmacro make-auth-hasher (hash-type)
  "Defines an HMAC hasher function for us. Simple wrapper around run-auth-hasher."
  `(defun ,(intern (string-upcase (format nil "hmac-~a" hash-type))) (key input)
     ,(format nil "HMAC-~a the input vector (octet vectors only, please) via the given key." hash-type)
     (run-auth-hasher ',hash-type key input)))
    
(make-hasher md5)
(make-hasher sha1)
(make-hasher sha256)
(make-hasher sha512)

(make-auth-hasher md5)
(make-auth-hasher sha1)
(make-auth-hasher sha256)
(make-auth-hasher sha512)

