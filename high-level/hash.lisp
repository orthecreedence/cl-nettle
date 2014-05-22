(in-package :nettle-highlevel)

(defun run-hasher (hash-type input)
  "Run a hasher of the given type. Since all hashers follow the same basic
   patterns, all we have to do is import some symbols and we're pretty much
   done."
  (flet ((import-sym (suffix)
           (intern (format nil "~a-~a" hash-type (string suffix)) :nettle)))
    (let ((ctx-struct (import-sym 'ctx))
          (init-fn (import-sym 'init))
          (digest-size (symbol-value (intern (format nil "+~a-DIGEST-SIZE+" hash-type) :nettle)))
          (update-fn (import-sym 'update))
          (digest-fn (import-sym 'digest)))
      (cffi:with-foreign-object (ctx `(:pointer (:struct ,ctx-struct)))
        (with-static-vectors ((input-s input)
                              (digest-s digest-size))
          (funcall init-fn ctx)
          (funcall update-fn ctx (length input) (static-vector-pointer input-s))
          (funcall digest-fn ctx digest-size (static-vector-pointer digest-s))
          (copy-seq digest-s))))))

(defmacro make-hasher (hash-type)
  "Defines a hasher function for us. Simple wrapper around run-hasher."
  `(defun ,hash-type (input)
     ,(format nil "~a the input vector (octet vectors only, please)." hash-type)
     (run-hasher ',hash-type input)))
    
(make-hasher sha1)
(make-hasher sha256)
(make-hasher sha512)
(make-hasher md5)

