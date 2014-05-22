(in-package :nettle-highlevel)

(define-condition nettle-error (error)
  ((msg :initarg :msg :reader nettle-error-msg :initform nil))
  (:report (lambda (c s) (format s "Nettle error: ~a" (nettle-error-msg c))))
  (:documentation "Describes a general Nettle library error event."))

(deftype octet () '(unsigned-byte 8))
(deftype octet-array () '(simple-array octet (*)))

;; The following define the sizes of some useful structs. we have to do this
;; manually because CFFI doesn't have a good way of nesting arrays in structs.
;;
;; See sizes.c
(defconstant +yarrow256-ctx-size+ 488
  "The actual size of a yarrow256_ctx struct in C.")
(defconstant +aes-ctx-size+ 244
  "The actual size of a aes_ctx struct in C.")
(defconstant +gcm-ctx-size+ 64
  "The actual size of a gcm_ctx struct in C.")
(defconstant +gcm-key-size+ 4096
  "The actual size of a gcm_key struct in C.")
(defconstant +gcm-aes-ctx-size+ 4408
  "The actual size of a gcm_aes_ctx struct in C.")

(defmacro with-crypto-object ((bind-var type) &body body)
  "Wrapper that makes it easier to instantiate remote objects correctly. This is
   useful when CFFI reports incorrcet sizes."
  (let ((size (case type
                (:yarrow +yarrow256-ctx-size+)
                (:aes +aes-ctx-size+)
                (:gcm +gcm-ctx-size+)
                (:gcm-key +gcm-key-size+)
                (:gcm-aes +gcm-aes-ctx-size+))))
    (unless size
      (error (format nil "with-object: Bad value given for type (~a). Should be :aes :gcm :gcm-key or :gcm-aes." type)))
    `(cffi:with-foreign-object (,bind-var :unsigned-char ,size)
       ,@body)))
                       
(defmacro with-static-vectors (bindings &body body)
  "Makes binding lisp arrays to static vectors much nicer. The binding forms
   can either be a lisp vector (must be unsigned byte array) OR a number value
   which just tells us we want a static vector of N size (without populating
   it). Works by wrapping the static-vectors:with-static-vector macro.
   
     (with-static-vectors ((key-s my-existing-key-vector)
                           (auth-s 16))
       (static-vector-pointer key-s)
       ...)"
  (let ((form body))
    (dolist (binding (reverse bindings))
      (let ((val (gensym "val"))
            (bind (car binding)))
        (setf form `((let ((,val ,(cadr binding)))
                       (static-vectors:with-static-vector (,bind
                                                            (if (numberp ,val)
                                                                ,val
                                                                (length ,val)))
                         (unless (numberp ,val)
                           (replace ,bind ,val))
                         ,@form))))))
    (car form)))

(defun int-to-bytes (int)
  "Convert an integer of arbitrary size into a byte array."
  (let ((bytes nil)
        (val int))
    (loop do
      (when (zerop val)
        (return))
      (push (logand val #xFF) bytes)
      (setf val (ash val -8)))
    (make-array (length bytes) :element-type 'octet :initial-contents bytes)))

