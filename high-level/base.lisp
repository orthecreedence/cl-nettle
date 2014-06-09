(in-package :nettle-highlevel)

(define-condition nettle-error (error)
  ((msg :initarg :msg :reader nettle-error-msg :initform nil))
  (:report (lambda (c s) (format s "Nettle error: ~a" (nettle-error-msg c))))
  (:documentation "Describes a general Nettle library error event."))

(deftype octet () '(unsigned-byte 8))
(deftype octet-array () '(simple-array octet (*)))
(deftype octet-array-32 () '(simple-array octet (32)))
(deftype octet-array-16 () '(simple-array octet (16)))

;; The following define the sizes of some useful structs. we have to do this
;; manually because CFFI doesn't have a good way of nesting arrays in structs.
;; Sizes are calculated on a x64 machine because doing them on x32 will cause
;; segfaults in x64 (sizes are too small for some structs that use unsigned/int
;; types instead of the uint*_t types).
;;
;; See sizes.c
;;
(defconstant +yarrow256-ctx-size+ 496
  "The actual size of a yarrow256_ctx struct in C.")
(defconstant +aes-ctx-size+ 244
  "The actual size of a aes_ctx struct in C.")
(defconstant +gcm-ctx-size+ 64
  "The actual size of a gcm_ctx struct in C.")
(defconstant +gcm-key-size+ 4096
  "The actual size of a gcm_key struct in C.")
(defconstant +gcm-aes-ctx-size+ 4408
  "The actual size of a gcm_aes_ctx struct in C.")
(defconstant +md5-ctx-size+ 92
  "The actual size of a md5_ctx struct in C.")
(defconstant +sha1-ctx-size+ 96
  "The actual size of a sha1_ctx struct in C.")
(defconstant +sha256-ctx-size+ 108
  "The actual size of a sha256_ctx struct in C.")
(defconstant +sha512-ctx-size+ 216
  "The actual size of a sha512_ctx struct in C.")
(defconstant +hmac-md5-ctx-size+ 276
  "The actual size of a hmac_md5_ctx struct in C.")
(defconstant +hmac-sha1-ctx-size+ 288
  "The actual size of a hmac_sha1_ctx struct in C.")
(defconstant +hmac-sha256-ctx-size+ 324
  "The actual size of a hmac_sha256_ctx struct in C.")
(defconstant +hmac-sha512-ctx-size+ 648
  "The actual size of a hmac_sha512_ctx struct in C.")
(defconstant +rsa-public-key-size+ 40
  "The actual size of a hmac_sha512_ctx struct in C.")
(defconstant +rsa-private-key-size+ 104
  "The actual size of a hmac_sha512_ctx struct in C.")

(defmacro with-crypto-object ((bind-var type) &body body)
  "Wrapper that makes it easier to instantiate remote objects correctly. This is
   useful when CFFI reports incorrcet sizes."
  `(let ((size (case ,type
                 (:yarrow +yarrow256-ctx-size+)
                 (:aes +aes-ctx-size+)
                 (:gcm +gcm-ctx-size+)
                 (:gcm-key +gcm-key-size+)
                 (:gcm-aes +gcm-aes-ctx-size+)
                 (:md5 +md5-ctx-size+)
                 (:sha1 +sha1-ctx-size+)
                 (:sha256 +sha256-ctx-size+)
                 (:sha512 +sha512-ctx-size+)
                 (:hmac-md5 +hmac-md5-ctx-size+)
                 (:hmac-sha1 +hmac-sha1-ctx-size+)
                 (:hmac-sha256 +hmac-sha256-ctx-size+)
                 (:hmac-sha512 +hmac-sha512-ctx-size+)
                 (:rsa-public-key +rsa-public-key-size+)
                 (:rsa-private-key +rsa-private-key-size+)
                 (t (error "bad type passed to with-crypto-object")))))
     (cffi:with-foreign-object (,bind-var :unsigned-char size)
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
            (startval (gensym "start"))
            (endval (gensym "end")))
        (destructuring-bind (bind bindval &key (start 0) end)
            binding
          (setf form `((let ((,val ,bindval)
                             (,startval ,start)
                             (,endval ,end))
                         (static-vectors:with-static-vector (,bind
                                                              (if (numberp ,val)
                                                                  ,val
                                                                  (cond ((and (numberp ,startval)
                                                                              (numberp ,endval))
                                                                         (- ,endval ,startval))
                                                                        ((numberp ,endval)
                                                                         ,endval)
                                                                        ((numberp ,startval)
                                                                         (- (length ,val) ,startval))
                                                                        (t (length ,val)))))
                           (unless (numberp ,val)
                             (replace ,bind ,val :start2 ,start :end2 ,end))
                           ,@form)))))))
    (car form)))

(defun* (secure-equal -> boolean) ((vec1 octet-array) (vec2 octet-array))
  "Securely compare two vectors. Returns t if they match, nil otherwise, and
   resists timing attacks using XOR for comparison."
  (when (= (length vec1) (length vec2))
    (let ((res (the fixnum 0)))
      (dotimes (i (the fixnum (length vec1)))
        (setf res (the fixnum (logior res (logxor (aref vec1 i) (aref vec2 i))))))
      (zerop res))))

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

(defun get-current-pid (&key if-not-exists-return)
  "Get the current process' PID. This function does it's best to be cross-
   implementation. If it isn't able to grab the PID from the system, it defaults
   to returning whatever value is passed into the :if-not-exists-return key."
  #+clisp
  (system::process-id)
  #+(and lispworks unix)
  (system::getpid)
  #+(and sbcl unix)
  (sb-unix:unix-getpid)
  #+(and cmu unix)
  (unix:unix-getpid)
  #+openmcl
  (ccl::getpid)
  #+ecl
  (ext:getpid)
  #-(or clisp (and lispworks unix) (and sbcl unix) (and cmu unix) (and openmcl unix) openmcl ecl)
  if-not-exists-return)

(defun get-env (name &optional (default ""))
  "Get the value of an ENV var. Tries to be cross-platform."
  #+clisp
  (ext:getenv name)
  #+lispworks
  (lispworks:environment-variable name)
  #+(and sbcl unix)
  (sb-ext:posix-getenv name)
  #+ccl
  (ccl:getenv name)
  #+ecl
  (si:getenv name)
  #-(or clisp lispworks (and sbcl unix) ecl ccl)
  default)

