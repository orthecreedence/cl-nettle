(in-package :nettle-highlevel)

(define-condition nettle-random-error (nettle-error) ()
  (:documentation "General PRNG error."))

(define-condition nettle-random-already-open (nettle-random-error) ()
  (:documentation "Thrown when initing the already-open random context."))

(define-condition nettle-random-closed (nettle-random-error) ()
  (:documentation "Thrown when operating on a closed random context."))

(defvar *yarrow-ctx* nil
  "Holds the foreign random context.")

#+(and windows (not cygwin))
(progn
  ;; NOTE: as hard as i tried, couldn't get CryptGenRandom working (kept handing
  ;; back a 0x80090017 error). so we go over its head, even though it's not
  ;; technically the "correct" thing to do
  ;; 
  ;; see http://msdn.microsoft.com/en-us/library/windows/desktop/aa387694%28v=vs.85%29.aspx
  (unless (cffi:foreign-symbol-pointer "SystemFunction036")
    (cffi:define-foreign-library advapi32 (:windows "advapi32"))
    (cffi:use-foreign-library advapi32))

  (cffi:defcfun ("GetLastError" %get-last-error) :unsigned-int)
  
  (cffi:defcfun ("SystemFunction036" %rtl-gen-random) :int
    (buffer :pointer)
    (buffer-size :unsigned-long)))

(defun random-init-windows ()
  (with-static-vectors ((bytes 32))
    (%rtl-gen-random (static-vector-pointer bytes) 32)
    (copy-seq bytes)))

(defun random-init-nix ()
  (let ((bytes (make-array 32 :element-type 'octet)))
    (with-open-file (rand "/dev/urandom" :direction :input :element-type 'octet)
      (dotimes (i 32)
        (set (aref bytes i) (read-byte rand))))
    bytes))

(defun random-init ()
  (when *yarrow-ctx*
    (error 'nettle-random-already-open :msg "Random context already open"))
  (let ((random-bytes (progn
                        #+(and windows (not cygwin)) (random-init-windows)
                        #-(and windows (not cygwin)) (random-init-posix))))
    (with-static-vectors ((seed 32))
      (let ((ctx (cffi:foreign-alloc :char :count +yarrow256-ctx-size+)))
        (replace seed random-bytes)
        (ne:yarrow256-init ctx 0 (cffi:null-pointer))
        (ne:yarrow256-seed ctx ne:+yarrow256-seed-file-size+ (static-vector-pointer seed))
        (setf *yarrow-ctx* ctx)))
    t))

(defun random-check-open ()
  "Make sure our random context is open."
  (unless (and *yarrow-ctx*
               (cffi:pointerp *yarrow-ctx*))
    (error 'nettle-random-closed :msg "Operating on a closed random context")))

(defun random-close ()
  "Close the random context."
  (when *yarrow-ctx*
    (cffi:foreign-free *yarrow-ctx*)
    (setf *yarrow-ctx* nil)))

(defun random-bytes (num)
  "Get N random bytes."
  (random-check-open)
  (with-static-vectors ((bytes num))
    (ne:yarrow256-random *yarrow-ctx* num (static-vector-pointer bytes))
    (copy-seq bytes)))

(defun random-byte ()
  "Get a random byte (0 - 255)."
  (aref (random-bytes 1) 0))

(defun random-float ()
  "Get a random floating point number."
  (let ((bytes (random-bytes 4))
        (uint32 0))
    (dotimes (i 4)
      (setf uint32 (ash uint32 8))
      (incf uint32 (aref bytes i)))
    (coerce (/ uint32 #xffffffff) 'float)))

