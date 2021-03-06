(in-package :nettle-highlevel)

(define-condition nettle-random-error (nettle-error) ()
  (:documentation "General PRNG error."))

(define-condition nettle-random-init-fail (nettle-random-error) ()
  (:documentation "Thrown when random context init fails."))

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

  (cffi:defcfun ("CryptAcquireContextW" %crypt-acquire-context) :int
                (hprovider :pointer)
                (container :pointer)
                (provider :pointer)
                (type :unsigned-int)
                (flags :unsigned-int))

  (cffi:defcfun ("CryptGenRandom" %crypt-gen-random) :int
                (hprovider :int)
                (length :unsigned-int)
                (buffer :pointer))
  
  (cffi:defcfun ("CryptReleaseContext" %crypt-release-context) :int
                (hprovider :int)
                (flags :unsigned-int))

  (cffi:defcfun ("GetLastError" %get-last-error) :unsigned-int)

  (cffi:defcfun ("SystemFunction036" %rtl-gen-random) :int
    (buffer :pointer)
    (buffer-size :unsigned-long)))

;; unused, this is the low-level way to generate randoms in win.
(defun random-init-bytes-windows-low-level (num-bytes)
  (with-static-vectors ((bytes num-bytes))
    (when (zerop (%rtl-gen-random (static-vector-pointer bytes) num-bytes))
      (error 'nettle-random-init-fail :msg (format nil "Error inializing random context: 0x~x" (%get-last-error))))
    (copy-seq bytes)))

(defun random-init-bytes-windows (num-bytes)
  "Grab N bytes of random data from the windows API."
  ;; we need some ECL/windows-specific stuff here because of annoying segfaults
  ;; in ECL when using CFFI (for some reason the call to CryptAcquireContextW
  ;; fails).
  #+ecl
  (with-static-vectors ((bytes num-bytes))
    (ffi:clines "
      #include <windows.h>
      #include <wincrypt.h>
      #include <stdio.h>
      int windows_get_random_bytes_nettle(unsigned size, unsigned char *bytes)
      {
          HCRYPTPROV hprov = 0;
          if(!CryptAcquireContextW(&hprov, 0, 0, 1, 4026531904)) return 0;
          if(!CryptGenRandom(hprov, size, bytes)) return 0;
          CryptReleaseContext(hprov, 0);
          return 1;
      }
    ")
    (let ((res (ffi:c-inline (num-bytes (static-vector-pointer bytes))
                             (:unsigned :pointer)
                             :int
                 "windows_get_random_bytes_nettle(fix(#0), ecl_to_pointer(#1))"
                 :side-effects nil
                 :one-liner t)))
      (when (zerop res)
        (error 'nettle-random-init-fail :msg (format nil "Error inializing random context: 0x~x" (%get-last-error)))))
    (copy-seq bytes))
  #-ecl
  (cffi:with-foreign-object (hprov :unsigned-int)
    (let ((res (%crypt-acquire-context hprov
                                       (cffi:null-pointer)
                                       (cffi:null-pointer)
                                       1
                                       4026531904)))
      (when (zerop res)
        (error 'nettle-random-init-fail :msg (format nil "Error inializing random context: 0x~x" (%get-last-error))))
      (with-static-vectors ((bytes (* 3 num-bytes)))
        (let* ((ctx (cffi:mem-aref hprov :unsigned-int))
               (res (%crypt-gen-random ctx num-bytes (static-vector-pointer bytes))))
          (when (zerop res)
            (error 'nettle-random-init-fail :msg (format nil "Error inializing random context: 0x~x" (%get-last-error))))
          (%crypt-release-context ctx 0)
          (copy-seq bytes))))))

(defun random-init-bytes-nix (num-bytes)
  "Grab N bytes of random data from a sane API."
  (let ((bytes (make-array num-bytes :element-type 'octet)))
    (with-open-file (rand "/dev/urandom" :direction :input :element-type 'octet)
      (dotimes (i num-bytes)
        (setf (aref bytes i) (read-byte rand))))
    bytes))

(defun random-init ()
  "Initialize the random context. This is done by taking random bytes from the
   underlying OS, appending some other process-specific information (the current
   PID, the machine name, HOME and PATH env vars, and SHA256 hashing the entire
   byte array. This gives us a 32 byte array we use as the initial Yarrow seed.
   The idea is that we don't necessarily want to trust the OS to give us good
   random data, so we supplement it with other various bits of information we
   have access to. Luckily because of the way hashing works, we can only ever
   add entropy (not weaken it) by piling in the data.
   
   Once initialized, the random context stays open until random-close is called
   (or until a comet hits earth, obliterating everything in its path).

   TODO: add in random-state-preserving comet countermeasures"
  (when *yarrow-ctx*
    (error 'nettle-random-already-open :msg "Random context already open"))
  (let* ((num-bytes 512)
         (seed-bytes (concatenate
                       'octet-array
                       (progn
                         #+(and windows (not cygwin)) (random-init-bytes-windows num-bytes)
                         #-(and windows (not cygwin)) (random-init-bytes-nix num-bytes))
                       (int-to-bytes (get-current-pid))
                       (int-to-bytes (get-internal-real-time))
                       (int-to-bytes (get-internal-run-time))
                       (babel:string-to-octets (machine-instance))
                       (babel:string-to-octets (get-env "HOME" ""))
                       (babel:string-to-octets (get-env "PATH" ""))))
         (seed (sha256 seed-bytes)))
    (with-static-vectors ((seed-s seed))
      (let ((ctx (cffi:foreign-alloc :unsigned-char :count +yarrow256-ctx-size+)))
        (setf *yarrow-ctx* ctx)
        (ne:yarrow256-init *yarrow-ctx* 0 (cffi:null-pointer))
        (ne:yarrow256-seed *yarrow-ctx* ne:+yarrow256-seed-file-size+ (static-vector-pointer seed-s))))
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
    (setf *yarrow-ctx* nil)
    t))

(defun* (random-bytes -> octet-array) ((num fixnum))
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

