(in-package :cl-nettle-test)
(in-suite nettle-hash)

;; TODO: auth (HMAC) hash tests

(defparameter *hash-data* (make-hash-table :test 'equal))

(defun load-hash-tests ()
  "Fill in some hashes loaded from our files under data/hash/*"
  (let ((files (directory (format nil "~a/data/hash/*.*" *root*))))
    (dolist (file files)
      (let* ((name (string (pathname-name file)))
             (type (pathname-type file))
             (var (intern (string-upcase (format nil "*~a-matches*" type)) :cl-nettle-test))
             (hash (if type
                       (symbol-value var)
                       *hash-data*))
             (contents (file-contents file))
             (contents (if type
                           (bytes-from-hash (string-trim '(#\return #\newline) (babel:octets-to-string contents)))
                           contents)))
        (setf (gethash name hash) contents)))))

(defmacro hash-tester (type)
  "Defines tests for a hash type."
  (let ((match-var (intern (string-upcase (format nil "*~a-matches*" type)))))
    `(progn
       (defparameter ,match-var (make-hash-table :test 'equal))
       (test ,type
         ,(format nil "Test that ~a works correctly." type)
         (loop for k being the hash-keys of *hash-data*
               for contents being the hash-values of *hash-data*
               for hash = (gethash k ,match-var) do
           (is (equalp (,type contents) hash)))))))

(hash-tester md5)
(hash-tester sha1)
(hash-tester sha256)
(hash-tester sha512)

;; populate the file -> hash lookup tables
(load-hash-tests)

