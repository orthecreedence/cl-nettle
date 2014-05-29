(defpackage :cl-nettle-test
  (:use :cl :fiveam :nettle-highlevel))
(in-package :cl-nettle-test)

(defparameter *root* (asdf:system-relative-pathname :cl-nettle-test #P"test/")
  "Defines the root directory turtl is loading from (basically the ASDF path).")

(def-suite nettle-main :description "main cl-nettle test suite")
(def-suite nettle-hash :description "hash tests" :in nettle-main)
(def-suite nettle-key :description "key derivation tests" :in nettle-main)
(def-suite nettle-random :description "random tests" :in nettle-main)
(def-suite nettle-crypto :description "symetric crypto tests" :in nettle-main)
(def-suite nettle-ecc :description "ECC tests" :in nettle-main)

(defun file-contents (path)
  "Grab the *binary* contents of a file."
  (let ((contents (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t)))
    (with-open-file (s path :direction :input :element-type '(unsigned-byte 8) :if-does-not-exist :error)
      (loop for b = (read-byte s nil nil)
            while b do
        (vector-push-extend b contents)))
    contents))

(defun bytes-from-hash (str)
  "Convert ASCII hex string into actual octets."
  (assert (evenp (length str)))
  (let ((bytes (make-array (/ (length str) 2) :element-type '(unsigned-byte 8))))
    (dotimes (i (round (/ (length str) 2)))
      (setf (aref bytes i) (parse-integer str :start (* i 2) :end (+ (* i 2) 2) :radix 16)))
    bytes))

