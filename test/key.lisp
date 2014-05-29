(in-package :cl-nettle-test)
(in-suite nettle-key)

(test pbkdf2-sha1
  "Test PBKDF2-SHA1."
  ;; the final key was generated via SJCL
  (let* ((final-json "[13,142,226,145,200,138,105,1,235,181,61,23,56,189,245,99,28,199,56,250,90,33,252,163,187,131,180,61,46,90,140,181]")
         (final (yason:parse final-json :json-arrays-as-vectors t))
         (pass (babel:string-to-octets "thisismypassword"))
         (salt (babel:string-to-octets "mr. toad")))
    (is (equalp final (pbkdf2 pass salt 1000 32 :hasher :sha1)))))

(test pbkdf2-sha256
  "Test PBKDF2-SHA256."
  ;; the final key was generated via SJCL
  (let* ((final-json "[223,4,126,44,122,144,74,208,34,188,26,140,37,196,41,195,114,2,31,73,20,188,7,240,117,218,189,229,239,174,141,246]")
         (final (yason:parse final-json :json-arrays-as-vectors t))
         (pass (babel:string-to-octets "if it quacks like a duck"))
         (salt (babel:string-to-octets "im team leader")))
    (is (equalp final (pbkdf2 pass salt 2000 32 :hasher :sha256)))))

