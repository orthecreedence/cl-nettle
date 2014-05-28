(in-package :cl-nettle-test)
(in-suite nettle-random)

;;; NOTE: we don't actually test for randomness here, just that the functions
;;; return the types of data we expect with the lengths we expect.

;; just in case tests fail, this gives us a clean slate on reload
(random-close)

(test random-init
  "Tests that the random system inits properly."
  (is (eq (random-init) t))
  (signals (nettle-random-already-open "Double-init didn't trigger error")
    (random-init)))

(test (random-bytes :depends-on random-init)
  "Can we grab random bytes?"
  (let ((bytes (random-bytes 16)))
    (is (= (length bytes) 16))
    (is (<= 0 (reduce '+ bytes :initial-value 0) (* 255 16)))))

(test (random-float :depends-on random-bytes)
  "Let's generate a float random."
  (let ((float (random-float)))
    (is (<= 0 float 1))
    (is (typep float 'float))))

(test (random-close :depends-on random-float)
  "Tests that the random system closes properly."
  (is (eq (random-close) t))
  (is (eq (random-close) nil)))

