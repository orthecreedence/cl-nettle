(in-package :nettle-highlevel)

(define-condition nettle-crypto-pad-error (nettle-error) ()
  (:documentation "Thrown when padding is incorrect."))

(defun calculate-padding (bytes pad-fn &key (block-size ne:+aes-block-size+))
  "Calculate the padding required for the given byte array."
  (let* ((pad-bytes (- block-size (mod (length bytes) block-size)))
         (pad-bytes (if (zerop pad-bytes)
                        block-size
                        pad-bytes))
         (padding (make-array pad-bytes :element-type 'octet :initial-element 0)))
    (concatenate 'octet-array bytes (funcall pad-fn pad-bytes padding))))

(defun pad-ansix923 (bytes &key (block-size ne:+aes-block-size+))
  "Pad Ansi x.923."
  (calculate-padding bytes
                     (lambda (pad-bytes padding)
                       (setf (aref padding (1- pad-bytes)) pad-bytes)
                       padding)
                     :block-size block-size))

(defun pad-pkcs7 (bytes &key (block-size ne:+aes-block-size+))
  "Pad using PKCS7"
  (calculate-padding bytes
                     (lambda (pad-bytes padding)
                       (dotimes (i pad-bytes)
                         (setf (aref padding i) pad-bytes))
                       padding)
                     :block-size block-size))

(defun unpad (bytes &key (block-size ne:+aes-block-size+))
  "Unpad, works for all types of padding."
  (let ((pad-bytes (aref bytes (1- (length bytes)))))
    (when (< block-size pad-bytes)
      (error 'nettle-crypto-pad-error :msg (format nil "unpad: bad pad length (~a), you probably gave a bad key" pad-bytes)))
    (subseq bytes 0 (- (length bytes) pad-bytes))))

