(in-package :cl-nettle-test)
(in-suite nettle-crypto)

(defparameter *crypto-key*
  (coerce #(233 24 218 235 181 31 244 9 133 123 68 84 89 7 146 87 210 184 112 208 55 252 53 166 203 54 135 26 108 250 168 229) 'nec:octet-array))
(defparameter *crypto-iv*
  (coerce #(72 7 132 125 188 100 31 249 116 69 128 94 75 97 1 91) 'nec:octet-array))
(defparameter *crypto-auth*
  (coerce #(11 138 78 3) 'nec:octet-array))

(defparameter *crypto-data*
  '("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla eleifend nunc ante, in rhoncus eros condimentum eu. Sed gravida arcu dui, non tempus risus tempor et."
    (:cbc "[34,33,27,145,118,115,129,165,251,177,222,43,244,30,139,188,64,5,137,152,96,234,44,64,137,209,227,166,77,59,124,84,112,210,198,54,57,132,184,225,59,215,19,109,145,34,138,10,229,23,44,244,148,63,60,18,213,64,41,104,62,211,150,20,241,28,223,43,77,59,48,51,189,10,28,242,224,168,250,104,0,87,224,108,203,74,123,230,88,128,107,141,57,128,208,47,207,213,160,128,206,156,110,130,253,190,97,217,234,210,174,102,126,69,154,188,237,228,62,189,1,130,139,11,146,0,127,211,170,153,4,138,144,246,126,13,11,166,209,70,114,123,142,32,144,108,33,3,200,160,163,29,211,89,43,28,158,33,193,28,56,30,112,125,191,71,48,20,30,253,58,214,160,211,18,235]"
     :gcm "[97,72,47,88,66,142,138,224,146,89,254,127,69,119,149,84,111,89,123,149,39,133,227,47,75,93,117,214,85,156,239,3,124,12,53,90,213,147,87,181,74,53,101,180,166,126,220,135,218,151,12,80,45,184,62,44,202,216,99,51,239,153,15,75,235,129,171,172,39,224,71,204,166,177,190,246,174,88,120,164,176,73,100,130,1,136,85,213,25,9,239,61,119,251,105,21,241,176,247,186,37,244,39,239,190,156,98,159,108,213,106,169,123,12,7,108,33,199,167,72,158,50,149,10,236,4,154,245,8,130,70,194,36,220,235,60,252,97,119,115,85,27,154,214,236,176,108,188,133,102,62,145,100,240,80,116,125,141,203,66,135,140,99,121,33,25,121,20,206,74,17,84,153,12,201,47,94,47,211,23]")
    "ive got something in my front pocket for you"
    (:cbc "[66,17,128,203,182,177,110,21,253,222,131,20,61,116,239,211,244,90,179,115,214,136,124,187,94,53,117,158,140,22,234,32,15,53,138,29,119,227,129,253,218,220,5,145,180,218,29,239]"
     :gcm "[68,81,56,29,72,193,151,176,146,67,254,58,85,112,144,85,122,89,97,146,115,200,251,98,72,91,54,152,66,211,241,31,122,4,36,75,129,128,74,231,11,40,99,177,143,186,221,114,55,178,39,223,181,19,119,54,243,94,207,213]"))
  "Define some encrypted bytes. These were generated via SJCL.")

(test aes-cbc
  "Test that the AES-CBC routines match what SJCL spits out."
  (loop for (plaintext enc) on *crypto-data* by #'cddr do
    (let ((plaintext (babel:string-to-octets plaintext))
          (ciphertext (coerce (yason:parse (getf enc :cbc) :json-arrays-as-vectors t) 'nec:octet-array)))
      (is (equalp ciphertext
                  (encrypt-aes-cbc *crypto-key*
                                   plaintext
                                   *crypto-iv*)) "(encrypt) ciphertexts to not match")
      (is (equalp plaintext
                  (decrypt-aes-cbc *crypto-key*
                                   ciphertext
                                   *crypto-iv*)) "(decrypt) plaintexts do not match"))))

(test aes-gcm
  "Test that the AES-GCM routines match what SJCL spits out."
  (loop for (plaintext enc) on *crypto-data* by #'cddr do
    (let ((plaintext (babel:string-to-octets plaintext))
          (ciphertext (coerce (yason:parse (getf enc :gcm) :json-arrays-as-vectors t) 'nec:octet-array)))
      (is (equalp ciphertext
                  (encrypt-aes-gcm *crypto-key*
                                   plaintext
                                   *crypto-iv*
                                   *crypto-auth*)) "(encrypt) ciphertexts to not match")
      (is (equalp plaintext
                  (decrypt-aes-gcm *crypto-key*
                                   ciphertext
                                   *crypto-iv*
                                   *crypto-auth*)) "(decrypt) plaintexts to not match"))))
(test aes-cbc-fail
  "Test various failures for CBC."
  (signals (simple-error "Assert failure (key size)")
    (encrypt-aes-cbc #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7)
                     (babel:string-to-octets "encrypt me, plz")
                     #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)))
  (signals (simple-error "Assert IV failure")
    (encrypt-aes-cbc #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7)
                     (babel:string-to-octets "encrypt me, plz")
                     #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7))))

(test aes-gcm-fail
  "Test various failures for GCM."
  (signals (simple-error "Assert failure (key size)")
    (encrypt-aes-gcm #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7)
                     (babel:string-to-octets "encrypt me, plz")
                     #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
                     #(45 89))))

