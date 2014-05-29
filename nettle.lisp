(defpackage #:nettle
  (:use :cl :cffi)
  (:nicknames :ne))

(defpackage #:nettle.accessors
  (:use :cl :cffi :nettle)
  (:nicknames :ne-a))

(in-package :nettle)

(unless (cffi:foreign-symbol-pointer "nettle_aes_encrypt")
  (define-foreign-library nettle
    (:darwin (:or "nettle.dylib"))
    (:unix (:or "libnettle.so.4.7"
                "libnettle.so"))
    (:windows (:or "libnettle-4-7.dll"
                   "libnettle.dll"))
    (t (:default "libnettle")))
  (unless (foreign-library-loaded-p 'nettle)
    (use-foreign-library nettle)))

(unless (cffi:foreign-symbol-pointer "nettle_rsa_encrypt")
  (define-foreign-library hogweed
    (:darwin (:or "hogweed.dylib"))
    (:unix (:or "libhogweek.so.2.5"
                "libhogweed.so"))
    (:windows (:or "libhogweed-2-5.dll"
                   "libhogweed.dll"))
    (t (:default "libhogweed")))
  (unless (foreign-library-loaded-p 'hogweed)
    (use-foreign-library hogweed)))

