(defpackage #:nettle
  (:use :cl :cffi)
  (:nicknames :ne))

(defpackage #:nettle.accessors
  (:use :cl :cffi :nettle)
  (:nicknames :ne-a))

(in-package :nettle)

(eval-when (:load-toplevel)
  (define-foreign-library nettle
    (:darwin (:or "nettle.dylib"))
    (:unix (:or "libnettle.so"))
    (:windows (:or "libnettle-4-6.dll"
                   "libnettle.dll"))
    (t (:default "libnettle")))
  (unless (foreign-library-loaded-p 'nettle)
    (use-foreign-library nettle))

  (define-foreign-library hogweed
    (:darwin (:or "hogweed.dylib"))
    (:unix (:or "libhogweed.so"))
    (:windows (:or "libhogweed-2-4.dll"
                   "libhogweed.dll"))
    (t (:default "libhogweed")))
  (unless (foreign-library-loaded-p 'hogweed)
    (use-foreign-library hogweed)))

