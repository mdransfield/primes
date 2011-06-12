;;;; package.lisp

(defpackage #:primes
  (:nicknames #:p)
  (:use #:cl)
  (:export #:primes-upto
	   #:primep
	   #:miller-rabin-prime-p))

