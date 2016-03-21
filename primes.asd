;;;; primes.asd

(asdf:defsystem #:primes
  :serial t
  :description "Prime number functions"
  :author "Martin Dransfield <mdransfield@gmail.com>"
  :license "GNU GPL 3 (see file LICENSE for details"
  :components ((:file "package")
               (:file "primes")))

