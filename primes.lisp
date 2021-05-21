;;;; primes.lisp

(in-package #:primes)

(defun insert-candidates (sieve)
  (loop with limit = (1- (array-dimension sieve 0))
     with isqrt-limit = (isqrt limit) 
     for x from 1 upto isqrt-limit
     for x2 = (expt x 2)
     do (loop for y from 1 upto isqrt-limit
	   for y2 = (expt y 2)
	   for n = (+ (* 4 x2) y2)
	   when (and (<= n limit) (member (mod n 12) '(1 5))) do
	     (setf (bit sieve n) (- 1 (bit sieve n)))
	   do (setf n (+ (* 3 x2) y2))
	   when (and (<= n limit ) (= (mod n 12) 7)) do
	     (setf (bit sieve n) (- 1 (bit sieve n)))
	   do (setf n (- (* 3 x2) y2))
	   when (and (> x y) (<= n limit) (= (mod n 12) 11)) do
	     (setf (bit sieve n) (- 1 (bit sieve n))))))

(defun sieve (sieve)
  (loop with limit = (1- (array-dimension sieve 0))
     for n from 5 upto (isqrt limit)
     for n2 = (expt n 2)
     when (= 1 (bit sieve n)) do
       (loop for i from 1
	  for k = (* i n2)
	  while (<= k limit)
	  do (setf (bit sieve k) 0))))

(defun make-sieve (n)
  (let ((sieve (make-array (1+ n) :element-type 'bit :initial-element 0)))
    (insert-candidates sieve)
    (sieve sieve)
    (setf (bit sieve 2) 1
	  (bit sieve 3) 1)
    sieve))

(defun sieve-prime-p (n sieve)
  (= 1 (bit sieve n)))

(defun collect-primes (sieve)
  (loop for i from 1 below (length sieve)
        when (eql 1 (bit sieve i)) collect i))

(defun primes-upto (n)
  "List of all prime numbers below N.
Uses the Sieve of Atkin pseudocode from
http://en.wikipedia.org/wiki/Sieve_of_Atkin."
  (collect-primes (make-sieve n)))

(defun primep (n)
  (setf n (abs n))
  (cond
    ((eql n 1) nil)
    ((< n 4) t)
    ((zerop (mod n 2)) nil)
    ((< n 9) t)
    ((zerop (mod n 3)) nil)
    (t (loop with r = (floor (sqrt n))
	     for f = 5 then (+ f 6)
	     while (<= f r)
	     if (or (zerop (mod n f))
		    (zerop (mod n (+ f 2)))) return nil
	     finally (return t)))))

(defun expt-mod (base exponent modulus)
  "Compute value of BASE ^ EXPONENT mod MODULUS."
  (loop with r = 1
     for b = base then (mod (expt b 2) modulus)
     for e = exponent then (ash e -1)
     while (> e 0)
     when (= 1 (logand e 1))
     do (setf r (mod (* r b) modulus))
     finally (return r)))

(defun decompose (x)
  (loop for c = 0 then (1+ c)
     for n = x then (ash n -1)
     while (= 0 (mod n 2))
     finally (return (values c n))))

(defun miller-rabin-prime-p (n &optional (k 40))
  (cond
    ((or (= 2 n) (= 3 n))
     t)
    ((evenp n)
     nil)
    (t
     (multiple-value-bind (s d) (decompose (1- n))
       (loop named outer
	     with n-minus-1 = (1- n)
	     repeat k
	     for a = (+ 2 (random (- n 4)))
	     for x = (expt-mod a d n)
	     unless (or (= x 1) (= x n-minus-1))
	       do (loop for r from 1 below s
			do (setf x (expt-mod x 2 n))
			if (= x 1)
			  do (return-from outer nil)
			if (= x n-minus-1)
			  return nil
			finally
			   (return-from outer nil))
	     finally (return-from outer t))))))
