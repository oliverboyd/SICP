(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (if (filter a)
	  (combiner (term a)
		    (filtered-accumulate combiner null-value term (next a) next b filter))
	  (filtered-accumulate combiner null-value term (next a) next b filter))))

(define (sum-squares-primes a b)
  (filtered-accumulate + 0 square a iter b prime?))

(define (coprime-product n)
  (define (coprime? x)
    (if (= (gcd x n) 1)
	true
	false))
  (filtered-accumulate * 1 identity 1 iter n coprime?))

(define (prime? n)
  (fast-prime? n 10))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (if (= n 1)
      false
      (try-it (+ 1 (random (- n 1))))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
			   m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))


(define (identity x) x)
(define (iter n) (+ n 1))
(define (nofilter x) true)
