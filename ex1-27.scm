(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (fermat-test a n)
  (cond ((= a n) true)
	((= (expmod a n n) a) (fermat-test (+ 1 a) n))
	(else false)))

(define (fast-prime? n)
  (fermat-test 2 n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))


(define (search-for-primes a b)
  (search-for-primes-iter a b))

(define (search-for-primes-iter counter max-count)
  (timed-prime-test counter)
  (if (< counter max-count)
      (search-for-primes-iter (+ counter 1) max-count)))
