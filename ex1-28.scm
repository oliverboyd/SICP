(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 3)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 2 (random (- n 3)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((miller-rabin-test n) (fast-prime? n (- times 1)))
	(else false)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (cond ((and (= (remainder (square (expmod base (/ exp 2) m))
				   m) 1)
		     (not (or (= (remainder (expmod base (/ exp 2) m)
					    m) 1)
			      (= (remainder (expmod base (/ exp 2) m)
					    m) (- m 1)))))
		0)
	       (else
		(remainder (square (expmod base (/ exp 2) m))
			   m))))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))


(define (search-for-primes a b)
  (search-for-primes-iter a b))

(define (search-for-primes-iter counter max-count)
  (timed-prime-test counter)
  (if (< counter max-count)
      (search-for-primes-iter (+ counter 1) max-count)))
