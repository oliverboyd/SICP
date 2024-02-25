(define (cont-frac-iter n d k)
  (define (iter j result)
    (if (= j 0)
	result
	(iter (- j 1) (/ (n j) (+ (d j) result)))))
  (iter k 0))

(define (eapprox n)
  (define (foo k)
    (if (= (remainder k 3) 2)
	(/ (* 2 (+ k 1)) 3)
	1))
  (cont-frac-iter (lambda (i) 1.0)
	     foo
	     n))
