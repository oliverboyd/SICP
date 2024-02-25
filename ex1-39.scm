(define (cont-frac-iter n d k)
  (define (iter j result)
    (if (= j 0)
	result
	(iter (- j 1) (/ (n j) (+ (d j) result)))))
  (iter k 0))

(define (tan-cf x k)
  (define (foo n)
    (if (= n 1) x (- (square x))))
  (cont-frac-iter foo (lambda (i) (- (* 2 i) 1)) k))
