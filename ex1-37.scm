(define (cont-frac n d k)
  (define (iter j)
    (if (= k j)
	(/ (n k) (d k))
	(/ (n j)
	   (+ (d j)
	      (iter (+ j 1))))))
  (iter 1))

(define (cont-frac-iter n d k)
  (define (iter j result)
    (if (= j 0)
	result
	(iter (- j 1) (/ (n j) (+ (d j) result)))))
  (iter k 0))


(define (gr n)
  (cont-frac (lambda (i) 1.0)
	     (lambda (i) 1.0)
	     n))

(define (gr-iter n)
  (cont-frac-iter (lambda (i) 1.0)
	     (lambda (i) 1.0)
	     n))

;; 12 iterations are required for an approximation of 1/phi accurate to 4 decimal places
