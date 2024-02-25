(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (*  a b)
  (*-iter a b 0))

(define (*-iter a counter sum)
  (cond ((= counter 0) sum)
	((even? counter) (*-iter (double a) (halve counter) sum))
	(else (*-iter a (- counter 1) (+ sum a)))))
