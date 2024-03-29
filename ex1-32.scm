(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate-rec combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (product term a next b)
  (accumulate-iter * 1 term a next b))

(define (sum term a next b)
  (accumulate-iter + 0 term a next b))


(define (identity x) x)
(define (iter x) (+ x 1))
