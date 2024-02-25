(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (! n)
  (product-iter identity 1 iter n))

(define (piapprox n)
  (define (wallis n)
    (/ (* 2 n (+ (* 2 n) 2)) (square (+ 1 (* 2 n)))))
  (* (product-iter wallis 1.0 iter n) 4))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))



(define (iter n) (+ n 1))
(define (identity x) x)


