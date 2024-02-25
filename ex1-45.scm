(define (nroot x n)
  (fixed-point (k-average-damp (lambda (y) (/ x (expt y (- n 1))))
			       (floor (/ (log n) (log 2))))
	       1.0))

(define (k-average-damp f k)
  ((repeated average-damp k) f))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average a b)
  (/ (+ a b) 2))


(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define tolerance 0.00001)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))
