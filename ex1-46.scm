(define (iterative-improve good-enough? improve)
  (lambda (x) (if (good-enough? x) x ((iterative-improve good-enough? improve) (improve x)))))

(define (sqrt x)
  (define (sqrt-iter guess x)
    (define (good-enough? guess)
      (< (abs (- (square guess) x)) 0.001))
    (define (improve guess)
      (average guess (/ x guess)))
    ((iterative-improve good-enough? improve) guess))
  (sqrt-iter 1.0 x))

(define (average x y)
  (/ (+ x y) 2))

(define (fixed-point f first-guess)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve good-enough? f) first-guess))

(define tolerance 0.00001)
