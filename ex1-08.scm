(define (cube-iter guess x)
(if (good-enough? guess x)
guess
(cube-iter (improve guess x)
x)))

(define (good-enough? guess x)
(< (abs (- (improve guess x) guess))
(* 0.00000001 (improve guess x))))

(define (cbrt x)
(cube-iter 1.0 x))

(define (improve guess x)
(/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
