(define (sum-of-squares x y) (+ (* x x) (* y y)))

(define (f x y z) (cond ((and (>= x y) (>= z y)) (sum-of-squares x z)) ((and (>= y x) (>= z x)) (sum-of-squares y z)) ((and (>= x z) (>= y z)) (sum-of-squares x y))))
