(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

(define (make-segment v w)
  (list v w))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cadr s))
