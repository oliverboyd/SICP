(define (make-segment a b)
  (cons a b))

(define (start-segment l)
  (car l))

(define (end-segment l)
  (cdr l))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment l)
  (make-point
   (average
    (x-point (start-segment l))
    (x-point (end-segment l)))
   (average
    (y-point (start-segment l))
    (y-point (end-segment l)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (average a b)
  (/ (+ a b) 2))
