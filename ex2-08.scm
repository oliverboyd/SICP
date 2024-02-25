(define (make-interval a b) (cons a b))

(define (upper-bound i) (max (car i) (cdr i)))

(define (lower-bound i) (min (car i) (cdr i)))

(define (sub-interval i j)
  (make-interval (- (lower-bound i) (upper-bound j))
                 (- (upper-bound i) (lower-bound j))))

