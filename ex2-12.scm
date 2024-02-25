(define (make-interval a b) (cons a b))

(define (upper-bound i) (max (car i) (cdr i)))

(define (lower-bound i) (min (car i) (cdr i)))

(define (make-centre-percent c p)
  (make-interval (- c (* (/ p 100) c)) (+ c (* (/ p 100) c))))

(define (centre i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (* 100 (/ (- (/ (upper-bound i) (centre i)) (/ (lower-bound i) (centre i))) 2)))
