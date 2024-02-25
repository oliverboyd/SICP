(define (make-interval a b) (cons a b))

(define (upper-bound i) (max (car i) (cdr i)))

(define (lower-bound i) (min (car i) (cdr i)))

(define (sub-interval i j)
  (make-interval (- (lower-bound i) (upper-bound j))
                 (- (upper-bound i) (lower-bound j))))

(define (div-interval x y)
  (if (or (and (> (upper-bound y) 0)
               (> (lower-bound y) 0))
          (and (< (upper-bound y) 0)
               (< (lower-bound y) 0)))
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))
      (display "ERROR: div0")))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (upper-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
