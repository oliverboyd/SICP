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
    (cond ((and (>= (lower-bound x) 0)
                (>= (upper-bound x) 0)
                (>= (lower-bound y) 0)
                (>= (upper-bound y) 0))
           (make-interval (* (lower-bound x) (lower-bound y))
                          (* (upper-bound x) (upper-bound y))))
          ((and (< (lower-bound x) 0)
                (< (upper-bound x) 0)
                (< (lower-bound y) 0)
                (< (upper-bound y) 0))
           (make-interval (* (upper-bound x) (upper-bound y))
                          (* (lower-bound x) (upper-bound y))))
          ((and (>= (lower-bound x) 0)
                (>= (upper-bound x) 0)
                (< (lower-bound y) 0)
                (< (upper-bound y) 0))
           (make-interval (* (upper-bound x) (lower-bound y))
                          (* (lower-bound x) (upper-bound y))))
          ((and (< (lower-bound x) 0)
                (< (upper-bound x) 0)
                (>= (lower-bound y) 0)
                (>= (upper-bound y) 0))
           (make-interval (* (lower-bound x) (upper-bound y))
                          (* (upper-bound x) (lower-bound y))))
          ((and (< (lower-bound x) 0)
                (>= (upper-bound x) 0)
                (>= (lower-bound y) 0)
                (>= (upper-bound y) 0))
           (make-interval (* (lower-bound x) (upper-bound y))
                          (* (upper-bound x) (upper-bound y))))
          ((and (< (lower-bound x) 0)
                (>= (upper-bound x) 0)
                (< (lower-bound y) 0)
                (< (upper-bound y) 0))
           (make-interval (* (upper-bound x) (lower-bound y))
                          (* (lower-bound x) (lower-bound y))))
          ((and (>= (lower-bound x) 0)
                (>= (upper-bound x) 0)
                (< (lower-bound y) 0)
                (>= (upper-bound y) 0))
           (make-interval (* (upper-bound x) (lower-bound y))
                          (* (upper-bound x) (upper-bound y))))
          ((and (< (lower-bound x) 0)
                (< (upper-bound x) 0)
                (< (lower-bound y) 0)
                (>= (upper-bound y) 0))
           (make-interval (* (lower-bound x) (upper-bound y))
                          (* (lower-bound x) (lower-bound y))))
          ((and (< (lower-bound x) 0)
                (>= (upper-bound x) 0)
                (< (lower-bound y) 0)
                (>= (upper-bound y) 0))
           (make-interval (min (* (upper-bound x) (lower-bound y))
                               (* (lower-bound x) (upper-bound y)))
                          (max (* (lower-bound x) (lower-bound y))
                               (* (upper-bound x) (upper-bound y)))))))
                          
          
          
          
            
          
                           
