(define f
  (let ((y 1))
    (lambda (x) (if (= x 0)
                    (begin (set! y 0)
                           0)
                    y))))

(define (g x)
  (let ((y 1))
    (if (= x 0)
        (begin (set! y 0)
               0)
        y)))
