(define (make-accumulator initial)
  (let ((balance initial))
    (lambda (input) (begin (set! balance (+ input balance))
                           balance))))
