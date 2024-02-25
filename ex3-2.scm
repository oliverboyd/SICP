(define (make-monitored f)
  (let ((counter 0))
    (lambda (x) (cond ((equal? x 'how-many-calls?) counter)
                      ((equal? x 'reset-count) (set! counter 0))
                      (else (begin (set! counter (+ counter 1))
                                   (f x)))))))
