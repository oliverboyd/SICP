(define (count-pairs x)
  (define (count-cdr x)
    (if (not (pair? x))
        0
        (+ (count-pairs (car x))
           (count-cdr (cdr x)))))
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-cdr (cdr x))
         1)))
