(define (make-rat n d)
  (let ((g (abs (gcd n d))))
    (cond ((and (> n 0) (< d 0)) (cons (- (/ n g)) (- (/ d g))))
	  ((and (< n 0) (< d 0)) (cons (- (/ n g)) (- (/ d g))))
	  (else (cons (/ n g) (/ d g))))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
