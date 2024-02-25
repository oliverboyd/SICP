(define (estimate-integral P x1 x2 y1 y2)
  (define (test)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (monte-carlo 10000000 test)
     (- y2 y1)
     (- x2 x1)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (unit-circle-predicate x y)
  (<= (+ (square x) (square y)) 1))
