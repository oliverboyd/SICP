(define (unique-triples n)
  (flatmap-n (lambda (i)
               (map (lambda (j)
                      (map (lambda (k) (list i j k))
                           (enumerate-interval 1 (- j 1))))
                    (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)
             2))

(define (s-sum-triples s)
  (filter (lambda (x) (= s (accumulate + 0 x))) (unique-triples (- s 3))))
     

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (flatmap-n proc seq n)
  (if (= n 1)
      (flatmap proc seq)
      (accumulate append () (flatmap-n proc seq (- n 1)))))
 
(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))

