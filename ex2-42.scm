(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (safe? k positions)
  (let ((new-row (list-ref positions (- k 1))))
    (define (row-safe?)
      (= (length (remove new-row positions)) (- k 1)))
    (define (diag-safe?)
      (define (diag-safe-iter p)
        (if (= p 0)
            #t
            (and (not (= (abs (- k p))
                         (abs (- new-row (list-ref positions (- p 1))))))
                 (diag-safe-iter (- p 1)))))
      (diag-safe-iter (- k 1)))
    (cond ((= k 1) #t)
          ((not (row-safe?)) #f)
          (else (diag-safe?)))))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))

(define empty-board ())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))
 
(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
