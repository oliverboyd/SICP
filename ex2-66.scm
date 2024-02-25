(define (look-up given-key tree)
  (cond ((null? tree) false)
        ((equal? given-key (entry tree)) (entry tree))
        ((< given-key (entry tree)) (look-up given-key (left-branch tree)))
        (else (look-up given-key (right-branch tree)))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
