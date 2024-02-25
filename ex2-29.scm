(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (cadr m))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cadr b))

(define (total-weight m)
  (if (not (pair? m))
      m
      (+ (total-weight (branch-structure (left-branch m)))
         (total-weight (branch-structure (right-branch m))))))
     
(define (balanced? m)
  (define (top-balanced? m)
    (= (* (branch-length (left-branch m))
          (total-weight (branch-structure (left-branch m))))
       (* (branch-length (right-branch m))
          (total-weight (branch-structure (right-branch m))))))
  (if (not (pair? m))
      #t
      (and (top-balanced? m)
           (balanced? (branch-structure (left-branch m)))
           (balanced? (branch-structure (right-branch m))))))

; d) if 'list' were replaced by 'cons', then one would only need to replace 'cadr' by 'cdr'
