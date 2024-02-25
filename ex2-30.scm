(define (square-tree1 tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree1 (car tree))
                    (square-tree1 (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree2 subtree)
             (square subtree)))
       tree))
       
       
         
