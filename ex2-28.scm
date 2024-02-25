(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (fringe l)
  (if (null? l)
      ()
      (append (if (not (pair? (car l)))
                  (list (car l))
                  (fringe (car l)))
              (fringe (cdr l)))))
