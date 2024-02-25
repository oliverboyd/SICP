(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (deep-reverse l)
  (if (null? l)
      ()
      (append (deep-reverse (cdr l))
              (if (not (pair? (car l)))
                  (list (car l))
                  (list (deep-reverse (car l)))))))
