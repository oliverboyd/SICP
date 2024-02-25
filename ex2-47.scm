(define (make-frameA origin edge1 edge2)
  (list origin edge1 edge2))

(define (originA f)
  (car f))

(define (edge1A f)
  (cadr f))

(define (edge2A f)
  (caddr f))

(define (make-frameB origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (originB f)
  (car f))

(define (edge1B f)
  (cadr f))

(define (edge2B f)
  (cddr f))
