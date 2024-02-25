(define (f n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	((= n 2) 2)
	(else (+ (f (- n 1))
		 (* 2 (f (- n 2)))
		 (* 3 (f (- n 3)))))))

(define (g n)
  (define (g-iter a b c count)
    (if (= count 0)
	c
	(g-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (g-iter 2 1 0 n))
