(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (add-2h x) (+ x (* 2 h)))
  (* (+ (f a)
	(f b)
	(* 2 (sum f (+ a (* 2 h)) add-2h (+ a (* (- n 2) h))))
	(* 4 (sum f (+ a h) add-2h (+ a (* (- n 1) h)))))
     (/ h 3)))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))
