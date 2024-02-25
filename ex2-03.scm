(define (make-segment a b)
  (cons a b))

(define (start-segment l)
  (car l))

(define (end-segment l)
  (cdr l))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment l)
  (make-point
   (average
    (x-point (start-segment l))
    (x-point (end-segment l)))
   (average
    (y-point (start-segment l))
    (y-point (end-segment l)))))

(define (length-segment l)
  (sqrt (+ (square (- (x-point (start-segment l))
		      (x-point (end-segment l))))
	   (square (- (y-point (start-segment l))
		      (y-point (end-segment l)))))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (average a b)
  (/ (+ a b) 2))

(define (rect1 a b c) ; a,b,c three points of rectangle clockwise or anticlockwise
  (define length (make-segment a b))
  (define width (make-segment c b))
  (if (= (+ (* (- (x-point a)
		  (x-point b))
	       (- (x-point c)
		  (x-point b)))
	    (* (- (y-point a)
		  (y-point b))
	       (- (y-point c)
		  (y-point b))))
	 0)
      (cons 0 (cons a (cons b c)))
      (display "not a rectangle")))

(define (rect2 v w) ; v and w two adjacent line segments of rectangle with same start-segment
  (if (and (= (x-point (start-segment v))
	      (x-point (start-segment w)))
	   (= (y-point (start-segment v))
	      (y-point (start-segment w)))
	   (= (+ (* (- (x-point (end-segment v))
		       (x-point (start-segment v)))
		    (- (x-point (end-segment w))
		       (x-point (start-segment w))))
		 (* (- (y-point (end-segment v))
		       (y-point (start-segment v)))
		    (- (y-point (end-segment w))
		       (y-point (start-segment w)))))
	      0))
	   (cons 1 (cons v w))
	   (display "not a rectangle")))

(define (length rect)
  (cond ((= (car rect) 0)
	 (make-segment (car (cdr rect)) (car (cdr (cdr rect)))))
	((= (car rect) 1)
	 (car (cdr rect)))))

(define (width rect)
  (cond ((= (car rect) 0)
	 (make-segment (cdr (cdr (cdr rect))) (car (cdr (cdr rect)))))
	((= (car rect) 1)
	 (cdr (cdr rect)))))

(define (perim rect)
  (* 2 (+ (length-segment (length rect))
	  (length-segment (width rect)))))
  
(define (area rect)
  (* (length-segment (length rect))
     (length-segment (width rect))))
	      
