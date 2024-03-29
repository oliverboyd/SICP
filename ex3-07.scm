(define (make-joint account password1 password2)
  (define (dispatch password m)
    (if (eq? password password2)
        (account password1 m)
        incorrect))
  (define (incorrect amount)
    "Incorrect password")
  (define (incorrect1 . input)
    (lambda (x) ("Access denied")))
  (if (number? ((account password1 'withdraw) 0))
      dispatch
      incorrect1))

(define (make-account balance secret-password)
  (let ((limit 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (incorrect amount)
      (set! limit (+ limit 1))
      (if (> limit 7)
          (call-the-police "Account locked")
          "Incorrect password"))
    (define (call-the-police x)
      (set! secret-password 'thou-shalt-not-guess-me)
      x)
    (define (dispatch password m)
      (cond ((not (eq? password secret-password)) incorrect)
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown requests -- MAKE ACCOUNT"
                         m))))
    dispatch))
