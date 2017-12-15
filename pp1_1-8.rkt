#lang racket

(define pyramid 
  (lambda (d v)
    (let ((a (* d (/ (sqrt 2) 2))))
      (* a (+ a (sqrt (+ (* 4 (* v v)) (* a a))))))))

(define my-negative? (lambda (x) (< x 0)))

(define roots 
  (lambda (a b c)
    (let ((D (- (* b b) (* 4 (* a c)))))
      (if (>= D 0) 
        (let ((result (lambda (sign) (/ (sign (- b) (sqrt D)) (* 2 a))))) 
          (cons (result +) (result -)))
        #f))))

(define weighted-sum 
  (lambda weights 
    (lambda digits 
      (if (= (length weights) (length digits))
      (apply + (build-list (length weights) 
                           (lambda (x) 
                             (* (list-ref weights x) (list-ref digits x)))))
      #f))))

(define my-cons 
  (lambda (a b)
    (lambda (value)
      (if value a b))))

(define my-car 
  (lambda (c)
    (c #t)))

(define my-cdr
  (lambda (c)
    (c #f)))

(define switch
  (lambda (value)
    (lambda (c) 
      (value (not c)))))

(define make-palindrom
  (lambda (a)
    (append a (build-list (length a) 
                          (lambda (x)
                            (list-ref a (- (- (length a) 1) x)))))))

(define euclid
  (lambda (a b)
    (if (= b 0) 
      a 
      (let ((r (modulo a b))) (euclid b r)))))

(define rle-coding
  (lambda (a)
    (let ((x lambda (counter position last)
             (if (equal? last (list-ref a position)) 
               (x (+ counter 1) (+ position 1) last)
               (cons counter last)))
          (l (list))
          (appender (lambda ()
