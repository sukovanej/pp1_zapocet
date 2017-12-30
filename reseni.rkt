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

; rle-coding

(define get-element 
  (lambda (input index count) 
    (if (and (< (+ index 1) (length input)) (eq? (list-ref input index) (list-ref input (+ index 1))))
      (get-element input (+ index 1) (+ count 1)) 
      (list (+ index 1) (list-ref input index) count))))

(define all-elements 
  (lambda (input index result)
    (let* ((element (get-element input index 1)) (new-index (list-ref element 0)))
      (if (> (+ new-index 1) (length input))
        (append result (list (list-ref element 1) (list-ref element 2)))
        (all-elements input new-index (append result (list (list-ref element 1) (list-ref element 2))))))))

(define rle-coding
  (lambda (input)
    (all-elements input 0 (list))))

; histogram

(define single-list
  (lambda (input)
    (apply append input)))

(define count-in-list
  (lambda (input value)
    (let* ((filtered (filter (lambda (x) (= x value)) input))
           (filtered-length (length filtered)))
      (if (= filtered-length 0)
          #f
          (cons value filtered-length)))))

(define histogram
  (lambda (input)
    (let ((list-input (single-list input)))
      (filter
       (lambda (x) (pair? x))
       (build-list 256
                   (lambda (x) (count-in-list list-input x)))))))
