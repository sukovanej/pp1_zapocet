#lang racket

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
