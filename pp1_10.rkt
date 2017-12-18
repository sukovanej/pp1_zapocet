#lang racket

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

(define input `((0 100 80) (255 0 255) (0 100 255) (0 0 0)))
(histogram input)
