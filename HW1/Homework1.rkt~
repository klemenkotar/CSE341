#lang racket

(define (squares-1 list)
  (cond
    [(eq? list '()) list]
    [else (cons (* (car list) (car list)) (squares (cdr list)))]))

(define (squares-2 list)
  (if (eq? list '())
    list
    (cons (* (car list) (car list)) (squares (cdr list)))))

(define (squares list)
  (if (eq? list '())
    list
    (let([head (car list)]
         [tail (cdr list)])
      (cons (* head head) (squares tail)))))