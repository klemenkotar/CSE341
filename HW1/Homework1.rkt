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
  (if (null? list)
    list
    (let([head (car list)]
         [tail (cdr list)])
      (cons (* head head) (squares tail)))))

(define (map-squares list)
  (map (lambda (x) (* x x)) list))

(define (ascending-1 list)
  (or (null? list) (eq? (length list) 1) (and (< (car list) (cadr list)) (ascending-1 (cdr list)))))

(define (ascending list)
  (cond [(null? list) #t]
        [(eq? (length list) 1) #t]
        [(>= (car list) (cadr list)) #f]
        [else (ascending (cdr list))]))



(provide squares
         map-squares
         ascending)