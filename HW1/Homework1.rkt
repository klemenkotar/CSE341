#lang racket

(define (squares-1 list)
  (cond
    [(eq? list '()) list]
    [else (cons (* (car list) (car list)) (squares (cdr list)))]))

(define (squares-2 list)
  (if (eq? list '())
    list
    (cons (* (car list) (car list)) (squares (cdr list)))))

;;; function that takes a list of numbers and returns a new list of the squares of those numbers
(define (squares list)
  (if (null? list)
    list
    (let([head (car list)]
         [tail (cdr list)])
      (cons (* head head) (squares tail)))))

;;; function that takes a list of numbers and returns a new list of the squares of those numbers
;;; using map and an anonymous function
(define (map-squares list)
  (map (lambda (x) (* x x)) list))

;;; non-tail-recursive function that tests whether a list of integers is in strict ascending order
(define (ascending-1 list)
  (or (null? list) (eq? (length list) 1) (and (< (car list) (cadr list)) (ascending-1 (cdr list)))))

;;; tail-recursive function that tests whether a list of integers is in strict ascending order
(define (ascending list)
  (cond [(null? list) #t]
        [(eq? (length list) 1) #t]
        [(>= (car list) (cadr list)) #f]
        [else (ascending (cdr list))]))


(define (let*->let expression)
  (let ([variables (cadr expression)]
        [tail (cddr expression)])
    (if (null? variables)
        tail
        (cons (list 'let (car variables)) (let*->let (list (car expression) (cdr variables) tail))))))

(provide squares
         map-squares
         ascending)