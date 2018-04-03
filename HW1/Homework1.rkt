#lang racket

;;; function that takes a list of numbers and returns a new list of the squares of those numbers
(define (squares numbers)
  (if (null? numbers)
    null
    (let([head (car numbers)])
      (cons (* head head) (squares (cdr numbers))))))

;;; function that takes a list of numbers and returns a new list of the squares of those numbers
;;; using map and an anonymous function
(define (map-squares numbers)
  (map (lambda (x) (* x x)) numbers))

;;; non-tail-recursive function that tests whether a list of integers is in strictly ascending order
(define (ascending numbers)
  (or (null? numbers) (eq? (length numbers) 1) (and (< (car numbers) (cadr numbers)) (ascending (cdr numbers)))))

;;; function that takes a list represneting a let* expression and returns a new list represneting the
;;; equvelent expression using let
(define (let*->let expression)
  (let*->let-aux (cadr expression) (cddr expression)))

;;; auxiliary funciton that 
(define (let*->let-aux definitions body-expressions)
  (cond [(null? definitions) body-expressions]
        [(eq? (length definitions) 1) (cons 'let (cons (list (car definitions)) (let*->let-aux (cdr definitions) body-expressions)))]
        [else (list 'let (list (car definitions)) (let*->let-aux (cdr definitions) body-expressions))]))


(provide squares
         map-squares
         ascending
         let*->let
         let*->let-aux)