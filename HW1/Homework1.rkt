#lang racket

;;; PROBLEM 1
;; function that takes a list of numbers and returns a new list of the squares of those numbers
(define (squares numbers)
  (if (null? numbers)
    null
    (let([head (car numbers)])
      (cons (* head head) (squares (cdr numbers))))))

;;; PROBLEM 2
;; function that takes a list of numbers and returns a new list of the squares of those numbers
;; using map and an anonymous function
(define (map-squares numbers)
  (map (lambda (x) (* x x)) numbers))

;;; PROBLEM 3
;; non-tail-recursive function that tests whether a list of integers is in strictly ascending order
(define (ascending numbers)
  (or (null? numbers) (eq? (length numbers) 1) (and (< (car numbers) (cadr numbers)) (ascending (cdr numbers)))))

;;; PROBLEM 4
;; function that takes a list represneting a let* expression and returns a new list represneting the
;; equvelent expression using let
(define (let*->let expression)
  (if (or (null? expression) (not (pair? expression))) ;check if expression is null or not a list
      expression ;if so return expression
      (let*->let-aux (cadr expression) (cddr expression)))) ; else call auxiliary function

;; Auxillary function from excercise 4, modified to check for more let* statements within
;; the definitions or the body-expressions of the given let* statment
(define (let*->let-aux definitions expressions)
  (if (eq? (cdr definitions) null)
      (append (list 'let (list (car definitions))) expressions)
      (list 'let (list (car definitions)) (let*->let-aux (cdr definitions) expressions))))


;;; PROBLEM 5
;;; This function from class has been modified to preform simple symbolic
;;; differentiation with the -, sin, cos and expt operators
;;; (negative values are represneted as expressions multiplied by -1)

;; the top-level function deriv takes an expression and a variable, 
;; and returns the derivative of that expression with respect to the variable,
;; in simplified form
(define (deriv exp var)
  (simplify (basic-deriv exp var)))

;; basic-deriv takes the derivative of an expression exp with respect to a variable and
;; return the result without simplification
(define (basic-deriv exp var)
  (cond [(number? exp) 0]
        [(symbol? exp)
         (if (eq? exp var) 1 0)]
        [(sum? exp)
         (list '+ (basic-deriv (left exp) var) (basic-deriv (right exp) var))]
        [(difference? exp) ; rule simmilar to addition
         (list '- (basic-deriv (left exp) var) (basic-deriv (right exp) var))]
        [(product? exp)
         (list '+
          (list '* (left exp) (basic-deriv (right exp) var))
          (list '* (basic-deriv (left exp) var) (right exp)))]
        [(sin? exp) ;multiply the cos of the left expression with a deriv of the left expression
         (list '* (list 'cos (left exp)) (basic-deriv (left exp) var))]
        [(cos? exp) ;simmilar to rule than sin, add -1 multiplier up front
         (list '* -1 (list '* (list 'sin (left exp)) (basic-deriv (left exp) var)))]
        [(expt? exp) ;multiply right by the muptiple of left and it's deriv to the power of right minus one
         (list '* (right exp) (list 'expt (list '* (left exp) (basic-deriv (left exp) var)) (list '- (right exp) 1)))]
        [else (error "unknown expression type -- basic-deriv" exp)]))

;; predicates and access functions

;; test whether a list structure is a sum
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
;; test whether a list structure is a product
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
;; test whether a list structure is a difference
(define (difference? x)
  (and (pair? x) (eq? (car x) '-)))
;; test whether a list structure is a sin
(define (sin? x)
  (and (pair? x) (eq? (car x) 'sin)))
;; test whether a list structure is a cos
(define (cos? x)
  (and (pair? x) (eq? (car x) 'cos)))
;; test whether a list structure is an exponential
(define (expt? x)
  (and (pair? x) (eq? (car x) 'expt)))

;; get the left hand part of a sum or product
(define (left exp)
  (cadr exp))
;; get the right hand part of a sum or product
(define (right exp)
  (caddr exp))

;; basic simplification function (nothing too fancy ... doesn't know about commutativity or associativity)
(define (simplify exp)
  (cond [(sum? exp) (simplify-sum exp)]
        [(difference? exp) (simplify-difference exp)]
        [(product? exp) (simplify-product exp)]
        [(sin? exp) (simplify-sin exp)]
        [(cos? exp) (simplify-cos exp)]
        [(expt? exp) (simplify-expt exp)]
        ;; if we get here, we can't simplify exp
        [else exp]))

(define (simplify-sum exp)
  ;; to simplify a sum, we need to first recursively simplify the left and right parts
  (let ([a (simplify (left exp))]
        [b (simplify (right exp))])
    (cond [(equal? 0 a) b]
          [(equal? 0 b) a]
          [(and (number? a) (number? b)) (+ a b)]
          [else (list '+ a b)])))

(define (simplify-difference exp)
  (let ([a (simplify (left exp))]
        [b (simplify (right exp))])
    (cond [(equal? 0 b) a]
          ; a = 0 is handled by the general case
          [(and (number? a) (number? b)) (- a b)]
          [else (list '- a b)])))

(define (simplify-product exp) 
  (let ([a (simplify (left exp))]
        [b (simplify (right exp))])
    (cond [(or (equal? 0 a) (equal? 0 b)) 0]
          [(equal? 1 a) b]
          [(equal? 1 b) a]
          [(and (number? a) (number? b)) (* a b)]
          [else (list '* a b)])))

(define (simplify-sin exp)
  (let ([a (simplify (left exp))])
    (if (number? a)
        (sin a) ;if a is a number cll Racket's sin function
        (list 'sin a)))) ;else leave as is

(define (simplify-cos exp)
  (let ([a (simplify (left exp))])
    (if (number? a)
        (cos a) ;if a is a number cll Racket's sin function
        (list 'cos a)))) ;else leave as is

(define (simplify-expt exp)
  (let ([a (simplify (left exp))]
        [b (simplify (right exp))])
    (cond
      [(equal? b 0) 1] ;any number to the 0 power is 0
      [(equal? b 1) a] ;any number to the 1st power is itself
      [(and (number? a) (number? b)) (expt a b)] ;if a and b are numbers call Racket's expt funciton
      [else (list 'expt a b)]))) ;else leave as is


;;; EXTRA CREDIT
;; Function that converts a Racket expression containing any number of let* expressions
;; into an equivelent expression using let
(define (let*-converter expression)
  (cond  [(or (null? expression) (not (pair? expression))) expression]
         [(eq? (car expression) 'let*) (let*->let-full (cadr expression) (cddr expression))]
         [else (map let*-converter expression)]))

;; Auxillary function from excercise 4, modified to check for more let* statements within
;; the definitions or the body-expressions of the given let* statment
(define (let*->let-full definitions expressions)
  (if (eq? (cdr definitions) null)
      (append (list 'let (list (let*-converter (car definitions)))) (map let*-converter expressions))
      (list 'let (list (let*-converter (car definitions))) (let*->let-full (cdr definitions) expressions))))


(provide squares
         map-squares
         ascending
         let*->let
         let*-converter
         deriv
         simplify)



