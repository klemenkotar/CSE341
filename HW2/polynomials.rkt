#lang racket

;;; PROBLEM 1
;; Function that takes two polynomials in a symbolic variable and returns their product
;; It has two arguments, both lists representing polynonials and returns a list representing
;; their product.
(define (poly-multiply left right)
  (poly-reduce (poly-simplify (poly-sort (poly-expand left right)))))

;; multiplies out the terms of the two polynomials given
(define (poly-expand left right)
  (if (or (null? left) (null? right))
      null
      (append (map (curry basic-multiply (car right)) left)
              (poly-expand left (cdr right)))))

;; multiplies two polynomial terms
(define (basic-multiply a b)
  (list (* (coefficient a) (coefficient b)) (+ (exponent a) (exponent b))))

;; sorts a polynomial in order of descending exponents
(define (poly-sort poly)
  (sort poly #:key exponent >))

;; combines like terms in a given polynomial
(define (poly-simplify poly)
  (if (or (null? poly) (one-term? poly))
      poly
      (let ([first (car poly)]
            [second (cadr poly)])
        (if (eq? (exponent first) (exponent second))
            (poly-simplify (cons
                            (list (+ (coefficient first) (coefficient second)) (exponent first))
                            (cddr poly)))
            (cons first (poly-simplify (cdr poly)))))))

;; removes all terms with coefficient 0 from a polynomial
(define (poly-reduce poly)
  (filter (lambda (x) (not (eq? (coefficient x) 0))) poly))

;; predicates and acess functions
(define (one-term? poly)
  (eq? (length poly) 1))

;; get the coefficient of a polynomial term
(define (coefficient term)
  (car term))
;; get the exponent of a polynomial term
(define (exponent term)
  (cadr term))

;;; PROBLEM 2
;; Function that converts a normalized polynomial into evaluable Racket code.
;; It has two arguments: a list representing the polynomial and the symbolic variable
;; It outpus a list representing the polynomial in evaluable Racket code.
;; (some helper functions from problem 1 are used)
(define (poly->code poly var)
  (cond [(null? poly) 0]
        [(eq? (length poly) 1) (poly->code-transform var (car poly))]
        [else (append '(+) (map (curry poly->code-transform var) poly))]))

;; converts a single term of the polynomial from normalized form to Racket code
(define (poly->code-transform var term)
  (cond [(zero? (exponent term)) (coefficient term)]
        [(and (eq? (exponent term) 1) (eq? (coefficient term) 1)) var]
        [(eq? (exponent term) 1) (list '* (coefficient term) var)]
        [(eq? (coefficient term) 1) (list 'expt var (exponent term))]
        [else (list '* (coefficient term) (list 'expt var (exponent term)))]))
  

;;; UNIT TESTS

(require rackunit)

(define polynomial-tests 
  (test-suite
   "Tests for Polynomial functions"
   ;; tests for poly-multiply
   (check-equal? (poly-multiply '() '()) '() "Two empty polynomails")
   (check-equal? (poly-multiply '((-3 4) (1 1) (5 0)) '()) '() "Second polynomial is empty")
   (check-equal? (poly-multiply '() '((1 2))) '() "First polynomial is empty")
   (check-equal? (poly-multiply '((3 0)) '((1 1))) '((3 1)) "Simple multiplication")
   (check-equal? (poly-multiply '((1 1)) '((3 0))) '((3 1)) "Commutative property test")
   (check-equal? (poly-multiply '((1 3) (1 1) (-1 0)) '((-5 0)))
                 '((-5 3) (-5 1) (5 0)) "Scala multiplication")
   (check-equal? (poly-multiply '((1 3) (1 2) (1 1) (1 0)) '((1 1) (-1 0)))
                 '((1 4) (-1 0)) "Simple polynomial")
   (check-equal? (poly-multiply '((-10 2) (100 1) (5 0)) '((1 999) (-1 7) (1 1) (3 0)))
                 '((-10 1001) (100 1000) (5 999) (10 9) (-100 8) (-5 7) (-10 3) (70 2) (305 1) (15 0))
                 "Long polynomials")
   ;; tests for poly->code
   (check-equal? (poly->code '() 'x) 0 "Zero")
   (check-equal? (poly->code '((1 0)) 'x)  1 "One")
   (check-equal? (poly->code '((7 0)) 'x)  7 "Seven")
   (check-equal? (poly->code '((1 1)) 'x) 'x "x")
   (check-equal? (poly->code '((1 1) (-10 0)) 'x) '(+ x -10) "Short polynomial")
   (check-equal? (poly->code '((1 3) (5 2) (7 1) (10 0)) 'x) '(+ (expt x 3) (* 5 (expt x 2)) (* 7 x) 10)
                 "Long polynomial")
   ;; eval the output of poly->code
   (let ([p1 '((1 3) (1 2) (1 1) (1 0))]
         [p2 '((1 1) (-1 0))]
         [ns (make-base-namespace)])
     (eval '(define x 4) ns)
     (check-equal? (eval (poly->code (poly-multiply p1 p2) 'x) ns) 255)
     (check-equal? (* (eval (poly->code p1 'x) ns) (eval (poly->code p2 'x ) ns) ) 255))

))
  
(require rackunit/text-ui)
;; this line runs the tests
(run-tests polynomial-tests )