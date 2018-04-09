#lang racket
(define (poly-multiply left right)
  (reduce (simplify (poly-sort (poly-multiply-expanded left right)))))

(define (poly-multiply-expanded left right)
  (if (or (null? left) (null? right))
      null
      (append (map (curry basic-multiply (car right)) left)
              (poly-multiply-expanded left (cdr right)))))

(define (poly-sort polynomial)
  (sort polynomial #:key exponent >))

(define (simplify polynomial)
  (if (or (null? polynomial) (eq? (length polynomial) 1))
      polynomial
      (simplify-like-terms polynomial)))

(define (simplify-like-terms polynomial)
  (let ([first (car polynomial)]
        [second (cadr polynomial)])
    (if (eq? (exponent first) (exponent second))
        (simplify (cons
                   (list (+ (coefficient first) (coefficient second)) (exponent first))
                   (cddr polynomial)))
        (cons first (simplify (cdr polynomial))))))

(define (basic-multiply a b)
  (list (* (coefficient a) (coefficient b)) (+ (exponent a) (exponent b))))

(define (reduce polynomial)
  (filter (lambda (x) (not (eq? (car x) 0))) polynomial))

(define (coefficient term)
  (car term))

(define (exponent term)
  (cadr term))



;(define (poly-multiply-expanded left right)
;  (if (or (null? left) (null? right))
;      null
;      (map (lambda (multiplier)
;             (map (lambda (multiply-list)
;                    (list (* (car multiply-list) (car multiplier)) (+ (cadr multiply-list) (cadr multiplier))))
;             left))
;           right)))

