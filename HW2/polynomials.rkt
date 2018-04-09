#lang racket
;(define (poly-multiply left right)
;  (simplify (poly-multiply-expanded)))

;(define (poly-multiply-expanded left right)
;  (if (or (null? left) (null? right))
;      null
;      (map (lambda multipliers
;             (map (curry basic-multiply (car right)) left)
;             right))

(define (basic-multiply multiplier multiply-list)
  (list (* (car multiply-list) (car multiplier)) (+ (cadr multiply-list) (cadr multiplier))))


(define (poly-multiply-expanded left right)
  (if (or (null? left) (null? right))
      null
      (append (map (lambda (multiplier)
             (map (lambda (multiply-list)
                    (list (* (car multiply-list) (car multiplier)) (+ (cadr multiply-list) (cadr multiplier))))
             left))
           right))))