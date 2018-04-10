#lang racket
;;; PROBLEM 4
;; Macro that does exactly the same thing as the built in Racket and
(define-syntax my-and
  (syntax-rules ()
    [(my-and) #t]
    [(my-and a) a]
    [(my-and a b ...) (if a
                          (my-and b ...)
                          #f)]))
