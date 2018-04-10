#lang racket
;;; PROBLEM 3
(struct delay-holder (is-evaluated value) #:transparent #:mutable)

(define-syntax my-delay
  (syntax-rules ()
    [(my-delay f1) ((delay-holder #f (lambda () f1)))]
    [(my-delay f1 f2 ...) (begin
                            (delay-holder #f (lambda () f1))
                            (my-delay f2 ...))]))

;(struct delay-holder (is-evaluated value) #:transparent #:mutable)

(define (my-force holder)
  (cond [(delay-holder-is-evaluated holder) (delay-holder-value holder)]
        [else (set-delay-holder-is-evaluated! holder #t)
              (set-delay-holder-value! holder ((delay-holder-value holder)))
              (delay-holder-value holder)]))


;;; PROBLEM 4
;; Macro that does exactly the same thing as the built in Racket and
(define-syntax my-and
  (syntax-rules ()
    [(my-and) #t]
    [(my-and a) a]
    [(my-and a b ...) (if a
                          (my-and b ...)
                          #f)]))


;;; UNIT TESTS

(require rackunit)

(define macros-tests 
  (test-suite
   "Tests for Polynomial functions"
   (check-equal? (my-and) #t "Zero arguments")
   (check-equal? (my-and (eq? 1 1)) #t "One argument true")
   (check-equal? (my-and (eq? 1 2)) #f "One argument false")
   (check-equal? (my-and 'squid) 'squid "One argument atom")
   (check-equal? (my-and #t #f) #f "Two argument false")
   (check-equal? (my-and 'squid #t) #t "Two arguments true")
   (check-equal? (my-and #t (eq? 1 1) 4) 4 "Three argument number")
   (check-equal? (my-and 'squid 'clam 'starfish 'octopus 'anemone) 'anemone "Multiple arguments atom")
))

(require rackunit/text-ui)
;; this line runs the tests
(run-tests macros-tests )