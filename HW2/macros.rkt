#lang racket
;;; PROBLEM 3
;; Struct used to store the value of delayed evaluations once they are forced
(struct delay-holder (is-evaluated value) #:transparent #:mutable)

;; Macro that behaves exactly as the built in delay macro
(define-syntax my-delay
  (syntax-rules ()
    [(my-delay f1 f2 ...) (delay-holder #f (lambda () f1 f2 ...))]))

;; Force function from class, no modifications necessary to work with our macro
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
   ;; Unit tests for my-delay macro
   (let ([x 0])
     (define f (my-delay (set! x (+ x 1)) '(Evaluated!)))
     (check-equal? x 0 "Check if f was delayed")
     (my-force f)
     (check-equal? x 1 "Check if f was evaluated once")
     (my-force f)
     (check-equal? x 1 "Check that f is not being evaluated anymore"))
   (let ([x 4])
     (define f1 (my-delay (+ x 5)))
     (define f2 (delay (+ x 5)))
     (check-equal? (my-force f1) (force f2) "Compare my-force and force")
     (check-equal? (my-force f1) (force f2) "Compare a second call to my-force and force"))
   (let* ([x 4]
          [y 3]
          [z (+ x y)])
     (define f1 (my-delay (- x y) 'squid '(oak maple redwood) (* x y z)))
     (define f2 (delay (- x y) 'squid '(oak maple redwood) (* x y z)))
     (check-equal? (my-force f1) (force f2) "Compare my-force and force - long expression")
     (check-equal? (my-force f1) (force f2) "Compare a second call to my-force and force - long expression"))
   ;; Unit tests for my-and macro
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