#lang racket

(require rackunit
         "Homework1.rkt")

(check-equal? (squares '()) '() "Empty List")
(check-equal? (squares '(1)) '(1) "Single Element List")
(check-equal? (squares '(1 2)) '(1 4) "Two Element List")
(check-equal? (squares '(1 -3 15 0 -20)) '(1 9 225 0 400) "Multiple Element + Negative Numbers List")
(test-case
 "List has decimal values that Squares rounds"
 (let ([output-list (squares '(0 -0.7 1.2))]
       [anwser-list '(0 0.49 1.44)])
       (for-each
        (lambda (output anwser)
          (check-= output anwser 0.000000001))
        output-list
        anwser-list)))


