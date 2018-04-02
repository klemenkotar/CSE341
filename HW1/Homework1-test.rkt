#lang racket

(require rackunit
         "Homework1.rkt")

(check-equal? (squares '()) '() "Empty List")
(check-equal? (squares '(1)) '(1) "Single Element List")
(check-equal? (squares '(1 2)) '(1 4) "Two Element List")
(check-equal? (squares '(1 -3 15 0 -20)) '(1 9 225 0 400) "Multiple Element + Negative Numbers List")
(test-case
 "List has decimal values that Squares rounds"
 (let ([actual-list (squares '(0 -0.7 1.2))]
       [expected-list '(0 0.49 1.44)])
       (for-each
        (lambda (actual expected)
          (check-= actual expected 0.000000001))
        actual-list
        expected-list)))


(check-equal? (map-squares '()) '() "Empty List")
(check-equal? (map-squares '(1)) '(1) "Single Element List")
(check-equal? (map-squares '(1 2)) '(1 4) "Two Element List")
(check-equal? (map-squares '(1 -3 15 0 -20)) '(1 9 225 0 400) "Multiple Element + Negative Numbers List")
(test-case
 "List has decimal values that Squares rounds"
 (let ([actual-list (map-squares '(0 -0.7 1.2))]
       [expected-list '(0 0.49 1.44)])
       (for-each
        (lambda (actual expected)
          (check-= actual expected 0.000000001))
        actual-list
        expected-list)))

(check-eq? (ascending '()) #t "Empty List")
(check-eq? (ascending '(42)) #t "Single Entry")
(check-eq? (ascending '(1 3 5 7 1200)) #t "Ascending List")
(check-eq? (ascending '(190 7 3 1)) #f "Strictly Descending")
(check-eq? (ascending '(1 2 3 5 9 7)) #f "PArtially Ascending, Partially Descending")