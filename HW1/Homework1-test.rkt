#lang racket

(require rackunit
         "Homework1.rkt")

(define hw1-tests 
  (test-suite
   "Tests for homework 1"

   ;; Porblem 1 
   (check-equal? (squares '()) '() "Empty List")
   (check-equal? (squares '(1)) '(1) "Single Element List")
   (check-equal? (squares '(1 2)) '(1 4) "Two Element List")
   (check-equal? (squares '(1 -3 15 0 -20)) '(1 9 225 0 400) "Multiple Element + Negative Numbers List")
   (test-case ; test if it can handle decimals
    "List has decimal values that Squares rounds"
    (for-each
     (lambda (actual expected)
       (check-= actual expected 0.000000001))
     (squares '(0 -0.7 1.2))
     '(0 0.49 1.44)))

   ;; Problem 2
   (check-equal? (map-squares '()) '() "Empty List")
   (check-equal? (map-squares '(1)) '(1) "Single Element List")
   (check-equal? (map-squares '(1 2)) '(1 4) "Two Element List")
   (check-equal? (map-squares '(1 -3 15 0 -20)) '(1 9 225 0 400) "Multiple Element + Negative Numbers List")
   (test-case ; test if it can handle decimals
    "List has decimal values that Squares rounds"
    (let ([actual-list (map-squares '(0 -0.7 1.2))]
          [expected-list '(0 0.49 1.44)])
      (for-each
       (lambda (actual expected)
         (check-= actual expected 0.000000001))
       actual-list
       expected-list)))

   ;; Problem 3
   (check-eq? (ascending '()) #t "Empty List")
   (check-eq? (ascending '(42)) #t "Single Entry")
   (check-eq? (ascending '(1 3 5 7 1200)) #t "Ascending List")
   (check-eq? (ascending '(190 7 3 1)) #f "Strictly Descending")
   (check-eq? (ascending '(1 2 3 5 9 7)) #f "PArtially Ascending, Partially Descending")


   ;; Problem 4
   (check-equal? (let*->let '(let* ([x 3] [y 4]) (+ x y))) '(let ((x 3)) (let ((y 4)) (+ x y))))
   (check-equal? (eval (let*->let '(let* ([x 3] [y 4]) (+ x y))) (make-base-namespace)) 7)
   (check-equal? (let*->let '(let* ([x 3] [y 4]) (+ x y) (* x y))) '(let ((x 3)) (let ((y 4)) (+ x y)(* x y))))
   (check-equal? (eval (let*->let '(let* ([x 3] [y 4]) (+ x y) (* x y))) (make-base-namespace)) 12)
   (check-equal? (let*->let '(let* ([x 0]))) '(let ((x 0))))

   ;; Probelm 5
   (check-equal? (deriv 'x 'x) 1 "deriv of x wrt x")
   (check-equal? (deriv 'y 'x) 0 "deriv of y wrt x")
   (check-equal? (deriv '(+ x 3) 'x) 1 "deriv of (+ x 3) wrt x")
   (check-equal? (deriv '(* (+ 2 3) x) 'x) 5 "deriv of unsimplified expression")
   (check-equal? (deriv '(+ x y) 'x) 1 "deriv of (+ x y) wrt x")
   ;; simplification is not as clever as it could be in the following case:
   (check-equal? (deriv '(* (+ x 1) (+ x -1)) 'x) '(+ (+ x 1) (+ x -1)) "deriv of (* (+ x 1) (+ x -1)) wrt x")
   (check-equal? (deriv '(* (* x y) (+ x 3)) 'x) '(+ (* x y) (* y (+ x 3))) "complex deriv")
   (check-equal? (deriv '(- x y) 'x) '1 "simple deriv with minus operator")
   (check-equal? (deriv '(sin x) 'x) '(cos x) "simple deriv with sin")
   (check-equal? (deriv '(cos x) 'x) '(* -1(sin x)) "simple deriv with cos")
   (check-equal? (deriv '(* (- 2 x) (- 7 x)) 'x) '(+ (* (- 2 x) -1) (* -1 (- 7 x))) "complex deriv with minus")
   (check-equal? (deriv '(sin (expt (* x 3) 2)) 'x) '(* (cos (expt (* x 3) 2)) (* 2 (* (* x 3) 3))) "complex deriv")
   (check-equal? (deriv '(expt x 2) 'x) '(* 2 x) "simple expt deriv")
   ;; test the simplifier
   (check-equal? (simplify '(expt (expt (expt 1 0) 1) 3)) 1 "several exponential cases")
   (check-equal? (simplify '(sin 0)) 0 "Check sin 0")
   (check-equal? (simplify '(cos 0)) 1 "check cos 0")

   ;; Problem Extra Credit
   (check-equal? (let*-converter '(let* ([x 3] [y 4]) (+ x y))) '(let ((x 3)) (let ((y 4)) (+ x y))))
   (check-equal? (eval (let*-converter '(let* ([x 3] [y 4]) (+ x y))) (make-base-namespace)) 7)
   (check-equal? (let*->let '(let* ([x 3] [y 4]) (+ x y) (* x y))) '(let ((x 3)) (let ((y 4)) (+ x y)(* x y))))
   (check-equal? (eval (let*-converter '(let* ([x 3] [y 4]) (+ x y) (* x y))) (make-base-namespace)) 12)
   (check-equal? (let*-converter '(let* ([x 0]))) '(let ((x 0))))
   (check-equal? (let*-converter '(define (ugly z)
  (let* ([x (let* ((y (* 2 z))) (* 2 y))]
         [y x])
    (let* ([x x])
(+ x y z))))) '(define (ugly z) (let ((x (let ((y (* 2 z))) (* 2 y)))) (let ((y x)) (let ((x x)) (+ x y z))))))
   
 ))



(require rackunit/text-ui)
;; this line runs the tests ....
(run-tests hw1-tests )