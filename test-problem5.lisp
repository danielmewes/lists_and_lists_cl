;;;; Test Problem 5 - Recursive SUM function

(load "lists-and-lists.lisp")

(in-package :lists-and-lists)

(init-global-env)

(format t "~%=== Testing Problem 5: Recursive SUM ===~%~%")

(format t "Problem: Define SUM to be a function that adds up a list of integers.~%")
(format t "  (SUM '(8 2 3)) should return 13~%")
(format t "  (SUM NIL) should return 0~%~%")

(format t "Solution:~%")
(format t "(define sum (lambda (s)~%")
(format t "  (cond~%")
(format t "    ((null? s) 0)~%")
(format t "    (t (+ (car s) (sum (cdr s)))))))~%~%")

;; Define the solution
(let* ((*eval-fuel* 1000))
  (scheme-eval '(define sum (lambda (s)
                              (cond
                                ((null? s) 0)
                                (t (+ (car s) (sum (cdr s)))))))
               *global-env*))

(format t "Testing...~%~%")

;; Test cases
(let ((tests (list
              '((8 2 3) 13)
              '(() 0)
              '((5) 5)
              '((10 -5 7 -2) 10)
              '((1 1 1 1 1) 5))))
  (dolist (test tests)
    (let* ((input-list (first test))
           (expected (second test))
           (quoted-input (scheme-read-quote input-list))
           (*eval-fuel* 1000)
           (sum-fn (multiple-value-bind (val found) (env-lookup 'sum *global-env*)
                     (if found val (error "SUM not defined"))))
           (result (scheme-apply sum-fn (list quoted-input))))
      (format t "  (SUM '~a) = ~a" input-list result)
      (if (and (numberp result) (= result expected))
          (format t " ✓~%")
          (format t " ✗ (expected ~a)~%" expected)))))

(format t "~%Checking with game's check-problem function:~%")
(check-problem 5)

(format t "~%=== Test Complete ===~%")
