;;;; Test script for Lists and Lists

(load "lists-and-lists.lisp")

(in-package :lists-and-lists)

;; Test the Scheme interpreter
(format t "~%=== Testing Scheme Interpreter ===~%")

(init-global-env)

;; Test basic arithmetic
(format t "~%Testing: (+ 2 3)~%")
(let* ((*eval-fuel* 1000)
       (result (scheme-eval '(+ 2 3) *global-env*)))
  (format t "Result: ")
  (scheme-print result)
  (terpri))

;; Test define
(format t "~%Testing: (define x 10)~%")
(let* ((*eval-fuel* 1000)
       (result (scheme-eval '(define x 10) *global-env*)))
  (format t "Result: ")
  (scheme-print result)
  (terpri))

(format t "~%Testing: x~%")
(let* ((*eval-fuel* 1000)
       (result (scheme-eval 'x *global-env*)))
  (format t "Result: ")
  (scheme-print result)
  (terpri))

;; Test quote
(format t "~%Testing: (quote (1 2 3))~%")
(let* ((*eval-fuel* 1000)
       (result (scheme-eval '(quote (1 2 3)) *global-env*)))
  (format t "Result: ")
  (scheme-print result)
  (terpri))

;; Test car and cdr
(format t "~%Testing: (car '(a b c))~%")
(let* ((*eval-fuel* 1000)
       (result (scheme-eval '(car (quote (a b c))) *global-env*)))
  (format t "Result: ")
  (scheme-print result)
  (terpri))

;; Test lambda
(format t "~%Testing: ((lambda (x) (+ x 5)) 10)~%")
(let* ((*eval-fuel* 1000)
       (result (scheme-eval '((lambda (x) (+ x 5)) 10) *global-env*)))
  (format t "Result: ")
  (scheme-print result)
  (terpri))

;; Test problem 2
(format t "~%~%=== Testing Problem 2 ===~%")
(scheme-eval '(define twentyseven 27) *global-env*)
(format t "Checking solution...~%")
(check-problem 2)

;; Test problem 3
(format t "~%~%=== Testing Problem 3 ===~%")
(scheme-eval '(define tail (quote (end))) *global-env*)
(scheme-eval '(define cat (cons (quote head) tail)) *global-env*)
(scheme-eval '(define dog (cons (quote head) tail)) *global-env*)
(format t "Checking solution...~%")
(check-problem 3)

;; Test problem 4
(format t "~%~%=== Testing Problem 4 ===~%")
(scheme-eval '(define abs (lambda (val)
                            (cond
                              ((> val 0) val)
                              ((< val 0) (- 0 val))
                              (t 0))))
             *global-env*)
(format t "Checking solution...~%")
(check-problem 4)

(format t "~%~%=== All tests completed ===~%")
