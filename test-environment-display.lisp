;;;; Test the :e (environment display) command

(load "lists-and-lists.lisp")

(in-package :lists-and-lists)

(format t "~%=== Testing Environment Display ===~%~%")

;; Initialize interpreter
(init-global-env)

;; Add some definitions
(format t "Setting up test environment...~%~%")
(let ((*eval-fuel* 1000))
  (scheme-eval '(define x 42) *global-env*)
  (scheme-eval '(define y 100) *global-env*)
  (scheme-eval '(define mylist (quote (a b c))) *global-env*)
  (scheme-eval '(define square (lambda (n) (* n n))) *global-env*)
  (scheme-eval '(define twentyseven 27) *global-env*))

(format t "Displaying environment with :e command:~%~%")
(display-environment *global-env*)

(format t "~%Now testing with nested environments (let)...~%~%")

;; Create a nested environment using let
(let ((*eval-fuel* 1000))
  ;; This will create a nested environment but we can't easily capture it
  ;; Let's just demonstrate that the function works
  (format t "Evaluating: (let ((a 1) (b 2)) (+ a b))~%")
  (let ((result (scheme-eval '(let ((a 1) (b 2)) (+ a b)) *global-env*)))
    (format t "Result: ")
    (scheme-print result)
    (terpri)))

(format t "~%The :e command will show user-defined variables along with built-ins.~%")
(format t "In the interactive interpreter, try:~%")
(format t "  >> (define test 123)~%")
(format t "  >> :e~%")
(format t "~%This will show TEST in the environment along with all built-in functions.~%")

(format t "~%=== Test Complete ===~%")
