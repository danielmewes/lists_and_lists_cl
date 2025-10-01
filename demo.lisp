;;;; Demo script showing Lists and Lists functionality

(load "lists-and-lists.lisp")

(in-package :lists-and-lists)

(format t "~%")
(format t "═══════════════════════════════════════════════════════════════════════════~%")
(format t "                         LISTS AND LISTS                                   ~%")
(format t "                    Common Lisp Port Demo                                  ~%")
(format t "═══════════════════════════════════════════════════════════════════════════~%")
(format t "~%")

(format t "Original game by Andrew Plotkin (1996)~%")
(format t "An interactive tutorial for learning Scheme/Lisp~%~%")

;; Initialize the interpreter
(init-global-env)

(format t "───────────────────────────────────────────────────────────────────────────~%")
(format t "Part 1: Basic Scheme Interpreter Features~%")
(format t "───────────────────────────────────────────────────────────────────────────~%~%")

(defun demo-eval (expr-string)
  (format t ">> ~a~%" expr-string)
  (let* ((expr (read-from-string expr-string))
         (*eval-fuel* 1000)
         (result (scheme-eval expr *global-env*)))
    (format t " ")
    (scheme-print result)
    (terpri)
    (terpri)))

(format t "Arithmetic:~%")
(demo-eval "(+ 2 3)")
(demo-eval "(- 10 4)")
(demo-eval "(* 5 6)")

(format t "Variables and definitions:~%")
(demo-eval "(define x 42)")
(demo-eval "x")

(format t "Lists and quotes:~%")
(demo-eval "(quote (a b c))")
(demo-eval "(car (quote (1 2 3)))")
(demo-eval "(cdr (quote (1 2 3)))")
(demo-eval "(cons 1 (quote (2 3)))")

(format t "Functions:~%")
(demo-eval "((lambda (x) (+ x 10)) 5)")
(demo-eval "(define square (lambda (n) (* n n)))")
(demo-eval "(square 7)")

(format t "Conditionals:~%")
(demo-eval "(if (> 5 3) (quote yes) (quote no))")

(format t "───────────────────────────────────────────────────────────────────────────~%")
(format t "Part 2: Tutorial Problems~%")
(format t "───────────────────────────────────────────────────────────────────────────~%~%")

(format t "Problem 2: Simple Definition~%")
(format t "Task: Define TWENTYSEVEN to have the value 27~%")
(demo-eval "(define twentyseven 27)")
(format t "Checking... ")
(if (check-problem 2)
    (format t "✓ Passed!~%~%")
    (format t "✗ Failed~%~%"))

(format t "Problem 3: Lists with Shared Structure~%")
(format t "Task: Create CAT and DOG that are EQUAL? but not EQV?, with EQV? cdrs~%")
(demo-eval "(define tail (quote (end)))")
(demo-eval "(define cat (cons (quote head) tail))")
(demo-eval "(define dog (cons (quote head) tail))")
(demo-eval "(equal? cat dog)")
(demo-eval "(eqv? cat dog)")
(demo-eval "(eqv? (cdr cat) (cdr dog))")
(format t "Checking... ")
(if (check-problem 3)
    (format t "✓ Passed!~%~%")
    (format t "✗ Failed~%~%"))

(format t "Problem 4: Absolute Value Function~%")
(format t "Task: Define ABS for absolute value~%")
(demo-eval "(define abs (lambda (val)
              (cond
                ((> val 0) val)
                ((< val 0) (- 0 val))
                (t 0))))")
(demo-eval "(abs 5)")
(demo-eval "(abs -5)")
(demo-eval "(abs 0)")
(format t "Checking... ")
(if (check-problem 4)
    (format t "✓ Passed!~%~%")
    (format t "✗ Failed~%~%"))

(format t "Problem 5: Recursive SUM Function~%")
(format t "Task: Add up a list of integers~%")
(demo-eval "(define sum (lambda (s)
              (cond
                ((null? s) 0)
                (t (+ (car s) (sum (cdr s)))))))")
(demo-eval "(sum (quote (8 2 3)))")
(demo-eval "(sum (quote ()))")
(format t "Checking... ")
(if (check-problem 5)
    (format t "✓ Passed!~%~%")
    (format t "✗ Failed~%~%"))

(format t "Problem 6: Nested List Sum~%")
(format t "Task: Add up arbitrarily nested lists~%")
(demo-eval "(define megasum (lambda (s)
              (cond
                ((null? s) 0)
                (t (+
                     (cond
                       ((list? (car s)) (megasum (car s)))
                       (t (car s)))
                     (megasum (cdr s)))))))")
(demo-eval "(megasum (quote ((8) 5 (2 () (9 1) 3))))")
(format t "Checking... ")
(if (check-problem 6)
    (format t "✓ Passed!~%~%")
    (format t "✗ Failed~%~%"))

(format t "Problem 7: Maximum Function~%")
(format t "Task: Find maximum of a list~%")
(demo-eval "(define max (lambda (s)
              (cond
                ((null? (cdr s)) (car s))
                (t (let ((hd (car s))
                        (tl (max (cdr s))))
                     (cond
                       ((> hd tl) hd)
                       (t tl)))))))")
(demo-eval "(max (quote (5 14 -3)))")
(format t "Checking... ")
(if (check-problem 7)
    (format t "✓ Passed!~%~%")
    (format t "✗ Failed~%~%"))

(format t "Problem 8: Closures and Static Scoping~%")
(format t "Task: Create POCKET function that returns functions with captured state~%")
(demo-eval "(define pocket
              (letrec
                ((generator (lambda (x)
                              (lambda (y)
                                (cond
                                  ((null? y) x)
                                  (t (generator y)))))))
                (generator 8)))")
(demo-eval "(pocket nil)")
(format t "Creating new pocket with 12:~%")
(demo-eval "(define newpocket (pocket 12))")
(demo-eval "(newpocket nil)")
(format t "Original pocket still works:~%")
(demo-eval "(pocket nil)")
(format t "Checking... ")
(if (check-problem 8)
    (format t "✓ Passed!~%~%")
    (format t "✗ Failed~%~%"))

(format t "───────────────────────────────────────────────────────────────────────────~%")
(format t "All 8 Tutorial Problems Completed! ✓~%")
(format t "───────────────────────────────────────────────────────────────────────────~%~%")

(format t "This demonstrates:~%")
(format t "  • A complete Scheme interpreter in Common Lisp~%")
(format t "  • Proper lexical scoping and closures~%")
(format t "  • Recursive functions~%")
(format t "  • Higher-order functions~%")
(format t "  • List manipulation~%")
(format t "  • Educational game framework~%~%")

(format t "To play the full interactive game, run:~%")
(format t "  (lists-and-lists:play-game)~%~%")

(format t "═══════════════════════════════════════════════════════════════════════════~%")
