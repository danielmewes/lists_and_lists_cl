;;;; Test script for Lists and Lists

(load "lists-and-lists.lisp")

(in-package :lists-and-lists)

;;; Test Framework

(defvar *test-results* nil "List of test results")

(defstruct test-result
  name
  passed
  output)

(defun run-test (name test-fn)
  "Run a test function, capturing its output. Returns t on success, nil on failure."
  (let ((output (make-array '(0) :element-type 'character :fill-pointer 0 :adjustable t))
        (passed nil))
    (handler-case
        (with-output-to-string (stream output)
          (let ((*standard-output* stream))
            (setf passed (funcall test-fn))))
      (error (e)
        (with-output-to-string (stream output)
          (let ((*standard-output* stream))
            (format t "ERROR: ~a~%" e)))
        (setf passed nil)))
    (push (make-test-result :name name :passed passed :output (coerce output 'string))
          *test-results*)
    (if passed
        (format t "✓ ~a~%" name)
        (format t "✗ ~a~%" name))
    passed))

(defun assert-equal (actual expected &optional description)
  "Assert that actual equals expected. Returns t on success, nil on failure.
   Prints detailed comparison on failure."
  (let ((passed (equal actual expected)))
    (when description
      (format t "~a~%" description))
    (if passed
        (progn
          (format t "  Result: ")
          (scheme-print actual)
          (terpri)
          t)
        (progn
          (format t "  Expected: ")
          (scheme-print expected)
          (terpri)
          (format t "  Actual:   ")
          (scheme-print actual)
          (terpri)
          nil))))

(defun assert-number-equal (actual expected &optional description)
  "Assert that actual equals expected (for numbers). Returns t on success, nil on failure."
  (let ((passed (and (numberp actual) (= actual expected))))
    (when description
      (format t "~a~%" description))
    (if passed
        (progn
          (format t "  Result: ~a~%" actual)
          t)
        (progn
          (format t "  Expected: ~a~%" expected)
          (format t "  Actual:   ")
          (if (numberp actual)
              (format t "~a~%" actual)
              (progn
                (scheme-print actual)
                (terpri)))
          nil))))

(defun assert-function-result (function-name input-list result expected)
  "Assert that a function application result matches expected value.
   Returns t on success, nil on failure. Prints checkmark or X inline."
  (format t "  (~a '~a) = ~a" function-name input-list result)
  (if (and (numberp result) (= result expected))
      (progn
        (format t " ✓~%")
        t)
      (progn
        (format t " ✗ (expected ~a)~%" expected)
        nil)))

(defun lookup-function (function-name env)
  "Lookup a function in the environment, raising an error if not found."
  (multiple-value-bind (val found) (env-lookup function-name env)
    (if found
        val
        (error "~a not defined" (string-upcase (symbol-name function-name))))))

(defun define-scheme-function (function-name definition)
  "Define a Scheme function in the global environment with standard boilerplate."
  (format t "Defining ~a function...~%" (string-upcase (symbol-name function-name)))
  (let ((*eval-fuel* 1000))
    (scheme-eval definition *global-env*)))

(defun run-function-test-cases (function-name test-cases)
  "Run multiple test cases for a Scheme function, returning t if all pass.
   test-cases should be a list of (input expected) pairs."
  (format t "Testing with multiple inputs...~%")
  (let ((func (lookup-function function-name *global-env*))
        (all-passed t))
    (dolist (test test-cases)
      (let* ((input-list (first test))
             (expected (second test))
             (quoted-input (scheme-read-quote input-list))
             (*eval-fuel* 1000)
             (result (scheme-apply func (list quoted-input))))
        (unless (assert-function-result function-name input-list result expected)
          (setf all-passed nil))))
    all-passed))

(defun check-problem-with-tests (problem-num test-results)
  "Check problem using game's check-problem function, combining with test results."
  (format t "Checking with game's check-problem function:~%")
  (and test-results (check-problem problem-num)))

(defun print-test-summary ()
  "Print summary of all test results, highlighting failures."
  (let* ((results (reverse *test-results*))
         (total (length results))
         (passed (count-if #'test-result-passed results))
         (failed (- total passed)))
    (format t "~%~%=== Test Summary ===~%")
    (format t "Total: ~a  Passed: ~a  Failed: ~a~%" total passed failed)

    (when (> failed 0)
      (format t "~%=== Failed Tests ===~%")
      (dolist (result results)
        (unless (test-result-passed result)
          (format t "~%--- ~a ---~%" (test-result-name result))
          (format t "~a" (test-result-output result)))))

    (if (= failed 0)
        (format t "~%All tests passed!~%")
        (format t "~%~a test(s) failed.~%" failed))))

;; Initialize test results
(setf *test-results* nil)

(init-global-env)

(format t "~%=== Running Tests ===~%~%")

;; Test basic interpreter features
(run-test "Basic arithmetic: (+ 2 3) = 5"
  (lambda ()
    (let* ((*eval-fuel* 1000)
           (result (scheme-eval '(+ 2 3) *global-env*)))
      (assert-number-equal result 5 "Testing: (+ 2 3)"))))

(run-test "Define and lookup variable"
  (lambda ()
    (let* ((*eval-fuel* 1000))
      (scheme-eval '(define test-x 10) *global-env*)
      (let ((result (scheme-eval 'test-x *global-env*)))
        (assert-number-equal result 10 "Defined test-x = 10, looking up test-x")))))

(run-test "Quote expression"
  (lambda ()
    (let* ((*eval-fuel* 1000)
           (result (scheme-eval '(quote (1 2 3)) *global-env*)))
      (format t "Testing: (quote (1 2 3))~%")
      (if (and (scheme-cons-p result)
               (= (scheme-cons-car result) 1))
          (progn
            (format t "  Result: ")
            (scheme-print result)
            (terpri)
            t)
          (progn
            (format t "  Expected: A cons cell starting with 1~%")
            (format t "  Actual:   ")
            (scheme-print result)
            (terpri)
            nil)))))

(run-test "Car and cdr operations"
  (lambda ()
    (let* ((*eval-fuel* 1000)
           (result (scheme-eval '(car (quote (a b c))) *global-env*)))
      (assert-equal result 'a "Testing: (car '(a b c))"))))

(run-test "Lambda application"
  (lambda ()
    (let* ((*eval-fuel* 1000)
           (result (scheme-eval '((lambda (x) (+ x 5)) 10) *global-env*)))
      (assert-number-equal result 15 "Testing: ((lambda (x) (+ x 5)) 10)"))))

;; Test problem 2
(run-test "Problem 2: TWENTYSEVEN"
  (lambda ()
    (define-scheme-function 'twentyseven '(define twentyseven 27))
    (check-problem-with-tests 2 t)))

;; Test problem 3
(run-test "Problem 3: CAT and DOG (EQUAL? but not EQV?)"
  (lambda ()
    (format t "Defining CAT and DOG...~%")
    (let ((*eval-fuel* 1000))
      (scheme-eval '(define tail (quote (end))) *global-env*)
      (scheme-eval '(define cat (cons (quote head) tail)) *global-env*)
      (scheme-eval '(define dog (cons (quote head) tail)) *global-env*))
    (check-problem-with-tests 3 t)))

;; Test problem 4
(run-test "Problem 4: ABS (absolute value)"
  (lambda ()
    (define-scheme-function 'abs
      '(define abs (lambda (val)
                     (cond
                       ((> val 0) val)
                       ((< val 0) (- 0 val))
                       (t 0)))))
    (check-problem-with-tests 4 t)))

;; Test problem 5
(run-test "Problem 5: SUM (recursive list sum)"
  (lambda ()
    (define-scheme-function 'sum
      '(define sum (lambda (s)
                     (cond
                       ((null? s) 0)
                       (t (+ (car s) (sum (cdr s))))))))
    (let ((test-results (run-function-test-cases 'sum
                          '(((8 2 3) 13)
                            (() 0)
                            ((5) 5)
                            ((10 -5 7 -2) 10)
                            ((1 1 1 1 1) 5)))))
      (check-problem-with-tests 5 test-results))))

;; Test problem 6
(run-test "Problem 6: MEGASUM (nested list sum)"
  (lambda ()
    (define-scheme-function 'megasum
      '(define megasum (lambda (s)
                         (cond
                           ((null? s) 0)
                           ((list? (car s))
                             (+ (megasum (car s)) (megasum (cdr s))))
                           (t (+ (car s) (megasum (cdr s))))))))
    (let ((test-results (run-function-test-cases 'megasum
                          '((((8) 5 (2 () (9 1) 3)) 28)
                            ((1 2 3) 6)
                            (((1 (2 (3)))) 6)
                            (() 0)
                            ((10) 10)
                            ((5 (10 (15))) 30)))))
      (check-problem-with-tests 6 test-results))))

;; Test problem 7
(run-test "Problem 7: MAX (find maximum in list)"
  (lambda ()
    (define-scheme-function 'max
      '(define max (lambda (s)
                     (cond
                       ((null? (cdr s)) (car s))
                       (t (let ((rest-max (max (cdr s))))
                            (cond
                              ((> (car s) rest-max) (car s))
                              (t rest-max))))))))
    (let ((test-results (run-function-test-cases 'max
                          '(((5 14 -3) 14)
                            ((42) 42)
                            ((-10 -5 -20) -5)
                            ((100 50 75 25) 100)
                            ((1 1 1 1 1) 1)
                            ((-100 -200) -100)))))
      (check-problem-with-tests 7 test-results))))

;; Test problem 8
(run-test "Problem 8: POCKET (closures with state)"
  (lambda ()
    (define-scheme-function 'pocket
      '(define pocket
         (letrec
           ((generator (lambda (x)
                         (lambda (y)
                           (cond
                             ((null? y) x)
                             (t (generator y)))))))
           (generator 8))))
    (format t "Testing closure state preservation...~%")
    (let* ((pocket-fn (lookup-function 'pocket *global-env*))
           (all-passed t)
           (*eval-fuel* 1000)
           (result1 (scheme-apply pocket-fn (list nil))))

      ;; Test 1: Initial pocket returns 8
      (unless (assert-number-equal result1 8 "  (POCKET NIL)")
        (setf all-passed nil))

      ;; Test 2-7: Creating nested pockets
      (let* ((*eval-fuel* 1000)
             (newpocket (scheme-apply pocket-fn (list 12))))
        (format t "  (POCKET 12) returns function: ")
        (if (scheme-function-p newpocket)
            (progn
              (format t "✓~%")
              (let* ((*eval-fuel* 1000)
                     (val2 (scheme-apply newpocket (list nil))))
                (unless (assert-number-equal val2 12 "  (NEWPOCKET NIL)")
                  (setf all-passed nil))

                (let* ((*eval-fuel* 1000)
                       (thirdpocket (scheme-apply newpocket (list 3))))
                  (when (scheme-function-p thirdpocket)
                    (let* ((*eval-fuel* 1000)
                           (val3 (scheme-apply thirdpocket (list nil)))
                           (*eval-fuel* 1000)
                           (val2-again (scheme-apply newpocket (list nil)))
                           (*eval-fuel* 1000)
                           (val1-again (scheme-apply pocket-fn (list nil))))
                      (unless (assert-number-equal val3 3 "  (THIRDPOCKET NIL)")
                        (setf all-passed nil))
                      (format t "  State preservation test: ")
                      (if (and (= val2-again 12) (= val1-again 8))
                          (format t "✓~%")
                          (progn
                            (format t "✗ (expected newpocket=12, pocket=8; got ~a, ~a)~%"
                                    val2-again val1-again)
                            (setf all-passed nil))))))))
            (progn
              (format t "✗ (expected function)~%")
              (setf all-passed nil))))

      (check-problem-with-tests 8 all-passed))))

;; Test environment display
(run-test "Environment display (:e command)"
  (lambda ()
    (format t "Setting up test environment...~%")
    (let ((*eval-fuel* 1000))
      (scheme-eval '(define y 100) *global-env*)
      (scheme-eval '(define mylist (quote (a b c))) *global-env*)
      (scheme-eval '(define square (lambda (n) (* n n))) *global-env*))

    (format t "Displaying environment:~%")
    (display-environment *global-env*)

    (format t "~%Testing let with nested environment:~%")
    (let* ((*eval-fuel* 1000)
           (result (scheme-eval '(let ((a 1) (b 2)) (+ a b)) *global-env*)))
      (format t "  (let ((a 1) (b 2)) (+ a b)) = ")
      (scheme-print result)
      (terpri)
      (and (numberp result) (= result 3)))))

;; Print test summary
(print-test-summary)
