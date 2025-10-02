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
    (format t "Defining: (define twentyseven 27)~%")
    (let ((*eval-fuel* 1000))
      (scheme-eval '(define twentyseven 27) *global-env*))
    (format t "Checking solution...~%")
    (check-problem 2)))

;; Test problem 3
(run-test "Problem 3: CAT and DOG (EQUAL? but not EQV?)"
  (lambda ()
    (format t "Defining CAT and DOG...~%")
    (let ((*eval-fuel* 1000))
      (scheme-eval '(define tail (quote (end))) *global-env*)
      (scheme-eval '(define cat (cons (quote head) tail)) *global-env*)
      (scheme-eval '(define dog (cons (quote head) tail)) *global-env*))
    (format t "Checking solution...~%")
    (check-problem 3)))

;; Test problem 4
(run-test "Problem 4: ABS (absolute value)"
  (lambda ()
    (format t "Defining ABS function...~%")
    (let ((*eval-fuel* 1000))
      (scheme-eval '(define abs (lambda (val)
                                  (cond
                                    ((> val 0) val)
                                    ((< val 0) (- 0 val))
                                    (t 0))))
                   *global-env*))
    (format t "Checking solution...~%")
    (check-problem 4)))

;; Test problem 5
(run-test "Problem 5: SUM (recursive list sum)"
  (lambda ()
    (format t "Defining SUM function...~%")
    (let ((*eval-fuel* 1000))
      (scheme-eval '(define sum (lambda (s)
                                  (cond
                                    ((null? s) 0)
                                    (t (+ (car s) (sum (cdr s)))))))
                   *global-env*))
    (format t "Testing with multiple inputs...~%")
    (let ((tests (list
                  '((8 2 3) 13)
                  '(() 0)
                  '((5) 5)
                  '((10 -5 7 -2) 10)
                  '((1 1 1 1 1) 5)))
          (all-passed t))
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
              (progn
                (format t " ✗ (expected ~a)~%" expected)
                (setf all-passed nil)))))
      (format t "Checking with game's check-problem function:~%")
      (and all-passed (check-problem 5)))))

;; Test problem 6
(run-test "Problem 6: MEGASUM (nested list sum)"
  (lambda ()
    (format t "Defining MEGASUM function...~%")
    (let ((*eval-fuel* 1000))
      (scheme-eval '(define megasum (lambda (s)
                                      (cond
                                        ((null? s) 0)
                                        ((list? (car s))
                                          (+ (megasum (car s)) (megasum (cdr s))))
                                        (t (+ (car s) (megasum (cdr s)))))))
                   *global-env*))
    (format t "Testing with multiple inputs...~%")
    (let ((tests (list
                  '(((8) 5 (2 () (9 1) 3)) 28)
                  '((1 2 3) 6)
                  '(((1 (2 (3)))) 6)
                  '(() 0)
                  '((10) 10)
                  '((5 (10 (15))) 30)))
          (all-passed t))
      (dolist (test tests)
        (let* ((input-list (first test))
               (expected (second test))
               (quoted-input (scheme-read-quote input-list))
               (*eval-fuel* 1000)
               (megasum-fn (multiple-value-bind (val found) (env-lookup 'megasum *global-env*)
                             (if found val (error "MEGASUM not defined"))))
               (result (scheme-apply megasum-fn (list quoted-input))))
          (format t "  (MEGASUM '~a) = ~a" input-list result)
          (if (and (numberp result) (= result expected))
              (format t " ✓~%")
              (progn
                (format t " ✗ (expected ~a)~%" expected)
                (setf all-passed nil)))))
      (format t "Checking with game's check-problem function:~%")
      (and all-passed (check-problem 6)))))

;; Test problem 7
(run-test "Problem 7: MAX (find maximum in list)"
  (lambda ()
    (format t "Defining MAX function...~%")
    (let ((*eval-fuel* 1000))
      (scheme-eval '(define max (lambda (s)
                                  (cond
                                    ((null? (cdr s)) (car s))
                                    (t (let ((rest-max (max (cdr s))))
                                         (cond
                                           ((> (car s) rest-max) (car s))
                                           (t rest-max)))))))
                   *global-env*))
    (format t "Testing with multiple inputs...~%")
    (let ((tests (list
                  '((5 14 -3) 14)
                  '((42) 42)
                  '((-10 -5 -20) -5)
                  '((100 50 75 25) 100)
                  '((1 1 1 1 1) 1)
                  '((-100 -200) -100)))
          (all-passed t))
      (dolist (test tests)
        (let* ((input-list (first test))
               (expected (second test))
               (quoted-input (scheme-read-quote input-list))
               (*eval-fuel* 1000)
               (max-fn (multiple-value-bind (val found) (env-lookup 'max *global-env*)
                         (if found val (error "MAX not defined"))))
               (result (scheme-apply max-fn (list quoted-input))))
          (format t "  (MAX '~a) = ~a" input-list result)
          (if (and (numberp result) (= result expected))
              (format t " ✓~%")
              (progn
                (format t " ✗ (expected ~a)~%" expected)
                (setf all-passed nil)))))
      (format t "Checking with game's check-problem function:~%")
      (and all-passed (check-problem 7)))))

;; Test problem 8
(run-test "Problem 8: POCKET (closures with state)"
  (lambda ()
    (format t "Defining POCKET function using LETREC...~%")
    (let ((*eval-fuel* 1000))
      (scheme-eval '(define pocket
                      (letrec
                        ((generator (lambda (x)
                                      (lambda (y)
                                        (cond
                                          ((null? y) x)
                                          (t (generator y)))))))
                        (generator 8)))
                   *global-env*))
    (format t "Testing closure state preservation...~%")
    (multiple-value-bind (pocket-fn found) (env-lookup 'pocket *global-env*)
      (unless found
        (error "POCKET not defined"))

      (let ((all-passed t))
        ;; Test 1: Initial pocket returns 8
        (format t "  (POCKET NIL) = ")
        (let* ((*eval-fuel* 1000)
               (result (scheme-apply pocket-fn (list nil))))
          (scheme-print result)
          (if (and (numberp result) (= result 8))
              (format t " ✓~%")
              (progn
                (format t " ✗ (expected 8)~%")
                (setf all-passed nil))))

        ;; Test 2-7: Creating nested pockets
        (let* ((*eval-fuel* 1000)
               (newpocket (scheme-apply pocket-fn (list 12))))
          (format t "  (POCKET 12) returns function: ")
          (if (scheme-function-p newpocket)
              (progn
                (format t "✓~%")
                (let* ((*eval-fuel* 1000)
                       (val2 (scheme-apply newpocket (list nil))))
                  (format t "  (NEWPOCKET NIL) = ")
                  (scheme-print val2)
                  (if (and (numberp val2) (= val2 12))
                      (format t " ✓~%")
                      (progn
                        (format t " ✗ (expected 12)~%")
                        (setf all-passed nil))))

                (let* ((*eval-fuel* 1000)
                       (thirdpocket (scheme-apply newpocket (list 3))))
                  (when (scheme-function-p thirdpocket)
                    (let* ((*eval-fuel* 1000)
                           (val3 (scheme-apply thirdpocket (list nil)))
                           (*eval-fuel* 1000)
                           (val2-again (scheme-apply newpocket (list nil)))
                           (*eval-fuel* 1000)
                           (val1-again (scheme-apply pocket-fn (list nil))))
                      (format t "  (THIRDPOCKET NIL) = ~a" val3)
                      (if (and (numberp val3) (= val3 3))
                          (format t " ✓~%")
                          (progn
                            (format t " ✗ (expected 3)~%")
                            (setf all-passed nil)))
                      (format t "  State preservation test: ")
                      (if (and (= val2-again 12) (= val1-again 8))
                          (format t "✓~%")
                          (progn
                            (format t "✗~%")
                            (setf all-passed nil)))))))
              (progn
                (format t "✗~%")
                (setf all-passed nil))))

        (format t "Checking with game's check-problem function:~%")
        (and all-passed (check-problem 8))))))

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
