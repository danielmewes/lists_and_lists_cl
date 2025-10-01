;;;; Test Problem 8 - POCKET function with closures

(load "lists-and-lists.lisp")

(in-package :lists-and-lists)

(init-global-env)

(format t "~%=== Testing Problem 8: POCKET (Closures) ===~%~%")

(format t "Problem: Define POCKET that uses closures to store values.~%")
(format t "  (POCKET NIL) => 8~%")
(format t "  (POCKET 12) => [function]~%")
(format t "  Multiple pocket functions should maintain separate state.~%~%")

(format t "Solution using LETREC:~%")
(format t "(define pocket~%")
(format t "  (letrec~%")
(format t "    ((generator (lambda (x)~%")
(format t "      (lambda (y)~%")
(format t "        (cond~%")
(format t "          ((null? y) x)~%")
(format t "          (t (generator y)))))))~%")
(format t "    (generator 8)))~%~%")

;; Define the solution
(let* ((*eval-fuel* 1000))
  (scheme-eval '(define pocket
                  (letrec
                    ((generator (lambda (x)
                                  (lambda (y)
                                    (cond
                                      ((null? y) x)
                                      (t (generator y)))))))
                    (generator 8)))
               *global-env*))

(format t "Testing...~%~%")

;; Get the pocket function
(multiple-value-bind (pocket-fn found) (env-lookup 'pocket *global-env*)
  (unless found
    (error "POCKET not defined"))

  ;; Test 1: Initial pocket returns 8
  (format t "Test 1: (POCKET NIL) = ")
  (let* ((*eval-fuel* 1000)
         (result (scheme-apply pocket-fn (list nil))))
    (scheme-print result)
    (if (and (numberp result) (= result 8))
        (format t " ✓~%")
        (format t " ✗ (expected 8)~%")))

  ;; Test 2: Creating a new pocket with value 12
  (format t "Test 2: (POCKET 12) = ")
  (let* ((*eval-fuel* 1000)
         (newpocket (scheme-apply pocket-fn (list 12))))
    (scheme-print newpocket)
    (if (scheme-function-p newpocket)
        (progn
          (format t " ✓~%")

          ;; Test 3: New pocket returns 12
          (format t "Test 3: (NEWPOCKET NIL) = ")
          (let* ((*eval-fuel* 1000)
                 (result (scheme-apply newpocket (list nil))))
            (scheme-print result)
            (if (and (numberp result) (= result 12))
                (format t " ✓~%")
                (format t " ✗ (expected 12)~%")))

          ;; Test 4: Create third pocket with value 3
          (format t "Test 4: (NEWPOCKET 3) = ")
          (let* ((*eval-fuel* 1000)
                 (thirdpocket (scheme-apply newpocket (list 3))))
            (scheme-print thirdpocket)
            (if (scheme-function-p thirdpocket)
                (progn
                  (format t " ✓~%")

                  ;; Test 5: Third pocket returns 3
                  (format t "Test 5: (THIRDPOCKET NIL) = ")
                  (let* ((*eval-fuel* 1000)
                         (result (scheme-apply thirdpocket (list nil))))
                    (scheme-print result)
                    (if (and (numberp result) (= result 3))
                        (format t " ✓~%")
                        (format t " ✗ (expected 3)~%")))

                  ;; Test 6: Second pocket still returns 12
                  (format t "Test 6: (NEWPOCKET NIL) still = ")
                  (let* ((*eval-fuel* 1000)
                         (result (scheme-apply newpocket (list nil))))
                    (scheme-print result)
                    (if (and (numberp result) (= result 12))
                        (format t " ✓~%")
                        (format t " ✗ (expected 12)~%")))

                  ;; Test 7: Original pocket still returns 8
                  (format t "Test 7: (POCKET NIL) still = ")
                  (let* ((*eval-fuel* 1000)
                         (result (scheme-apply pocket-fn (list nil))))
                    (scheme-print result)
                    (if (and (numberp result) (= result 8))
                        (format t " ✓~%")
                        (format t " ✗ (expected 8)~%"))))
                (format t " ✗ (expected function)~%"))))
        (format t " ✗ (expected function)~%"))))

(format t "~%Checking with game's check-problem function:~%")
(check-problem 8)

(format t "~%=== Test Complete ===~%")
(format t "~%This demonstrates proper lexical closures and static scoping!~%")
