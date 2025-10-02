;;;; Lists and Lists - A Common Lisp Port
;;;; Copyright 1996 by Andrew Plotkin (original Z-machine version)
;;;; Common Lisp port 2025
;;;;
;;;; An Interactive Tutorial for learning Scheme/Lisp

(defpackage :lists-and-lists
  (:use :cl)
  (:export #:play-game))

(in-package :lists-and-lists)

;;; ============================================================================
;;; SCHEME INTERPRETER CORE
;;; ============================================================================

;;; Data structures for Scheme values

(defstruct scheme-atom
  name)

(defstruct scheme-cons
  car
  cdr)

(defstruct scheme-function
  params
  body
  env)

(defstruct scheme-builtin
  name
  fn)

;;; Environment handling

(defun make-env (&optional parent)
  (cons 'env (cons parent nil)))

(defun env-parent (env)
  (cadr env))

(defun env-lookup (sym env)
  (cond
    ((null env) (values nil nil))
    ((eq (car env) 'env)
     (let ((pair (assoc sym (cddr env))))
       (if pair
           (values (cdr pair) t)
           (env-lookup sym (env-parent env)))))
    (t (values nil nil))))

(defun env-define (sym val env)
  (let ((pair (assoc sym (cddr env))))
    (if pair
        (setf (cdr pair) val)
        (push (cons sym val) (cddr env))))
  val)

(defun env-set! (sym val env)
  (cond
    ((null env) (error "Undefined variable: ~a" sym))
    ((eq (car env) 'env)
     (let ((pair (assoc sym (cddr env))))
       (if pair
           (setf (cdr pair) val)
           (env-set! sym val (env-parent env)))))))

;;; Global environment with built-in functions

(defvar *global-env* nil)
(defvar *eval-fuel* 1000)

(defun init-global-env ()
  (let ((env (make-env)))
    ;; Arithmetic
    (env-define '+ (make-scheme-builtin :name '+
                                        :fn (lambda (args) (apply #'+ args)))
                env)
    (env-define '- (make-scheme-builtin :name '-
                                        :fn (lambda (args) (apply #'- args)))
                env)
    (env-define '* (make-scheme-builtin :name '*
                                        :fn (lambda (args) (apply #'* args)))
                env)

    ;; Comparisons
    (env-define '> (make-scheme-builtin :name '>
                                        :fn (lambda (args) (apply #'> args)))
                env)
    (env-define '< (make-scheme-builtin :name '<
                                        :fn (lambda (args) (apply #'< args)))
                env)
    (env-define '= (make-scheme-builtin :name '=
                                        :fn (lambda (args) (apply #'= args)))
                env)

    ;; List operations
    (env-define 'car (make-scheme-builtin :name 'car
                                          :fn (lambda (args)
                                                (scheme-cons-car (first args))))
                env)
    (env-define 'cdr (make-scheme-builtin :name 'cdr
                                          :fn (lambda (args)
                                                (scheme-cons-cdr (first args))))
                env)
    (env-define 'cons (make-scheme-builtin :name 'cons
                                           :fn (lambda (args)
                                                 (make-scheme-cons :car (first args)
                                                                   :cdr (second args))))
                env)

    ;; Predicates
    (env-define 'null? (make-scheme-builtin :name 'null?
                                            :fn (lambda (args) (null (first args))))
                env)
    (env-define 'list? (make-scheme-builtin :name 'list?
                                            :fn (lambda (args)
                                                  (or (null (first args))
                                                      (scheme-cons-p (first args)))))
                env)
    (env-define 'number? (make-scheme-builtin :name 'number?
                                              :fn (lambda (args) (numberp (first args))))
                env)
    (env-define 'atom? (make-scheme-builtin :name 'atom?
                                            :fn (lambda (args)
                                                  (scheme-atom-p (first args))))
                env)

    ;; Equality
    (env-define 'eq? (make-scheme-builtin :name 'eq?
                                          :fn (lambda (args) (eq (first args) (second args))))
                env)
    (env-define 'eqv? (make-scheme-builtin :name 'eqv?
                                           :fn (lambda (args) (eql (first args) (second args))))
                env)
    (env-define 'equal? (make-scheme-builtin :name 'equal?
                                             :fn (lambda (args) (scheme-equal (first args) (second args))))
                env)

    ;; Special constants
    (env-define 't t env)
    (env-define 'nil nil env)

    (setf *global-env* env)))

(defun scheme-equal (a b)
  (cond
    ((and (scheme-cons-p a) (scheme-cons-p b))
     (and (scheme-equal (scheme-cons-car a) (scheme-cons-car b))
          (scheme-equal (scheme-cons-cdr a) (scheme-cons-cdr b))))
    ((and (numberp a) (numberp b)) (= a b))
    ((and (scheme-atom-p a) (scheme-atom-p b))
     (eq (scheme-atom-name a) (scheme-atom-name b)))
    (t (eql a b))))

;;; Evaluator

(defun scheme-eval (expr env)
  (when (<= *eval-fuel* 0)
    (error "Evaluation exceeded fuel limit (possible infinite loop)"))
  (decf *eval-fuel*)

  (cond
    ;; Self-evaluating
    ((numberp expr) expr)
    ((null expr) nil)
    ((eq expr t) t)

    ;; Variables
    ((symbolp expr)
     (multiple-value-bind (val found) (env-lookup expr env)
       (if found
           val
           (error "Undefined variable: ~a" expr))))

    ;; Special forms and function calls
    ((consp expr)
     (let ((op (car expr))
           (args (cdr expr)))
       (cond
         ;; Quote
         ((eq op 'quote)
          (scheme-read-quote (car args)))

         ;; Define
         ((eq op 'define)
          (let ((var (car args))
                (val (scheme-eval (cadr args) env)))
            (env-define var val env)))

         ;; Lambda
         ((eq op 'lambda)
          (make-scheme-function :params (car args)
                                :body (cadr args)
                                :env env))

         ;; If
         ((eq op 'if)
          (if (scheme-truthy (scheme-eval (first args) env))
              (scheme-eval (second args) env)
              (if (cddr args)
                  (scheme-eval (third args) env)
                  nil)))

         ;; Cond
         ((eq op 'cond)
          (scheme-eval-cond args env))

         ;; Let
         ((eq op 'let)
          (scheme-eval-let (first args) (second args) env))

         ;; Letrec
         ((eq op 'letrec)
          (scheme-eval-letrec (first args) (second args) env))

         ;; Function call
         (t
          (let ((fn (scheme-eval op env))
                (arg-vals (mapcar (lambda (arg) (scheme-eval arg env)) args)))
            (scheme-apply fn arg-vals))))))

    (t expr)))

(defun scheme-truthy (val)
  (not (null val)))

(defun scheme-eval-cond (clauses env)
  (when clauses
    (let* ((clause (car clauses))
           (test (car clause))
           (body (cadr clause)))
      (if (or (eq test 't)
              (scheme-truthy (scheme-eval test env)))
          (scheme-eval body env)
          (scheme-eval-cond (cdr clauses) env)))))

(defun scheme-eval-let (bindings body env)
  (let ((new-env (make-env env)))
    (dolist (binding bindings)
      (let ((var (car binding))
            (val (scheme-eval (cadr binding) env)))
        (env-define var val new-env)))
    (scheme-eval body new-env)))

(defun scheme-eval-letrec (bindings body env)
  (let ((new-env (make-env env)))
    ;; First define all variables as nil
    (dolist (binding bindings)
      (env-define (car binding) nil new-env))
    ;; Then evaluate and assign
    (dolist (binding bindings)
      (let ((var (car binding))
            (val (scheme-eval (cadr binding) new-env)))
        (env-define var val new-env)))
    (scheme-eval body new-env)))

(defun scheme-apply (fn args)
  (cond
    ((scheme-builtin-p fn)
     (funcall (scheme-builtin-fn fn) args))

    ((scheme-function-p fn)
     (let ((new-env (make-env (scheme-function-env fn))))
       ;; Bind parameters
       (loop for param in (scheme-function-params fn)
             for arg in args
             do (env-define param arg new-env))
       (scheme-eval (scheme-function-body fn) new-env)))

    (t (error "Cannot apply non-function: ~a" fn))))

;;; Reader

(defun scheme-read-quote (expr)
  "Convert a quoted s-expression to scheme data structures"
  (cond
    ((null expr) nil)
    ((numberp expr) expr)
    ((eq expr 't) t)
    ((symbolp expr) (make-scheme-atom :name expr))
    ((consp expr)
     (make-scheme-cons :car (scheme-read-quote (car expr))
                       :cdr (scheme-read-quote (cdr expr))))
    (t expr)))

;;; Printer

(defun scheme-print (obj &optional (stream t))
  (cond
    ((null obj) (format stream "nil"))
    ((eq obj t) (format stream "t"))
    ((numberp obj) (format stream "~a" obj))
    ((scheme-atom-p obj) (format stream "~a" (scheme-atom-name obj)))
    ((scheme-cons-p obj)
     (format stream "(")
     (scheme-print-list obj stream)
     (format stream ")"))
    ((scheme-function-p obj) (format stream "[function]"))
    ((scheme-builtin-p obj) (format stream "[builtin: ~a]" (scheme-builtin-name obj)))
    (t (format stream "~a" obj))))

(defun scheme-print-list (obj stream)
  (scheme-print (scheme-cons-car obj) stream)
  (let ((tail (scheme-cons-cdr obj)))
    (cond
      ((null tail))
      ((scheme-cons-p tail)
       (format stream " ")
       (scheme-print-list tail stream))
      (t
       (format stream " . ")
       (scheme-print tail stream)))))

;;; ============================================================================
;;; GAME STATE
;;; ============================================================================

(defvar *current-room* 'entry)
(defvar *genie-state* 0) ; 0=asleep, 1=awake, 2-9=tutorial problems
(defvar *genie-waiting* nil) ; t if genie asked a question
(defvar *alarm-box-used* nil)
(defvar *manual-available* nil)
(defvar *prize-won* nil)
(defvar *hint-problem* -1)
(defvar *hint-level* 0)

;;; ============================================================================
;;; GAME PROBLEMS
;;; ============================================================================

(defun problem-text (num)
  (ecase num
    (2 "Your first problem is just to acquaint you with the system. Start up the machine,
and define TWENTYSEVEN to have the value 27. You can ask me to 'check' when you're ready,
or 'repeat' the problem if you need me to.")

    (3 "Let's try creating some lists. Define values for CAT and DOG so that CAT and DOG
are EQUAL? but not EQV?. Furthermore, CDR(CAT) and CDR(DOG) must be EQV?.")

    (4 "Define ABS to be the absolute value function for integers. That is, (ABS 4) should
return 4; (ABS -5) should return 5; and (ABS 0) should return 0.")

    (5 "Define SUM to be a function that adds up a list of integers. So (SUM '(8 2 3))
should return 13. Make sure it works correctly for the empty list; (SUM NIL) should
return 0.")

    (6 "This problem is like the last one, but more general. Define MEGASUM to add up an
arbitrarily nested list of integers. That is, (MEGASUM '((8) 5 (2 () (9 1) 3))) should
return 28.")

    (7 "Define MAX to be a function that finds the maximum of a list of integers. So
(MAX '(5 14 -3)) should return 14. You can assume the list will have at least one term.")

    (8 "Last problem. You're going to define a function called POCKET. This function should
take one argument. Now pay attention here: POCKET does two different things, depending on
the argument. If you give it NIL as the argument, it should simply return 8. But if you
give POCKET any integer as an argument, it should return a new pocket function -- a function
just like POCKET, but with that new integer hidden inside, replacing the 8.

Examples:
  (POCKET NIL) => 8
  (POCKET 12) => [function]
  (DEFINE NEWPOCKET (POCKET 12)) => [function]
  (NEWPOCKET NIL) => 12
  (DEFINE THIRDPOCKET (NEWPOCKET 3)) => [function]
  (THIRDPOCKET NIL) => 3
  (NEWPOCKET NIL) => 12
  (POCKET NIL) => 8

Note that when you create a new pocket function, previously-existing functions should
keep working.")))

(defun check-problem (num)
  (handler-case
      (ecase num
        (2 (check-problem-2))
        (3 (check-problem-3))
        (4 (check-problem-4))
        (5 (check-problem-5))
        (6 (check-problem-6))
        (7 (check-problem-7))
        (8 (check-problem-8)))
    (error (e)
      (format t "~%The genie shakes his head. \"Something's wrong: ~a\"~%" e)
      nil)))

(defun check-problem-2 ()
  (multiple-value-bind (val found) (env-lookup 'twentyseven *global-env*)
    (if (and found (numberp val) (= val 27))
        (progn
          (format t "~%\"Aha! Very good.\"~%")
          t)
        (progn
          (format t "~%\"Nope; that's not 27. Try again.\"~%")
          nil))))

(defun check-problem-3 ()
  (multiple-value-bind (cat found1) (env-lookup 'cat *global-env*)
    (multiple-value-bind (dog found2) (env-lookup 'dog *global-env*)
      (cond
        ((not (and found1 found2))
         (format t "~%\"You need to define both CAT and DOG.\"~%")
         nil)
        ((not (scheme-equal cat dog))
         (format t "~%\"Oops -- that's not right. They should be EQUAL?.\"~%")
         nil)
        ((eql cat dog)
         (format t "~%\"Oops -- that's not right. They should not be EQV?.\"~%")
         nil)
        ((not (and (scheme-cons-p cat) (scheme-cons-p dog)))
         (format t "~%\"They need to be lists.\"~%")
         nil)
        ((not (eql (scheme-cons-cdr cat) (scheme-cons-cdr dog)))
         (format t "~%\"Nope. Remember that CDR(CAT) and CDR(DOG) must be EQV?.\"~%")
         nil)
        (t
         (format t "~%\"Perfect!\"~%")
         t)))))

(defun check-problem-4 ()
  (multiple-value-bind (abs-fn found) (env-lookup 'abs *global-env*)
    (if (not found)
        (progn
          (format t "~%\"You need to define ABS.\"~%")
          nil)
        (let ((test-cases '((4 4) (-5 5) (0 0) (17 17) (-23 23))))
          (dolist (test test-cases t)
            (let* ((input (first test))
                   (expected (second test))
                   (result (scheme-apply abs-fn (list input))))
              (unless (and (numberp result) (= result expected))
                (format t "~%\"Oops -- (ABS ~a) should be ~a, not ~a.\"~%"
                        input expected result)
                (return nil))))
          (format t "~%\"Very good.\"~%")
          t))))

(defun check-problem-5 ()
  (multiple-value-bind (sum-fn found) (env-lookup 'sum *global-env*)
    (if (not found)
        (progn
          (format t "~%\"You need to define SUM.\"~%")
          nil)
        (let ((test-cases (list
                          (list nil 0)
                          (list (scheme-read-quote '(5)) 5)
                          (list (scheme-read-quote '(8 2 3)) 13)
                          (list (scheme-read-quote '(10 -5 7 -2)) 10))))
          (dolist (test test-cases t)
            (let* ((input (first test))
                   (expected (second test))
                   (result (scheme-apply sum-fn (list input))))
              (unless (and (numberp result) (= result expected))
                (format t "~%\"Oops -- that's not right. The result should be ~a.\"~%"
                        expected)
                (return nil))))
          (format t "~%\"Seems to work.\"~%")
          t))))

(defun check-problem-6 ()
  (multiple-value-bind (megasum-fn found) (env-lookup 'megasum *global-env*)
    (if (not found)
        (progn
          (format t "~%\"You need to define MEGASUM.\"~%")
          nil)
        (let ((test-cases (list
                          (list (scheme-read-quote '((8) 5 (2 () (9 1) 3))) 28)
                          (list (scheme-read-quote '(1 2 3)) 6)
                          (list (scheme-read-quote '((1 (2 (3))))) 6)
                          (list nil 0))))
          (dolist (test test-cases t)
            (let* ((input (first test))
                   (expected (second test))
                   (result (scheme-apply megasum-fn (list input))))
              (unless (and (numberp result) (= result expected))
                (format t "~%\"Oops -- that's not right. The result should be ~a.\"~%"
                        expected)
                (return nil))))
          (format t "~%\"Seems to work.\"~%")
          t))))

(defun check-problem-7 ()
  (multiple-value-bind (max-fn found) (env-lookup 'max *global-env*)
    (if (not found)
        (progn
          (format t "~%\"You need to define MAX.\"~%")
          nil)
        (let ((test-cases (list
                          (list (scheme-read-quote '(5 14 -3)) 14)
                          (list (scheme-read-quote '(42)) 42)
                          (list (scheme-read-quote '(-10 -5 -20)) -5))))
          (dolist (test test-cases t)
            (let* ((input (first test))
                   (expected (second test))
                   (result (scheme-apply max-fn (list input))))
              (unless (and (numberp result) (= result expected))
                (format t "~%\"Oops -- that's not right. The result should be ~a.\"~%"
                        expected)
                (return nil))))
          (format t "~%\"Seems to work.\"~%")
          t))))

(defun check-problem-8 ()
  (multiple-value-bind (pocket-fn found) (env-lookup 'pocket *global-env*)
    (if (not found)
        (progn
          (format t "~%\"You need to define POCKET.\"~%")
          nil)
        (let ((val1 (scheme-apply pocket-fn (list nil)))
              (fn2 (scheme-apply pocket-fn (list 12))))
          (unless (and (numberp val1) (= val1 8))
            (format t "~%\"No; the initial pocket function should return 8 when given NIL.\"~%")
            (return-from check-problem-8 nil))
          (unless (scheme-function-p fn2)
            (format t "~%\"No; pocket should return a function when given an integer.\"~%")
            (return-from check-problem-8 nil))
          (let ((val2 (scheme-apply fn2 (list nil)))
                (fn3 (scheme-apply fn2 (list 3))))
            (unless (and (numberp val2) (= val2 12))
              (format t "~%\"No; the new pocket function should return 12 when given NIL.\"~%")
              (return-from check-problem-8 nil))
            (unless (scheme-function-p fn3)
              (format t "~%\"No; a pocket function should return another function.\"~%")
              (return-from check-problem-8 nil))
            (let ((val3 (scheme-apply fn3 (list nil)))
                  (val2-again (scheme-apply fn2 (list nil)))
                  (val1-again (scheme-apply pocket-fn (list nil))))
              (unless (and (numberp val3) (= val3 3))
                (format t "~%\"No; the third pocket function should return 3.\"~%")
                (return-from check-problem-8 nil))
              (unless (and (numberp val2-again) (= val2-again 12))
                (format t "~%\"No; the second pocket function should still return 12.\"~%")
                (return-from check-problem-8 nil))
              (unless (and (numberp val1-again) (= val1-again 8))
                (format t "~%\"No; the original pocket function should still return 8.\"~%")
                (return-from check-problem-8 nil))
              (format t "~%\"Perfect.\"~%")
              t))))))

;;; ============================================================================
;;; GAME INTERFACE
;;; ============================================================================

(defun describe-room ()
  (ecase *current-room*
    (entry
     (format t "~%A Familiar Place~%")
     (format t "Everything here is just like it always is, except for that door.~%")
     (format t "~%You can see a strange door to the north.~%"))

    (lab
     (format t "~%White Room~%")
     (format t "This is a comfortably cluttered room. Cluttered with bookshelves, mostly.~%")
     (format t "To one side is a large desk, on which a computer squats regally.~%")
     (when (< *genie-state* 9)
       (format t "A lumpy couch is the only other furniture of note.~%"))
     (format t "~%You can see:~%")
     (when (< *genie-state* 9)
       (if (= *genie-state* 0)
           (format t "  a huge genie (sleeping on the couch)~%")
           (format t "  a huge genie (on the couch)~%")))
     (format t "  a computer (with green and yellow buttons)~%")
     (when (and (= *genie-state* 0) (not *alarm-box-used*))
       (format t "  a small glass box~%"))
     (when *manual-available*
       (format t "  a massive book~%"))
     (when *prize-won*
       (format t "  a gold plaque~%")))))

(defun game-loop ()
  (format t "~%Lists And Lists~%")
  (format t "An Interactive Tutorial~%")
  (format t "Copyright 1996 by Andrew Plotkin~%")
  (format t "(Common Lisp port 2025)~%")
  (format t "~%(First-time players should type 'about')~%~%")

  (format t "Hey, that door wasn't there last time you walked by this spot. What the heck?~%")

  (describe-room)

  (loop
    (format t "~%> ")
    (finish-output)
    (let* ((input (read-line *standard-input* nil))
           (words (and input (parse-command input))))
      (cond
        ((null input)
         (return))

        ((null words)
         (format t "I beg your pardon?~%"))

        (t
         (let ((cmd (first words))
               (rest (rest words)))
           (cond
            ((member cmd '("quit" "q") :test #'string-equal)
             (format t "~%Thanks for playing!~%")
             (return))

            ((member cmd '("look" "l") :test #'string-equal)
             (describe-room))

            ((member cmd '("north" "n") :test #'string-equal)
             (cmd-go-north))

            ((member cmd '("south" "s") :test #'string-equal)
             (cmd-go-south))

            ((member cmd '("examine" "x") :test #'string-equal)
             (cmd-examine (first rest)))

            ((member cmd '("take" "get") :test #'string-equal)
             (cmd-take (first rest)))

            ((string-equal cmd "break")
             (cmd-break (first rest)))

            ((string-equal cmd "push")
             (cmd-push (first rest)))

            ((string-equal cmd "run")
             (cmd-run-interpreter))

            ((string-equal cmd "reset")
             (cmd-reset-interpreter))

            ((member cmd '("yes" "y") :test #'string-equal)
             (cmd-yes))

            ((member cmd '("no" "n") :test #'string-equal)
             (cmd-no))

            ((member cmd '("check") :test #'string-equal)
             (cmd-check))

            ((member cmd '("repeat" "problem") :test #'string-equal)
             (cmd-repeat))

            ((member cmd '("help" "hint") :test #'string-equal)
             (cmd-help))

            ((string-equal cmd "about")
             (cmd-about))

            ((string-equal cmd "manual")
             (cmd-manual))

            (t
             (format t "I don't understand that command.~%")
             (format t "(Try: look, north, south, examine, push, run, check, help, quit)~%")))))))))

(defun split-string (string separator)
  "Simple string splitter"
  (let ((parts nil)
        (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) separator)
          do (when (> i start)
               (push (subseq string start i) parts))
             (setf start (1+ i)))
    (when (< start (length string))
      (push (subseq string start) parts))
    (nreverse parts)))

(defun parse-command (str)
  (let ((words (split-string (string-trim " " str) #\Space)))
    (remove-if (lambda (s) (string= s "")) words)))

(defun cmd-go-north ()
  (if (eq *current-room* 'entry)
      (progn
        (setf *current-room* 'lab)
        (describe-room))
      (format t "You can't go that way.~%")))

(defun cmd-go-south ()
  (if (eq *current-room* 'lab)
      (if (>= *genie-state* 9)
          (progn
            (format t "~%You step back through the door...~%")
            (format t "~%*** You have won ***~%")
            (format t "~%Thanks for playing!~%")
            (return-from cmd-go-south t))
          (format t "Leaving so soon?~%"))
      (format t "You ARE outside.~%")))

(defun cmd-examine (what)
  (cond
    ((null what)
     (format t "Examine what?~%"))
    ((string-equal what "door")
     (format t "The door to the north is ancient, stained, knotted wood.~%"))
    ((member what '("genie") :test #'string-equal)
     (cmd-examine-genie))
    ((member what '("computer" "machine") :test #'string-equal)
     (format t "The computer has two buttons: a green \"run\" button and a yellow \"reset\" button.~%"))
    ((member what '("box" "glass") :test #'string-equal)
     (if (and (= *genie-state* 0) (not *alarm-box-used*))
         (format t "It's a small cube of frosted glass. Neatly etched on one side are the words
\"Break glass to wake owner.\" Something turns slowly inside the box...~%")
         (format t "You don't see that here.~%")))
    ((member what '("book" "manual") :test #'string-equal)
     (if *manual-available*
         (cmd-manual)
         (format t "You don't see that here.~%")))
    ((member what '("plaque") :test #'string-equal)
     (if *prize-won*
         (format t "It's a plate of thin gold, engraved with angular designs. In the center
you see the words \"*** You have won ***\"~%")
         (format t "You don't see that here.~%")))
    (t
     (format t "You don't see that here.~%"))))

(defun cmd-examine-genie ()
  (if (< *genie-state* 9)
      (progn
        (format t "You always thought genies were folklore, but now that you've encountered one
you find you really can't mistake it. He's eight feet tall, bright shimmering bronze,
absolutely covered with tasteless wrought-gold jewelry, and he smells of ozone.~%")
        (when (= *genie-state* 0)
          (format t "He's also quite dead to the world, snoring like mad on the lumpy couch.~%")))
      (format t "The genie has departed.~%")))

(defun cmd-take (what)
  (format t "That's not important right now.~%"))

(defun cmd-break (what)
  (if (and (member what '("box" "glass") :test #'string-equal)
           (= *genie-state* 0)
           (not *alarm-box-used*))
      (progn
        (setf *alarm-box-used* t)
        (setf *genie-state* 1)
        (setf *genie-waiting* t)
        (format t "~%You turn the box over carefully, then shrug and swing it sharply...~%")
        (format t "~%\"No no don't break it I'm awake!\"~%")
        (format t "A gleaming hand catches your wrist. The genie gently -- very gently --
removes the box from your grasp, and tucks it carefully away into nothing.~%")
        (format t "~%The genie looks you over, squinching his face in a manner to which mere
mortal flesh could not aspire. \"So. You're here to learn, are you?\"~%"))
      (format t "Break what?~%")))

(defun cmd-push (what)
  (cond
    ((member what '("green" "run") :test #'string-equal)
     (cmd-run-interpreter))
    ((member what '("yellow" "reset") :test #'string-equal)
     (cmd-reset-interpreter))
    (t
     (format t "Push what?~%"))))

(defun cmd-run-interpreter ()
  (when (null *global-env*)
    (init-global-env))

  (format t "~%The computer comes to life: whirr, feeple, feep! You settle yourself
before the keyboard as text appears on the screen...~%")
  (format t "~%[Welcome to the interpreter. Enter :q to exit, or :m for documentation,
or :? for a list of other : commands.]~%")

  (run-interpreter)

  (format t "~%[Suspending interpreter. Type 'run' to reactivate.]~%")
  (when (and (>= *genie-state* 2) (<= *genie-state* 8))
    (setf *genie-waiting* t)
    (format t "~%You lean back. The genie glances over, and asks, \"Got it working yet?\"~%")))

(defun cmd-reset-interpreter ()
  (setf *global-env* nil)
  (format t "~%[Interpreter reset.]~%"))

(defun display-environment (env &optional (indent 0))
  "Display all bindings in the environment"
  (when env
    (when (eq (car env) 'env)
      (let ((parent (cadr env))
            (bindings (cddr env)))
        ;; Display bindings in this frame
        (when bindings
          (if (= indent 0)
              (format t "[Current environment:]~%")
              (format t "[Parent environment ~d:]~%" indent))
          (dolist (binding bindings)
            (let ((sym (car binding))
                  (val (cdr binding)))
              (format t "  ~a = " sym)
              (scheme-print val)
              (terpri)))
          (terpri))
        ;; Recursively display parent
        (when parent
          (display-environment parent (1+ indent)))))))

(defun run-interpreter ()
  (loop
    (format t "~%>> ")
    (finish-output)
    (let ((line (read-line *standard-input* nil)))
      (when (null line)
        (return))

      (cond
        ((string= line ":q")
         (return))

        ((string= line ":?")
         (format t "[The following codes have special meaning at the >> prompt:~%")
         (format t "  :?  Print this list.~%")
         (format t "  :q  Leave the interpreter.~%")
         (format t "  :m  Read the manual.~%")
         (format t "  :r  Redisplay the current problem.~%")
         (format t "  :c  Cancel the expression you are typing.~%")
         (format t "  :e  Display everything in the current environment.]~%"))

        ((string= line ":m")
         (cmd-manual))

        ((string= line ":r")
         (if (and (>= *genie-state* 2) (<= *genie-state* 8))
             (format t "~%~a~%" (problem-text *genie-state*))
             (format t "[No problem has been posed.]~%")))

        ((string= line ":c")
         (format t "[Cancelled.]~%"))

        ((string= line ":e")
         (display-environment *global-env*))

        (t
         (handler-case
             (let* ((*package* (find-package :lists-and-lists))
                    (expr (read-from-string line nil))
                    (*eval-fuel* 1000)
                    (result (scheme-eval expr *global-env*)))
               (format t " ")
               (scheme-print result)
               (terpri))
           (end-of-file ()
             (format t "[Incomplete expression]~%"))
           (error (e)
             (format t "[Error: ~a]~%" e))))))))

(defun cmd-yes ()
  (cond
    ((= *genie-state* 0)
     (format t "The genie, unconscious, quite ignores you.~%"))

    ((= *genie-state* 1)
     (setf *genie-state* 2)
     (setf *genie-waiting* nil)
     (setf *manual-available* t)
     (format t "~%The genie nods in satisfaction. \"Right. Let's see, let's see...\"~%")
     (format t "He pulls a massive tome out of nowhere; opens it; pokes studiously at it;
turns a page; snorts. Then he arises from the couch to his full height, raises the book,
and booms...~%")
     (format t "~%\"HOW TO PROGRAM IN LISP!\"~%")
     (format t "~%Then he plops back into the couch, and adds, \"...a self-paced course.\"
He hands you the book.~%")
     (format t "~%~a~%" (problem-text 2)))

    ((and (>= *genie-state* 2) (<= *genie-state* 8))
     (if *genie-waiting*
         (progn
           (setf *genie-waiting* nil)
           (if (check-problem *genie-state*)
               (progn
                 (incf *genie-state*)
                 (if (> *genie-state* 8)
                     (progn
                       (setf *prize-won* t)
                       (format t "~%\"Congratulations,\" the genie booms. \"You are now an accredited
hacker of Lisp.\" He hands you something. \"I'll let you keep playing with the machine.
I,\" he adds with sudden intensity, \"am going to return to my nap.\"~%")
                       (format t "~%The genie vanishes in a puff of silver smoke. A moment later,
the couch follows.~%"))
                     (format t "~%~a~%" (problem-text *genie-state*))))
               (format t "~%(Try again!)~%")))
         (format t "\"What?\"~%")))

    (t
     (format t "\"What?\"~%"))))

(defun cmd-no ()
  (cond
    ((= *genie-state* 0)
     (format t "The genie, unconscious, quite ignores you.~%"))

    ((= *genie-state* 1)
     (setf *genie-state* 0)
     (setf *genie-waiting* nil)
     (format t "The genie frowns thunderously. \"Fine, go play around on your own. See where
it gets you. Wake me when you're tired of wasting time.\" He turns over, and begins
snoring. Thunderously.~%"))

    ((and (>= *genie-state* 2) (<= *genie-state* 8) *genie-waiting*)
     (setf *genie-waiting* nil)
     (format t "\"Tell me when you're ready, then.\"~%"))

    (t
     (format t "\"What?\"~%"))))

(defun cmd-check ()
  (if (and (>= *genie-state* 2) (<= *genie-state* 8))
      (cmd-yes)
      (format t "Check what?~%")))

(defun cmd-repeat ()
  (if (and (>= *genie-state* 2) (<= *genie-state* 8))
      (format t "~%~a~%" (problem-text *genie-state*))
      (if (= *genie-state* 1)
          (format t "\"I thought the question was simple enough. Are you interested in learning
what I have to teach? Yes or no will do.\"~%")
          (format t "\"What problem?\"~%"))))

(defun cmd-help ()
  (cond
    ((eq *current-room* 'entry)
     (format t "Consider going inside.~%"))

    ((= *genie-state* 0)
     (if *alarm-box-used*
         (format t "Check out the box.~%")
         (format t "Check out the desk.~%")))

    ((> *genie-state* 8)
     (format t "Read the plaque.~%"))

    ((= *genie-state* 1)
     (format t "The genie is your guide.~%"))

    ((and (>= *genie-state* 2) (<= *genie-state* 8))
     (give-hint *genie-state*))))

(defun give-hint (problem)
  (when (= *hint-problem* -1)
    (setf *hint-problem* 0)
    (format t "The genie glowers hugely at you. \"Sigh. Yes, I do give hints. I am required
to tell you, blah blah blah, irreparable loss of fun, blah blah, no refunds, fine. So if
you still want help, ask again. If any hint I give isn't enough, ask again.\"~%")
    (return-from give-hint))

  (when (/= *hint-problem* problem)
    (setf *hint-problem* problem)
    (setf *hint-level* 0))

  (incf *hint-level*)

  ;; Simplified hints - just provide the basic guidance
  (ecase problem
    (2
     (case *hint-level*
       (1 (format t "\"Have you read up through chapter 6 of the manual?\"~%"))
       (2 (format t "\"You need to use the DEFINE command.\"~%"))
       (t (format t "\"Do this: (DEFINE TWENTYSEVEN 27)\"~%"))))

    (3
     (case *hint-level*
       (1 (format t "\"Have you read up through chapter 10 of the manual?\"~%"))
       (2 (format t "\"If you define CAT and DOG to be identical lists, that will satisfy
the first condition. They will be EQUAL?, but since they are created in two separate places,
they will not be EQV?.\"~%"))
       (t (format t "\"Define a single list to be the cdr for both of them, then use CONS to
attach atoms to it. Do this:
  (DEFINE TAIL '(END))
  (DEFINE CAT (CONS 'HEAD TAIL))
  (DEFINE DOG (CONS 'HEAD TAIL))\"~%"))))

    (4
     (case *hint-level*
       (1 (format t "\"Have you read up through chapter 12 of the manual?\"~%"))
       (2 (format t "\"You can modify the example from chapter 11.\"~%"))
       (t (format t "\"Use LAMBDA and COND with tests for positive, negative, and zero.\"~%"))))

    (5
     (case *hint-level*
       (1 (format t "\"Have you read up through chapter 13 of the manual?\"~%"))
       (2 (format t "\"Use recursion, like the LAST example in chapter 13.\"~%"))
       (3 (format t "\"The base case is when the list is empty; then return 0.\"~%"))
       (t (format t "\"If the list is not empty, add the first term to the sum of the rest.\"~%"))))

    (6
     (case *hint-level*
       (1 (format t "\"You can build MEGASUM the same way you built SUM, with one change.\"~%"))
       (2 (format t "\"The change is that the first term might be a list instead of a number.\"~%"))
       (t (format t "\"Use COND and test the first term with LIST?. If it is a list, call
MEGASUM recursively to add it up.\"~%"))))

    (7
     (case *hint-level*
       (1 (format t "\"Have you read up through chapter 14 of the manual?\"~%"))
       (2 (format t "\"Consider using LET. Look at the first term, look at MAX of the remaining
terms, choose the larger.\"~%"))
       (t (format t "\"The base case is a one-term list, not an empty list.\"~%"))))

    (8
     (case *hint-level*
       (1 (format t "\"The obvious approach won't work. You can't use a top-level variable
because you need multiple pocket functions working at once.\"~%"))
       (2 (format t "\"Think about a pocket-generator function that takes a value and returns
a pocket function containing that value.\"~%"))
       (3 (format t "\"Use LETREC to create a recursive function. The generator should return
a function that either returns its stored value (if given NIL) or calls the generator to
create a new pocket (if given an integer).\"~%"))
       (t (format t "\"The key insight: use static scoping. Each function remembers the
environment where it was created.\"~%"))))))

(defun cmd-about ()
  (format t "~%Lists And Lists is copyright 1996 by Andrew Plotkin.~%")
  (format t "It may be copied, distributed, and played freely.~%")
  (format t "~%Type 'help' for help with whatever you are currently stuck on.~%")
  (format t "~%This is a Common Lisp port of the original Z-machine version.~%"))

(defun cmd-manual ()
  (if (or *manual-available* (>= *genie-state* 2))
      (progn
        (format t "~%=== A Simple Programmer's Introduction to Scheme ===~%")
        (format t "~%This manual contains 21 chapters covering Scheme basics.~%")
        (format t "(Full manual text is available in the original game.)~%")
        (format t "~%Key concepts:~%")
        (format t "  - Atoms and Lists~%")
        (format t "  - Functions (CAR, CDR, CONS)~%")
        (format t "  - Quote (')~%")
        (format t "  - DEFINE, LAMBDA~%")
        (format t "  - COND, IF~%")
        (format t "  - LET, LETREC~%")
        (format t "  - Recursion~%")
        (format t "  - Static scoping~%"))
      (format t "You don't have the manual yet.~%")))

;;; ============================================================================
;;; MAIN ENTRY POINT
;;; ============================================================================

(defun play-game ()
  "Start playing Lists and Lists"
  (setf *current-room* 'entry)
  (setf *genie-state* 0)
  (setf *genie-waiting* nil)
  (setf *alarm-box-used* nil)
  (setf *manual-available* nil)
  (setf *prize-won* nil)
  (setf *hint-problem* -1)
  (setf *hint-level* 0)
  (setf *global-env* nil)

  (game-loop))

;;; EOF
