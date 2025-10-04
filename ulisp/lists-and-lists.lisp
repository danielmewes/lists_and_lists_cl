;;;; Lists and Lists - A uLisp Port
;;;; Copyright 1996 by Andrew Plotkin - original Z-machine version
;;;; Common Lisp port 2025
;;;; uLisp port 2025
;;;;
;;;; An Interactive Tutorial for learning Scheme/Lisp

;;; ============================================================================
;;; UTILITY FUNCTIONS
;;; ============================================================================

(defun string-downcase (str)
  "Convert a string to lowercase using only uLisp built-in functions."
  (let ((result ""))
    (dotimes (i (length str))
      (let* ((ch (char str i))
             (code (char-code ch)))
        ;; If character is uppercase - ASCII 65-90 -, convert to lowercase
        (when (and (>= code 65) (<= code 90))
          (setq code (+ code 32)))
        (setq result (concatenate 'string result (string (code-char code))))))
    result))

(defun char-in-string-p (ch str)
  "Check if character ch is in string str."
  (let ((found nil))
    (dotimes (i (length str))
      (when (eq ch (char str i))
        (setq found t)
        (return)))
    found))

(defun remove-if (predicate lst)
  "Remove all elements from lst for which predicate returns true."
  (let ((result nil))
    (dolist (item lst)
      (unless (funcall predicate item)
        (push item result)))
    (reverse result)))

(defun string-trim (char-bag str)
  "Remove characters in char-bag from both ends of str."
  (let ((start 0)
        (end (- (length str) 1))
        (len (length str)))
    ;; Find first non-trimmable character
    (loop
      (when (or (>= start len)
                (not (char-in-string-p (char str start) char-bag)))
        (return))
      (setq start (+ start 1)))
    ;; Find last non-trimmable character
    (loop
      (when (or (< end 0)
                (not (char-in-string-p (char str end) char-bag)))
        (return))
      (setq end (- end 1)))
    ;; Extract substring
    (if (> start end)
        ""
        (let ((result ""))
          (dotimes (i (+ (- end start) 1))
            (setq result (concatenate 'string result
                                     (string (char str (+ start i))))))
          result))))

;;; ============================================================================
;;; ULOS - uLisp Simple Object System
;;; ============================================================================

(defun object (&optional parent slots)
  (let ((obj (when parent (list (cons 'parent parent)))))
    (loop
      (when (null slots) (return obj))
      (push (cons (first slots) (second slots)) obj)
      (setq slots (cddr slots)))))

(defun value (obj slot)
  (when (symbolp obj) (setq obj (eval obj)))
  (let ((pair (assoc slot obj)))
    (if pair
        (cdr pair)
        (let ((p (cdr (assoc 'parent obj))))
          (and p (value p slot))))))

(defun update (obj slot value)
  (when (symbolp obj) (setq obj (eval obj)))
  (let ((pair (assoc slot obj)))
    (when pair (setf (cdr pair) value))))

;;; ============================================================================
;;; SCHEME INTERPRETER CORE
;;; ============================================================================

;;; Data structures for Scheme values
;;; Converted from defstruct to list-based structures for uLisp

;;; scheme-atom
(defun make-scheme-atom (name)
  (list 'scheme-atom name))

(defun scheme-atom-name (obj)
  (cadr obj))

(defun scheme-atom-p (obj)
  (and (consp obj) (eq (car obj) 'scheme-atom)))

;;; scheme-cons
(defun make-scheme-cons (car-val cdr-val)
  (list 'scheme-cons car-val cdr-val))

(defun scheme-cons-car (obj)
  (cadr obj))

(defun scheme-cons-cdr (obj)
  (caddr obj))

(defun scheme-cons-p (obj)
  (and (consp obj) (eq (car obj) 'scheme-cons)))

;;; scheme-function
(defun make-scheme-function (params body env)
  (list 'scheme-function params body env))

(defun scheme-function-params (obj)
  (cadr obj))

(defun scheme-function-body (obj)
  (caddr obj))

(defun scheme-function-env (obj)
  (cadddr obj))

(defun scheme-function-p (obj)
  (and (consp obj) (eq (car obj) 'scheme-function)))

;;; scheme-builtin
(defun make-scheme-builtin (name fn)
  (list 'scheme-builtin name fn))

(defun scheme-builtin-name (obj)
  (cadr obj))

(defun scheme-builtin-fn (obj)
  (caddr obj))

(defun scheme-builtin-p (obj)
  (and (consp obj) (eq (car obj) 'scheme-builtin)))

;;; scheme-syntax
(defun make-scheme-syntax (name)
  (list 'scheme-syntax name))

(defun scheme-syntax-name (obj)
  (cadr obj))

(defun scheme-syntax-p (obj)
  (and (consp obj) (eq (car obj) 'scheme-syntax)))

;;; Environment handling

(defun make-env (&optional parent)
  (cons 'env (cons parent nil)))

(defun env-parent (env)
  (cadr env))

(defun env-lookup (sym env)
  (cond
    ((null env) (list nil nil))
    ((eq (car env) 'env)
     (let ((pair (assoc sym (cddr env))))
       (if pair
           (list (cdr pair) t)
           (env-lookup sym (env-parent env)))))
    (t (list nil nil))))

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
    (env-define '+ (make-scheme-builtin '+
                                        (lambda (args) (apply '+ args)))
                env)
    (env-define '- (make-scheme-builtin '-
                                        (lambda (args) (apply '- args)))
                env)
    (env-define '* (make-scheme-builtin '*
                                        (lambda (args) (apply '* args)))
                env)

    ;; Comparisons
    (env-define '> (make-scheme-builtin '>
                                        (lambda (args) (apply '> args)))
                env)
    (env-define '< (make-scheme-builtin '<
                                        (lambda (args) (apply '< args)))
                env)
    (env-define '= (make-scheme-builtin '=
                                        (lambda (args) (apply '= args)))
                env)
    (env-define '>= (make-scheme-builtin '>=
                                         (lambda (args) (apply '>= args)))
                env)
    (env-define '<= (make-scheme-builtin '<=
                                         (lambda (args) (apply '<= args)))
                env)

    ;; List operations
    (env-define 'car (make-scheme-builtin 'car
                                          (lambda (args)
                                            (scheme-cons-car (first args))))
                env)
    (env-define 'cdr (make-scheme-builtin 'cdr
                                          (lambda (args)
                                            (scheme-cons-cdr (first args))))
                env)
    (env-define 'cons (make-scheme-builtin 'cons
                                           (lambda (args)
                                             (make-scheme-cons (first args)
                                                               (second args))))
                env)
    (env-define 'list (make-scheme-builtin 'list
                                           (lambda (args)
                                             (scheme-list-to-cons args)))
                env)
    (env-define 'length (make-scheme-builtin 'length
                                             (lambda (args)
                                               (scheme-length (first args))))
                env)

    ;; Predicates
    (env-define 'null? (make-scheme-builtin 'null?
                                            (lambda (args) (null (first args))))
                env)
    (env-define 'list? (make-scheme-builtin 'list?
                                            (lambda (args)
                                              (or (null (first args))
                                                  (scheme-cons-p (first args)))))
                env)
    (env-define 'number? (make-scheme-builtin 'number?
                                              (lambda (args) (numberp (first args))))
                env)
    (env-define 'atom? (make-scheme-builtin 'atom?
                                            (lambda (args)
                                              (scheme-atom-p (first args))))
                env)

    ;; Equality
    (env-define 'eq? (make-scheme-builtin 'eq?
                                          (lambda (args) (eq (first args) (second args))))
                env)
    (env-define 'eqv? (make-scheme-builtin 'eqv?
                                           (lambda (args) (eql (first args) (second args))))
                env)
    (env-define 'equal? (make-scheme-builtin 'equal?
                                             (lambda (args) (scheme-equal (first args) (second args))))
                env)
    (env-define 'not (make-scheme-builtin 'not
                                          (lambda (args) (not (scheme-truthy (first args)))))
                env)

    ;; Meta operations
    (env-define 'eval (make-scheme-builtin 'eval
                                           (lambda (args)
                                             (scheme-eval (first args) *global-env*)))
                env)

    ;; Special forms
    (env-define 'quote (make-scheme-syntax 'quote) env)
    (env-define 'define (make-scheme-syntax 'define) env)
    (env-define 'lambda (make-scheme-syntax 'lambda) env)
    (env-define 'if (make-scheme-syntax 'if) env)
    (env-define 'cond (make-scheme-syntax 'cond) env)
    (env-define 'let (make-scheme-syntax 'let) env)
    (env-define 'let* (make-scheme-syntax 'let*) env)
    (env-define 'letrec (make-scheme-syntax 'letrec) env)
    (env-define 'error (make-scheme-syntax 'error) env)

    ;; Special constants
    (env-define 't t env)
    (env-define 'nil nil env)

    (setf *global-env* env)))

(defun scheme-list-to-cons (lst)
  "Convert a Common Lisp list to a scheme cons structure"
  (if (null lst)
      nil
      (make-scheme-cons (car lst)
                        (scheme-list-to-cons (cdr lst)))))

(defun scheme-length (obj)
  "Return the length of a scheme list"
  (cond
    ((null obj) 0)
    ((scheme-cons-p obj)
     (1+ (scheme-length (scheme-cons-cdr obj))))
    (t (error "LENGTH called on non-list"))))

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
     (let* ((result (env-lookup expr env))
            (val (car result))
            (found (cadr result)))
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
          (make-scheme-function (car args)
                                (cadr args)
                                env))

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

         ;; Let*
         ((eq op 'let*)
          (scheme-eval-let* (first args) (second args) env))

         ;; Letrec
         ((eq op 'letrec)
          (scheme-eval-letrec (first args) (second args) env))

         ;; Error
         ((eq op 'error)
          (error "~a" (first args)))

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

(defun scheme-eval-let* (bindings body env)
  (let ((new-env (make-env env)))
    (dolist (binding bindings)
      (let ((var (car binding))
            (val (scheme-eval (cadr binding) new-env)))
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
       (let ((params (scheme-function-params fn))
             (args-list args))
         (loop
           (when (null params) (return))
           (env-define (car params) (car args-list) new-env)
           (setq params (cdr params))
           (setq args-list (cdr args-list))))
       (scheme-eval (scheme-function-body fn) new-env)))

    (t (error "Cannot apply non-function: ~a" fn))))

;;; Reader

(defun scheme-read-quote (expr)
  "Convert a quoted s-expression to scheme data structures"
  (cond
    ((null expr) nil)
    ((numberp expr) expr)
    ((eq expr 't) t)
    ((symbolp expr) (make-scheme-atom expr))
    ((consp expr)
     (make-scheme-cons (scheme-read-quote (car expr))
                       (scheme-read-quote (cdr expr))))
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
    ((scheme-syntax-p obj) (format stream "[syntax]"))
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
(defvar *door-open* nil) ; t if the entry door is open
(defvar *genie-state* 0) ; 0=asleep, 1=awake, 2-9=tutorial problems
(defvar *genie-waiting* nil) ; t if genie asked a question
(defvar *alarm-box-used* nil)
(defvar *manual-available* nil)
(defvar *prize-won* nil)
(defvar *hint-problem* -1)
(defvar *hint-level* 0)

;;; ============================================================================
;;; HELPER FUNCTIONS
;;; ============================================================================

;;; Command matching helper
(defun cmd-matches-p (cmd &rest alternatives)
  "Check if CMD matches any of the ALTERNATIVES (case-insensitive)"
  ;; Use recursive helper instead of member with :test
  (defun matches-in-list (c alts)
    (cond
      ((null alts) nil)
      ((string= c (car alts)) t)
      (t (matches-in-list c (cdr alts)))))
  (matches-in-list cmd alternatives))

;;; Genie state predicates
(defun genie-asleep-p ()
  "Return T if the genie is asleep"
  (= *genie-state* 0))

(defun genie-awake-p ()
  "Return T if the genie is awake"
  (>= *genie-state* 1))

(defun genie-teaching-p ()
  "Return T if the genie is in teaching mode (problems 2-8)"
  (and (>= *genie-state* 2) (<= *genie-state* 8)))

(defun genie-finished-p ()
  "Return T if the genie has finished teaching"
  (>= *genie-state* 9))

;;; Room helpers
(defun in-lab-p ()
  "Return T if currently in the lab"
  (eq *current-room* 'lab))

;;; Error message helpers
(defun print-not-here ()
  "Print standard 'not here' message"
  (format t "You don't see that here.~%"))

(defun print-what-verb (verb)
  "Print 'What do you want to [verb]?' message"
  (format t "What do you want to ~A?~%" verb))

;;; Genie response helpers
(defun respond-genie-asleep ()
  "Respond when player interacts with sleeping genie"
  (format t "The genie, unconscious, quite ignores you.~%"))

(defun respond-genie-asleep-shout ()
  "Respond when player shouts at sleeping genie"
  (format t "(to the genie)~%The genie, unconscious, quite ignores you.~%"))

(defun respond-genie-no-shout ()
  "Respond when player shouts at awake genie"
  (format t "The genie looks at you quizzically. \"No need to shout.\"~%"))

(defun respond-genie-confused ()
  "Respond when genie doesn't understand"
  (format t "\"What?\"~%"))

;;; System message helpers
(defun print-goodbye ()
  "Print game ending message"
  (format t "~%Thanks for playing!~%"))

(defun print-cant-see-object ()
  "Print message for objects not visible in interpreter context"
  (format t "You can't see any such thing.~%"))

(defun print-what-problem ()
  "Print genie's response when no problem is active"
  (format t "\"What problem?\"~%"))

;;; Wake genie sequence
(defun wake-genie (initial-action-msg)
  "Handle the sequence of waking the genie with a specific action message"
  (setf *alarm-box-used* t)
  (setf *genie-state* 1)
  (setf *genie-waiting* t)
  (format t "~%~A~%" initial-action-msg)
  (format t "~%The genie looks you over, squinching his face in a manner to which mere mortals cannot aspire. \"Okay, okay,\" he rumbles. \"Welcome to Hell. Might as well get to work.\"~%~%")
  (format t "He leaps from the couch, lands soundlessly on the table, and gestures. \"Over here. Workstation. State of the art -- well, it was fifty years ago. But the language is timeless.\"~%~%")
  (format t "\"Now. I am required by the Last Rite to offer you tutorial instruction. Do you want it?\"~%"))

;;; Command dispatch table
(defvar *command-table* nil
  "Association list mapping command names to handler functions")

(defvar *command-table-initialized* nil
  "Whether the command table has been initialized")

(defun register-command (handler &rest names)
  "Register a command handler for multiple command names"
  ;; Use recursion instead of dolist
  (defun register-names (names-list)
    (when names-list
      (let ((name (car names-list)))
        (setf *command-table*
              (cons (cons (string-downcase name) handler)
                    *command-table*))
        (register-names (cdr names-list)))))
  (register-names names))

(defun init-command-table ()
  "Initialize the command dispatch table"
  (unless *command-table-initialized*
    (setf *command-table* nil)
    (register-command 'cmd-examine "examine" "x")
    (register-command 'cmd-read "read")
    (register-command 'cmd-take "take" "get")
    (register-command 'cmd-break "break")
    (register-command 'cmd-push "push" "press")
    (register-command 'cmd-yes "yes" "y")
    (register-command 'cmd-no "no")
    (register-command 'cmd-check "check")
    (register-command 'cmd-repeat "repeat" "problem")
    (register-command 'cmd-help "help" "hint")
    (register-command 'cmd-about "about")
    (register-command 'cmd-save "save")
    (register-command 'cmd-load "load")
    (register-command 'cmd-wake "wake" "wakeup")
    (register-command 'cmd-shout "shout" "yell" "scream")
    (register-command 'cmd-attack "attack" "hit" "kick" "punch")
    (register-command 'cmd-kiss "kiss" "hug")
    (register-command 'cmd-open "open")
    (register-command 'cmd-close "close")
    (register-command 'cmd-go "go")
    (register-command 'cmd-enter "enter")
    (register-command 'cmd-in "in")
    (register-command 'cmd-out "out")
    (register-command 'cmd-search "search")
    (register-command 'cmd-sit "sit")
    (register-command 'cmd-put "put")
    (register-command 'cmd-turn "turn" "switch")
    ;; Direction commands
    (register-command (lambda (&rest _args) (cmd-go "north")) "north" "n")
    (register-command (lambda (&rest _args) (cmd-go "south")) "south" "s")
    (setf *command-table-initialized* t)))

(defun find-command (cmd-string)
  "Find a command handler by command string"
  (unless *command-table-initialized*
    (init-command-table))
  ;; Use helper function to find with string comparison
  (defun find-in-alist (key alist)
    (cond
      ((null alist) nil)
      ((equal key (car (car alist))) (cdr (car alist)))
      (t (find-in-alist key (cdr alist)))))
  (find-in-alist (string-downcase cmd-string) *command-table*))

;;; ============================================================================
;;; OBJECT-ORIENTED GAME OBJECT SYSTEM
;;; ============================================================================

;;; Base game-object constructor
(defun make-game-object (type-name names location description)
  "Create a base game object using ULOS"
  (object nil
          (list 'object-type 'game-object
                'type-name type-name
                'names names
                'location location
                'description description)))

;;; Accessor functions for game objects
(defun object-type-name (obj)
  (value obj 'type-name))

(defun object-names (obj)
  (value obj 'names))

(defun object-location (obj)
  (value obj 'location))

(defun object-description (obj)
  (value obj 'description))

(defun set-object-location (obj loc)
  (update obj 'location loc))

;;; Type checking
(defun game-object-p (obj)
  (eq (value obj 'object-type) 'game-object))

;;; Default implementations for object methods

(defun object-matches-name-p-default (obj name)
  "Default implementation checks against the object's names list"
  ;; Build list with name prepended to object names, then apply cmd-matches-p
  (defun check-names (n name-list)
    (cond
      ((null name-list) nil)
      ((cmd-matches-p n (car name-list)) t)
      (t (check-names n (cdr name-list)))))
  (check-names name (object-names obj)))

(defun object-visible-p-default (obj)
  "Default implementation checks if object's location matches current room"
  (eq (object-location obj) *current-room*))

(defun examine-object-default (obj)
  "Default examine behavior - print the object's description"
  (format t "~A~%" (object-description obj)))

(defun open-object-default (obj)
  "Default open behavior - can't open"
  (format t "You can't open that.~%"))

(defun close-object-default (obj)
  "Default close behavior - can't close"
  (format t "You can't close that.~%"))

(defun search-object-default (obj)
  "Default search behavior - nothing found"
  (format t "You find nothing of interest.~%"))

;;; Define all game object classes using ULOS

;;; Genie object
(defun make-genie-object ()
  (let ((base (make-game-object 'genie '("genie") 'lab
    "You always thought genies were folklore, but now that you've encountered one
you find you really can't mistake it. He's eight feet tall, bright shimmering bronze,
absolutely covered with tasteless wrought-gold jewelry, and he smells of ozone.")))
    (update base 'object-type 'genie-object)
    base))

(defun genie-object-p (obj)
  (eq (value obj 'object-type) 'genie-object))

(defun genie-object-visible-p (obj)
  "Genie is visible in lab when not finished"
  (and (in-lab-p) (not (genie-finished-p))))

(defun examine-genie-object (obj)
  "Examine the genie with state-dependent text"
  (if (not (genie-finished-p))
      (progn
        (format t "You always thought genies were folklore, but now that you've encountered one
you find you really can't mistake it. He's eight feet tall, bright shimmering bronze,
absolutely covered with tasteless wrought-gold jewelry, and he smells of ozone.~%")
        (when (genie-asleep-p)
          (format t "He's also quite dead to the world, snoring like mad on the lumpy couch.~%")))
      (format t "The genie has departed.~%")))

;;; Alarm-box object
(defun make-alarm-box-object ()
  (let ((base (make-game-object 'alarm-box '("box" "glass" "alarm") 'lab
    "It's a small cube of frosted glass. Neatly etched on one side are the words
\"Break glass to wake owner.\" Something turns slowly inside the box...")))
    (update base 'object-type 'alarm-box-object)
    base))

(defun alarm-box-object-p (obj)
  (eq (value obj 'object-type) 'alarm-box-object))

(defun alarm-box-object-visible-p (obj)
  "Alarm box is visible in lab when genie is asleep and not used"
  (and (in-lab-p) (genie-asleep-p) (not *alarm-box-used*)))

(defun open-alarm-box-object (obj)
  (format t "The translucent glass is seamless.~%"))

(defun search-alarm-box-object (obj)
  (format t "You can't make out what's inside the translucent box.~%"))

;;; Computer object
(defun make-computer-object ()
  (let ((base (make-game-object 'computer '("computer" "machine") 'lab
    "The computer has two buttons: a green \"run\" button and a yellow \"reset\" button.")))
    (update base 'object-type 'computer-object)
    base))

(defun computer-object-p (obj)
  (eq (value obj 'object-type) 'computer-object))

;;; Book object
(defun make-book-object ()
  (let ((base (make-game-object 'book '("book" "manual") 'lab "")))
    (update base 'object-type 'book-object)
    base))

(defun book-object-p (obj)
  (eq (value obj 'object-type) 'book-object))

(defun book-object-visible-p (obj)
  "Book is visible in lab when available"
  (and (in-lab-p) *manual-available*))

(defun examine-book-object (obj)
  (cmd-manual))

(defun open-book-object (obj)
  (cmd-manual))

(defun search-book-object (obj)
  (cmd-manual))

;;; Plaque object
(defun make-plaque-object ()
  (let ((base (make-game-object 'plaque '("plaque") 'lab
    "It's a plate of thin gold, engraved with angular designs. In the center
you see the words \"*** You have won ***\"")))
    (update base 'object-type 'plaque-object)
    base))

(defun plaque-object-p (obj)
  (eq (value obj 'object-type) 'plaque-object))

(defun plaque-object-visible-p (obj)
  "Plaque is visible in lab when prize is won"
  (and (in-lab-p) *prize-won*))

;;; Green-button object
(defun make-green-button-object ()
  (let ((base (make-game-object 'green-button '("green" "run") 'lab "")))
    (update base 'object-type 'green-button-object)
    base))

(defun green-button-object-p (obj)
  (eq (value obj 'object-type) 'green-button-object))

;;; Yellow-button object
(defun make-yellow-button-object ()
  (let ((base (make-game-object 'yellow-button '("yellow" "reset") 'lab "")))
    (update base 'object-type 'yellow-button-object)
    base))

(defun yellow-button-object-p (obj)
  (eq (value obj 'object-type) 'yellow-button-object))

;;; Door object
(defun make-door-object ()
  (let ((base (make-game-object 'door '("door") nil "")))
    (update base 'object-type 'door-object)
    base))

(defun door-object-p (obj)
  (eq (value obj 'object-type) 'door-object))

(defun door-object-visible-p (obj)
  "Door is visible in entry room or as inner door in lab"
  (or (eq *current-room* 'entry) (in-lab-p)))

(defun examine-door-object (obj)
  (if (eq *current-room* 'entry)
      (format t "The door to the north is ancient, stained, knotted wood. It looks terribly out of place here. In fact, it IS out of place here. The door ~A.~%"
              (if *door-open* "stands open" "is closed"))
      (format t "The door isn't nearly so interesting from the inside.~%")))

(defun open-door-object (obj)
  (if (eq *current-room* 'entry)
      (if *door-open*
          (format t "It's already open.~%")
          (progn
            (format t "You push the door open. It doesn't creak at all.~%")
            (setf *door-open* t)))
      (format t "Leave it alone. It's done its job.~%")))

(defun close-door-object (obj)
  (if (eq *current-room* 'entry)
      (if *door-open*
          (progn
            (format t "Closed.~%")
            (setf *door-open* nil))
          (format t "It's already closed.~%"))
      (format t "Leave it alone. It's done its job.~%")))

(defun search-door-object (obj)
  (if (eq *current-room* 'entry)
      (if *door-open*
          (format t "I refuse to ruin the suspense.~%")
          (format t "The door is closed.~%"))
      (format t "Leave it alone. It's done its job.~%")))

;;; Couch object
(defun make-couch-object ()
  (let ((base (make-game-object 'couch '("couch") 'lab
    "The couch has that peculiar slump of cushion that says that this couch has seen much service, mostly to a single vast rear end. Indeed, the depression is perfectly molded to the tuchus that occupies it at this very moment.")))
    (update base 'object-type 'couch-object)
    base))

(defun couch-object-p (obj)
  (eq (value obj 'object-type) 'couch-object))

(defun couch-object-visible-p (obj)
  "Couch is visible in lab when genie not finished"
  (and (in-lab-p) (not (genie-finished-p))))

(defun search-couch-object (obj)
  (format t "The couch is occupied by a genie.~%"))

;;; Desk object
(defun make-desk-object ()
  (let ((base (make-game-object 'desk '("desk") 'lab
    "The desk is obviously from that school of design that says that furniture should be clean, efficient, unadorned, and capable of being disassembled with allen wrenches and put into a box six feet by three feet by two inches high.")))
    (update base 'object-type 'desk-object)
    base))

(defun desk-object-p (obj)
  (eq (value obj 'object-type) 'desk-object))

(defun open-desk-object (obj)
  (format t "The desk doesn't have any drawers. It doesn't even have an inside.~%"))

(defun close-desk-object (obj)
  (format t "The desk doesn't have any drawers. It doesn't even have an inside.~%"))

;;; Bookshelves object
(defun make-bookshelves-object ()
  (let ((base (make-game-object 'bookshelves '("bookshelves" "shelves" "bookshelf" "books") 'lab
    "Clearly a geek's collection. Fantasy and science fiction on one side, puzzle books and loony philosophy on the other, and several shelves of little toys and puzzles in the middle.")))
    (update base 'object-type 'bookshelves-object)
    base))

(defun bookshelves-object-p (obj)
  (eq (value obj 'object-type) 'bookshelves-object))

;;; Toys object
(defun make-toys-object ()
  (let ((base (make-game-object 'toys '("toys" "toy" "puzzles" "puzzle") 'lab
    "You expected puzzle-less IF?")))
    (update base 'object-type 'toys-object)
    base))

(defun toys-object-p (obj)
  (eq (value obj 'object-type) 'toys-object))

;;; Genie-possessions object
(defun make-genie-possessions-object ()
  (let ((base (make-game-object 'genie-possessions
                '("magazine" "turkish" "delight" "yo-yo" "yo" "cigar" "laptop" "berrocal" "sculpture")
                'lab "")))
    (update base 'object-type 'genie-possessions-object)
    base))

(defun genie-possessions-object-p (obj)
  (eq (value obj 'object-type) 'genie-possessions-object))

(defun genie-possessions-object-visible-p (obj)
  "Genie possessions are visible in lab when genie present"
  (and (in-lab-p) (not (genie-finished-p))))

;;; Stuff object
(defun make-stuff-object ()
  (let ((base (make-game-object 'stuff '("stuff" "things" "thing" "wall" "everything") 'entry "")))
    (update base 'object-type 'stuff-object)
    base))

(defun stuff-object-p (obj)
  (eq (value obj 'object-type) 'stuff-object))

;;; Registry of all game objects
(defvar *game-objects* nil
  "List of all game object instances")

(defun init-game-objects ()
  "Initialize all game objects"
  (setf *game-objects*
        (list (make-genie-object)
              (make-alarm-box-object)
              (make-computer-object)
              (make-book-object)
              (make-plaque-object)
              (make-green-button-object)
              (make-yellow-button-object)
              (make-door-object)
              (make-couch-object)
              (make-desk-object)
              (make-bookshelves-object)
              (make-toys-object)
              (make-genie-possessions-object)
              (make-stuff-object))))

;;; Dispatch functions for polymorphic methods

(defun object-matches-name-p (obj name)
  "Dispatch to type-specific implementation"
  (object-matches-name-p-default obj name))

(defun object-visible-p (obj)
  "Dispatch to type-specific implementation"
  (cond
    ((genie-object-p obj) (genie-object-visible-p obj))
    ((alarm-box-object-p obj) (alarm-box-object-visible-p obj))
    ((book-object-p obj) (book-object-visible-p obj))
    ((plaque-object-p obj) (plaque-object-visible-p obj))
    ((door-object-p obj) (door-object-visible-p obj))
    ((couch-object-p obj) (couch-object-visible-p obj))
    ((genie-possessions-object-p obj) (genie-possessions-object-visible-p obj))
    (t (object-visible-p-default obj))))

(defun examine-object (obj)
  "Dispatch to type-specific implementation"
  (cond
    ((genie-object-p obj) (examine-genie-object obj))
    ((book-object-p obj) (examine-book-object obj))
    ((door-object-p obj) (examine-door-object obj))
    (t (examine-object-default obj))))

(defun open-object (obj)
  "Dispatch to type-specific implementation"
  (cond
    ((alarm-box-object-p obj) (open-alarm-box-object obj))
    ((book-object-p obj) (open-book-object obj))
    ((door-object-p obj) (open-door-object obj))
    ((desk-object-p obj) (open-desk-object obj))
    (t (open-object-default obj))))

(defun close-object (obj)
  "Dispatch to type-specific implementation"
  (cond
    ((door-object-p obj) (close-door-object obj))
    ((desk-object-p obj) (close-desk-object obj))
    (t (close-object-default obj))))

(defun search-object (obj)
  "Dispatch to type-specific implementation"
  (cond
    ((alarm-box-object-p obj) (search-alarm-box-object obj))
    ((book-object-p obj) (search-book-object obj))
    ((door-object-p obj) (search-door-object obj))
    ((couch-object-p obj) (search-couch-object obj))
    (t (search-object-default obj))))

;;; Helper functions that use the object system

(defun find-object-by-name (name)
  "Find a game object by name, return NIL if not found"
  ;; Use recursive helper instead of find-if
  (defun find-in-list (n objs)
    (cond
      ((null objs) nil)
      ((object-matches-name-p (car objs) n) (car objs))
      (t (find-in-list n (cdr objs)))))
  (find-in-list name *game-objects*))

(defun find-object-by-type (type-name)
  "Find a game object by type-name"
  ;; Use recursive helper to find by type-name
  (defun find-by-type (tn objs)
    (cond
      ((null objs) nil)
      ((eq (object-type-name (car objs)) tn) (car objs))
      (t (find-by-type tn (cdr objs)))))
  (find-by-type type-name *game-objects*))

(defun object-is-p (object-name object-type)
  "Check if OBJECT-NAME refers to OBJECT-TYPE"
  (let ((obj (find-object-by-type object-type)))
    (and obj (object-matches-name-p obj object-name))))

(defun object-visible-p-by-name (object-name)
  "Check if an object is visible by its name"
  (let ((obj (find-object-by-name object-name)))
    (and obj (object-visible-p obj))))

(defun require-object (object-name)
  "Check if object is visible, print error if not. Return T if visible, NIL otherwise."
  (if (object-visible-p-by-name object-name)
      t
      (progn
        (print-not-here)
        nil)))

;;; ============================================================================
;;; GAME PROBLEMS
;;; ============================================================================

(defun problem-text (num)
  (cond
    ((= num 2) "Your first problem is just to acquaint you with the system. Start up the machine,
and define TWENTYSEVEN to have the value 27. You can ask me to 'check' when you're ready,
or 'repeat' the problem if you need me to.")

    ((= num 3) "Let's try creating some lists. Define values for CAT and DOG so that CAT and DOG
are EQUAL? but not EQV?. Furthermore, CDR(CAT) and CDR(DOG) must be EQV?.")

    ((= num 4) "Define ABS to be the absolute value function for integers. That is, (ABS 4) should
return 4; (ABS -5) should return 5; and (ABS 0) should return 0.")

    ((= num 5) "Define SUM to be a function that adds up a list of integers. So (SUM '(8 2 3))
should return 13. Make sure it works correctly for the empty list; (SUM NIL) should
return 0.")

    ((= num 6) "This problem is like the last one, but more general. Define MEGASUM to add up an
arbitrarily nested list of integers. That is, (MEGASUM '((8) 5 (2 () (9 1) 3))) should
return 28.")

    ((= num 7) "Define MAX to be a function that finds the maximum of a list of integers. So
(MAX '(5 14 -3)) should return 14. You can assume the list will have at least one term.")

    ((= num 8) "Last problem. You're going to define a function called POCKET. This function should
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
keep working.")
    (t (error "Invalid problem number"))))

(defun check-problem (num)
  ;; Note: handler-case removed for uLisp compatibility
  (cond
    ((= num 2) (check-problem-2))
    ((= num 3) (check-problem-3))
    ((= num 4) (check-problem-4))
    ((= num 5) (check-problem-5))
    ((= num 6) (check-problem-6))
    ((= num 7) (check-problem-7))
    ((= num 8) (check-problem-8))
    (t (progn
         (format t "~%The genie shakes his head. \"Invalid problem number.\"~%")
         nil))))

(defun check-problem-2 ()
  (let* ((result (env-lookup 'twentyseven *global-env*))
         (val (car result))
         (found (cadr result)))
    (if (and found (numberp val) (= val 27))
        (progn
          (format t "~%\"Aha! Very good.\"~%")
          t)
        (progn
          (format t "~%\"Nope; that's not 27. Try again.\"~%")
          nil))))

(defun check-problem-3 ()
  (let* ((result1 (env-lookup 'cat *global-env*))
         (cat (car result1))
         (found1 (cadr result1))
         (result2 (env-lookup 'dog *global-env*))
         (dog (car result2))
         (found2 (cadr result2)))
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
       t))))

(defun check-problem-4 ()
  (let* ((result (env-lookup 'abs *global-env*))
         (abs-fn (car result))
         (found (cadr result)))
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
  (let* ((result (env-lookup 'sum *global-env*))
         (sum-fn (car result))
         (found (cadr result)))
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
  (let* ((result (env-lookup 'megasum *global-env*))
         (megasum-fn (car result))
         (found (cadr result)))
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
  (let* ((result (env-lookup 'max *global-env*))
         (max-fn (car result))
         (found (cadr result)))
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
  (let* ((result (env-lookup 'pocket *global-env*))
         (pocket-fn (car result))
         (found (cadr result)))
    (if (not found)
        (progn
          (format t "~%\"You need to define POCKET.\"~%")
          nil)
        (let ((val1 (scheme-apply pocket-fn (list nil)))
              (fn2 (scheme-apply pocket-fn (list 12))))
          (if (not (and (numberp val1) (= val1 8)))
              (progn
                (format t "~%\"No; the initial pocket function should return 8 when given NIL.\"~%")
                nil)
              (if (not (scheme-function-p fn2))
                  (progn
                    (format t "~%\"No; pocket should return a function when given an integer.\"~%")
                    nil)
                  (let ((val2 (scheme-apply fn2 (list nil)))
                        (fn3 (scheme-apply fn2 (list 3))))
                    (if (not (and (numberp val2) (= val2 12)))
                        (progn
                          (format t "~%\"No; the new pocket function should return 12 when given NIL.\"~%")
                          nil)
                        (if (not (scheme-function-p fn3))
                            (progn
                              (format t "~%\"No; a pocket function should return another function.\"~%")
                              nil)
                            (let ((val3 (scheme-apply fn3 (list nil)))
                                  (val2-again (scheme-apply fn2 (list nil)))
                                  (val1-again (scheme-apply pocket-fn (list nil))))
                              (if (not (and (numberp val3) (= val3 3)))
                                  (progn
                                    (format t "~%\"No; the third pocket function should return 3.\"~%")
                                    nil)
                                  (if (not (and (numberp val2-again) (= val2-again 12)))
                                      (progn
                                        (format t "~%\"No; the second pocket function should still return 12.\"~%")
                                        nil)
                                      (if (not (and (numberp val1-again) (= val1-again 8)))
                                          (progn
                                            (format t "~%\"No; the original pocket function should still return 8.\"~%")
                                            nil)
                                          (progn
                                            (format t "~%\"Perfect.\"~%")
                                            t))))))))))))))

;;; ============================================================================
;;; GAME INTERFACE
;;; ============================================================================

(defun describe-room ()
  (cond
    ((eq *current-room* 'entry)
     (format t "~%A Familiar Place~%")
     (format t "Everything here is just like it always is, except for that door.~%")
     (format t "~%You can see a strange door to the north.~%"))

    ((eq *current-room* 'lab)
     (format t "~%White Room~%")
     (format t "This is a comfortably cluttered room. Cluttered with bookshelves, mostly.~%")
     (format t "To one side is a large desk, on which a computer squats regally.~%")
     (when (not (genie-finished-p))
       (format t "A lumpy couch is the only other furniture of note.~%"))
     (format t "~%You can see:~%")
     (when (not (genie-finished-p))
       (if (genie-asleep-p)
           (format t "  a huge genie (sleeping on the couch)~%")
           (format t "  a huge genie (on the couch)~%")))
     (format t "  a computer (with green and yellow buttons)~%")
     (when (and (genie-asleep-p) (not *alarm-box-used*))
       (format t "  a small glass box~%"))
     (when *manual-available*
       (format t "  a massive book~%"))
     (when *prize-won*
       (format t "  a gold plaque~%")))

    (t (error "Invalid room"))))

(defun game-loop ()
  (format t "~%Lists And Lists~%")
  (format t "An Interactive Tutorial~%")
  (format t "Copyright 1996 by Andrew Plotkin~%")
  (format t "(Common Lisp port 2025 by Daniel Mewes)~%")
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
            ;; Special cases that need custom handling
            ((cmd-matches-p cmd "quit" "q")
             (print-goodbye)
             (return))

            ((cmd-matches-p cmd "look" "l")
             (if (and rest (cmd-matches-p (first rest) "under"))
                 (cmd-look-under (second rest))
                 (describe-room)))

            ;; Try hash table dispatch
            (t
             (let ((handler (find-command cmd)))
               (if handler
                   (let ((downcased-cmd (string-downcase cmd)))
                     (cond
                       ;; Commands that take no arguments
                       ((or (string= downcased-cmd "yes") (string= downcased-cmd "y")
                            (string= downcased-cmd "no") (string= downcased-cmd "check")
                            (string= downcased-cmd "repeat") (string= downcased-cmd "problem")
                            (string= downcased-cmd "help") (string= downcased-cmd "hint")
                            (string= downcased-cmd "about") (string= downcased-cmd "in")
                            (string= downcased-cmd "out") (string= downcased-cmd "north")
                            (string= downcased-cmd "n") (string= downcased-cmd "south")
                            (string= downcased-cmd "s"))
                        (if (symbolp handler)
                            (funcall (symbol-function handler))
                            (funcall handler)))
                       ;; Commands that take rest as list
                       ((or (string= downcased-cmd "put") (string= downcased-cmd "turn")
                            (string= downcased-cmd "switch"))
                      (if (symbolp handler)
                          (funcall (symbol-function handler) rest)
                          (funcall handler rest)))
                     ;; Commands that take first arg
                     (t
                      (if (symbolp handler)
                          (funcall (symbol-function handler) (first rest))
                          (funcall handler (first rest))))))
                   ;; Unrecognized verb
                   (format t "That's not a verb I recognise.~%")))))))))))

(defun split-string (string separator)
  "Simple string splitter"
  (let ((parts nil)
        (start 0)
        (len (length string)))
    (dotimes (i len)
      (when (eq (char string i) separator)
        (when (> i start)
          (push (subseq string start i) parts))
        (setf start (1+ i))))
    (when (< start len)
      (push (subseq string start) parts))
    (reverse parts)))

(defun parse-command (str)
  (let ((words (split-string (string-trim " " str) #\Space)))
    (remove-if (lambda (s) (string= s "")) words)))

(defun cmd-examine (what)
  (cond
    ((null what)
     (print-what-verb "examine"))
    (t
     (let ((obj (find-object-by-name what)))
       (cond
         ((and obj (object-visible-p obj))
          (examine-object obj))
         (obj
          (print-not-here))
         (t
          (print-not-here)))))))

(defun cmd-take (what)
  (format t "That's not important right now.~%"))

(defun cmd-read (what)
  (cond
    ((null what)
     (print-what-verb "read"))
    ((object-is-p what 'book)
     (if (require-object what)
         (cmd-manual)))
    (t
     (format t "You can't read that.~%"))))

(defun cmd-break (what)
  (if (and (object-is-p what 'alarm-box)
           (object-visible-p-by-name what))
      (wake-genie "You turn the box over carefully, then shrug and swing it sharply...

\"No no don't break it I'm awake!\"
A gleaming hand catches your wrist. The genie gently -- very gently --
removes the box from your grasp, and tucks it carefully away into nothing.")
      (print-what-verb "break")))

(defun cmd-push (what)
  (cond
    ((object-is-p what 'green-button)
     (if (require-object what)
         (cmd-run-interpreter)))
    ((object-is-p what 'yellow-button)
     (if (require-object what)
         (cmd-reset-interpreter)))
    (t
     (print-what-verb "push"))))

(defun cmd-run-interpreter ()
  (if (not (in-lab-p))
      (print-cant-see-object)
      (progn
        (when (null *global-env*)
          (init-global-env))

        (format t "~%The computer comes to life: whirr, feeple, feep! You settle yourself
before the keyboard as text appears on the screen...~%")
        (format t "~%[Welcome to the interpreter. Enter :q to exit, or :m for documentation,
or :? for a list of other : commands.]~%")

        (run-interpreter)

        (format t "~%[Suspending interpreter. Press green button to reactivate.]~%")
        (when (genie-teaching-p)
          (setf *genie-waiting* t)
          (format t "~%You lean back. The genie glances over, and asks, \"Got it working yet?\"~%")))))

(defun cmd-reset-interpreter ()
  (if (not (in-lab-p))
      (print-cant-see-object)
      (progn
        (setf *global-env* nil)
        (format t "~%[Interpreter reset.]~%"))))

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
  (if (not (in-lab-p))
      (format t "Yes to what?~%")
      (cond
        ((genie-asleep-p)
         (respond-genie-asleep))

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

        ((genie-teaching-p)
         (if *genie-waiting*
             (progn
               (setf *genie-waiting* nil)
               (if (check-problem *genie-state*)
                   (progn
                     (incf *genie-state*)
                     (if (genie-finished-p)
                         (progn
                           (setf *prize-won* t)
                           (format t "~%\"Congratulations,\" the genie booms. \"You are now an accredited
hacker of Lisp.\" He hands you something. \"I'll let you keep playing with the machine.
I,\" he adds with sudden intensity, \"am going to return to my nap.\"~%")
                           (format t "~%The genie vanishes in a puff of silver smoke. A moment later,
the couch follows.~%"))
                         (format t "~%~a~%" (problem-text *genie-state*))))
                   (format t "~%(Try again!)~%")))
             (respond-genie-confused)))

        (t
         (respond-genie-confused)))))

(defun cmd-no ()
  (if (not (in-lab-p))
      (format t "No to what?~%")
      (cond
        ((genie-asleep-p)
         (respond-genie-asleep))

        ((= *genie-state* 1)
         (setf *genie-state* 0)
         (setf *genie-waiting* nil)
         (format t "The genie frowns thunderously. \"Fine, go play around on your own. See where
it gets you. Wake me when you're tired of wasting time.\" He turns over, and begins
snoring. Thunderously.~%"))

        ((and (genie-teaching-p) *genie-waiting*)
         (setf *genie-waiting* nil)
         (format t "\"Tell me when you're ready, then.\"~%"))

        (t
         (respond-genie-confused)))))

(defun cmd-check ()
  (if (not (in-lab-p))
      (format t "Check what?~%")
      (if (genie-teaching-p)
          (cmd-yes)
          (format t "Check what?~%"))))

(defun cmd-repeat ()
  (if (not (in-lab-p))
      (format t "Repeat what?~%")
      (if (genie-teaching-p)
          (format t "~%~a~%" (problem-text *genie-state*))
          (if (= *genie-state* 1)
              (format t "\"I thought the question was simple enough. Are you interested in learning
what I have to teach? Yes or no will do.\"~%")
              (print-what-problem)))))

(defun cmd-help ()
  (cond
    ((eq *current-room* 'entry)
     (format t "Consider going inside.~%"))

    ((genie-asleep-p)
     (if *alarm-box-used*
         (format t "Check out the box.~%")
         (format t "Check out the desk.~%")))

    ((genie-finished-p)
     (format t "Read the plaque.~%"))

    ((= *genie-state* 1)
     (format t "The genie is your guide.~%"))

    ((genie-teaching-p)
     (give-hint *genie-state*))))

(defun give-hint (problem)
  (if (= *hint-problem* -1)
      (progn
        (setf *hint-problem* 0)
        (format t "The genie glowers hugely at you. \"Sigh. Yes, I do give hints. I am required
to tell you, blah blah blah, irreparable loss of fun, blah blah, no refunds, fine. So if
you still want help, ask again. If any hint I give isn't enough, ask again.\"~%"))
      (progn
        (when (/= *hint-problem* problem)
          (setf *hint-problem* problem)
          (setf *hint-level* 0))

        (incf *hint-level*)

  ;; Simplified hints - just provide the basic guidance
  (cond
    ((= problem 2)
     (case *hint-level*
       (1 (format t "\"Have you read up through chapter 6 of the manual?\"~%"))
       (2 (format t "\"You need to use the DEFINE command.\"~%"))
       (t (format t "\"Do this: (DEFINE TWENTYSEVEN 27)\"~%"))))

    ((= problem 3)
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

    ((= problem 4)
     (case *hint-level*
       (1 (format t "\"Have you read up through chapter 12 of the manual?\"~%"))
       (2 (format t "\"You can modify the example from chapter 11.\"~%"))
       (t (format t "\"Use LAMBDA and COND with tests for positive, negative, and zero.\"~%"))))

    ((= problem 5)
     (case *hint-level*
       (1 (format t "\"Have you read up through chapter 13 of the manual?\"~%"))
       (2 (format t "\"Use recursion, like the LAST example in chapter 13.\"~%"))
       (3 (format t "\"The base case is when the list is empty; then return 0.\"~%"))
       (t (format t "\"If the list is not empty, add the first term to the sum of the rest.\"~%"))))

    ((= problem 6)
     (case *hint-level*
       (1 (format t "\"You can build MEGASUM the same way you built SUM, with one change.\"~%"))
       (2 (format t "\"The change is that the first term might be a list instead of a number.\"~%"))
       (t (format t "\"Use COND and test the first term with LIST?. If it is a list, call
MEGASUM recursively to add it up.\"~%"))))

    ((= problem 7)
     (case *hint-level*
       (1 (format t "\"Have you read up through chapter 14 of the manual?\"~%"))
       (2 (format t "\"Consider using LET. Look at the first term, look at MAX of the remaining
terms, choose the larger.\"~%"))
       (t (format t "\"The base case is a one-term list, not an empty list.\"~%"))))

    ((= problem 8)
     (case *hint-level*
       (1 (format t "\"The obvious approach won't work. You can't use a top-level variable
because you need multiple pocket functions working at once.\"~%"))
       (2 (format t "\"Think about a pocket-generator function that takes a value and returns
a pocket function containing that value.\"~%"))
       (3 (format t "\"Use LETREC to create a recursive function. The generator should return
a function that either returns its stored value (if given NIL) or calls the generator to
create a new pocket (if given an integer).\"~%"))
       (t (format t "\"The key insight: use static scoping. Each function remembers the
environment where it was created.\"~%"))))

    (t (error "Invalid problem number"))))))

(defun cmd-about ()
  (format t "~%Lists And Lists is copyright 1996 by Andrew Plotkin.~%")
  (format t "It may be copied, distributed, and played freely.~%")
  (format t "~%Type 'help' for help with whatever you are currently stuck on.~%")
  (format t "~%This is a Common Lisp port of the original Z-machine version.~%"))

(defun cmd-wake (what)
  "Handle WAKE verb"
  (cond
    ((null what)
     (print-what-verb "wake"))
    ((object-is-p what 'genie)
     (if (require-object what)
         (if (genie-asleep-p)
             ;; Genie is asleep - show one of three random responses
             (case (random 3)
               (0 (format t "The genie rolls over.~%"))
               (1 (format t "The genie snorts. \"'z a louse,\" he mumbles.~%"))
               (2 (format t "The genie mumbles, \"M'm awake, mmf,\" and throws an arm over his ear.~%")))
             ;; Genie is awake
             (format t "The genie glances up at you. \"I'm not about to fall asleep, not with you muttering to yourself and scribbling all those notes.\"~%"))))
    (t
     (format t "That's not something you can wake.~%"))))

(defun cmd-shout (what)
  "Handle SHOUT verb"
  (cond
    ((null what)
     (format t "You shout, but nothing happens.~%"))
    ((object-is-p what 'genie)
     (if (require-object what)
         (if (genie-asleep-p)
             (respond-genie-asleep-shout)
             (respond-genie-no-shout))))
    ;; Special case: "shout at" without object means shout at genie
    ((cmd-matches-p what "at")
     (if (object-visible-p-by-name "genie")
         (if (genie-asleep-p)
             (respond-genie-asleep-shout)
             (respond-genie-no-shout))
         (print-not-here)))
    (t
     (format t "You shout at ~A, but nothing happens.~%" what))))

(defun cmd-attack (what)
  "Handle ATTACK/HIT/KICK/PUNCH verb"
  (cond
    ((null what)
     (print-what-verb "attack"))
    ((object-is-p what 'alarm-box)
     (if (object-visible-p-by-name what)
         (cmd-break what)
         (print-not-here)))
    ((object-is-p what 'genie)
     (if (require-object what)
         (if (genie-asleep-p)
             ;; Attacking the sleeping genie wakes him
             (wake-genie "A gleaming hand catches your fist. The genie gently -- very gently -- stops your attack.")
             (format t "Violence is not the answer. The genie frowns at you.~%"))))
    (t
     (format t "That's not something you want to attack.~%"))))

(defun cmd-kiss (what)
  "Handle KISS/HUG verb"
  (cond
    ((null what)
     (print-what-verb "kiss"))
    ((object-is-p what 'genie)
     (if (require-object what)
         (if (genie-asleep-p)
             ;; Kissing the sleeping genie wakes him
             (wake-genie "A gleaming hand catches your wrist. The genie gently -- very gently -- pushes you away.")
             (format t "The genie looks at you with amusement. \"Let's keep this professional.\"~%"))))
    (t
     (format t "That's not something you want to kiss.~%"))))

(defun cmd-open (what)
  "Handle OPEN verb"
  (cond
    ((null what)
     (print-what-verb "open"))
    (t
     (let ((obj (find-object-by-name what)))
       (cond
         ((and obj (object-visible-p obj))
          (open-object obj))
         (obj
          (print-not-here))
         (t
          (format t "You can't open that.~%")))))))

(defun cmd-close (what)
  "Handle CLOSE verb"
  (cond
    ((null what)
     (print-what-verb "close"))
    (t
     (let ((obj (find-object-by-name what)))
       (cond
         ((and obj (object-visible-p obj))
          (close-object obj))
         (obj
          (print-not-here))
         (t
          (format t "You can't close that.~%")))))))

(defun cmd-go-north ()
  "Go north direction - for backward compatibility with tests"
  (cmd-go "north"))

(defun cmd-go-south ()
  "Go south direction - for backward compatibility with tests"
  (cmd-go "south"))

(defun cmd-go (direction)
  "Handle GO <direction> verb"
  (cond
    ((null direction)
     (format t "Go where?~%"))
    ((cmd-matches-p direction "north" "n")
     ;; Go north - from entry to lab
     (if (eq *current-room* 'entry)
         (progn
           (unless *door-open*
             (format t "You push the door open. It doesn't creak at all.~%")
             (setf *door-open* t))
           (setf *current-room* 'lab)
           (describe-room)
           t)
         (format t "You can't go that way.~%")))
    ((cmd-matches-p direction "south" "s")
     ;; Go south - from lab to entry or leave game
     (if (in-lab-p)
         (if (genie-finished-p)
             (progn
               (format t "~%You step back through the door...~%")
               (format t "~%*** You have won ***~%")
               (print-goodbye)
               t)
             (format t "Leaving so soon?~%"))
         (format t "You ARE outside.~%")))
    ((cmd-matches-p direction "in")
     (cmd-in))
    ((cmd-matches-p direction "out")
     (cmd-out))
    ((or (cmd-matches-p direction "through") (cmd-matches-p direction "door"))
     ;; Handle "go through" or "go door" as entering the door
     (cmd-enter "door"))
    (t
     (format t "You can't go that way.~%"))))

(defun cmd-enter (what)
  "Handle ENTER verb"
  (cond
    ((null what)
     (print-what-verb "enter"))
    ((object-is-p what 'door)
     (if (eq *current-room* 'entry)
         (cmd-go "north")
         (cmd-go "south")))
    ((object-is-p what 'couch)
     (if (require-object what)
         (format t "The couch is occupied.~%")))
    (t
     (format t "You can't enter that.~%"))))

(defun cmd-in ()
  "Handle IN direction"
  (if (eq *current-room* 'entry)
      (cmd-go "north")
      (format t "You can't go that way.~%")))

(defun cmd-out ()
  "Handle OUT direction"
  (if (eq *current-room* 'entry)
      (format t "You ARE outside.~%")
      (cmd-go "south")))

(defun cmd-search (what)
  "Handle SEARCH verb"
  (cond
    ((null what)
     (print-what-verb "search"))
    (t
     (let ((obj (find-object-by-name what)))
       (cond
         ((and obj (object-visible-p obj))
          (search-object obj))
         (t
          (format t "You find nothing of interest.~%")))))))

(defun cmd-look-under (what)
  "Handle LOOK UNDER verb"
  (cond
    ((null what)
     (format t "Look under what?~%"))
    ((or (object-is-p what 'desk) (object-is-p what 'couch))
     (if (require-object what)
         (format t "This is not that sort of game.~%")))
    (t
     (format t "You find nothing of interest.~%"))))

(defun cmd-sit (what)
  "Handle SIT verb"
  (cond
    ((or (null what) (cmd-matches-p what "on"))
     (format t "Sit on what?~%"))
    ((object-is-p what 'couch)
     (if (require-object what)
         (format t "The couch is occupied.~%")))
    (t
     (format t "You can't sit on that.~%"))))

(defun cmd-put (args)
  "Handle PUT verb"
  (cond
    ((null args)
     (format t "Put what where?~%"))
    ((< (length args) 3)
     (format t "Put what where?~%"))
    (t
     (let ((what (first args))
           (on-in (second args))
           (where (third args)))
       (cond
         ((and (cmd-matches-p on-in "on" "in") (object-is-p where 'couch))
          (if (require-object where)
              (format t "The couch is occupied.~%")))
         (t
          (format t "You can't do that.~%")))))))

(defun cmd-turn (args)
  "Handle TURN/SWITCH verb"
  (cond
    ((null args)
     (format t "Turn what?~%"))
    ((< (length args) 2)
     (format t "Turn what?~%"))
    (t
     (let ((on-off (first args))
           (what (second args)))
       (cond
         ((and (cmd-matches-p on-off "on") (object-is-p what 'computer))
          (if (require-object what)
              (cmd-run-interpreter)))
         (t
          (format t "You can't do that.~%")))))))

;;; ============================================================================
;;; SAVE/LOAD SYSTEM Using uLisp SD Card Interface
;;; ============================================================================

(defun cmd-save (&optional filename)
  "Save the current game state to SD card"
  (let ((save-file (or filename "SAVE.TXT")))
    ;; Simple error handling - check if save succeeds
    (let ((success nil))
      (setf success
            (with-sd-card (out save-file 2)  ; Mode 2 = overwrite
              (when out
                ;; Write game state as s-expressions
                (print (list 'setf '*current-room* (list 'quote *current-room*)) out)
                (terpri out)
                (print (list 'setf '*door-open* *door-open*) out)
                (terpri out)
                (print (list 'setf '*genie-state* *genie-state*) out)
                (terpri out)
                (print (list 'setf '*genie-waiting* *genie-waiting*) out)
                (terpri out)
                (print (list 'setf '*alarm-box-used* *alarm-box-used*) out)
                (terpri out)
                (print (list 'setf '*manual-available* *manual-available*) out)
                (terpri out)
                (print (list 'setf '*prize-won* *prize-won*) out)
                (terpri out)
                (print (list 'setf '*hint-problem* *hint-problem*) out)
                (terpri out)
                (print (list 'setf '*hint-level* *hint-level*) out)
                (terpri out)
                ;; Save environment - user-defined Scheme functions
                (print (list 'setf '*global-env* (save-user-env *global-env*)) out)
                (terpri out)
                t)))
      (if success
          (format t "Game saved to ~A~%" save-file)
          (format t "Error: Could not save game (SD card not available?)~%")))))

(defun cmd-load (&optional filename)
  "Load game state from SD card"
  (let ((save-file (or filename "SAVE.TXT")))
    ;; Try to read and evaluate saved state
    (let ((success nil))
      (with-sd-card (in save-file 0)  ; Mode 0 = read
        (when in
          ;; Read and evaluate each form
          (let ((form (read in nil)))
            (loop
              (when (null form) (return))
              (eval form)
              (setf form (read in nil)))
            (setf success t))))
      (if success
          (progn
            (format t "Game loaded from ~A~%" save-file)
            (describe-room))
          (format t "Error: Could not load game (file not found or SD card not available?)~%")))))

(defun save-user-env (env)
  "Save user-defined bindings from environment, recreating it on load"
  (let ((user-bindings (collect-user-bindings env)))
    (cons 'let
          (cons (list (list 'new-env (list 'init-global-env)))
                (append (mapcar (lambda (binding)
                                  (list 'env-define
                                        (list 'quote (car binding))
                                        (serialize-scheme-value (cdr binding))
                                        'new-env))
                                user-bindings)
                        (list 'new-env))))))

(defun collect-user-bindings (env)
  "Collect user-defined bindings (non-builtin) from environment"
  (if (or (null env) (not (eq (car env) 'env)))
      nil
      (let ((parent-bindings (collect-user-bindings (env-parent env)))
            (local-bindings (remove-if (lambda (binding)
                                        (or (scheme-builtin-p (cdr binding))
                                            (scheme-syntax-p (cdr binding))))
                                      (cddr env))))
        (append local-bindings parent-bindings))))

(defun serialize-scheme-value (val)
  "Convert a Scheme value to a serializable form"
  (cond
    ((null val) nil)
    ((numberp val) val)
    ((eq val t) t)
    ((scheme-atom-p val) `(make-scheme-atom ',(scheme-atom-name val)))
    ((scheme-cons-p val)
     `(make-scheme-cons ,(serialize-scheme-value (scheme-cons-car val))
                        ,(serialize-scheme-value (scheme-cons-cdr val))))
    ((scheme-function-p val)
     `(make-scheme-function ,(serialize-scheme-value (scheme-function-params val))
                           ,(serialize-scheme-value (scheme-function-body val))
                           ,(serialize-scheme-env (scheme-function-env val))))
    (t `',val)))

(defun serialize-scheme-env (env)
  "Serialize a Scheme environment, preserving user bindings"
  (if (or (null env) (not (eq (car env) 'env)))
      '*global-env*
      (let ((bindings (collect-user-bindings env)))
        (cons 'let
              (cons (list (list 'new-env (list 'make-env '*global-env*)))
                    (append (mapcar (lambda (binding)
                                      (list 'env-define
                                            (list 'quote (car binding))
                                            (serialize-scheme-value (cdr binding))
                                            'new-env))
                                    bindings)
                            (list 'new-env)))))))

;;; ============================================================================
;;; MANUAL SYSTEM
;;; ============================================================================

(defun manual-chapter-name (n)
  "Return the name of chapter N"
  (case n
    (0 "Introduction")
    (1 "What's Going On Here")
    (2 "What's An Atom?")
    (3 "What's a List?")
    (4 "Functions")
    (5 "Quoting Expressions")
    (6 "Defining Atoms")
    (7 "List Chopping")
    (8 "List Constructing")
    (9 "Tests and Logic")
    (10 "Comparisons")
    (11 "Conditionals")
    (12 "Creating Functions")
    (13 "Fun With Recursion")
    (14 "Local Definitions With Let")
    (15 "Recursion, Functions, Endless Fun")
    (16 "Scope")
    (17 "Return")
    (18 "Reference: Functions")
    (19 "Reference: Syntactic Forms")
    (20 "Reference: Improper Lists")
    (t "Unknown Chapter")))

(defun print-bold (text)
  "Print text in bold"
  (format t "~a" text))

(defun print-example (input &optional (output nil output-supplied-p))
  "Print a Scheme example with optional output"
  (format t " >>~a~%" input)
  (when output-supplied-p
    (when output
      (format t " ~a~%" output))))

(defun print-chapter-0 ()
  (format t "~%~%=== 0. Introduction ===~%~%")
  (format t "A Simple Programmer's Introduction to Scheme~%~%")
  (format t "This is a story about Lisp. Actually -- I'll admit it right now -- it's about ~%")
  (format t "Scheme, which is a cleaned-up version of Lisp. More suitable for teaching, ~%")
  (format t "and (more to the point) easier for me to implement inside the Z-Machine. And -- ~%")
  (format t "to be really honest -- I didn't implement much of Scheme either. But I think ~%")
  (format t "it'll do for a start.~%~%")
  (format t "Since you have the Scheme interpreter right underneath your fingers, your job ~%")
  (format t "is to try all the examples. And experiment with new ones. Your friend the genie ~%")
  (format t "will pose problems for you to solve, but it's all self-paced. Play around as ~%")
  (format t "much as you want. Refer to this manual as much as you want. Or blow the whole ~%")
  (format t "thing and go fight dragons. Will I care? (Sniff.)~%~%"))

(defun print-chapter-1 ()
  (format t "~%~%=== 1. What's Going On Here ===~%~%")
  (format t "When you start the Scheme interpreter, you see a prompt:~%~%")
  (print-example "" nil)
  (format t "~%You type things, and the interpreter responds. Try it. Type '5' and hit Enter. ~%")
  (format t "The interpreter will respond with '5'.~%~%")
  (print-example "5" "5")
  (format t "~%That's all that happens in Scheme. You can go home now. Bye!~%~%")
  (format t "Ok, I'm kidding. Sort of. It's true, really -- all you do in Scheme is type ~%")
  (format t "things, and get responses. The process is dynamic; the response can depend on ~%")
  (format t "earlier things that you typed. Notice that this is different from languages ~%")
  (format t "like C, or Pascal, or Inform. In those languages, you type in an entire ~%")
  (format t "program, and chuck it into the compiler, which either grinds you out a program ~%")
  (format t "or complains. In Scheme, or Lisp, you build programs out of small pieces. It's ~%")
  (format t "much friendlier.~%~%")
  (format t "But enough about Pascal. What happens in Scheme?~%~%")
  (format t "What happens -- really -- is that you type an expression, and the interpreter ~%")
  (format t "evaluates the expression and prints the result. Read, evaluate, print. Every ~%")
  (format t "expression evaluates to another expression (unless it produces an error.) ~%")
  (format t "We just demonstrated this; 5 is an expression, and it evaluates ~%")
  (format t "to the expression 5. Numbers evaluate to themselves. If that's confusing, ~%")
  (format t "just try to get used to it. If it makes sense, you'll probably be confused ~%")
  (format t "later on, because most expressions don't evaluate to themselves. Oh well.~%~%"))

(defun print-chapter-2 ()
  (format t "~%~%=== 2. What's An Atom? ===~%~%")
  (format t "We start with atoms.~%~%")
  (format t "An atom is a bunch of characters. Letters, numbers, most punctuation. Here ~%")
  (format t "are some atoms: 5 hello -5 goodbye darlene+mitchell q-5 qq=5++qqq+ ***~%~%")
  (format t "If you're familiar with languages like Pascal, you're probably desperately ~%")
  (format t "trying to read some of those as additions, or subtractions, or whatever. Don't ~%")
  (format t "bother. They're all just atoms. Letters, numbers, and punctuation are all ~%")
  (format t "treated the same in an atom.~%~%")
  (format t "(For completeness, here's the list of punctuation which you can't use in an ~%")
  (format t "atom: ( ) ; : ' . Any kind of whitespace, such as spaces or line ~%")
  (format t "breaks, will separate atoms as well.)~%~%")
  (format t "So ho, you ask, what about numbers? Good point. An atom which is entirely made ~%")
  (format t "up of numbers, except possibly for a minus sign at the front, counts as a number. ~%")
  (format t "(Get it? Counts? Never mind.)~%~%")
  (format t "We've already said that a number evaluates to itself. That is, if an expression ~%")
  (format t "consists of just an atom, and the atom is a number, the value of that expression ~%")
  (format t "is that very expression -- which is to say, that atom.~%~%")
  (print-example "5" "5")
  (print-example "-597" "-597")
  (print-example "0" "0")
  (format t "~%What do other atoms evaluate to? Well, try it. I'll just cover my ears --~%~%")
  (print-example "hello" "[Error: undefined atom: hello]")
  (print-example "q-5" "[Error: undefined atom: q-5]")
  (print-example "***" "[Error: undefined atom: ***]")
  (format t "~%Don't worry; no permanent damage done. Most non-numeric atoms produce errors when ~%")
  (format t "you try to evaluate them. This is because they are not bound to any value. But ~%")
  (format t "what that means, we'll get to later.~%~%"))

(defun print-chapter-3 ()
  (format t "~%~%=== 3. What's a List? ===~%~%")
  (format t "A single atom is an expression. Is there any other kind of expression? Of course. ~%")
  (format t "An expression can also be a list. One list -- that's important. An expression is ~%")
  (format t "a single atom or a single list.~%~%")
  (format t "What's a list? A list is a list of expressions. That is, a list is a list of atoms ~%")
  (format t "and lists. A list is written as a pair of parentheses surrounding the contents of ~%")
  (format t "the list. Here's a list: (1) It contains one atom. Here's another list: (2 fred) ~%")
  (format t "It contains two atoms. The order is significant; (2 fred) and (fred 2) are two ~%")
  (format t "different lists.~%~%")
  (format t "A list can contain nothing: () is the empty list. It's famous. It's also called ~%")
  (format t "nil, which I think isn't as pretty as (), but it's historical, so there we are. ~%")
  (format t "nil and () both refer to the empty list.~%~%")
  (format t "As I said, a list can contain both atoms and other lists. So here are four more ~%")
  (format t "lists: (one (two) three) (((one) 2 fred) xxxx9) (()) (hello mr ((operator))) ~%")
  (format t "Don't be frightened by the stacks of parentheses. It's all recursive. That last ~%")
  (format t "list, for example, contains three expressions: the atom hello, the atom mr, and ~%")
  (format t "the list ((operator)). Which, itself, is one-term list containing only the list ~%")
  (format t "(operator). Which is, itself, a one-term list containing only the atom operator. ~%")
  (format t "Get it?~%~%")
  (format t "By the way, remember that an expression is either a single atom or a single list. ~%")
  (format t "1 2 3 is three separate expressions, not any funky kind of parenthesis-stripped ~%")
  (format t "list or anything. If you type several expressions at the Scheme prompt, the ~%")
  (format t "interpreter will evaluate them one at a time.~%~%")
  (print-example "1 2 3 four" "1")
  (print-example "" "2")
  (print-example "" "3")
  (print-example "" "[Error: undefined atom: four]")
  (format t "~%"))

(defun print-chapter-4 ()
  (format t "~%~%=== 4. Functions ===~%~%")
  (format t "We've seen that numeric atoms evaluate to themselves, and other atoms seem to ~%")
  (format t "produce raging error messages. What does a list evaluate to? Aha! You've heard of ~%")
  (format t "functions? I really hope so. Anyway, when you evaluate a list, you're calling a ~%")
  (format t "function. The first element of the list is the function; the rest of the elements ~%")
  (format t "are its arguments.~%~%")
  (print-example "(+ 1 1)" "2")
  (format t "~%Right! Addition! Let's take a closer look. We typed in an expression: ~%")
  (format t "(+ 1 1). Nothing magic there; it's a list containing three atoms, of which ~%")
  (format t "the first is a plus sign and the last two are both the number 1. The Scheme ~%")
  (format t "interpreter evaluates all of these. The plus sign means addition; the 1 atoms ~%")
  (format t "both evaluate to themselves. So the interpreter runs the addition function, and ~%")
  (format t "hands it a pair of 1 atoms to work with. One plus one is two, so the function ~%")
  (format t "returns the expression 2. And that's what (+ 1 1) evaluates to.~%~%")
  (format t "Sneaky people will raise their hands at this point. What am I trying to pull? ~%")
  (format t "'The plus sign means addition?' Ok, ok. What I mean is, the atom + is not ~%")
  (format t "one of those pernicious undefined atoms. It is defined. Its value is the ~%")
  (format t "addition function. Here, try it:~%~%")
  (print-example "+" "[function]")
  (format t "~%The interpreter doesn't try to actually print out the addition function, because ~%")
  (format t "that's a bunch of internal program code. All functions print out looking like ~%")
  (format t "[function].~%~%")
  (print-example "(- 1 1)" "0")
  (print-example "-" "[function]")
  (format t "~%You can probably guess what function - evaluates to.~%~%")
  (format t "Did I say that the interpreter evaluates all the terms of a list? Indeed I ~%")
  (format t "did. This is important. Guess what (+ (+ 1 1) 4) evaluates to? Ok, it's not too ~%")
  (format t "hard, but we'll go through it step by step anyway. It's a three-term list. The ~%")
  (format t "first term is +, which evaluates to the addition function. The second term is ~%")
  (format t "the list (+ 1 1), and we've already discovered that evaluates to 2. The third ~%")
  (format t "term is 4, and that evaluates to 4. The addition function is handed the results, ~%")
  (format t "2 and 4, and guess what...~%~%")
  (print-example "(+ (+ 1 1) 4)" "6")
  (format t "~%Actually, the addition and subtraction functions can take any number of arguments, ~%")
  (format t "not just two.~%~%")
  (print-example "(+ 1 1 4)" "6")
  (format t "~%What happens if you try to evaluate a list, and the first term doesn't evaluate ~%")
  (format t "to a function? You get errors, that's what. There are a couple of ways you can ~%")
  (format t "get this wrong. As your end-of-chapter exercise, meditate upon the following ~%")
  (format t "exchanges:~%~%")
  (print-example "(1 2 3 4)" "[Error: object is not a function: 1]")
  (print-example "(spoggly 2 3 4)" "[Error: undefined atom: spoggly]")
  (format t "~%Oops, one more thing. The empty list -- whether you write it as () or nil -- ~%")
  (format t "evaluates to itself. No function is called.~%~%")
  (print-example "()" "nil")
  (print-example "nil" "nil")
  (format t "~%"))

(defun print-chapter-5 ()
  (format t "~%~%=== 5. Quoting Expressions ===~%~%")
  (format t "What's an expression which evaluates to the atom hello? We know that hello doesn't; ~%")
  (format t "that produces an error.~%~%")
  (print-example "hello" "[Error: undefined atom: hello]")
  (format t "~%It sure would be handy if there were a way to produce an arbitrary atom. Well, ~%")
  (format t "watch:~%~%")
  (print-example "(quote hello)" "hello")
  (format t "~%quote acts sort of like a function which returns its argument unchanged. ~%")
  (format t "(Actually, it's not a function; it's a special piece of syntax. Can you see why? ~%")
  (format t "It breaks a rule of functions. Answer at the end of this chapter.) You can quote ~%")
  (format t "a list too:~%~%")
  (print-example "(quote (+ 1 1))" "(+ 1 1)")
  (format t "~%quote is so useful that you can abbreviate it, by skipping the parentheses and ~%")
  (format t "just writing a single quote mark.~%~%")
  (print-example "'hello" "hello")
  (print-example "'(+ 1 1)" "(+ 1 1)")
  (print-example "'5" "5")
  (print-example "'+'" "+")
  (format t "~%That last example is important. It doesn't matter that + has a value; the ~%")
  (format t "expression '+ -- that is, (quote +) -- just evaluates to +.~%~%")
  (format t "End of the chapter time. Why is quote not a function? Because when Scheme evaluates ~%")
  (format t "a function, it evaluates all the arguments before handing them in to the function. ~%")
  (format t "The argument to quote is not evaluated; it's handed straight in, so that it can ~%")
  (format t "be handed out unchanged. Watch this:~%~%")
  (print-example "quote" "[syntax]")
  (format t "~%Special syntax forms are applied the same way functions are, but they can have ~%")
  (format t "funkier effects, and they don't necessarily evaluate all their arguments. Keep track ~%")
  (format t "of this fact, because someday it'll unconfuse you about something.~%~%"))

(defun print-chapter-6 ()
  (format t "~%~%=== 6. Defining Atoms ===~%~%")
  (format t "+ has a value which is automatically defined by the interpreter. Can we define ~%")
  (format t "values for atoms ourselves? Would I ask if the answer were no? Watch this:~%~%")
  (print-example "(define x 5)" "5")
  (format t "~%define is another piece of special syntax. It evaluates its third argument, and ~%")
  (format t "assigns the resulting value to its second argument, which must be an atom. (Why can't ~%")
  (format t "define be a function? Because all the arguments to a function are evaluated. If define ~%")
  (format t "tried to evaluate its second argument, it would get an undefined atom error, because ~%")
  (format t "the atom hasn't been defined until define defines it! Whew.)  For clarity, define ~%")
  (format t "returns the value it just assigned. We set x to 5; now the expression x evaluates ~%")
  (format t "to 5.~%~%")
  (print-example "x" "5")
  (print-example "(+ x 1)" "6")
  (format t "~%We don't have to use numbers, by the way. Let's set x to be bob:~%~%")
  (print-example "(define x bob)" "[Error: undefined atom: bob]")
  (format t "~%Oops -- forgot that the third value in a define statement is evaluated. We can't ~%")
  (format t "use the atom bob; we need to use an expression whose value is bob.~%~%")
  (print-example "(define x 'bob)" "bob")
  (print-example "x" "bob")
  (print-example "(+ x 1)" "[Error: +: non-numeric argument: bob]")
  (print-example "'x" "x")
  (format t "~%That works. Notice that we've replaced the earlier definition of x as 5. Also note ~%")
  (format t "that addition righteously complains when we feed it an atom which isn't a number. And ~%")
  (format t "also also note that 'x is, still and always, just plain x.~%~%")
  (format t "You can replace the definitions of predefined functions, too. You could do (define + 5), ~%")
  (format t "for example. But I really don't recommend it. Reset the interpreter if you get carried ~%")
  (format t "away with this stuff.~%~%")
  (format t "Last trick of the chapter. What's going on here?~%~%")
  (print-example "(define addify +)" "[function]")
  (print-example "(addify 5 6)" "11")
  (format t "~%"))

(defun print-chapter-7 ()
  (format t "~%~%=== 7. List Chopping ===~%~%")
  (format t "Back to lists. We often want to take them apart, see what makes them tick, and put ~%")
  (format t "together new ones. For that, we'll need some tools.~%~%")
  (format t "A list can be split into two parts: its first term, and its everything else. Now, ~%")
  (format t "in Scheme (and Lisp), the first term is called the list's car, and the everything ~%")
  (format t "else is called the list's cdr (rhymes with 'wooder', more or less.) Now every ~%")
  (format t "damn Lisp book in the universe explains why these things are called car and cdr, ~%")
  (format t "and I'm so sick of it I'm going to leave you in the dark. Go look it up, if you ~%")
  (format t "care.~%~%")
  (format t "The car of (a bb ccc) is a. The cdr of (a bb ccc) is (bb ccc). See that? The ~%")
  (format t "car is the first term; the cdr is the list minus the first term. Important, that. ~%")
  (format t "Now, this doesn't mean that the car of a list is always an atom. The car of ~%")
  (format t "((1 2) x y z) is (1 2). But the cdr of a list is always a list. It might be the ~%")
  (format t "empty list, though. The cdr of (hello) is the empty list nil.~%~%")
  (format t "The empty list has neither a car nor a cdr.~%~%")
  (format t "car and cdr are pre-defined functions in Scheme. (Ok, I'm lying -- car and cdr ~%")
  (format t "are atoms. But they're defined to evaluate to functions, just like + and -. Get ~%")
  (format t "over it.)~%~%")
  (print-example "(car '(a bb ccc))" "a")
  (print-example "(cdr '(a bb ccc))" "(bb ccc)")
  (format t "~%Note the quote marks. What happens if you try to evaluate (car (a bb cc))? Why?~%~%")
  (format t "car and cdr are well-wired to complain if you try to mess with their heads:~%~%")
  (print-example "(car 'a)" "[Error: car: bad argument: a]")
  (print-example "(car '())" "[Error: car: bad argument: nil]")
  (format t "~%a isn't a list, so it can't have a car or cdr; and the empty list, as we've said, ~%")
  (format t "has no car or cdr either.~%~%"))

(defun print-chapter-8 ()
  (format t "~%~%=== 8. List Constructing ===~%~%")
  (format t "We takes 'em apart, we puts 'em together. If you have a car and a cdr, you can ~%")
  (format t "construct a list. The cons function does this.~%~%")
  (print-example "(cons 'aaa '(bb c))" "(aaa bb c)")
  (print-example "(cons 'aaa nil)" "(aaa)")
  (print-example "(cons (car '(x y z z y)) (cdr '(x y z z y)))" "(x y z z y)")
  (format t "~%That last example, dreadful as it appears, just demonstrates that you can take ~%")
  (format t "a single list apart into car and cdr, and reassemble it. Maybe it'd help if I ~%")
  (format t "did it like this:~%~%")
  (print-example "(define magic-word '(x y z z y))" "(x y z z y)")
  (print-example "magic-word" "(x y z z y)")
  (print-example "(cons (car magic-word) (cdr magic-word))" "(x y z z y)")
  (format t "~%Oh, those lovely inside-out Scheme expressions! When in doubt, break out the ~%")
  (format t "magnifying lens and count parentheses.~%~%")
  (format t "The cdr of a non-empty list is always a list, so the second argument you give ~%")
  (format t "to cons had better be a list. If you break this rule, you get a broken, twisted, ~%")
  (format t "evil-twin kind of not-really-a-list, called an 'improper' or 'dotted' list.~%~%")
  (print-example "(cons 'a 'b)" "(a . b)")
  (format t "~%I don't feel like explaining this right now, so just try not to do it.~%~%")
  (format t "Another convenient function is list, which takes a bunch of expressions and ~%")
  (format t "makes a list out of them.~%~%")
  (print-example "(list 'a 'zzz)" "(a zzz)")
  (print-example "(list 'a '(1 2 3) 'end)" "(a (1 2 3) end)")
  (print-example "(list)" "nil")
  (print-example "(list '() (+ 4 5) '(+ 1 2) (cdr magic-word))" "(nil 9 (+ 1 2) (y z z y))")
  (format t "~%I'm being tricky with that last one. If you try it yourself, make sure you've ~%")
  (format t "defined magic-word the way I did a few paragraphs back. Also, see the difference ~%")
  (format t "a quote can make?~%~%"))

(defun print-chapter-9 ()
  (format t "~%~%=== 9. Tests and Logic ===~%~%")
  (format t "Some functions test a condition. The = function, for example, tests to see if ~%")
  (format t "two numbers are equal:~%~%")
  (print-example "(= 2 2)" "t")
  (print-example "(= 2 5)" "nil")
  (format t "~%As you see, the convention in Scheme is to use nil to signify falsity, and ~%")
  (format t "the atom t to signify truth. (t evaluates to itself, by the way, just as nil ~%")
  (format t "does. More convenience; you don't have to quote t when you use it.) Actually, ~%")
  (format t "when a true/false value is required, you can generally supply any value; nil ~%")
  (format t "will be counted as false, and anything else at all will be counted as true.~%~%")
  (format t "The <, >, <=, and >= functions work as you'd expect. There are also some ~%")
  (format t "pre-defined functions which test other properties. null? returns t if its ~%")
  (format t "argument is nil, and nil otherwise. list? returns t if its argument is ~%")
  (format t "a list (including nil), and nil otherwise.~%~%")
  (print-example "(null? nil)" "t")
  (print-example "(null? t)" "nil")
  (print-example "(null? 0)" "nil")
  (print-example "(list? nil)" "t")
  (print-example "(list? 1)" "nil")
  (print-example "(list? 'a)" "nil")
  (print-example "(list? '(a))" "t")
  (format t "~%"))

(defun print-chapter-10 ()
  (format t "~%~%=== 10. Comparisons ===~%~%")
  (format t "The = function compares numbers, but it produces an error if you try to ~%")
  (format t "use it on atoms or lists. For that, you need the eqv? and equal? functions. ~%")
  (format t "These are slightly different, and the difference is sort of technical and ~%")
  (format t "confusing, but it's historical. You want to learn Scheme, you have to know eqv? ~%")
  (format t "and equal? (In fact, real Scheme has eq? as well, but the difference wasn't ~%")
  (format t "significant in this implementation, so I left it out.)~%~%")
  (format t "For atoms (including numbers), they both work as you'd expect. nil, also.~%~%")
  (print-example "(eqv? 1 2)" "nil")
  (print-example "(eqv? 2 2)" "t")
  (print-example "(eqv? 'one 'two)" "nil")
  (print-example "(eqv? 'two 'two)" "t")
  (print-example "(eqv? 2 'two)" "nil")
  (print-example "(eqv? nil 'two)" "nil")
  (print-example "(eqv? nil nil)" "t")
  (format t "~%(And equal? would produce the same results.)~%~%")
  (format t "For complex objects, such as lists and functions, things are trickier. If you ~%")
  (format t "hand two complex objects to eqv?, you'll get t only if they both stem from the ~%")
  (format t "same act of creation. (See, I told you it was confusing.)~%~%")
  (print-example "(define greeting '(hi there))" "(hi there)")
  (print-example "(define aloha (list 'hi 'there))" "(hi there)")
  (print-example "(eqv? greeting greeting)" "t")
  (print-example "(eqv? greeting '(hi there))" "nil")
  (print-example "(eqv? greeting aloha)" "nil")
  (print-example "(eqv? '(hi there) '(hi there))" "nil")
  (print-example "(define hug greeting)" "(hi there)")
  (print-example "(eqv? greeting hug)" "t")
  (print-example "(eqv? aloha hug)" "nil")
  (format t "~%How to put this... evaluating '(hi there) creates a list of two atoms. Evaluating ~%")
  (format t "(list 'hi 'there) creates another list of two atoms. The lists have the same ~%")
  (format t "contents, but they're two separate objects. So they aren't eqv? to each other. ~%")
  (format t "Now, when we do (define hug greeting), we take the value of greeting and ~%")
  (format t "assign that very value to hug, so they are eqv?... see? Well, maybe.~%~%")
  (format t "equal?, on the other hand, works sensibly. Two lists are equal? if they are ~%")
  (format t "the same length and each pair of terms is equal?. Given the definitions above, ~%")
  (format t "you'll see that:~%~%")
  (print-example "(equal? greeting greeting)" "t")
  (print-example "(equal? greeting '(hi there))" "t")
  (print-example "(equal? '(hi there) '(hi there))" "t")
  (print-example "(equal? greeting aloha)" "t")
  (print-example "(equal? greeting hug)" "t")
  (print-example "(equal? aloha hug)" "t")
  (format t "~%What's the point of eqv?, seeing how weirdly it works? It's faster. equal? ~%")
  (format t "has to compare every element of a list, but eqv? just compares two internal ~%")
  (format t "pointers.~%~%")
  (format t "Up above I said that functions are as tricky as lists, when it comes to ~%")
  (format t "comparison. In fact, they're trickier. If two functions stem from the same ~%")
  (format t "act of creation, they're eqv? and equal?, just like lists. But otherwise, ~%")
  (format t "two functions are never eqv? or equal?, even if they do the exact same thing. ~%")
  (format t "(There's no good way to tell if two functions do the exact same thing. I mean, ~%")
  (format t "there's no way for a person to tell. Not in all cases. Computers, forget it.)~%~%"))

(defun print-chapter-11 ()
  (format t "~%~%=== 11. Conditionals ===~%~%")
  (format t "Scheme offers a nice assortment of conditional syntax forms. Since I'm a lazy ~%")
  (format t "bum, I've only implemented the most basic one, which is cond. A cond structure ~%")
  (format t "looks like this:~%~%")
  (format t " (cond~%")
  (format t "    (test1 result1)~%")
  (format t "    (test2 result2)~%")
  (format t "    ...~%")
  (format t " )~%~%")
  (format t "I've written it on multiple lines, but that's just for clarity. The point is, ~%")
  (format t "you have a list of clauses, and each clause is a list of two expressions: a ~%")
  (format t "test and a result. The cond syntax goes through the clauses, in order. For ~%")
  (format t "each one, it evaluates the test. If the test returns true (anything but nil), ~%")
  (format t "it evaluates the result, and returns that value. If the test returns nil, it ~%")
  (format t "goes on to the next clause. If all of the clauses return nil, it returns nil.~%~%")
  (format t "If you're not sure what this means, look at it this way: in C, we'd write this ~%")
  (format t "as something like~%~%")
  (format t "  if (test1) return result1;~%")
  (format t "  else if (test2) return result2;~%")
  (format t "  ...~%")
  (format t "  else return nil;~%~%")
  (format t "If you want a final 'default' clause, which will be used if all the others fail, ~%")
  (format t "you can use (t defaultresult). t is always true, and that's the effect you want.~%~%")
  (format t "You're still confused. Well, hark to the example.~%~%")
  (print-example "(define val -25)" "-25")
  (format t " >>(cond~%")
  (format t "  >  ((> val 0) 'positive)~%")
  (format t "  >  ((< val 0) 'negative)~%")
  (format t "  >  (t 'zero)~%")
  (format t "  >)~%")
  (format t " negative~%~%")
  (format t "The conditional tests val and returns one of the atoms positive, negative, or ~%")
  (format t "zero. (Again, we've typed it in on several lines. Notice that the interpreter ~%")
  (format t "prints a single-arrow prompt if you've started a list and not finished it yet; ~%")
  (format t "the expression won't be evaluated until you've typed it all in. As a bonus, the ~%")
  (format t "status line shows how many left parentheses are currently hanging open. Cool, ~%")
  (format t "huh?)~%~%"))

(defun print-chapter-12 ()
  (format t "~%~%=== 12. Creating Functions ===~%~%")
  (format t "There'd be no point to Scheme (or Lisp) if you couldn't define your own functions. ~%")
  (format t "You get a function when you evaluate a lambda-expression. lambda is another piece ~%")
  (format t "of special syntax, and here's how you use it:~%~%")
  (print-example "(lambda (n) (+ n 1))" "[function]")
  (format t "~%See? A function! You can probably guess what it does, but let's check:~%~%")
  (print-example "(define fred (lambda (n) (+ n 1)))" "[function]")
  (print-example "(fred 56)" "57")
  (format t "~%First we define fred to be our new function, and then we call it with 56 as the ~%")
  (format t "argument. You don't need the definition, by the way. (lambda (n) (+ n 1)) ~%")
  (format t "evaluates to a function, so you can use it as the first term of a list, just like ~%")
  (format t "+ or car:~%~%")
  (print-example "((lambda (n) (+ n 1)) 56)" "57")
  (format t "~%It's a little hard to read, but count the parentheses and you'll see how it works.~%~%")
  (format t "Time to be more specific. The lambda syntax takes two arguments. The first must ~%")
  (format t "be a list of atoms, which are the names of the arguments of the function. The ~%")
  (format t "second is an expression which is evaluated to produce the function result.~%~%")
  (format t "So when we call our little one-argument function, it's kind of like we did the ~%")
  (format t "following:~%~%")
  (print-example "(define n 56)" "56")
  (print-example "(+ n 1)" "57")
  (format t "~%Only, not really. Function argument definitions are local, not global. The assignment ~%")
  (format t "of 56 to n is only visible to the function. In the outside world, the real world, ~%")
  (format t "n isn't affected at all.~%~%")
  (print-example "(define n 11)" "11")
  (print-example "(fred 93)" "94")
  (print-example "n" "11")
  (format t "~%This is important. It's called static binding. Comp Sci types think it's really cool. ~%")
  (format t "It means that functions are all wrapped up in themselves -- they can't have side ~%")
  (format t "effects, and they can't be affected by outside influences (besides their arguments, ~%")
  (format t "of course.) Um, mostly. If you're careful. We'll get back to this, I hope.~%~%")
  (format t "A function can have any number of arguments, including zero:~%~%")
  (print-example "(define greeter (lambda () 'hello))" "[function]")
  (print-example "(greeter)" "hello")
  (print-example "(greeter 5)" "[Error: too many arguments to function]")
  (format t "~%You can also have a function with a variable number of arguments. (Remember +?) ~%")
  (format t "You do this by giving an atom as the second argument to lambda, instead of a list ~%")
  (format t "of atoms. The whole list of arguments (possibly empty) gets assigned to the atom, ~%")
  (format t "inside the function.~%~%")
  (print-example "(define prefix-foo (lambda args (cons 'foo args)))" "[function]")
  (print-example "(prefix-foo 5)" "(foo 5)")
  (print-example "(prefix-foo 'a 'bb 'ccc)" "(foo a bb ccc)")
  (print-example "(prefix-foo)" "(foo)")
  (format t "~%"))

(defun print-chapter-13 ()
  (format t "~%~%=== 13. Fun With Recursion ===~%~%")
  (format t "How shall we define recursion? The old (old, old) joke is 'Recursion: See recursion.' ~%")
  (format t "This is a stinking lie. The correct definition is 'Recursion: If you already know ~%")
  (format t "what recursion is, just remember the answer. Otherwise, find someone who is standing ~%")
  (format t "closer to Douglas Hofstadter than you are; then ask him or her what recursion is.'~%~%")
  (format t "(That's original with me, folks, so attribute it if you quote it. :-)~%~%")
  (format t "The idea is that you handle the very simplest case directly. For more complex cases, ~%")
  (format t "you chop off the littlest toe of the problem, handle that bit, and call yourself on ~%")
  (format t "the remaining nearly-as-big problem... because you know you can handle it. Because ~%")
  (format t "you know you can handle the slightly-smaller-than-that problem. Because... eventually, ~%")
  (format t "because you know you can handle the very simplest case. In math, your teacher called ~%")
  (format t "this 'proof by induction.'~%~%")
  (format t "Scheme loves this stuff. Look at the car and cdr stuff; they're designed to split ~%")
  (format t "a list into the first bit and the remaining nearly-as-big bit. Perfect! Let's use this ~%")
  (format t "to define a function which finds the last term in a list.~%~%")
  (format t " >>(define last (lambda (ls)~%")
  (format t "  >   (cond~%")
  (format t "  >     ((null? (cdr ls)) (car ls))~%")
  (format t "  >     (t (last (cdr ls))))))~%~%")
  (format t "Well, that's a bargeload of parentheses, isn't it. Let's sort through it. We define ~%")
  (format t "last to be a function, that is, the result of a lambda-expression. This function ~%")
  (format t "takes one argument, ls. It consists of a cond expression. First clause: if ~%")
  (format t "(cdr ls) is null?, then return (car ls). Otherwise, return (last (cdr ls)). ~%")
  (format t "That's all.~%~%")
  (format t "Got it yet? The idea is this: if you have a one-term list, the last term is the ~%")
  (format t "first term. You can check this by seeing if (cdr ls) is nil, because only in a ~%")
  (format t "one-term list is the cdr empty. So if (null? (cdr ls)), we should return the first ~%")
  (format t "term in the list, which is (car ls). Otherwise, we have a list which is two terms or ~%")
  (format t "more -- so we can find its last element by calling last on its cdr! It's not an ~%")
  (format t "infinite loop, because the cdr of ls is shorter than ls is.~%~%")
  (print-example "(last '(a))" "a")
  (print-example "(last '(a bb))" "bb")
  (print-example "(last '(a bb c))" "c")
  (print-example "(last '(a bb c (xx)))" "(xx)")
  (format t "~%And lo, it works.~%~%"))

(defun print-chapter-14 ()
  (format t "~%~%=== 14. Local Definitions With Let ===~%~%")
  (format t "You often want to define temporary values -- helper functions, or temporary storage ~%")
  (format t "for partially-computed values, or whatever. Scheme allows you to do this with the ~%")
  (format t "let syntax, and a few related ones.~%~%")
  (format t " (let~%")
  (format t "    (~%")
  (format t "      (atom1 value1)~%")
  (format t "      (atom2 value2)~%")
  (format t "      ...~%")
  (format t "    )~%")
  (format t "    result~%")
  (format t " )~%~%")
  (format t "What goes on here is this: First, each of the value expressions is evaluated. Then ~%")
  (format t "a local binding is made, with each value assigned to its atom. Then the result ~%")
  (format t "expression is evaluated, with those assignments in place. The local assignments ~%")
  (format t "are thrown away, and the value of result is returned.~%~%")
  (format t "This sort of temporary assignment is exactly like what happens with function arguments. ~%")
  (format t "The definitions are only visible to the result expression -- not in the outside world. ~%")
  (format t "In fact, you can rewrite a let expression as a function definition and call:~%~%")
  (format t " ((lambda (atom1 atom2 ...) result)  (value1 value2 ...))~%~%")
  (format t "But the let syntax is easier to read. Example time...~%~%")
  (format t " >>(let~%")
  (format t "  >    ((a 1) (b 2))~%")
  (format t "  >  (+ a b))~%")
  (format t " 3~%~%")
  (format t " >>(let~%")
  (format t "  >    ((func car))~%")
  (format t "  >  (func '(a b c)))~%")
  (format t " a~%~%")
  (format t " >>(let~%")
  (format t "  >    ((cost '(nickel dime)))~%")
  (format t "  >  (eqv? cost cost))~%")
  (format t " t~%")
  (print-example "a" "[Error: undefined atom: a]")
  (print-example "func" "[Error: undefined atom: func]")
  (print-example "cost" "[Error: undefined atom: cost]")
  (format t "~%It's important to understand that all the values in a let are evaluated before ~%")
  (format t "any of the assignments are made. More correctly, none of the local assignments are ~%")
  (format t "visible to the value expressions; they're only visible to the result expression. If you ~%")
  (format t "want to make several definitions that refer to each other, you could use nested let ~%")
  (format t "statements. It's easier to use the let* syntax, however. let* is just like let, ~%")
  (format t "except that the assignments are made in order, and each one is visible to all the ~%")
  (format t "values after it.~%~%")
  (format t " >>(let*~%")
  (format t "  >  ((a 1)  (b (+ a 1)))~%")
  (format t "  >  b)~%")
  (format t " 2~%~%")
  (format t "If you tried that with let, you'd get an 'undefined atom: a' error, because a ~%")
  (format t "isn't defined at the point where (+ a 1) is evaluated.~%~%"))

(defun print-chapter-15 ()
  (format t "~%~%=== 15. Recursion, Functions, Endless Fun ===~%~%")
  (format t "let* is cool, but it doesn't allow you to do really funky recursive stuff, with ~%")
  (format t "several definitions that really all reference each other. For that, you need ~%")
  (format t "letrec. letrec has the same form as let and let*, but any of the assigned ~%")
  (format t "values can use any of the other atoms being bound. Sort of. The catch is, the ~%")
  (format t "atoms being bound can only be used inside lambda-expressions. So this isn't ~%")
  (format t "legal:~%~%")
  (format t " >>(letrec~%")
  (format t "  >  ((a a))~%")
  (format t "  >  a)~%~%")
  (format t "This is bad because the use of the atom being defined (the second a, that ~%")
  (format t "is) isn't 'protected' inside a lambda-expression. If you think this is an ~%")
  (format t "arbitrary restriction, consider: if it were legal, what the heck would it ~%")
  (format t "evaluate to?~%~%")
  (format t "In case you were about to try this, by the way -- yes, I see you in the corner ~%")
  (format t "-- you'll find it doesn't produce an error; it returns nil. If there were ~%")
  (format t "more terms being defined in the letrec, the results could be even stranger. ~%")
  (format t "This is because I used a fairly unpleasant hack when writing the letrec ~%")
  (format t "code. (A legal hack, honest. The Scheme standard says that this sort of ~%")
  (format t "letrec abuse produces undefined results.)~%~%")
  (format t "So how can letrec be used? Well, the following is legal:~%~%")
  (format t " >>(letrec~%")
  (format t "  >  ((a (lambda () a)))~%")
  (format t "  >  a)~%~%")
  (format t "The assignment value is (lambda () a), which only uses a inside the ~%")
  (format t "lambda-expression. (The final term of the letrec, which in this case is a, ~%")
  (format t "is allowed to use all the defined atoms -- they've all been defined by that ~%")
  (format t "time.) What does this do? Let's see:~%~%")
  (format t " >>(define thing~%")
  (format t "  >  (letrec~%")
  (format t "  >    ((a (lambda () a)))~%")
  (format t "  >    a))~%")
  (format t " [function]~%~%")
  (format t "Okay, thing has been defined to be a function. What function is this? It's the ~%")
  (format t "function which the letrec statement temporarily defined to be a. What was a ~%")
  (format t "defined as? A function which takes no arguments, and returns whatever a was ~%")
  (format t "defined as.~%~%")
  (format t "In other words, this is everyone's favorite twisted example: the function which ~%")
  (format t "returns itself. Let's check that:~%~%")
  (print-example "thing" "[function]")
  (print-example "(thing)" "[function]")
  (print-example "((thing))" "[function]")
  (print-example "(eqv? thing (thing))" "t")
  (format t "~%Even eqv? agrees: the function assigned to thing is the very same function ~%")
  (format t "returned by evaluating (thing), which is to say, the result of calling the ~%")
  (format t "function assigned to thing. (Notice that in this chapter, I'm being careful ~%")
  (format t "to distinguish the value assigned to thing from the atom thing itself. The ~%")
  (format t "function doesn't return an atom; it returns a function.)~%~%")
  (format t "What would happen if we tried this with let instead of letrec? Go ahead, ~%")
  (format t "try it.~%~%")
  (format t " >>(define thing~%")
  (format t "  >  (let~%")
  (format t "  >    ((a (lambda () a)))~%")
  (format t "  >    a))~%")
  (format t " [function]~%")
  (print-example "(thing)" "[Error: undefined atom: a]")
  (format t "~%Like we said, all the values in a let are evaluated before any of the ~%")
  (format t "assignments are made. So that lambda-expression is evaluated, producing a ~%")
  (format t "function which returns whatever is assigned to a. But at this point, nothing ~%")
  (format t "has been assigned to a yet. And -- remember static binding -- functions ~%")
  (format t "are all wrapped up in themselves. The function can't see the temporary ~%")
  (format t "assignment to a which is made later on. That assignment only exists within ~%")
  (format t "the scope of the let statement. You can do it with letrec, because all the ~%")
  (format t "assignments in a letrec exist within each other's scope. (Although they're ~%")
  (format t "incomplete, in a funny sense, which is why they have to be protected in a ~%")
  (format t "function.)~%~%"))

(defun print-chapter-16 ()
  (format t "~%~%=== 16. Scope ===~%~%")
  (format t "Oops, I started talking about scope. Time to define it.~%~%")
  (format t "All of the local assignments in a function -- its arguments, and any ~%")
  (format t "assignments made by let and variants -- are fixed at the point where the ~%")
  (format t "function was defined. That's what 'static' means. Look at the position of the ~%")
  (format t "lambda expression, see what let statements it's inside, and you can tell what ~%")
  (format t "assignments it knows about. That's the function's scope.~%~%")
  (format t "Top-level definitions -- those created with define -- are different. ~%")
  (format t "They're visible everywhere. They can also change at any time. Convenient, ~%")
  (format t "but possibly a source of bugs.~%~%")
  (format t "Best to illustrate the difference with an example. Can't you define the evil ~%")
  (format t "function that returns itself more simply, this way?~%~%")
  (format t " >>(define selfer~%")
  (format t "  >  (lambda () selfer))~%")
  (format t " [function]~%~%")
  (format t "Well, yes, that works. But it's dependent on the global definition of selfer. ~%")
  (format t "If you break that, the function stops working.~%~%")
  (print-example "(define another-selfer selfer)" "[function]")
  (print-example "(define selfer 'toast)" "toast")
  (print-example "(another-selfer)" "toast")
  (print-example "(eqv? another-selfer (another-selfer))" "nil")
  (format t "~%The thing function defined in the previous chapter will always keep working, ~%")
  (format t "even if we assigned it to another-thing and changed the definition of thing. ~%")
  (format t "If a function doesn't rely on top-level definitions that might change, its ~%")
  (format t "behavior is completely predictable. This is generally a good thing.~%~%")
  (format t "On the other hand, a function that does rely on top-level definitions can be ~%")
  (format t "affected by other functions that change that definition. This sort of thing is ~%")
  (format t "called a side effect; doing one thing has a side effect which affects another ~%")
  (format t "thing. In functional programming, people say they hate side effects, but actually ~%")
  (format t "it's hard to break the habit of using them. (Imperative languages like Pascal ~%")
  (format t "and C are made of side effects. Every time you change the value of a variable, ~%")
  (format t "you affect everything else that uses that variable.)~%~%"))

(defun print-chapter-17 ()
  (format t "~%~%=== 17. Return ===~%~%")
  (format t "Well, that's it for the manual. We haven't covered all of Scheme by any means, ~%")
  (format t "but we've gone through all the foundations. If you've been following along with ~%")
  (format t "the genie's exercises, you have a handle on how to think in Scheme.~%~%")
  (format t "The definitive reference book on Scheme is The Scheme Programming Language, ~%")
  (format t "by R. Kent Dybvig. (No, I have no idea. I can't think of a color which starts ~%")
  (format t "with D.) Pick it up, and find a real Scheme interpreter, if you're interested ~%")
  (format t "in learning more. If you ever figure out how continuations work, please come and ~%")
  (format t "explain them to me.~%~%")
  (format t "If it crosses your path of life to learn Lisp, you'll find that it's pretty much ~%")
  (format t "what you've learned here; just a little messier. (The biggest difference is that ~%")
  (format t "atoms have two distinct values assigned to them -- a data value, and a function ~%")
  (format t "value. I've never understood why. It's just something to remember.)~%~%")
  (format t "If you think this whole thing is a waste of your time... then why did you read ~%")
  (format t "this far?~%~%")
  (format t "Have fun.~%~%"))

(defun print-chapter-18 ()
  (format t "~%~%=== 18. Reference: Functions ===~%~%")
  (format t "This is a list of all pre-defined functions. Some of them have more functionality ~%")
  (format t "than the tutorial describes, so listen up.~%~%")
  (format t "(car v) : 1 argument (a non-empty list)~%")
  (format t "Returns the first term of v.~%~%")
  (format t "(cdr v) : 1 argument (a non-empty list)~%")
  (format t "Returns the list containing all but the first term of v.~%~%")
  (format t "(cons v w) : 2 arguments (the second a list)~%")
  (format t "Returns the list whose first term is v and the rest of whose terms are ~%")
  (format t "the terms of w.~%~%")
  (format t "(length v) : 1 argument (a list)~%")
  (format t "Returns the number of terms in v.~%~%")
  (format t "(list v ...) : 0 or more arguments~%")
  (format t "Returns the list whose terms are the given arguments.~%~%")
  (format t "(not v) : 1 argument~%")
  (format t "Returns t if v is nil, and nil otherwise.~%~%")
  (format t "(eqv? v w) : 2 arguments~%")
  (format t "Returns t if v and w are both nil, or are the same atom, or were created at ~%")
  (format t "the same time. Returns nil otherwise.~%~%")
  (format t "(equal? v w) : 2 arguments~%")
  (format t "Returns t if v and w are eqv?, or are lists of the same length all of whose ~%")
  (format t "terms are equal?.~%~%")
  (format t "(null? v) : 1 argument~%")
  (format t "Returns t if v is nil, and nil otherwise. (Yes, this is the same as not.)~%~%")
  (format t "(list? v) : 1 argument~%")
  (format t "Returns t if v is a list, including nil. Returns nil if v is anything ~%")
  (format t "else, such as an atom or function.~%~%")
  (format t "(= v ...) : 1 or more arguments (all numbers)~%")
  (format t "Returns t if all the arguments are the same number. If there is only one ~%")
  (format t "argument, always returns t.~%~%")
  (format t "(> v ...) : 1 or more arguments (all numbers)~%")
  (format t "Returns t if all the arguments are numbers in strictly descending sequence. ~%")
  (format t "Otherwise returns nil. If there is only one argument, always returns t.~%~%")
  (format t "(>= v ...) : 1 or more arguments (all numbers)~%")
  (format t "Returns t if all the arguments are numbers in descending sequence, not necessarily ~%")
  (format t "strictly. Otherwise returns nil. If there is only one argument, always returns t.~%~%")
  (format t "(< v ...) : 1 or more arguments (all numbers)~%")
  (format t "Returns t if all the arguments are numbers in strictly ascending sequence. ~%")
  (format t "Otherwise returns nil. If there is only one argument, always returns t.~%~%")
  (format t "(<= v ...) : 1 or more arguments (all numbers)~%")
  (format t "Returns t if all the arguments are numbers in ascending sequence, not necessarily ~%")
  (format t "strictly. Otherwise returns nil. If there is only one argument, always returns t.~%~%")
  (format t "(+ v ...) : 0 or more arguments (all numbers)~%")
  (format t "Returns the sum of all the arguments. If there are no arguments, returns 0.~%~%")
  (format t "(- v ...) : 0 or more arguments (all numbers)~%")
  (format t "Returns the first argument minus the sum of all the other arguments. ~%")
  (format t "If there is only one argument, returns its negative. If there are no arguments, ~%")
  (format t "returns 0.~%~%")
  (format t "(eval v) : 1 argument~%")
  (format t "Returns the result of evaluating v. (Note that since eval is a function, ~%")
  (format t "whatever you give as the argument is evaluated before it is handed in. So ~%")
  (format t "eval sort of double-evaluates whatever you give it.)~%~%"))

(defun print-chapter-19 ()
  (format t "~%~%=== 19. Reference: Syntactic Forms ===~%~%")
  (format t "This is a list of all the special syntax forms available.~%~%")
  (format t "(quote v) : 1 argument~%")
  (format t "Returns v, without evaluating it at all.~%~%")
  (format t "(error ...) : any number of arguments~%")
  (format t "Causes an error. The arguments are ignored. This aborts the evaluation of an ~%")
  (format t "expression; once any part causes an error, the entire thing results in an error.~%~%")
  (format t "(cond clause1 ...) : any number of clauses; each clause is a list of either one ~%")
  (format t "or two terms~%")
  (format t "Goes through the clauses, in order. If the first (or only) term of a clause ~%")
  (format t "evaluates to nil, it is skipped and the next one tested. The leftmost clause whose ~%")
  (format t "first term evaluates to non-nil is the winner. If it has only one term, that ~%")
  (format t "non-nil value is returned. If it has two, the result of evaluating the second ~%")
  (format t "term is returned. If no clause is a winner, nil is returned.~%~%")
  (format t "(define atom v) : two arguments; the first an atom~%")
  (format t "The result of evaluating v is assigned to atom (a top-level definition). If ~%")
  (format t "atom already has a top-level definition, the older definition is replaced. ~%")
  (format t "The new value is also returned.~%~%")
  (format t "(lambda arglist v) : two arguments~%")
  (format t "Returns a function. The scope of the function is the scope in which the ~%")
  (format t "lambda-expression is evaluated to produce it. When a function is called, the arguments ~%")
  (format t "it is given are assigned to the atoms in arglist, producing a new scope on top ~%")
  (format t "of the function's scope; v is then evaluated in this new scope. arglist may ~%")
  (format t "be a single atom (in which case all the function's arguments are put into a list ~%")
  (format t "which is assigned to that atom), or nil (in which case the function takes zero ~%")
  (format t "arguments), or a list of atoms (in which case the function takes that many ~%")
  (format t "arguments.)~%~%")
  (format t "(let ((atom1 def1) ...) v) : two arguments; the first is a list of clauses; each ~%")
  (format t "clause is a list of two terms~%")
  (format t "All the definitions in the list of clauses are evaluated, in the current scope. ~%")
  (format t "Then a new scope is created, in which those values are assigned to their respective ~%")
  (format t "atoms (a local or temporary definition.) v is evaluated in this new scope, and ~%")
  (format t "the result is returned.~%~%")
  (format t "(let* ((atom1 def1) ...) v) : two arguments; the first is a list of clauses; each ~%")
  (format t "clause is a list of two terms~%")
  (format t "In the current scope, def1 is evaluated. A new scope is created in which ~%")
  (format t "the resulting value is assigned to atom1. In this new scope, def2 is evaluated. ~%")
  (format t "A newer scope is created in which the resulting value is assigned to atom2. This ~%")
  (format t "continues until all clauses are handled. v is evaluated in the final scope, and ~%")
  (format t "the result is returned.~%~%")
  (format t "(letrec ((atom1 def1) ...) v) : two arguments; the first is a list of clauses; each ~%")
  (format t "clause is a list of two terms~%")
  (format t "A new scope is created in which the atoms of all the clauses are assigned undefined ~%")
  (format t "values. All the definitions of the clauses are then evaluated, in this new scope. ~%")
  (format t "(For the results to be valid, all uses of the atoms must be inside lambda-expressions.) ~%")
  (format t "The resulting values are written into the scope, completing it, and then v is ~%")
  (format t "evaluated in the scope.~%~%"))

(defun print-chapter-20 ()
  (format t "~%~%=== 20. Reference: Improper Lists ===~%~%")
  (format t "And finally, I have been convinced to put in more about improper lists. (Chapter ~%")
  (format t "8 was where I said that I didn't feel like explaining them.) This section is tacked ~%")
  (format t "on as reference material because you don't really need to know about improper lists ~%")
  (format t "for the purposes of this tutorial, but you're undoubtedly going to stumble into ~%")
  (format t "them anyway. So you can think of this chapter as an appendix. A weird little ~%")
  (format t "continuation tacked on after the manual's full stop. Ha! I just kill myself sometimes. ~%")
  (format t "Or at least strain my back reaching after feeble jokes.~%~%")
  (format t "An improper list, or dotted list, is just what you get when the cdr of a list is ~%")
  (format t "not a list. That is, if the second argument of a cons call is not a list, the ~%")
  (format t "result of the cons will be an improper list.~%~%")
  (format t "You can also create an improper list by typing the dotted form itself.~%~%")
  (print-example "'(a . b)" "(a . b)")
  (format t "~%If the second argument of a cons call is an improper list, the result will be a ~%")
  (format t "longer improper list.~%~%")
  (print-example "(cons 'z '(a . b))" "(z a . b)")
  (print-example "(cons 'rats '(z a . b))" "(rats z a . b)")
  (format t "~%There can only be one dot in an improper list, and it will always be just before ~%")
  (format t "the last term. Why? Well, in a proper list, the cdr is either a non-empty list ~%")
  (format t "(meaning there's more to come) or the empty list (meaning you've reached the ~%")
  (format t "end.) In an improper list, the cdr is a non-list, meaning you've reached a ~%")
  (format t "strange sort of appendix; but you can't go on after that, because the appendix ~%")
  (format t "isn't a list, it's just one thing; so there's no more to do. The appendix is ~%")
  (format t "written after the dot, and then you're done.~%~%")
  (format t "Of course, if the appendix is a list, then you're not writing an improper ~%")
  (format t "list at all. This follows directly from the rules, and a little experimentation ~%")
  (format t "shows that it's true:~%~%")
  (print-example "'(a . (b c))" "(a b c)")
  (format t "~%An improper list isn't actually that awful; list? will say that it is a list, ~%")
  (format t "and you can do anything with it that you can do with a proper list. The only ~%")
  (format t "problem is that if you take its cdr, and assume the result is a list, you will ~%")
  (format t "get an ugly surprise. Many Scheme functions (including some built-in ones) do ~%")
  (format t "make this assumption, and they will choke horribly when fed improper lists. ~%")
  (format t "However, if you want to use improper lists for your own purposes, there's no ~%")
  (format t "reason not to.~%~%")
  (format t "Now we're done.~%~%"))

(defun print-manual-chapter (n)
  "Print chapter N of the manual"
  (case n
    (0 (print-chapter-0))
    (1 (print-chapter-1))
    (2 (print-chapter-2))
    (3 (print-chapter-3))
    (4 (print-chapter-4))
    (5 (print-chapter-5))
    (6 (print-chapter-6))
    (7 (print-chapter-7))
    (8 (print-chapter-8))
    (9 (print-chapter-9))
    (10 (print-chapter-10))
    (11 (print-chapter-11))
    (12 (print-chapter-12))
    (13 (print-chapter-13))
    (14 (print-chapter-14))
    (15 (print-chapter-15))
    (16 (print-chapter-16))
    (17 (print-chapter-17))
    (18 (print-chapter-18))
    (19 (print-chapter-19))
    (20 (print-chapter-20))
    (t (format t "Chapter not found.~%"))))

(defun display-manual-menu ()
  "Display the table of contents"
  (format t "~%~%")
  (format t "=======================================================================~%")
  (format t "       A Simple Programmer's Introduction to Scheme~%")
  (format t "=======================================================================~%~%")
  (format t "Table of Contents:~%~%")
  (dotimes (i 21)
    (format t "  ~2d: ~a~%" i (manual-chapter-name i)))
  (format t "~%Enter chapter number (0-20), or 'q' to quit: "))

(defun cmd-manual ()
  (if (or *manual-available* (>= *genie-state* 2))
      (loop
        (display-manual-menu)
        (finish-output)
        (let ((input (read-line)))
          (cond
            ((or (string= input "q") (string= input "quit"))
             (format t "~%Closing manual.~%")
             (return))
            ((and (every #'digit-char-p input)
                  (not (string= input "")))
             (let ((n (parse-integer input)))
               (if (<= 0 n 20)
                   (progn
                     (print-manual-chapter n)
                     (format t "~%[Press Enter to continue]")
                     (read-line))
                   (format t "~%Invalid chapter number. Please enter 0-20.~%"))))
            (t (format t "~%Invalid input. Enter a number 0-20, or 'q' to quit.~%")))))
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

  ;; Initialize game systems
  (init-game-objects)
  (init-command-table)

  (game-loop))

;;; EOF
