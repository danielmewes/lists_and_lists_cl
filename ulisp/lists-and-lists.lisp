;;;; Lists and Lists - A uLisp Port
;;;; Copyright 1996 by Andrew Plotkin - original Z-machine version
;;;; uLisp port 2025 by Daniel Mewes
;;;;
;;;; An Interactive Tutorial for learning Scheme/Lisp

;;; ============================================================================
;;; COMMON LISP FUNCTION STAND-INS FOR uLISP
;;; ============================================================================

(defun string-downcase (str)
 (let ((result ""))
   (dotimes (i (length str))
     (let* ((ch (char str i))
            (code (char-code ch)))
       (when (and (>= code 65) (<= code 90))
         (setq code (+ code 32)))
       (setq result (concatenate 'string result (string (code-char code)))))) result))

(defun digit-char-p (ch)
  (let ((code (char-code ch)))
    (and (>= code 48) (<= code 57))))

(defun every (predicate sequence)
  (let ((result t))
    (cond
      ((stringp sequence)
       (dotimes (i (length sequence))
         (unless (funcall predicate (char sequence i))
           (setq result nil)
           (return))))
      ((listp sequence)
       (dolist (item sequence)
         (unless (funcall predicate item)
           (setq result nil)
           (return))))) result))

(defun parse-integer (str)
  (let ((result 0)
        (len (length str)))
    (dotimes (i len)
      (let* ((ch (char str i))
             (digit (- (char-code ch) 48)))
        (setq result (+ (* result 10) digit)))) result))

(defun char-in-string-p (ch str)
  (let ((found nil))
    (dotimes (i (length str))
      (when (eq ch (char str i))
        (setq found t)
        (return))) found))

(defun remove-if (predicate lst)
  (let ((result nil))
    (dolist (item lst)
      (unless (funcall predicate item)
        (push item result)))
    (reverse result)))

(defun string-trim (char-bag str)
  (let ((start 0)
        (end (- (length str) 1))
        (len (length str)))
    (loop
      (when (or (>= start len)
                (not (char-in-string-p (char str start) char-bag)))
        (return))
      (setq start (+ start 1)))
    (loop
      (when (or (< end 0)
                (not (char-in-string-p (char str end) char-bag)))
        (return))
      (setq end (- end 1)))
    (if (> start end) ""
        (let ((result ""))
          (dotimes (i (+ (- end start) 1))
            (setq result (concatenate 'string result
                                     (string (char str (+ start i)))))) result))))

(defun symbol-function (f) f)

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
  (let ((pair (assoc sym (cddr env)))
        ;; Workaround for uLisp not supporting cddr as place argument to push
        (env_cdr (cdr env)))
    (if pair
        (setf (cdr pair) val)
        (push (cons sym val) (cdr env_cdr))))
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
                                             (scheme-eval (scheme-cons-to-list (first args)) *global-env*)))
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

(defun scheme-cons-to-list (obj)
  "Convert a scheme data structure back to a Common Lisp s-expression"
  (cond
    ((null obj) nil)
    ((eq obj t) t)
    ((numberp obj) obj)
    ((scheme-atom-p obj) (scheme-atom-name obj))
    ((scheme-cons-p obj)
     (cons (scheme-cons-to-list (scheme-cons-car obj))
           (scheme-cons-to-list (scheme-cons-cdr obj))))
    (t obj)))

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
(defun wait-for-enter ()
  (format t "~%[Press Enter to continue]")
  (read-line))

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
  (format t "The genie looks at you quizzically. \"No need to~%shout.\"~%"))

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
  (setf *genie-state* 1)
  (setf *genie-waiting* t)
  (format t "~%~A~%" initial-action-msg)
  (format t "~%The genie looks you over, squinching his face in a~%manner to which mere mortals cannot aspire. \"Okay,~%okay,\" he rumbles. \"Welcome to Hell. Might as well~%get to work.\"~%~%")
  (format t "He leaps from the couch, lands soundlessly on the~%table, and gestures. \"Over here. Workstation. State~%of the art -- well, it was fifty years ago. But the~%language is timeless.\"~%~%")
  (format t "\"Now. I am required by the Last Rite to offer you~%tutorial instruction. Do you want it?\"~%"))

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
    "You always thought genies were folklore, but now
that you've encountered one you find you really can't
mistake it. He's eight feet tall, bright shimmering
bronze, absolutely covered with tasteless
wrought-gold jewelry, and he smells of ozone.")))
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
        (format t "~a~%" (object-description obj))
        (when (genie-asleep-p)
          (format t "He's also quite dead to the world, snoring like mad~%on the lumpy couch.~%")))
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
  "Alarm box is visible in lab when genie is asleep"
  (and (in-lab-p) (genie-asleep-p)))

(defun open-alarm-box-object (obj)
  (format t "The translucent glass is seamless.~%"))

(defun search-alarm-box-object (obj)
  (format t "You can't make out what's inside the translucent~%box.~%"))

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
    "It's a plate of thin gold, engraved with angular
designs. In the center you see the words \"*** You
have won ***\"")))
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
      (format t "The door to the north is ancient, stained, knotted~%wood. It looks terribly out of place here. In fact,~%it IS out of place here. The door ~A.~%"
              (if *door-open* "stands open" "is closed"))
      (format t "The door isn't nearly so interesting from the~%inside.~%")))

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
    "The couch has that peculiar slump of cushion that
says that this couch has seen much service, mostly to
a single vast rear end. Indeed, the depression is
perfectly molded to the tuchus that occupies it at
this very moment.")))
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
    "The desk is obviously from that school of design
that says that furniture should be clean, efficient,
unadorned, and capable of being disassembled with
allen wrenches and put into a box six feet by three
feet by two inches high.")))
    (update base 'object-type 'desk-object)
    base))

(defun desk-object-p (obj)
  (eq (value obj 'object-type) 'desk-object))

(defun open-desk-object (obj)
  (format t "The desk doesn't have any drawers. It doesn't even~%have an inside.~%"))

(defun close-desk-object (obj)
  (format t "The desk doesn't have any drawers. It doesn't even~%have an inside.~%"))

(defun examine-desk-object (obj)
  (format t "~a" (object-description obj))
  (if (and (genie-asleep-p) (not *alarm-box-used*))
    (format t "On the desk are a computer and a small glass box.~%")
    (format t "On the desk is a computer.~%")))

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
    ((desk-object-p obj) (examine-desk-object obj))
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
    ((= num 2) "Your first problem is just to acquaint you with the
system. Start up the machine, and define TWENTYSEVEN
to have the value 27. You can ask me to 'check' when
you're ready, or 'repeat' the problem if you need me
to.")

    ((= num 3) "Let's try creating some lists. Define values for CAT
and DOG so that CAT and DOG are EQUAL? but not EQV?.
Furthermore, CDR(CAT) and CDR(DOG) must be EQV?.")

    ((= num 4) "Define ABS to be the absolute value function for
integers. That is, (ABS 4) should return 4; (ABS -5)
should return 5; and (ABS 0) should return 0.")

    ((= num 5) "Define SUM to be a function that adds up a list of
integers. So (SUM '(8 2 3)) should return 13. Make
sure it works correctly for the empty list; (SUM NIL)
should return 0.")

    ((= num 6) "This problem is like the last one, but more general.
Define MEGASUM to add up an arbitrarily nested list
of integers. That is, (MEGASUM '((8) 5 (2 () (9 1)
3))) should return 28.")

    ((= num 7) "Define MAX to be a function that finds the maximum
of a list of integers. So (MAX '(5 14 -3)) should
return 14. You can assume the list will have at least
one term.")

    ((= num 8) "Last problem. You're going to define a function
called POCKET. This function should take one
argument. Now pay attention here: POCKET does two
different things, depending on the argument. If you
give it NIL as the argument, it should simply return
8. But if you give POCKET any integer as an argument,
it should return a new pocket function -- a function
just like POCKET, but with that new integer hidden
inside, replacing the 8. Examples: (POCKET NIL) => 8
(POCKET 12) => [function] (DEFINE NEWPOCKET (POCKET
12)) => [function] (NEWPOCKET NIL) => 12 (DEFINE
THIRDPOCKET (NEWPOCKET 3)) => [function] (THIRDPOCKET
NIL) => 3 (NEWPOCKET NIL) => 12 (POCKET NIL) => 8
Note that when you create a new pocket function,
previously-existing functions should keep working.")
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
         (format t "~%The genie shakes his head. \"Invalid problem~%number.\"~%")
         nil))))

(defun check-problem-2 ()
  (let* ((result (env-lookup 'twentyseven *global-env*))
         (val (car result))
         (found (cadr result)))
    (cond
      ((not found)
       (format t "~%The genie shakes his head. \"Looks like TWENTYSEVEN~%isn't defined at all. Or if it is, you've done~%something really magical to it. Try again.\"~%")
       nil)
      ((and (numberp val) (= val 27))
       (format t "~%\"Aha! Very good.\"~%")
       t)
      (t
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
       (format t "~%\"Oops -- that's not right. They should not be~%EQV?.\"~%")
       nil)
      ((not (and (scheme-cons-p cat) (scheme-cons-p dog)))
       (format t "~%\"They need to be lists.\"~%")
       nil)
      ((not (eql (scheme-cons-cdr cat) (scheme-cons-cdr dog)))
       (format t "~%\"Nope. Remember that CDR(CAT) and CDR(DOG) must be~%EQV?.\"~%")
       nil)
      (t
       (format t "~%\"Perfect! There are actually two ways to solve this~%problem.")
       (if (and (scheme-cons-p cat) (null (scheme-cons-cdr (scheme-cons-cdr cat))))
           ;; One-term list solution
           (format t "You used the simpler one, using one-term lists. The~%trickier solution would be something like this:~%(define tail '(end))~%(define cat (cons 'head tail))~%(define dog (cons 'head tail))~%The cdrs are EQV? because they are both the thing~%defined on the first line. See?\"~%")
           ;; Multi-term or shared cdr solution
           (format t "The simple way is just to define both CAT and DOG to~%be one-term lists. That way, the cdrs are both NIL,~%and NIL is always EQV? to NIL.\"~%"))
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
                (format t "~%\"Oops -- that's not right. The result should be~%~a.\"~%"
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
                (format t "~%\"Oops -- that's not right. The result should be~%~a.\"~%"
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
                (format t "~%\"Oops -- that's not right. The result should be~%~a.\"~%"
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
                (format t "~%\"No; the initial pocket function should return 8~%when given NIL.\"~%")
                nil)
              (if (not (scheme-function-p fn2))
                  (progn
                    (format t "~%\"No; pocket should return a function when given an~%integer.\"~%")
                    nil)
                  (let ((val2 (scheme-apply fn2 (list nil)))
                        (fn3 (scheme-apply fn2 (list 3))))
                    (if (not (and (numberp val2) (= val2 12)))
                        (progn
                          (format t "~%\"No; the new pocket function should return 12 when~%given NIL.\"~%")
                          nil)
                        (if (not (scheme-function-p fn3))
                            (progn
                              (format t "~%\"No; a pocket function should return another~%function.\"~%")
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
                                        (format t "~%\"No; the second pocket function should still return~%12.\"~%")
                                        nil)
                                      (if (not (and (numberp val1-again) (= val1-again 8)))
                                          (progn
                                            (format t "~%\"No; the original pocket function should still~%return 8.\"~%")
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
     (format t "Everything here is just like it always is, except~%for that door.~%")
     (format t "~%You can see a strange door to the north.~%"))

    ((eq *current-room* 'lab)
     (format t "~%White Room~%")
     (format t "This is a comfortably cluttered room. Cluttered with~%bookshelves, mostly.~%")
     (format t "To one side is a large desk, on which a computer~%squats regally.~%")
     (when (not (genie-finished-p))
       (format t "A lumpy couch is the only other furniture of note.~%"))
     (format t "~%You can see:~%")
     (when (not (genie-finished-p))
       (if (genie-asleep-p)
           (format t "a huge genie (sleeping on the couch)~%")
           (format t "a huge genie (on the couch)~%")))
     (format t "a computer (with green and yellow buttons)~%")
     (when (and (genie-asleep-p) (not *alarm-box-used*))
       (format t "a small glass box~%"))
     (when *manual-available*
       (format t "a massive book~%"))
     (when *prize-won*
       (format t "a gold plaque~%")))

    (t (error "Invalid room"))))

(defun game-loop ()
  (format t "~%Lists And Lists~%")
  (format t "An Interactive Tutorial~%")
  (format t "Copyright 1996 by Andrew Plotkin~%")
  (format t "(Common Lisp port 2025 by Daniel Mewes)~%")
  (format t "~%(First-time players should type 'about')~%~%")

  (format t "Hey, that door wasn't there last time you walked by~%this spot. What the heck?~%")

  (describe-room)

  (loop
    (format t "~%>")
    (let* ((input (read-line))
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
            (cond
              ((and rest (cmd-matches-p (first rest) "under")) (cmd-look-under (second rest)))
              ((and rest (cmd-matches-p (first rest) "at")) (cmd-examine (second rest)))
              (t (describe-room))))

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
  (cond
    ((null what)
     (print-what-verb "break"))
    ((and (object-is-p what 'alarm-box)
           (object-visible-p-by-name what))
      (progn
        (setf *alarm-box-used* t)
        (wake-genie "You turn the box over carefully, then shrug and
swing it sharply... \"No no don't break it I'm
awake!\" A gleaming hand catches your wrist. The
genie gently -- very gently -- removes the box from
your grasp, and tucks it carefully away into nothing.")))
    (t (format t "If you're getting frustrated, maybe ask for help.~%"))))

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

        (format t "~%The computer comes to life: whirr, feeple, feep! You~%settle yourself before the keyboard as text appears~%on the screen...~%")
        (format t "~%[Welcome to the interpreter. Enter :q to exit, or :m~%for documentation, or :? for a list of other :~%commands.]~%")

        (run-interpreter)

        (format t "~%[Suspending interpreter. Press green button to~%reactivate.]~%")
        (when (genie-teaching-p)
          (setf *genie-waiting* t)
          (format t "~%You lean back. The genie glances over, and asks,~%\"Got it working yet?\"~%")))))

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
              (format t "~a =" sym)
              (scheme-print val)
              (terpri)))
          (terpri))
        ;; Recursively display parent
        (when parent
          (display-environment parent (1+ indent)))))))

(defun balanced-parens-p (str)
  "Check if parentheses are balanced in a string"
  (let ((depth 0))
    (dotimes (i (length str))
      (let ((ch (char str i)))
        (cond
          ((eq ch #\() (setq depth (+ depth 1)))
          ((eq ch #\)) (setq depth (- depth 1))))
        (when (< depth 0)
          (return nil))))
    (= depth 0)))

(defun run-interpreter ()
  (loop
    (format t "~%>>")
    (let ((line (read-line)))
      (when (null line)
        (return))

      (cond
        ((string= line ":q")
         (return))

        ((string= line ":?")
         (format t "[The following codes have special meaning at the >>~%prompt:~%")
         (format t ":? Print this list.~%")
         (format t ":q Leave the interpreter.~%")
         (format t ":m Read the manual.~%")
         (format t ":r Redisplay the current problem.~%")
         (format t ":c Cancel the expression you are typing.~%")
         (format t ":e Display everything in the current environment.]~%"))

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
         ;; Note: handler-case replaced with ignore-errors for uLisp compatibility
         (if (not (balanced-parens-p line))
             (format t "[Incomplete expression]~%")
             (let ((result (ignore-errors
                             (let* ((expr (read-from-string line))
                                    (*eval-fuel* 1000)
                                    (result (scheme-eval expr *global-env*)))
                               (format t "")
                               (scheme-print result)
                               (terpri)
                               t))))
               (when (eq result nothing)
                 (format t "[Error occurred]~%")))))))))

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
         (format t "~%The genie nods in satisfaction. \"Right. Let's see,~%let's see...\"~%")
         (format t "He pulls a massive tome out of nowhere; opens it;~%pokes studiously at it; turns a page; snorts. Then~%he~%arises from the couch to his full height, raises the~%book, and booms...~%")
         (format t "~%\"HOW TO PROGRAM IN LISP!\"~%")
         (format t "~%Then he plops back into the couch, and adds, \"...a~%self-paced course.\" He hands you the book.~%")
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
                           (format t "~%\"Congratulations,\" the genie booms. \"You are now~%an accredited hacker of Lisp.\" He hands you~%something. \"I'll let you keep playing with the~%machine. I,\" he adds with sudden intensity, \"am~%going to return to my nap.\"~%")
                           (format t "~%The genie vanishes in a puff of silver smoke. A~%moment later, the couch follows.~%"))
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
         (format t "The genie frowns thunderously. \"Fine, go play~%around on your own. See where it gets you. Wake me~%when you're tired of wasting time.\"")
         (if *alarm-box-used*
           (format t "He tosses you the glass box, turns over, and begins~%snoring. Thunderously.~%")
           (format t "He turns over, and begins snoring. Thunderously.~%")))

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
              (format t "\"I thought the question was simple enough. Are you~%interested in learning what I have to teach? Yes or~%no will do.\"~%")
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
        (format t "The genie glowers hugely at you. \"Sigh. Yes, I do~%give hints. I am required to tell you, blah blah~%blah, irreparable loss of fun, blah blah, no refunds,~%fine. So if you still want help, ask again. If any~%hint I give isn't enough, ask again.\"~%"))
      (progn
        (when (/= *hint-problem* problem)
          (setf *hint-problem* problem)
          (setf *hint-level* 0))

        (incf *hint-level*)

  ;; Simplified hints - just provide the basic guidance
  (cond
    ((= problem 2)
     (case *hint-level*
       (1 (format t "\"Have you read up through chapter 6 of the~%manual?\"~%"))
       (2 (format t "\"You need to use the DEFINE command.\"~%"))
       (t (format t "\"Do this: (DEFINE TWENTYSEVEN 27)\"~%"))))

    ((= problem 3)
     (case *hint-level*
       (1 (format t "\"Have you read up through chapter 10 of the~%manual?\"~%"))
       (2 (format t "\"If you define CAT and DOG to be identical lists,~%that will satisfy the first condition. They will be~%EQUAL?, but since they are created in two separate~%places, they will not be EQV?.\"~%"))
       (t (format t "\"Define a single list to be the cdr for both of~%them, then use CONS to attach atoms to it. Do this:~%(DEFINE TAIL '(END)) (DEFINE CAT (CONS 'HEAD TAIL))~%(DEFINE DOG (CONS 'HEAD TAIL))\"~%"))))

    ((= problem 4)
     (case *hint-level*
       (1 (format t "\"Have you read up through chapter 12 of the~%manual?\"~%"))
       (2 (format t "\"You can modify the example from chapter 11.\"~%"))
       (t (format t "\"Use LAMBDA and COND with tests for positive,~%negative, and zero.\"~%"))))

    ((= problem 5)
     (case *hint-level*
       (1 (format t "\"Have you read up through chapter 13 of the~%manual?\"~%"))
       (2 (format t "\"Use recursion, like the LAST example in chapter~%13.\"~%"))
       (3 (format t "\"The base case is when the list is empty; then~%return 0.\"~%"))
       (t (format t "\"If the list is not empty, add the first term to~%the sum of the rest.\"~%"))))

    ((= problem 6)
     (case *hint-level*
       (1 (format t "\"You can build MEGASUM the same way you built SUM,~%with one change.\"~%"))
       (2 (format t "\"The change is that the first term might be a list~%instead of a number.\"~%"))
       (t (format t "\"Use COND and test the first term with LIST?. If it~%is a list, call MEGASUM recursively to add it up.\"~%"))))

    ((= problem 7)
     (case *hint-level*
       (1 (format t "\"Have you read up through chapter 14 of the~%manual?\"~%"))
       (2 (format t "\"Consider using LET. Look at the first term, look~%at MAX of the remaining terms, choose the larger.\"~%"))
       (t (format t "\"The base case is a one-term list, not an empty~%list.\"~%"))))

    ((= problem 8)
     (case *hint-level*
       (1 (format t "\"The obvious approach won't work. You can't use a~%top-level variable because you need multiple pocket~%functions working at once.\"~%"))
       (2 (format t "\"Think about a pocket-generator function that takes~%a value and returns a pocket function containing that~%value.\"~%"))
       (3 (format t "\"Use LETREC to create a recursive function. The~%generator should return a function that either~%returns its stored value (if given NIL) or calls the~%generator to create a new pocket (if given an~%integer).\"~%"))
       (t (format t "\"The key insight: use static scoping. Each function~%remembers the environment where it was created.\"~%"))))

    (t (error "Invalid problem number"))))))

(defun cmd-about ()
  (format t "~%Lists And Lists is copyright 1996 by Andrew Plotkin.~%")
  (format t "It may be copied, distributed, and played freely.~%")
  (format t "~%Type 'help' for help with whatever you are currently~%stuck on.~%")
  (format t "~%This is a Common Lisp port of the original Z-machine~%version.~%"))

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
               (2 (format t "The genie mumbles, \"M'm awake, mmf,\" and throws an~%arm over his ear.~%")))
             ;; Genie is awake
             (format t "The genie glances up at you. \"I'm not about to fall~%asleep, not with you muttering to yourself and~%scribbling all those notes.\"~%"))))
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
             (wake-genie "A gleaming hand catches your fist. The genie gently
-- very gently -- stops your attack.")
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
             (wake-genie "A gleaming hand catches your wrist. The genie gently
-- very gently -- pushes you away.")
             (format t "The genie looks at you with amusement. \"Let's keep~%this professional.\"~%"))))
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
          (format t "Error: Could not load game (file not found or SD~%card not available?)~%")))))

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

(defun print-example (input &optional (output nil) (output-supplied-p nil))
  "Print a Scheme example with optional output"
  (format t ">>~a~%" input)
  (when output-supplied-p
    (when output
      (format t "~a~%" output))))

(defun print-chapter-0 ()

  (format t "~%")
  (format t "=== 0. Introduction ===~%")
  (format t "~%")
  (format t "A Simple Programmer's Introduction to Scheme~%")
  (format t "~%")
  (format t "This is a story about Lisp. Actually -- I'll admit it~%")
  (format t "right now -- it's about Scheme, which is a cleaned-up~%")
  (format t "version of Lisp. More suitable for teaching, and~%")
  (format t "(more to the point) easier for me to implement inside~%")
  (format t "the Z-Machine. And -- to be really honest -- I didn't~%")
  (format t "implement much of Scheme either. But I think it'll do~%")
  (format t "for a start.~%")
  (format t "~%")
  (format t "Since you have the Scheme interpreter right~%")
  (format t "underneath your fingers, your job is to try all the~%")
  (format t "examples. And experiment with new ones. Your friend~%")
  (format t "the genie will pose problems for you to solve, but~%")
  (format t "it's all self-paced. Play around as much as you want.~%")
  (format t "Refer to this manual as much as you want. Or blow the~%")
  (format t "whole thing and go fight dragons. Will I care?~%")
  (format t "(Sniff.)~%")
  (format t "~%")
)

(defun print-chapter-1 ()

  (format t "~%")
  (format t "=== 1. What's Going On Here ===~%")
  (format t "~%")
  (format t "When you start the Scheme interpreter, you see a~%")
  (format t "prompt:~%")
  (format t "~%")
  (print-example "" nil t)
  (format t "~%")
  (format t "You type things, and the interpreter responds. Try~%")
  (format t "it. Type '5' and hit Enter. The interpreter will~%")
  (format t "respond with '5'.~%")
  (format t "~%")
  (print-example "5" "5" t)
  (format t "~%")
  (format t "That's all that happens in Scheme. You can go home~%")
  (format t "now. Bye!~%")
  (format t "~%")
  (format t "Ok, I'm kidding. Sort of. It's true, really -- all~%")
  (format t "you do in Scheme is type things, and get responses.~%")
  (format t "The process is dynamic; the response can depend on~%")
  (format t "earlier things that you typed. Notice that this is~%")
  (format t "different from languages like C, or Pascal, or~%")
  (format t "Inform. In those languages, you type in an entire~%")
  (format t "program, and chuck it into the compiler, which either~%")
  (format t "grinds you out a program or complains. In Scheme, or~%")
  (format t "Lisp, you build programs out of small pieces. It's~%")
  (wait-for-enter)
  (format t "much friendlier.~%")
  (format t "~%")
  (format t "But enough about Pascal. What happens in Scheme?~%")
  (format t "~%")
  (format t "What happens -- really -- is that you type an~%")
  (format t "expression, and the interpreter evaluates the~%")
  (format t "expression and prints the result. Read, evaluate,~%")
  (format t "print. Every expression evaluates to another~%")
  (format t "expression (unless it produces an error.) We just~%")
  (format t "demonstrated this; 5 is an expression, and it~%")
  (format t "evaluates to the expression 5. Numbers evaluate to~%")
  (format t "themselves. If that's confusing, just try to get used~%")
  (format t "to it. If it makes sense, you'll probably be confused~%")
  (format t "later on, because most expressions don't evaluate to~%")
  (format t "themselves. Oh well.~%")
  (format t "~%")
)

(defun print-chapter-2 ()

  (format t "~%")
  (format t "=== 2. What's An Atom? ===~%")
  (format t "~%")
  (format t "We start with atoms.~%")
  (format t "~%")
  (format t "An atom is a bunch of characters. Letters, numbers,~%")
  (format t "most punctuation. Here are some atoms: 5 hello -5~%")
  (format t "goodbye darlene+mitchell q-5 qq=5++qqq+ ***~%")
  (format t "~%")
  (format t "If you're familiar with languages like Pascal, you're~%")
  (format t "probably desperately trying to read some of those as~%")
  (format t "additions, or subtractions, or whatever. Don't~%")
  (format t "bother. They're all just atoms. Letters, numbers, and~%")
  (format t "punctuation are all treated the same in an atom.~%")
  (format t "~%")
  (format t "(For completeness, here's the list of punctuation~%")
  (format t "which you can't use in an atom: ( ) ; : ' . Any kind~%")
  (format t "of whitespace, such as spaces or line breaks, will~%")
  (format t "separate atoms as well.)~%")
  (format t "~%")
  (format t "So ho, you ask, what about numbers? Good point. An~%")
  (format t "atom which is entirely made up of numbers, except~%")
  (format t "possibly for a minus sign at the front, counts as a~%")
  (format t "number. (Get it? Counts? Never mind.)~%")
  (format t "~%")
  (format t "We've already said that a number evaluates to itself.~%")
  (format t "That is, if an expression consists of just an atom,~%")
  (format t "and the atom is a number, the value of that~%")
  (format t "expression is that very expression -- which is to~%")
  (format t "say, that atom.~%")
  (wait-for-enter)
  (format t "~%")
  (print-example "5" "5" t)
  (print-example "-597" "-597" t)
  (print-example "0" "0" t)
  (format t "~%")
  (format t "What do other atoms evaluate to? Well, try it. I'll~%")
  (format t "just cover my ears --~%")
  (format t "~%")
  (print-example "hello" "[Error: undefined atom: hello]" t)
  (print-example "q-5" "[Error: undefined atom: q-5]" t)
  (print-example "***" "[Error: undefined atom: ***]" t)
  (format t "~%")
  (format t "Don't worry; no permanent damage done. Most~%")
  (format t "non-numeric atoms produce errors when you try to~%")
  (format t "evaluate them. This is because they are not bound to~%")
  (format t "any value. But what that means, we'll get to later.~%")
  (format t "~%")
)

(defun print-chapter-3 ()

  (format t "~%")
  (format t "=== 3. What's a List? ===~%")
  (format t "~%")
  (format t "A single atom is an expression. Is there any other~%")
  (format t "kind of expression? Of course. An expression can also~%")
  (format t "be a list. One list -- that's important. An~%")
  (format t "expression is a single atom or a single list.~%")
  (format t "~%")
  (format t "What's a list? A list is a list of expressions. That~%")
  (format t "is, a list is a list of atoms and lists. A list is~%")
  (format t "written as a pair of parentheses surrounding the~%")
  (format t "contents of the list. Here's a list: (1) It contains~%")
  (format t "one atom. Here's another list: (2 fred) It contains~%")
  (format t "two atoms. The order is significant; (2 fred) and~%")
  (format t "(fred 2) are two different lists.~%")
  (format t "~%")
  (format t "A list can contain nothing: () is the empty list.~%")
  (format t "It's famous. It's also called nil, which I think~%")
  (format t "isn't as pretty as (), but it's historical, so there~%")
  (format t "we are. nil and () both refer to the empty list.~%")
  (format t "~%")
  (format t "As I said, a list can contain both atoms and other~%")
  (format t "lists. So here are four more lists: (one (two) three)~%")
  (format t "(((one) 2 fred) xxxx9) (()) (hello mr ((operator)))~%")
  (format t "Don't be frightened by the stacks of parentheses.~%")
  (format t "It's all recursive. That last list, for example,~%")
  (format t "contains three expressions: the atom hello, the atom~%")
  (format t "mr, and the list ((operator)). Which, itself, is~%")
  (format t "one-term list containing only the list (operator).~%")
  (format t "Which is, itself, a one-term list containing only the~%")
  (wait-for-enter)
  (format t "atom operator. Get it?~%")
  (format t "~%")
  (format t "By the way, remember that an expression is either a~%")
  (format t "single atom or a single list. 1 2 3 is three separate~%")
  (format t "expressions, not any funky kind of~%")
  (format t "parenthesis-stripped list or anything. If you type~%")
  (format t "several expressions at the Scheme prompt, the~%")
  (format t "interpreter will evaluate them one at a time.~%")
  (format t "~%")
  (print-example "1 2 3 four" "1" t)
  (print-example "" "2" t)
  (print-example "" "3" t)
  (print-example "" "[Error: undefined atom: four]" t)
  (format t "~%")
)

(defun print-chapter-4 ()

  (format t "~%")
  (format t "=== 4. Functions ===~%")
  (format t "~%")
  (format t "We've seen that numeric atoms evaluate to themselves,~%")
  (format t "and other atoms seem to produce raging error~%")
  (format t "messages. What does a list evaluate to? Aha! You've~%")
  (format t "heard of functions? I really hope so. Anyway, when~%")
  (format t "you evaluate a list, you're calling a function. The~%")
  (format t "first element of the list is the function; the rest~%")
  (format t "of the elements are its arguments.~%")
  (format t "~%")
  (print-example "(+ 1 1)" "2" t)
  (format t "~%")
  (format t "Right! Addition! Let's take a closer look. We typed~%")
  (format t "in an expression: (+ 1 1). Nothing magic there; it's~%")
  (format t "a list containing three atoms, of which the first is~%")
  (format t "a plus sign and the last two are both the number 1.~%")
  (format t "The Scheme interpreter evaluates all of these. The~%")
  (format t "plus sign means addition; the 1 atoms both evaluate~%")
  (format t "to themselves. So the interpreter runs the addition~%")
  (format t "function, and hands it a pair of 1 atoms to work~%")
  (format t "with. One plus one is two, so the function returns~%")
  (format t "the expression 2. And that's what (+ 1 1) evaluates~%")
  (format t "to.~%")
  (format t "~%")
  (format t "Sneaky people will raise their hands at this point.~%")
  (format t "What am I trying to pull? 'The plus sign means~%")
  (format t "addition?' Ok, ok. What I mean is, the atom + is not~%")
  (wait-for-enter)
  (format t "one of those pernicious undefined atoms. It is~%")
  (format t "defined. Its value is the addition function. Here,~%")
  (format t "try it:~%")
  (format t "~%")
  (print-example "+" "[function]" t)
  (format t "~%")
  (format t "The interpreter doesn't try to actually print out the~%")
  (format t "addition function, because that's a bunch of internal~%")
  (format t "program code. All functions print out looking like~%")
  (format t "[function].~%")
  (format t "~%")
  (print-example "(- 1 1)" "0" t)
  (print-example "-" "[function]" t)
  (format t "~%")
  (format t "You can probably guess what function - evaluates to.~%")
  (format t "~%")
  (format t "Did I say that the interpreter evaluates all the~%")
  (format t "terms of a list? Indeed I did. This is important.~%")
  (format t "Guess what (+ (+ 1 1) 4) evaluates to? Ok, it's not~%")
  (format t "too hard, but we'll go through it step by step~%")
  (format t "anyway. It's a three-term list. The first term is +,~%")
  (format t "which evaluates to the addition function. The second~%")
  (format t "term is the list (+ 1 1), and we've already~%")
  (format t "discovered that evaluates to 2. The third term is 4,~%")
  (format t "and that evaluates to 4. The addition function is~%")
  (format t "handed the results, 2 and 4, and guess what...~%")
  (wait-for-enter)
  (format t "~%")
  (print-example "(+ (+ 1 1) 4)" "6" t)
  (format t "~%")
  (format t "Actually, the addition and subtraction functions can~%")
  (format t "take any number of arguments, not just two.~%")
  (format t "~%")
  (print-example "(+ 1 1 4)" "6" t)
  (format t "~%")
  (format t "What happens if you try to evaluate a list, and the~%")
  (format t "first term doesn't evaluate to a function? You get~%")
  (format t "errors, that's what. There are a couple of ways you~%")
  (format t "can get this wrong. As your end-of-chapter exercise,~%")
  (format t "meditate upon the following exchanges:~%")
  (format t "~%")
  (print-example "(1 2 3 4)" "[Error: object is not a function: 1]" t)
  (print-example "(spoggly 2 3 4)" "[Error: undefined atom: spoggly]" t)
  (format t "~%")
  (format t "Oops, one more thing. The empty list -- whether you~%")
  (format t "write it as () or nil -- evaluates to itself. No~%")
  (format t "function is called.~%")
  (format t "~%")
  (print-example "()" "nil" t)
  (wait-for-enter)
  (print-example "nil" "nil" t)
  (format t "~%")
)

(defun print-chapter-5 ()

  (format t "~%")
  (format t "=== 5. Quoting Expressions ===~%")
  (format t "~%")
  (format t "What's an expression which evaluates to the atom~%")
  (format t "hello? We know that hello doesn't; that produces an~%")
  (format t "error.~%")
  (format t "~%")
  (print-example "hello" "[Error: undefined atom: hello]" t)
  (format t "~%")
  (format t "It sure would be handy if there were a way to produce~%")
  (format t "an arbitrary atom. Well, watch:~%")
  (format t "~%")
  (print-example "(quote hello)" "hello" t)
  (format t "~%")
  (format t "quote acts sort of like a function which returns its~%")
  (format t "argument unchanged. (Actually, it's not a function;~%")
  (format t "it's a special piece of syntax. Can you see why? It~%")
  (format t "breaks a rule of functions. Answer at the end of this~%")
  (format t "chapter.) You can quote a list too:~%")
  (format t "~%")
  (print-example "(quote (+ 1 1))" "(+ 1 1)" t)
  (format t "~%")
  (format t "quote is so useful that you can abbreviate it, by~%")
  (format t "skipping the parentheses and just writing a single~%")
  (wait-for-enter)
  (format t "quote mark.~%")
  (format t "~%")
  (print-example "'hello" "hello" t)
  (print-example "'(+ 1 1)" "(+ 1 1)" t)
  (print-example "'5" "5" t)
  (print-example "'+'" "+" t)
  (format t "~%")
  (format t "That last example is important. It doesn't matter~%")
  (format t "that + has a value; the expression '+ -- that is,~%")
  (format t "(quote +) -- just evaluates to +.~%")
  (format t "~%")
  (format t "End of the chapter time. Why is quote not a function?~%")
  (format t "Because when Scheme evaluates a function, it~%")
  (format t "evaluates all the arguments before handing them in to~%")
  (format t "the function. The argument to quote is not evaluated;~%")
  (format t "it's handed straight in, so that it can be handed out~%")
  (format t "unchanged. Watch this:~%")
  (format t "~%")
  (print-example "quote" "[syntax]" t)
  (format t "~%")
  (format t "Special syntax forms are applied the same way~%")
  (format t "functions are, but they can have funkier effects, and~%")
  (format t "they don't necessarily evaluate all their arguments.~%")
  (format t "Keep track of this fact, because someday it'll~%")
  (wait-for-enter)
  (format t "unconfuse you about something.~%")
  (format t "~%")
)

(defun print-chapter-6 ()

  (format t "~%")
  (format t "=== 6. Defining Atoms ===~%")
  (format t "~%")
  (format t "+ has a value which is automatically defined by the~%")
  (format t "interpreter. Can we define values for atoms~%")
  (format t "ourselves? Would I ask if the answer were no? Watch~%")
  (format t "this:~%")
  (format t "~%")
  (print-example "(define x 5)" "5" t)
  (format t "~%")
  (format t "define is another piece of special syntax. It~%")
  (format t "evaluates its third argument, and assigns the~%")
  (format t "resulting value to its second argument, which must be~%")
  (format t "an atom. (Why can't define be a function? Because all~%")
  (format t "the arguments to a function are evaluated. If define~%")
  (format t "tried to evaluate its second argument, it would get~%")
  (format t "an undefined atom error, because the atom hasn't been~%")
  (format t "defined until define defines it! Whew.) For clarity,~%")
  (format t "define returns the value it just assigned. We set x~%")
  (format t "to 5; now the expression x evaluates to 5.~%")
  (format t "~%")
  (print-example "x" "5" t)
  (print-example "(+ x 1)" "6" t)
  (format t "~%")
  (format t "We don't have to use numbers, by the way. Let's set x~%")
  (wait-for-enter)
  (format t "to be bob:~%")
  (format t "~%")
  (print-example "(define x bob)" "[Error: undefined atom: bob]" t)
  (format t "~%")
  (format t "Oops -- forgot that the third value in a define~%")
  (format t "statement is evaluated. We can't use the atom bob; we~%")
  (format t "need to use an expression whose value is bob.~%")
  (format t "~%")
  (print-example "(define x 'bob)" "bob" t)
  (print-example "x" "bob" t)
  (print-example "(+ x 1)" "[Error: +: non-numeric argument: bob]" t)
  (print-example "'x" "x" t)
  (format t "~%")
  (format t "That works. Notice that we've replaced the earlier~%")
  (format t "definition of x as 5. Also note that addition~%")
  (format t "righteously complains when we feed it an atom which~%")
  (format t "isn't a number. And also also note that 'x is, still~%")
  (format t "and always, just plain x.~%")
  (format t "~%")
  (format t "You can replace the definitions of predefined~%")
  (format t "functions, too. You could do (define + 5), for~%")
  (format t "example. But I really don't recommend it. Reset the~%")
  (format t "interpreter if you get carried away with this stuff.~%")
  (format t "~%")
  (wait-for-enter)
  (format t "Last trick of the chapter. What's going on here?~%")
  (format t "~%")
  (print-example "(define addify +)" "[function]" t)
  (print-example "(addify 5 6)" "11" t)
  (format t "~%")
)

(defun print-chapter-7 ()

  (format t "~%")
  (format t "=== 7. List Chopping ===~%")
  (format t "~%")
  (format t "Back to lists. We often want to take them apart, see~%")
  (format t "what makes them tick, and put together new ones. For~%")
  (format t "that, we'll need some tools.~%")
  (format t "~%")
  (format t "A list can be split into two parts: its first term,~%")
  (format t "and its everything else. Now, in Scheme (and Lisp),~%")
  (format t "the first term is called the list's car, and the~%")
  (format t "everything else is called the list's cdr (rhymes with~%")
  (format t "'wooder', more or less.) Now every damn Lisp book in~%")
  (format t "the universe explains why these things are called car~%")
  (format t "and cdr, and I'm so sick of it I'm going to leave you~%")
  (format t "in the dark. Go look it up, if you care.~%")
  (format t "~%")
  (format t "The car of (a bb ccc) is a. The cdr of (a bb ccc) is~%")
  (format t "(bb ccc). See that? The car is the first term; the~%")
  (format t "cdr is the list minus the first term. Important,~%")
  (format t "that. Now, this doesn't mean that the car of a list~%")
  (format t "is always an atom. The car of ((1 2) x y z) is (1 2).~%")
  (format t "But the cdr of a list is always a list. It might be~%")
  (format t "the empty list, though. The cdr of (hello) is the~%")
  (format t "empty list nil.~%")
  (format t "~%")
  (format t "The empty list has neither a car nor a cdr.~%")
  (format t "~%")
  (format t "car and cdr are pre-defined functions in Scheme. (Ok,~%")
  (format t "I'm lying -- car and cdr are atoms. But they're~%")
  (format t "defined to evaluate to functions, just like + and -.~%")
  (wait-for-enter)
  (format t "Get over it.)~%")
  (format t "~%")
  (print-example "(car '(a bb ccc))" "a" t)
  (print-example "(cdr '(a bb ccc))" "(bb ccc)" t)
  (format t "~%")
  (format t "Note the quote marks. What happens if you try to~%")
  (format t "evaluate (car (a bb cc))? Why?~%")
  (format t "~%")
  (format t "car and cdr are well-wired to complain if you try to~%")
  (format t "mess with their heads:~%")
  (format t "~%")
  (print-example "(car 'a)" "[Error: car: bad argument: a]" t)
  (print-example "(car '())" "[Error: car: bad argument: nil]" t)
  (format t "~%")
  (format t "a isn't a list, so it can't have a car or cdr; and~%")
  (format t "the empty list, as we've said, has no car or cdr~%")
  (format t "either.~%")
  (format t "~%")
)

(defun print-chapter-8 ()

  (format t "~%")
  (format t "=== 8. List Constructing ===~%")
  (format t "~%")
  (format t "We takes 'em apart, we puts 'em together. If you have~%")
  (format t "a car and a cdr, you can construct a list. The cons~%")
  (format t "function does this.~%")
  (format t "~%")
  (print-example "(cons 'aaa '(bb c))" "(aaa bb c)" t)
  (print-example "(cons 'aaa nil)" "(aaa)" t)
  (print-example "(cons (car '(x y z z y)) (cdr '(x y z z y)))" "(x y z z y)" t)
  (format t "~%")
  (format t "That last example, dreadful as it appears, just~%")
  (format t "demonstrates that you can take a single list apart~%")
  (format t "into car and cdr, and reassemble it. Maybe it'd help~%")
  (format t "if I did it like this:~%")
  (format t "~%")
  (print-example "(define magic-word '(x y z z y))" "(x y z z y)" t)
  (print-example "magic-word" "(x y z z y)" t)
  (print-example "(cons (car magic-word) (cdr magic-word))" "(x y z z y)" t)
  (format t "~%")
  (format t "Oh, those lovely inside-out Scheme expressions! When~%")
  (format t "in doubt, break out the magnifying lens and count~%")
  (wait-for-enter)
  (format t "parentheses.~%")
  (format t "~%")
  (format t "The cdr of a non-empty list is always a list, so the~%")
  (format t "second argument you give to cons had better be a~%")
  (format t "list. If you break this rule, you get a broken,~%")
  (format t "twisted, evil-twin kind of not-really-a-list, called~%")
  (format t "an 'improper' or 'dotted' list.~%")
  (format t "~%")
  (print-example "(cons 'a 'b)" "(a . b)" t)
  (format t "~%")
  (format t "I don't feel like explaining this right now, so just~%")
  (format t "try not to do it.~%")
  (format t "~%")
  (format t "Another convenient function is list, which takes a~%")
  (format t "bunch of expressions and makes a list out of them.~%")
  (format t "~%")
  (print-example "(list 'a 'zzz)" "(a zzz)" t)
  (print-example "(list 'a '(1 2 3) 'end)" "(a (1 2 3) end)" t)
  (print-example "(list)" "nil" t)
  (print-example "(list '() (+ 4 5) '(+ 1 2) (cdr magic-word))" "(nil 9 (+ 1 2) (y z z y))" t)
  (format t "~%")
  (format t "I'm being tricky with that last one. If you try it~%")
  (format t "yourself, make sure you've defined magic-word the way~%")
  (format t "I did a few paragraphs back. Also, see the difference~%")
  (wait-for-enter)
  (format t "a quote can make?~%")
  (format t "~%")
)

(defun print-chapter-9 ()

  (format t "~%")
  (format t "=== 9. Tests and Logic ===~%")
  (format t "~%")
  (format t "Some functions test a condition. The = function, for~%")
  (format t "example, tests to see if two numbers are equal:~%")
  (format t "~%")
  (print-example "(= 2 2)" "t" t)
  (print-example "(= 2 5)" "nil" t)
  (format t "~%")
  (format t "As you see, the convention in Scheme is to use nil to~%")
  (format t "signify falsity, and the atom t to signify truth. (t~%")
  (format t "evaluates to itself, by the way, just as nil does.~%")
  (format t "More convenience; you don't have to quote t when you~%")
  (format t "use it.) Actually, when a true/false value is~%")
  (format t "required, you can generally supply any value; nil~%")
  (format t "will be counted as false, and anything else at all~%")
  (format t "will be counted as true.~%")
  (format t "~%")
  (format t "The <, >, <=, and >= functions work as you'd expect.~%")
  (format t "There are also some pre-defined functions which test~%")
  (format t "other properties. null? returns t if its argument is~%")
  (format t "nil, and nil otherwise. list? returns t if its~%")
  (format t "argument is a list (including nil), and nil~%")
  (format t "otherwise.~%")
  (format t "~%")
  (print-example "(null? nil)" "t" t)
  (wait-for-enter)
  (print-example "(null? t)" "nil" t)
  (print-example "(null? 0)" "nil" t)
  (print-example "(list? nil)" "t" t)
  (print-example "(list? 1)" "nil" t)
  (print-example "(list? 'a)" "nil" t)
  (print-example "(list? '(a))" "t" t)
  (format t "~%")
)

(defun print-chapter-10 ()

  (format t "~%")
  (format t "=== 10. Comparisons ===~%")
  (format t "~%")
  (format t "The = function compares numbers, but it produces an~%")
  (format t "error if you try to use it on atoms or lists. For~%")
  (format t "that, you need the eqv? and equal? functions. These~%")
  (format t "are slightly different, and the difference is sort of~%")
  (format t "technical and confusing, but it's historical. You~%")
  (format t "want to learn Scheme, you have to know eqv? and~%")
  (format t "equal? (In fact, real Scheme has eq? as well, but the~%")
  (format t "difference wasn't significant in this implementation,~%")
  (format t "so I left it out.)~%")
  (format t "~%")
  (format t "For atoms (including numbers), they both work as~%")
  (format t "you'd expect. nil, also.~%")
  (format t "~%")
  (print-example "(eqv? 1 2)" "nil" t)
  (print-example "(eqv? 2 2)" "t" t)
  (print-example "(eqv? 'one 'two)" "nil" t)
  (print-example "(eqv? 'two 'two)" "t" t)
  (print-example "(eqv? 2 'two)" "nil" t)
  (print-example "(eqv? nil 'two)" "nil" t)
  (print-example "(eqv? nil nil)" "t" t)
  (wait-for-enter)
  (format t "~%")
  (format t "(And equal? would produce the same results.)~%")
  (format t "~%")
  (format t "For complex objects, such as lists and functions,~%")
  (format t "things are trickier. If you hand two complex objects~%")
  (format t "to eqv?, you'll get t only if they both stem from the~%")
  (format t "same act of creation. (See, I told you it was~%")
  (format t "confusing.)~%")
  (format t "~%")
  (print-example "(define greeting '(hi there))" "(hi there)" t)
  (print-example "(define aloha (list 'hi 'there))" "(hi there)" t)
  (print-example "(eqv? greeting greeting)" "t" t)
  (print-example "(eqv? greeting '(hi there))" "nil" t)
  (print-example "(eqv? greeting aloha)" "nil" t)
  (print-example "(eqv? '(hi there) '(hi there))" "nil" t)
  (print-example "(define hug greeting)" "(hi there)" t)
  (print-example "(eqv? greeting hug)" "t" t)
  (print-example "(eqv? aloha hug)" "nil" t)
  (format t "~%")
  (format t "How to put this... evaluating '(hi there) creates a~%")
  (format t "list of two atoms. Evaluating (list 'hi 'there)~%")
  (wait-for-enter)
  (format t "creates another list of two atoms. The lists have the~%")
  (format t "same contents, but they're two separate objects. So~%")
  (format t "they aren't eqv? to each other. Now, when we do~%")
  (format t "(define hug greeting), we take the value of greeting~%")
  (format t "and assign that very value to hug, so they are~%")
  (format t "eqv?... see? Well, maybe.~%")
  (format t "~%")
  (format t "equal?, on the other hand, works sensibly. Two lists~%")
  (format t "are equal? if they are the same length and each pair~%")
  (format t "of terms is equal?. Given the definitions above,~%")
  (format t "you'll see that:~%")
  (format t "~%")
  (print-example "(equal? greeting greeting)" "t" t)
  (print-example "(equal? greeting '(hi there))" "t" t)
  (print-example "(equal? '(hi there) '(hi there))" "t" t)
  (print-example "(equal? greeting aloha)" "t" t)
  (print-example "(equal? greeting hug)" "t" t)
  (print-example "(equal? aloha hug)" "t" t)
  (format t "~%")
  (format t "What's the point of eqv?, seeing how weirdly it~%")
  (format t "works? It's faster. equal? has to compare every~%")
  (format t "element of a list, but eqv? just compares two~%")
  (format t "internal pointers.~%")
  (format t "~%")
  (wait-for-enter)
  (format t "Up above I said that functions are as tricky as~%")
  (format t "lists, when it comes to comparison. In fact, they're~%")
  (format t "trickier. If two functions stem from the same act of~%")
  (format t "creation, they're eqv? and equal?, just like lists.~%")
  (format t "But otherwise, two functions are never eqv? or~%")
  (format t "equal?, even if they do the exact same thing.~%")
  (format t "(There's no good way to tell if two functions do the~%")
  (format t "exact same thing. I mean, there's no way for a person~%")
  (format t "to tell. Not in all cases. Computers, forget it.)~%")
  (format t "~%")
)

(defun print-chapter-11 ()

  (format t "~%")
  (format t "=== 11. Conditionals ===~%")
  (format t "~%")
  (format t "Scheme offers a nice assortment of conditional syntax~%")
  (format t "forms. Since I'm a lazy bum, I've only implemented~%")
  (format t "the most basic one, which is cond. A cond structure~%")
  (format t "looks like this:~%")
  (format t "~%")
  (format t " (cond~%")
  (format t "    (test1 result1)~%")
  (format t "    (test2 result2)~%")
  (format t "    ...~%")
  (format t " )~%")
  (format t "~%")
  (format t "I've written it on multiple lines, but that's just~%")
  (format t "for clarity. The point is, you have a list of~%")
  (format t "clauses, and each clause is a list of two~%")
  (format t "expressions: a test and a result. The cond syntax~%")
  (format t "goes through the clauses, in order. For each one, it~%")
  (format t "evaluates the test. If the test returns true~%")
  (format t "(anything but nil), it evaluates the result, and~%")
  (format t "returns that value. If the test returns nil, it goes~%")
  (format t "on to the next clause. If all of the clauses return~%")
  (format t "nil, it returns nil.~%")
  (format t "~%")
  (format t "If you're not sure what this means, look at it this~%")
  (format t "way: in C, we'd write this as something like~%")
  (format t "~%")
  (format t "  if (test1) return result1;~%")
  (wait-for-enter)
  (format t "  else if (test2) return result2;~%")
  (format t "  ...~%")
  (format t "  else return nil;~%")
  (format t "~%")
  (format t "If you want a final 'default' clause, which will be~%")
  (format t "used if all the others fail, you can use (t~%")
  (format t "defaultresult). t is always true, and that's the~%")
  (format t "effect you want.~%")
  (format t "~%")
  (format t "You're still confused. Well, hark to the example.~%")
  (format t "~%")
  (print-example "(define val -25)" "-25" t)
  (format t " >>(cond~%")
  (format t "  >  ((> val 0) 'positive)~%")
  (format t "  >  ((< val 0) 'negative)~%")
  (format t "  >  (t 'zero)~%")
  (format t "  >)~%")
  (format t " negative~%")
  (format t "~%")
  (format t "The conditional tests val and returns one of the~%")
  (format t "atoms positive, negative, or zero. (Again, we've~%")
  (format t "typed it in on several lines. Notice that the~%")
  (format t "interpreter prints a single-arrow prompt if you've~%")
  (format t "started a list and not finished it yet; the~%")
  (format t "expression won't be evaluated until you've typed it~%")
  (format t "all in. As a bonus, the status line shows how many~%")
  (format t "left parentheses are currently hanging open. Cool,~%")
  (format t "huh?)~%")
  (format t "~%")
)

(defun print-chapter-12 ()

  (format t "~%")
  (format t "=== 12. Creating Functions ===~%")
  (format t "~%")
  (format t "There'd be no point to Scheme (or Lisp) if you~%")
  (format t "couldn't define your own functions. You get a~%")
  (format t "function when you evaluate a lambda-expression.~%")
  (format t "lambda is another piece of special syntax, and here's~%")
  (format t "how you use it:~%")
  (format t "~%")
  (print-example "(lambda (n) (+ n 1))" "[function]" t)
  (format t "~%")
  (format t "See? A function! You can probably guess what it does,~%")
  (format t "but let's check:~%")
  (format t "~%")
  (print-example "(define fred (lambda (n) (+ n 1)))" "[function]" t)
  (print-example "(fred 56)" "57" t)
  (format t "~%")
  (format t "First we define fred to be our new function, and then~%")
  (format t "we call it with 56 as the argument. You don't need~%")
  (format t "the definition, by the way. (lambda (n) (+ n 1))~%")
  (format t "evaluates to a function, so you can use it as the~%")
  (format t "first term of a list, just like + or car:~%")
  (format t "~%")
  (print-example "((lambda (n) (+ n 1)) 56)" "57" t)
  (wait-for-enter)
  (format t "~%")
  (format t "It's a little hard to read, but count the parentheses~%")
  (format t "and you'll see how it works.~%")
  (format t "~%")
  (format t "Time to be more specific. The lambda syntax takes two~%")
  (format t "arguments. The first must be a list of atoms, which~%")
  (format t "are the names of the arguments of the function. The~%")
  (format t "second is an expression which is evaluated to produce~%")
  (format t "the function result.~%")
  (format t "~%")
  (format t "So when we call our little one-argument function,~%")
  (format t "it's kind of like we did the following:~%")
  (format t "~%")
  (print-example "(define n 56)" "56" t)
  (print-example "(+ n 1)" "57" t)
  (format t "~%")
  (format t "Only, not really. Function argument definitions are~%")
  (format t "local, not global. The assignment of 56 to n is only~%")
  (format t "visible to the function. In the outside world, the~%")
  (format t "real world, n isn't affected at all.~%")
  (format t "~%")
  (print-example "(define n 11)" "11" t)
  (print-example "(fred 93)" "94" t)
  (print-example "n" "11" t)
  (wait-for-enter)
  (format t "~%")
  (format t "This is important. It's called static binding. Comp~%")
  (format t "Sci types think it's really cool. It means that~%")
  (format t "functions are all wrapped up in themselves -- they~%")
  (format t "can't have side effects, and they can't be affected~%")
  (format t "by outside influences (besides their arguments, of~%")
  (format t "course.) Um, mostly. If you're careful. We'll get~%")
  (format t "back to this, I hope.~%")
  (format t "~%")
  (format t "A function can have any number of arguments,~%")
  (format t "including zero:~%")
  (format t "~%")
  (print-example "(define greeter (lambda () 'hello))" "[function]" t)
  (print-example "(greeter)" "hello" t)
  (print-example "(greeter 5)" "[Error: too many arguments to function]" t)
  (format t "~%")
  (format t "You can also have a function with a variable number~%")
  (format t "of arguments. (Remember +?) You do this by giving an~%")
  (format t "atom as the second argument to lambda, instead of a~%")
  (format t "list of atoms. The whole list of arguments (possibly~%")
  (format t "empty) gets assigned to the atom, inside the~%")
  (format t "function.~%")
  (format t "~%")
  (print-example "(define prefix-foo (lambda args (cons 'foo args)))" "[function]" t)
  (print-example "(prefix-foo 5)" "(foo 5)" t)
  (wait-for-enter)
  (print-example "(prefix-foo 'a 'bb 'ccc)" "(foo a bb ccc)" t)
  (print-example "(prefix-foo)" "(foo)" t)
  (format t "~%")
)

(defun print-chapter-13 ()

  (format t "~%")
  (format t "=== 13. Fun With Recursion ===~%")
  (format t "~%")
  (format t "How shall we define recursion? The old (old, old)~%")
  (format t "joke is 'Recursion: See recursion.' This is a~%")
  (format t "stinking lie. The correct definition is 'Recursion:~%")
  (format t "If you already know what recursion is, just remember~%")
  (format t "the answer. Otherwise, find someone who is standing~%")
  (format t "closer to Douglas Hofstadter than you are; then ask~%")
  (format t "him or her what recursion is.'~%")
  (format t "~%")
  (format t "(That's original with me, folks, so attribute it if~%")
  (format t "you quote it. :-)~%")
  (format t "~%")
  (format t "The idea is that you handle the very simplest case~%")
  (format t "directly. For more complex cases, you chop off the~%")
  (format t "littlest toe of the problem, handle that bit, and~%")
  (format t "call yourself on the remaining nearly-as-big~%")
  (format t "problem... because you know you can handle it.~%")
  (format t "Because you know you can handle the~%")
  (format t "slightly-smaller-than-that problem. Because...~%")
  (format t "eventually, because you know you can handle the very~%")
  (format t "simplest case. In math, your teacher called this~%")
  (format t "'proof by induction.'~%")
  (format t "~%")
  (format t "Scheme loves this stuff. Look at the car and cdr~%")
  (format t "stuff; they're designed to split a list into the~%")
  (format t "first bit and the remaining nearly-as-big bit.~%")
  (format t "Perfect! Let's use this to define a function which~%")
  (format t "finds the last term in a list.~%")
  (wait-for-enter)
  (format t "~%")
  (format t " >>(define last (lambda (ls)~%")
  (format t "  >   (cond~%")
  (format t "  >     ((null? (cdr ls)) (car ls))~%")
  (format t "  >     (t (last (cdr ls))))))~%")
  (format t "~%")
  (format t "Well, that's a bargeload of parentheses, isn't it.~%")
  (format t "Let's sort through it. We define last to be a~%")
  (format t "function, that is, the result of a lambda-expression.~%")
  (format t "This function takes one argument, ls. It consists of~%")
  (format t "a cond expression. First clause: if (cdr ls) is~%")
  (format t "null?, then return (car ls). Otherwise, return (last~%")
  (format t "(cdr ls)). That's all.~%")
  (format t "~%")
  (format t "Got it yet? The idea is this: if you have a one-term~%")
  (format t "list, the last term is the first term. You can check~%")
  (format t "this by seeing if (cdr ls) is nil, because only in a~%")
  (format t "one-term list is the cdr empty. So if (null? (cdr~%")
  (format t "ls)), we should return the first term in the list,~%")
  (format t "which is (car ls). Otherwise, we have a list which is~%")
  (format t "two terms or more -- so we can find its last element~%")
  (format t "by calling last on its cdr! It's not an infinite~%")
  (format t "loop, because the cdr of ls is shorter than ls is.~%")
  (format t "~%")
  (print-example "(last '(a))" "a" t)
  (print-example "(last '(a bb))" "bb" t)
  (wait-for-enter)
  (print-example "(last '(a bb c))" "c" t)
  (print-example "(last '(a bb c (xx)))" "(xx)" t)
  (format t "~%")
  (format t "And lo, it works.~%")
  (format t "~%")
)

(defun print-chapter-14 ()

  (format t "~%")
  (format t "=== 14. Local Definitions With Let ===~%")
  (format t "~%")
  (format t "You often want to define temporary values -- helper~%")
  (format t "functions, or temporary storage for~%")
  (format t "partially-computed values, or whatever. Scheme allows~%")
  (format t "you to do this with the let syntax, and a few related~%")
  (format t "ones.~%")
  (format t "~%")
  (format t "(let ( (atom1 value1) (atom2 value2) ... ) result )~%")
  (format t "~%")
  (format t "What goes on here is this: First, each of the value~%")
  (format t "expressions is evaluated. Then a local binding is~%")
  (format t "made, with each value assigned to its atom. Then the~%")
  (format t "result expression is evaluated, with those~%")
  (format t "assignments in place. The local assignments are~%")
  (format t "thrown away, and the value of result is returned.~%")
  (format t "~%")
  (format t "This sort of temporary assignment is exactly like~%")
  (format t "what happens with function arguments. The definitions~%")
  (format t "are only visible to the result expression -- not in~%")
  (format t "the outside world. In fact, you can rewrite a let~%")
  (format t "expression as a function definition and call:~%")
  (format t "~%")
  (format t "((lambda (atom1 atom2 ...) result) (value1 value2~%")
  (format t "...))~%")
  (format t "~%")
  (format t "But the let syntax is easier to read. Example time...~%")
  (format t "~%")
  (format t ">>(let > ((a 1) (b 2)) > (+ a b)) 3~%")
  (wait-for-enter)
  (format t "~%")
  (format t ">>(let > ((func car)) > (func '(a b c))) a~%")
  (format t "~%")
  (format t ">>(let > ((cost '(nickel dime))) > (eqv? cost cost))~%")
  (format t "t~%")
  (format t "~%")
  (print-example "a" "[Error: undefined atom: a]" t)
  (print-example "func" "[Error: undefined atom: func]" t)
  (print-example "cost" "[Error: undefined atom: cost]" t)
  (format t "~%")
  (format t "It's important to understand that all the values in a~%")
  (format t "let are evaluated before any of the assignments are~%")
  (format t "made. More correctly, none of the local assignments~%")
  (format t "are visible to the value expressions; they're only~%")
  (format t "visible to the result expression. If you want to make~%")
  (format t "several definitions that refer to each other, you~%")
  (format t "could use nested let statements. It's easier to use~%")
  (format t "the let* syntax, however. let* is just like let,~%")
  (format t "except that the assignments are made in order, and~%")
  (format t "each one is visible to all the values after it.~%")
  (format t "~%")
  (format t ">>(let* > ((a 1) (b (+ a 1))) > b) 2~%")
  (format t "~%")
  (format t "If you tried that with let, you'd get an 'undefined~%")
  (format t "atom: a' error, because a isn't defined at the point~%")
  (format t "where (+ a 1) is evaluated.~%")
  (format t "~%")
)

(defun print-chapter-15 ()

  (format t "~%")
  (format t "=== 15. Recursion, Functions, Endless Fun ===~%")
  (format t "~%")
  (format t "let* is cool, but it doesn't allow you to do really~%")
  (format t "funky recursive stuff, with several definitions that~%")
  (format t "really all reference each other. For that, you need~%")
  (format t "letrec. letrec has the same form as let and let*, but~%")
  (format t "any of the assigned values can use any of the other~%")
  (format t "atoms being bound. Sort of. The catch is, the atoms~%")
  (format t "being bound can only be used inside~%")
  (format t "lambda-expressions. So this isn't legal:~%")
  (format t "~%")
  (format t ">>(letrec > ((a a)) > a)~%")
  (format t "~%")
  (format t "This is bad because the use of the atom being defined~%")
  (format t "(the second a, that is) isn't 'protected' inside a~%")
  (format t "lambda-expression. If you think this is an arbitrary~%")
  (format t "restriction, consider: if it were legal, what the~%")
  (format t "heck would it evaluate to?~%")
  (format t "~%")
  (format t "In case you were about to try this, by the way --~%")
  (format t "yes, I see you in the corner -- you'll find it~%")
  (format t "doesn't produce an error; it returns nil. If there~%")
  (format t "were more terms being defined in the letrec, the~%")
  (format t "results could be even stranger. This is because I~%")
  (format t "used a fairly unpleasant hack when writing the letrec~%")
  (format t "code. (A legal hack, honest. The Scheme standard says~%")
  (format t "that this sort of letrec abuse produces undefined~%")
  (format t "results.)~%")
  (format t "~%")
  (wait-for-enter)
  (format t "So how can letrec be used? Well, the following is~%")
  (format t "legal:~%")
  (format t "~%")
  (format t ">>(letrec > ((a (lambda () a))) > a)~%")
  (format t "~%")
  (format t "The assignment value is (lambda () a), which only~%")
  (format t "uses a inside the lambda-expression. (The final term~%")
  (format t "of the letrec, which in this case is a, is allowed to~%")
  (format t "use all the defined atoms -- they've all been defined~%")
  (format t "by that time.) What does this do? Let's see:~%")
  (format t "~%")
  (format t ">>(define thing > (letrec > ((a (lambda () a))) > a))~%")
  (format t "[function]~%")
  (format t "~%")
  (format t "Okay, thing has been defined to be a function. What~%")
  (format t "function is this? It's the function which the letrec~%")
  (format t "statement temporarily defined to be a. What was a~%")
  (format t "defined as? A function which takes no arguments, and~%")
  (format t "returns whatever a was defined as.~%")
  (format t "~%")
  (format t "In other words, this is everyone's favorite twisted~%")
  (format t "example: the function which returns itself. Let's~%")
  (format t "check that:~%")
  (format t "~%")
  (print-example "thing" "[function]" t)
  (print-example "(thing)" "[function]" t)
  (wait-for-enter)
  (print-example "((thing))" "[function]" t)
  (print-example "(eqv? thing (thing))" "t" t)
  (format t "~%")
  (format t "Even eqv? agrees: the function assigned to thing is~%")
  (format t "the very same function returned by evaluating~%")
  (format t "(thing), which is to say, the result of calling the~%")
  (format t "function assigned to thing. (Notice that in this~%")
  (format t "chapter, I'm being careful to distinguish the value~%")
  (format t "assigned to thing from the atom thing itself. The~%")
  (format t "function doesn't return an atom; it returns a~%")
  (format t "function.)~%")
  (format t "~%")
  (format t "What would happen if we tried this with let instead~%")
  (format t "of letrec? Go ahead, try it.~%")
  (format t "~%")
  (format t ">>(define thing > (let > ((a (lambda () a))) > a))~%")
  (format t "[function]~%")
  (format t "~%")
  (print-example "(thing)" "[Error: undefined atom: a]" t)
  (format t "~%")
  (wait-for-enter)
  (format t "Like we said, all the values in a let are evaluated~%")
  (format t "before any of the assignments are made. So that~%")
  (format t "lambda-expression is evaluated, producing a function~%")
  (format t "which returns whatever is assigned to a. But at this~%")
  (format t "point, nothing has been assigned to a yet. And --~%")
  (format t "remember static binding -- functions are all wrapped~%")
  (format t "up in themselves. The function can't see the~%")
  (format t "temporary assignment to a which is made later on.~%")
  (wait-for-enter)
  (format t "That assignment only exists within the scope of the~%")
  (wait-for-enter)
  (format t "let statement. You can do it with letrec, because all~%")
  (format t "the assignments in a letrec exist within each other's~%")
  (format t "scope. (Although they're incomplete, in a funny~%")
  (format t "sense, which is why they have to be protected in a~%")
  (format t "function.)~%")
  (format t "~%")
)

(defun print-chapter-16 ()

  (format t "~%")
  (format t "=== 16. Scope ===~%")
  (format t "~%")
  (format t "Oops, I started talking about scope. Time to define~%")
  (format t "it.~%")
  (format t "~%")
  (format t "All of the local assignments in a function -- its~%")
  (format t "arguments, and any assignments made by let and~%")
  (format t "variants -- are fixed at the point where the function~%")
  (format t "was defined. That's what 'static' means. Look at the~%")
  (format t "position of the lambda expression, see what let~%")
  (format t "statements it's inside, and you can tell what~%")
  (format t "assignments it knows about. That's the function's~%")
  (format t "scope.~%")
  (format t "~%")
  (format t "Top-level definitions -- those created with define --~%")
  (format t "are different. They're visible everywhere. They can~%")
  (format t "also change at any time. Convenient, but possibly a~%")
  (format t "source of bugs.~%")
  (format t "~%")
  (format t "Best to illustrate the difference with an example.~%")
  (format t "Can't you define the evil function that returns~%")
  (format t "itself more simply, this way?~%")
  (format t "~%")
  (format t " >>(define selfer~%")
  (format t "  >  (lambda () selfer))~%")
  (format t " [function]~%")
  (format t "~%")
  (wait-for-enter)
  (format t "Well, yes, that works. But it's dependent on the~%")
  (format t "global definition of selfer. If you break that, the~%")
  (format t "function stops working.~%")
  (format t "~%")
  (wait-for-enter)
  (format t "~%")
  (print-example "(define another-selfer selfer)" "[function]" t)
  (print-example "(define selfer 'toast)" "toast" t)
  (print-example "(another-selfer)" "toast" t)
  (print-example "(eqv? another-selfer (another-selfer))" "nil" t)
  (format t "~%")
  (format t "The thing function defined in the previous chapter~%")
  (format t "will always keep working, even if we assigned it to~%")
  (format t "another-thing and changed the definition of thing. If~%")
  (format t "a function doesn't rely on top-level definitions that~%")
  (format t "might change, its behavior is completely predictable.~%")
  (format t "This is generally a good thing.~%")
  (format t "~%")
  (format t "On the other hand, a function that does rely on~%")
  (format t "top-level definitions can be affected by other~%")
  (format t "functions that change that definition. This sort of~%")
  (format t "thing is called a side effect; doing one thing has a~%")
  (format t "side effect which affects another thing. In~%")
  (format t "functional programming, people say they hate side~%")
  (format t "effects, but actually it's hard to break the habit of~%")
  (format t "using them. (Imperative languages like Pascal and C~%")
  (format t "are made of side effects. Every time you change the~%")
  (format t "value of a variable, you affect everything else that~%")
  (format t "uses that variable.)~%")
  (format t "~%")
)

(defun print-chapter-17 ()

  (format t "~%")
  (format t "=== 17. Return ===~%")
  (format t "~%")
  (format t "Well, that's it for the manual. We haven't covered~%")
  (format t "all of Scheme by any means, but we've gone through~%")
  (format t "all the foundations. If you've been following along~%")
  (format t "with the genie's exercises, you have a handle on how~%")
  (format t "to think in Scheme.~%")
  (format t "~%")
  (format t "The definitive reference book on Scheme is The Scheme~%")
  (format t "Programming Language, by R. Kent Dybvig. (No, I have~%")
  (format t "no idea. I can't think of a color which starts with~%")
  (format t "D.) Pick it up, and find a real Scheme interpreter,~%")
  (format t "if you're interested in learning more. If you ever~%")
  (format t "figure out how continuations work, please come and~%")
  (format t "explain them to me.~%")
  (format t "~%")
  (format t "If it crosses your path of life to learn Lisp, you'll~%")
  (format t "find that it's pretty much what you've learned here;~%")
  (format t "just a little messier. (The biggest difference is~%")
  (format t "that atoms have two distinct values assigned to them~%")
  (format t "-- a data value, and a function value. I've never~%")
  (format t "understood why. It's just something to remember.)~%")
  (format t "~%")
  (format t "If you think this whole thing is a waste of your~%")
  (format t "time... then why did you read this far?~%")
  (format t "~%")
  (format t "Have fun.~%")
  (format t "~%")
)

(defun print-chapter-18 ()

  (format t "~%")
  (format t "=== 18. Reference: Functions ===~%")
  (format t "~%")
  (format t "This is a list of all pre-defined functions. Some of~%")
  (format t "them have more functionality than the tutorial~%")
  (format t "describes, so listen up.~%")
  (format t "~%")
  (format t "(car v) : 1 argument (a non-empty list) Returns the~%")
  (format t "first term of v.~%")
  (format t "~%")
  (format t "(cdr v) : 1 argument (a non-empty list) Returns the~%")
  (format t "list containing all but the first term of v.~%")
  (format t "~%")
  (format t "(cons v w) : 2 arguments (the second a list) Returns~%")
  (format t "the list whose first term is v and the rest of whose~%")
  (format t "terms are the terms of w.~%")
  (format t "~%")
  (format t "(length v) : 1 argument (a list) Returns the number~%")
  (format t "of terms in v.~%")
  (format t "~%")
  (format t "(list v ...) : 0 or more arguments Returns the list~%")
  (format t "whose terms are the given arguments.~%")
  (format t "~%")
  (format t "(not v) : 1 argument Returns t if v is nil, and nil~%")
  (format t "otherwise.~%")
  (format t "~%")
  (format t "(eqv? v w) : 2 arguments Returns t if v and w are~%")
  (format t "both nil, or are the same atom, or were created at~%")
  (format t "the same time. Returns nil otherwise.~%")
  (wait-for-enter)
  (format t "~%")
  (format t "(equal? v w) : 2 arguments Returns t if v and w are~%")
  (format t "eqv?, or are lists of the same length all of whose~%")
  (format t "terms are equal?.~%")
  (format t "~%")
  (format t "(null? v) : 1 argument Returns t if v is nil, and nil~%")
  (format t "otherwise. (Yes, this is the same as not.)~%")
  (format t "~%")
  (format t "(list? v) : 1 argument Returns t if v is a list,~%")
  (format t "including nil. Returns nil if v is anything else,~%")
  (format t "such as an atom or function.~%")
  (format t "~%")
  (format t "(= v ...) : 1 or more arguments (all numbers) Returns~%")
  (format t "t if all the arguments are the same number. If there~%")
  (format t "is only one argument, always returns t.~%")
  (format t "~%")
  (format t "(> v ...) : 1 or more arguments (all numbers) Returns~%")
  (format t "t if all the arguments are numbers in strictly~%")
  (format t "descending sequence. Otherwise returns nil. If there~%")
  (format t "is only one argument, always returns t.~%")
  (format t "~%")
  (format t "(>= v ...) : 1 or more arguments (all numbers)~%")
  (format t "Returns t if all the arguments are numbers in~%")
  (format t "descending sequence, not necessarily strictly.~%")
  (format t "Otherwise returns nil. If there is only one argument,~%")
  (format t "always returns t.~%")
  (format t "~%")
  (wait-for-enter)
  (format t "(< v ...) : 1 or more arguments (all numbers) Returns~%")
  (format t "t if all the arguments are numbers in strictly~%")
  (format t "ascending sequence. Otherwise returns nil. If there~%")
  (format t "is only one argument, always returns t.~%")
  (format t "~%")
  (format t "(<= v ...) : 1 or more arguments (all numbers)~%")
  (format t "Returns t if all the arguments are numbers in~%")
  (format t "ascending sequence, not necessarily strictly.~%")
  (format t "Otherwise returns nil. If there is only one argument,~%")
  (format t "always returns t.~%")
  (format t "~%")
  (format t "(+ v ...) : 0 or more arguments (all numbers) Returns~%")
  (format t "the sum of all the arguments. If there are no~%")
  (format t "arguments, returns 0.~%")
  (format t "~%")
  (format t "(- v ...) : 0 or more arguments (all numbers) Returns~%")
  (format t "the first argument minus the sum of all the other~%")
  (format t "arguments. If there is only one argument, returns its~%")
  (format t "negative. If there are no arguments, returns 0.~%")
  (format t "~%")
  (format t "(eval v) : 1 argument Returns the result of~%")
  (format t "evaluating v. (Note that since eval is a function,~%")
  (format t "whatever you give as the argument is evaluated before~%")
  (format t "it is handed in. So eval sort of double-evaluates~%")
  (format t "whatever you give it.)~%")
  (format t "~%")
)

(defun print-chapter-19 ()

  (format t "~%")
  (format t "=== 19. Reference: Syntactic Forms ===~%")
  (format t "~%")
  (format t "This is a list of all the special syntax forms~%")
  (format t "available.~%")
  (format t "~%")
  (format t "(quote v) : 1 argument Returns v, without evaluating~%")
  (format t "it at all.~%")
  (format t "~%")
  (format t "(error ...) : any number of arguments Causes an~%")
  (format t "error. The arguments are ignored. This aborts the~%")
  (format t "evaluation of an expression; once any part causes an~%")
  (format t "error, the entire thing results in an error.~%")
  (format t "~%")
  (format t "(cond clause1 ...) : any number of clauses; each~%")
  (format t "clause is a list of either one or two terms Goes~%")
  (format t "through the clauses, in order. If the first (or only)~%")
  (format t "term of a clause evaluates to nil, it is skipped and~%")
  (format t "the next one tested. The leftmost clause whose first~%")
  (format t "term evaluates to non-nil is the winner. If it has~%")
  (format t "only one term, that non-nil value is returned. If it~%")
  (format t "has two, the result of evaluating the second term is~%")
  (format t "returned. If no clause is a winner, nil is returned.~%")
  (format t "~%")
  (format t "(define atom v) : two arguments; the first an atom~%")
  (format t "The result of evaluating v is assigned to atom (a~%")
  (format t "top-level definition). If atom already has a~%")
  (format t "top-level definition, the older definition is~%")
  (format t "replaced. The new value is also returned.~%")
  (wait-for-enter)
  (format t "~%")
  (format t "(lambda arglist v) : two arguments Returns a~%")
  (format t "function. The scope of the function is the scope in~%")
  (format t "which the lambda-expression is evaluated to produce~%")
  (format t "it. When a function is called, the arguments it is~%")
  (format t "given are assigned to the atoms in arglist, producing~%")
  (format t "a new scope on top of the function's scope; v is then~%")
  (format t "evaluated in this new scope. arglist may be a single~%")
  (format t "atom (in which case all the function's arguments are~%")
  (format t "put into a list which is assigned to that atom), or~%")
  (format t "nil (in which case the function takes zero~%")
  (format t "arguments), or a list of atoms (in which case the~%")
  (format t "function takes that many arguments.)~%")
  (format t "~%")
  (format t "(let ((atom1 def1) ...) v) : two arguments; the first~%")
  (format t "is a list of clauses; each clause is a list of two~%")
  (format t "terms All the definitions in the list of clauses are~%")
  (wait-for-enter)
  (format t "evaluated, in the current scope. Then a new scope is~%")
  (format t "created, in which those values are assigned to their~%")
  (format t "respective atoms (a local or temporary definition.) v~%")
  (format t "is evaluated in this new scope, and the result is~%")
  (format t "returned.~%")
  (format t "~%")
  (format t "(let* ((atom1 def1) ...) v) : two arguments; the~%")
  (format t "first is a list of clauses; each clause is a list of~%")
  (format t "two terms In the current scope, def1 is evaluated. A~%")
  (format t "new scope is created in which the resulting value is~%")
  (format t "assigned to atom1. In this new scope, def2 is~%")
  (format t "evaluated. A newer scope is created in which the~%")
  (format t "resulting value is assigned to atom2. This continues~%")
  (wait-for-enter)
  (format t "until all clauses are handled. v is evaluated in the~%")
  (format t "final scope, and the result is returned.~%")
  (format t "~%")
  (format t "(letrec ((atom1 def1) ...) v) : two arguments; the~%")
  (format t "first is a list of clauses; each clause is a list of~%")
  (format t "two terms A new scope is created in which the atoms~%")
  (format t "of all the clauses are assigned undefined values. All~%")
  (format t "the definitions of the clauses are then evaluated, in~%")
  (format t "this new scope. (For the results to be valid, all~%")
  (format t "uses of the atoms must be inside lambda-expressions.)~%")
  (format t "The resulting values are written into the scope,~%")
  (format t "completing it, and then v is evaluated in the scope.~%")
  (format t "~%")
)

(defun print-chapter-20 ()

  (format t "~%")
  (format t "=== 20. Reference: Improper Lists ===~%")
  (format t "~%")
  (format t "And finally, I have been convinced to put in more~%")
  (format t "about improper lists. (Chapter 8 was where I said~%")
  (format t "that I didn't feel like explaining them.) This~%")
  (format t "section is tacked on as reference material because~%")
  (format t "you don't really need to know about improper lists~%")
  (format t "for the purposes of this tutorial, but you're~%")
  (format t "undoubtedly going to stumble into them anyway. So you~%")
  (format t "can think of this chapter as an appendix. A weird~%")
  (format t "little continuation tacked on after the manual's full~%")
  (format t "stop. Ha! I just kill myself sometimes. Or at least~%")
  (format t "strain my back reaching after feeble jokes.~%")
  (format t "~%")
  (format t "An improper list, or dotted list, is just what you~%")
  (format t "get when the cdr of a list is not a list. That is, if~%")
  (format t "the second argument of a cons call is not a list, the~%")
  (format t "result of the cons will be an improper list.~%")
  (format t "~%")
  (format t "You can also create an improper list by typing the~%")
  (format t "dotted form itself.~%")
  (format t "~%")
  (print-example "'(a . b)" "(a . b)" t)
  (format t "~%")
  (format t "If the second argument of a cons call is an improper~%")
  (format t "list, the result will be a longer improper list.~%")
  (format t "~%")
  (wait-for-enter)
  (format t "~%")
  (print-example "(cons 'z '(a . b))" "(z a . b)" t)
  (print-example "(cons 'rats '(z a . b))" "(rats z a . b)" t)
  (format t "~%")
  (format t "There can only be one dot in an improper list, and it~%")
  (format t "will always be just before the last term. Why? Well,~%")
  (format t "in a proper list, the cdr is either a non-empty list~%")
  (format t "(meaning there's more to come) or the empty list~%")
  (format t "(meaning you've reached the end.) In an improper~%")
  (format t "list, the cdr is a non-list, meaning you've reached a~%")
  (format t "strange sort of appendix; but you can't go on after~%")
  (format t "that, because the appendix isn't a list, it's just~%")
  (format t "one thing; so there's no more to do. The appendix is~%")
  (format t "written after the dot, and then you're done.~%")
  (format t "~%")
  (format t "Of course, if the appendix is a list, then you're not~%")
  (format t "writing an improper list at all. This follows~%")
  (format t "directly from the rules, and a little experimentation~%")
  (format t "shows that it's true:~%")
  (format t "~%")
  (print-example "'(a . (b c))" "(a b c)" t)
  (format t "~%")
  (format t "An improper list isn't actually that awful; list?~%")
  (format t "will say that it is a list, and you can do anything~%")
  (format t "with it that you can do with a proper list. The only~%")
  (format t "problem is that if you take its cdr, and assume the~%")
  (format t "result is a list, you will get an ugly surprise. Many~%")
  (wait-for-enter)
  (format t "Scheme functions (including some built-in ones) do~%")
  (format t "make this assumption, and they will choke horribly~%")
  (format t "when fed improper lists. However, if you want to use~%")
  (format t "improper lists for your own purposes, there's no~%")
  (format t "reason not to.~%")
  (format t "~%")
  (format t "Now we're done.~%")
  (format t "~%")
)

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
  (format t "=====================================================~%")
  (format t "    A Simple Programmer's Introduction to Scheme~%")
  (format t "=====================================================~%~%")
  (format t "Table of Contents:~%~%")
  (dotimes (i 21)
    (format t "  ~2d: ~a~%" i (manual-chapter-name i)))
  (format t "~%Enter chapter number (0-20), or 'q' to quit: "))

(defun cmd-manual ()
  (if (or *manual-available* (>= *genie-state* 2))
      (loop
        (display-manual-menu)
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
                     (wait-for-enter))
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
