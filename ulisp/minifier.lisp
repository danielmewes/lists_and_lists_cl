;;;; Lisp Minifier Script
;;;;
;;;; This script reads a Lisp source file and removes all non-essential
;;;; whitespace and comments, writing the result to a new file.
;;;; Whitespace within string literals is preserved.
;;;;
;;;; This version uses a portable character-based approach to ensure
;;;; compatibility with various Lisp dialects (e.g., uLisp).
;;;;
;;;; Usage (with SBCL):
;;;;   sbcl --script minify-lisp.lisp <input-file.lisp> <output-file.lisp>
;;;;
;;;; Example:
;;;;   sbcl --script minify-lisp.lisp my-program.lisp my-program.min.lisp
;;;;

(defun minify-lisp-file (input-path output-path)
  "Reads Lisp source from input-path, removes comments and extraneous
  whitespace using a character-based approach, and writes the minified
  code to output-path."
  (handler-case
      (with-open-file (in-stream input-path :direction :input)
        (with-open-file (out-stream output-path :direction :output
                                                :if-exists :supersede)
          (let ((in-string-p nil)
                (escape-next-char-p nil)
                (whitespace-pending-p nil)) ; Tracks if a space needs to be emitted.
            (loop for char = (read-char in-stream nil :eof)
                  until (eq char :eof)
                  do
                     (cond
                       ;; 1. Handle escaped character (must be inside a string).
                       (escape-next-char-p
                        (write-char char out-stream)
                        (setf escape-next-char-p nil))

                       ;; 2. Handle characters inside a string literal.
                       (in-string-p
                        (write-char char out-stream)
                        (cond
                          ((char= char #\\) (setf escape-next-char-p t))
                          ((char= char #\") (setf in-string-p nil))))

                       ;; 3. Handle characters outside of a string.
                       (t
                        (cond
                          ;; A. Comment character: consume the rest of the line and flag whitespace.
                          ((char= char #\;)
                           (read-line in-stream nil :eof)
                           (setf whitespace-pending-p t))

                          ;; B. String delimiter: emit pending space, then start string.
                          ((char= char #\")
                           (when whitespace-pending-p
                             (write-char #\Space out-stream)
                             (setf whitespace-pending-p nil))
                           (setf in-string-p t)
                           (write-char char out-stream))

                          ;; C. Whitespace: flag that a space is pending.
                          ((member char '(#\Space #\Tab #\Newline #\Return))
                           (setf whitespace-pending-p t))

                          ;; D. Any other code character: emit pending space, then the char.
                          (t
                           (when whitespace-pending-p
                             (write-char #\Space out-stream)
                             (setf whitespace-pending-p nil))
                           (write-char char out-stream)))))))))
    (error (e)
      (format *error-output* "An error occurred: ~a~%" e)
      #+sbcl (sb-ext:quit :unix-status 1)
      #-sbcl (quit 1))))

(defun main ()
  "Parses command-line arguments and executes the minification."
  ;; This version uses SBCL-specific features to avoid external dependencies.
  (let* ((all-args #+sbcl sb-ext:*posix-argv*
                   #-sbcl (error "This script is configured to run with SBCL for command-line argument processing."))
         (script-name (car all-args))
         (user-args (cdr all-args)))
    (if (/= (length user-args) 2)
        (progn
          (format *error-output* "Usage: sbcl --script ~a <input-file> <output-file>~%" script-name)
          #+sbcl (sb-ext:quit :unix-status 1))
        (let ((input-file (first user-args))
              (output-file (second user-args)))
          (minify-lisp-file input-file output-file)
          (format t "Successfully minified '~a' -> '~a'~%" input-file output-file)))))

;; Execute the main function to run the script.
(main)

