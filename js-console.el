;;; js-console.el -- shell interface to javascript interpreter

;; Author: Steve Yegge (steve.yegge@gmail.com)
;; Version: 2008.10.30
;; Keywords:  javascript languages

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;; This package provides a simple interactive shell interface to
;; the JavaScript interpreter.  See ./README for instructions.

(require 'cl)
(require 'js-parse)
(require 'js-exec)
(require 'js-debug)

;;; Code:

(defvar js-console-squelch-errors nil
  "If true, prints lisp errors on the console.
If nil, throws the errors and enters the debugger.")

(defvar js-console-mode nil)
(defvar js-console-buffer-name "*js*")
(defvar js-console-prompt "js> ")
(defvar js-console-mode-map nil)
(defvar js-console-global nil)
(defvar js-console-input-start nil)
(defvar js-console-last-parsed-input nil)
(make-variable-buffer-local 'js-console-last-parsed-input)

;; expression history
(defvar js-console-prev-history '())
(defvar js-console-next-history '())

(defun js-console-restore-history ()
  "Restore input-expression history"
  (setq js-console-prev-history
	(append (reverse js-console-next-history)
		js-console-prev-history))
  (setq js-console-next-history '()))

(defun js-console-push-prev-expression-history (exp)
  "Add input-expression to history"
  (setq js-console-prev-history (cons exp js-console-prev-history)))

(defun js-console-push-next-expression-history (exp)
  "Add previous expression to history for next expression"
  (setq js-console-next-history (cons exp js-console-next-history)))

(defun js-console-pop-prev-expression-history ()
  "Pop previous expression from history"
  (let ((prev-exp (car js-console-prev-history)))
    (setq js-console-prev-history (cdr js-console-prev-history))
    (js-console-push-next-expression-history prev-exp)
    prev-exp))

(defun js-console-pop-next-expression-history ()
  "Pop next expression from next expression history"
  (let ((prev-exp (car js-console-next-history))
	(next-exp (cadr js-console-next-history)))
    (setq js-console-next-history (cdr js-console-next-history))
    (js-console-push-prev-expression-history prev-exp)
    next-exp))

(defun js-console-prev-evaluated-expression ()
  "Insert prev expression into console."
  (interactive)
  (if (null (car js-console-prev-history))
      (progn (kill-region js-console-input-start (point-max))
	     (message "history is empty."))
    (progn (kill-region js-console-input-start (point-max))       
	   (insert (js-console-pop-prev-expression-history)))))

(defun js-console-next-evaluated-expression ()
  "Insert next expression into console."
  (interactive)
  (if (null (cadr js-console-next-history))
      (progn (kill-region js-console-input-start (point-max))
	     (message "history is empty."))
    (progn (kill-region js-console-input-start (point-max))
	   (insert (js-console-pop-next-expression-history)))))

(defmacro with-js-runtime (&rest body)
  "Execute BODY with a new JavaScript runtime and execution context.
BODY is a lisp expression - this function is not for evaluating
JavaScript; it's for testing the runtime functions interactively.
Creating a new JavaScript runtime is an expensive operation, so if
you're planning on doing this in a loop, you should use
`js-create-interpreter' to associate an interpreter with a buffer."
  (let ((global (gensym))
        (exc (gensym))
        (result (gensym)))
    `(let* ((,global (make-js-Object))
            (js-current-context (js-context 'GLOBAL_CODE ,global))
            ,exc ,result)
       (js-init-standard-objects ,global)
       (setq ,exc
             (catch 'js-THROW
               (progn
                 (setq ,result (progn ,@body))
                 nil)))
       (if ,exc
           (js-console-js-to-string
            (js-Context-result js-current-context))
         ,result))))
           
(defun js-console ()
  (interactive)
  (let ((have-buf (get-buffer js-console-buffer-name))
        (buf (get-buffer-create js-console-buffer-name)))
    (set-buffer buf)
    (unless have-buf
      (js-console-welcome)
      (js-console-prompt))
    (unless (eq major-mode 'js-console-mode)
      (js-console-mode))
    (pop-to-buffer buf)))

(defun js-console-mode ()
  "JavaScript interactive mode.

\\{js-console-mode-map}"
  (interactive)
  (kill-all-local-variables)

  (setq major-mode 'js-console-mode)
  (setq mode-name "JS-Console")
  (set (make-local-variable 'js-console-mode) t)

  (set (make-local-variable 'js-console-mode-map)
        (make-sparse-keymap))
  (use-local-map js-console-mode-map)

  (define-key js-console-mode-map [return] 'js-console-send-input)
  (define-key js-console-mode-map [(control ?m)] 'js-console-send-input)
  (define-key js-console-mode-map [(control ?j)] 'js-console-send-input)

  (define-key js-console-mode-map [(control a)] 'js-console-bol)

  (define-key js-console-mode-map [(meta p)] 'js-console-prev-evaluated-expression)
  (define-key js-console-mode-map [(meta n)] 'js-console-next-evaluated-expression)

  (js-console-init-interpreter)

  (set (make-local-variable 'max-lisp-eval-depth)
       (max 3000 max-lisp-eval-depth))
  (set (make-local-variable 'max-specpdl-size)
       (max 6000 max-lisp-eval-depth))

  (run-hooks 'js-console-mode-hook))

(defun js-console-init-interpreter ()
  "Initializes an interpreter instance for the console."
  (let* ((context (js-create-interpreter))
         (global (js-Context-scope context)))
  (js-console-add-helpers global)
  (set (make-local-variable 'js-console-global) global)))

(defun js-console-welcome ()
  (insert "Welcome to the EmacScript Console.\n\n")
  (insert "Semicolon (`;') on its own line cancels partial input.\n\n"))

(defun js-console-prompt ()
  (insert js-console-prompt)
  (setq js-console-input-start (point-marker)))

(defun js-console-bol ()
  (interactive)
  (if (looking-back (concat "^" js-console-prompt "\\s-*"))
      (goto-char (point-at-bol))
    (goto-char (point-at-bol))
    (js-console-skip-prompt)))

(defun js-console-skip-prompt ()
  (while (looking-at js-console-prompt)
    (forward-char (length js-console-prompt))))

(defun js-console-send-input ()
  "Send input to JavaScript interpreter."
  (interactive)
  (if (and js-console-input-start
           (> js-console-input-start (point-max)))
      (setq js-console-input-start (point-max-marker)))
  (unless js-console-input-start
    (setq js-console-input-start
          (save-excursion
            (goto-char (point-at-bol))
            (js-console-skip-prompt)
            (point))))
  (if (< (point) js-console-input-start)
      (js-console-send-current-line))
    (let ((input (buffer-substring-no-properties
                  js-console-input-start (point-marker))))
      (if js-console-squelch-errors
          (condition-case err
              (js-console-exec-input input)
            (error
             (js-console-handle-error err)))
	(js-console-restore-history)
	(js-console-push-prev-expression-history input)
        (js-console-exec-input input))))

(defun js-console-exec-input (input)
  (let (result)
    (setf (js-Context-result js-current-context) nil)
    (if (equal 0 (string-match "^[ \t\r\n]*$" input))
        (js-console-display-output nil)
      (setq result (js-attempt-compile input))
      (cond
       ((eq result 'more)
        (insert "\n"))                  ; wait for more input
       ((eq result t)
        (js-console-display-output
         (save-excursion
           (js-console-exec
            js-console-last-parsed-input))))
       ((listp result)
        (js-console-handle-error result))
       (t
        (js-console-display-output
         (format
          "Console code error:  Unrecognized result: %s" result)))))))

(defun js-console-exec (ast)
  "Execute AST and return the result as a printable string."
  (let (result)
    (setf (js-Context-result js-current-context) nil)
    (condition-case err
        (progn
          (setq result
                (catch 'js-THROW
                  (progn
                    (js-exec ast)
                    nil)))
          (if result
              (js-console-handle-exception)
            (js-console-js-to-string
             (js-Context-result js-current-context))))
      (error
       (js-console-handle-lisp-error err)))))

(defun js-console-handle-lisp-error (err)
  "Handle a lisp error ERR during evaluation of JS code.
When this happens it's indicative of a bug in the implementation.
Returns the resulting error message as a printable string."
  (let ((name (car err))
        (data (cdr err))
        (header "JavaScript-elispimplementation error"))
    (case name
      (no-catch
       (let ((tag (car data))
             (value (cadr data)))
         (format "%s: no catch for %s\nJavaScript value: %s"
                 header
                 tag
                 (js-console-js-to-string value))))
      ;; We'll figure out other errors as we encounter them.
      (t
       (format "%s" err)))))
      
(defun js-console-js-to-string (value)
  "Return a printable version of VALUE, a JavaScript value.
May call toString() on VALUE, if it's a JS object."
  (let (result err)
    (cond
     ((js-Object-p value)
      (setq err
            (catch 'js-THROW
              (progn
                (setq result (js-call-method value "toString"))
                nil)))
      (if err
          (js-console-handle-exception)
        result))
     ((stringp value)
      value)
     (t
      (js-console-unescape-string
       (js-to-source value))))))

(defun js-console-unescape-string (s)
  "Removes one level of double-quote escaping from S.
Includes removing the outermost double-quotes."
  (let ((match (string-match "^\"\\(.+\\)\"$" s)))
    (if (and match (zerop match))
        (replace-regexp-in-string "\\\\\"" "\""
                                  (match-string 1 s))
      s)))
      
(defun js-console-send-current-line ()
  (let ((line (save-excursion
                (goto-char (point-at-bol))
                (js-console-skip-prompt)
                (buffer-substring-no-properties (point)
                                                (point-at-eol))))
        prev-input)
    ;; This more or less mimics what comint shells do.
    (goto-char (point-max))
    (goto-char (point-at-bol))
    (js-console-skip-prompt)
    (setq prev-input (buffer-substring (point) (point-at-eol)))
    (delete-region (point) (point-at-eol))
    (insert line)
    (js-console-display-output
     (js-console-js-to-string
      (js-evaluate line)))
    (insert prev-input)))

(defun js-console-display-output (output)
  "Display OUTPUT, a string, followed by the prompt."
  (goto-char (point-max))
  (when output
    (newline)
    (insert output))
  (unless (looking-back "[\r\n]" nil t)
    (newline))
  (js-console-prompt))

(defun js-attempt-compile (text)
  "Try parsing TEXT.
If successful, `js-console-last-parsed-input' is set to the AST,
and t is returned.  If there was a parse error due to reaching the
end of input, then it returns the symbol `more', meaning the console
should simply await more input.  Otherwise it returns a list of the
data associated with the syntax error."
  (condition-case err
      (let ((js-no-parent-links t)
            (js-recover-from-parse-errors nil))
        (setq js-console-last-parsed-input
              (js-parse-string text))
        t)
    (js-parse-syntax-error
     (setq js-console-last-parsed-input nil)
     (let ((eobp (car (last err))))
       (if eobp
           'more
         err)))))
     
(defun js-console-handle-error (err)
  "Handle a Lisp error.  Currently includes parse errors."
  (setf (js-Context-result js-current-context) nil)
  (setq js-console-input-start nil)
  (js-console-display-output (error-message-string err)))

(defun js-console-handle-exception ()
  "Handle a JavaScript exception by printing it as a string."
  (setq js-console-input-start nil)
  (let ((err (js-Context-result js-current-context)))
    (if (js-Error-p err)
        (js-console-js-to-string err)
      (concat "Exception from uncaught JavaScript throw: "
              (js-console-js-to-string  err)))))
           
(defun js-console-js-print (thisobj args)
  "print() function that can be called from JavaScript.
Installed in the Global object associated with the console."
  (goto-char (point-max))
  (newline)
  (loop with len = (length args)
        for arg in args
        for i from 0 do
        ;; This lets us mimic the behavior of Rhino and SquareFree,
        ;; whose built-in print() functions both print "undefined" for
        ;; the value 'undefined, but the console exec'er prints nothing.
        (insert (if (eq arg 'undefined)
                    "undefined"
                  (js-console-js-to-string arg)))
        if (< i (1- len))
        do (newline))
  (sit-for 0)
  'undefined)  ;; don't print "true" to console after printing value

(defmacro js-time (form)
  "Evaluate FORM, discard result, and return elapsed time in sec"
  (let ((beg (gensym))
        (delta (gensym)))
    `(let ((,beg (current-time))
           ,delta)
       ,form
       (setq ,delta (time-subtract (current-time) ,beg))
       (+ (cadr ,delta) (/ (caddr ,delta) 1000000.0)))))

(defun js-console-display-error (format-string &rest args)
  (let ((msg (apply #'format format-string args)))
    (js-console-display-output
     (concat "Error: " msg))))

(defun js-console-check-file (path)
  "Verify that PATH exists, displaying console error if not.
Returns t if the path is present."
  (if (and (file-exists-p path)
           (file-readable-p path))  ; check plain file as well?
      t
    (js-console-display-error "No such file: %s" path)
    nil))

(defun js-console-file-contents (path)
  "Return contents of PATH, a file, as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun js-console-js-load (thisobj args)
  "Loads and evaluates a JavaScript file, given its path."
  (let ((path (car args))
        time
        buf)
    (when (js-console-check-file path)
      (setq time
            (js-time
             (js-evaluate (js-console-file-contents path) path 1)))
      (js-console-display-output
       (format "load completed in %f sec" time))
      nil)))
      
(defun js-console-js-snarf (thisobj args)
  "Loads and returns a file as a string, given its path."
  (let ((path (car args)))
    (when (js-console-check-file path)
      (js-console-file-contents path))))

(defun js-console-js-evaluate (thisobj args)
  "Evaluates a string.  Basically just here for hosting Narcissus.
See Mozilla sources, js/src/js.c for corresponding Evaluate function."
  (let ((source (first args))
        (file (second args))
        (line (third args)))
    (js-evaluate source file line)))

(defun js-console-add-helpers (global)
  "Install some additional JavaScript functions into GLOBAL."
  (js-define-builtin global "print" 'js-console-js-print 'no-wrap)
  (js-define-builtin global "load" 'js-console-js-load 'no-wrap)
  ;; These two are for Narcissus.
  (js-define-builtin global "snarf" 'js-console-js-snarf 'no-wrap)
  (js-define-builtin global "evaluate" 'js-console-js-evaluate 'no-wrap))

(provide 'js-console)

;;; js-console.el ends here
