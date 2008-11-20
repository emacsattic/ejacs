;;; js-exec.el - a JavaScript/ECMAScript interpreter

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
;; This file implements a JavaScript 1.5 compatible-ish interpreter.
;; It's just a naive port of jsexec.js from Narcissus.

;;; Performance:
;;
;; Interpretation vs. Compilation
;; ------------------------------
;; The performance is guaranteed to suck until I write the bytecode
;; interpreter.  Currently it just interprets the AST directly,
;; calling functions for each operation.
;;
;; JavaScript Nonlocal Exits
;; -------------------------
;; A quick-and-dirty test showed catch/throw to be about 7 times faster
;; than using `signal' and `condition-case'.  `signal' is more flexible,
;; though.  Need to figure out a good way to use catch/throw to propagate
;; nonlocal exits (i.e. errors, break and continue).  For now, I'm using
;; catch/throw for break/continue, and signal/condition-case for throwing
;; JavaScript exceptions.

;;; Code:

(eval-and-compile
  (require 'js-parse)
  (require 'js-runtime))

(eval-when-compile
  (require 'cl))

;;; debugger hooks

(defvar js-exec-debugger nil
  "Non-nil to register a system-wide EmacsScript debugger.
Don't set this variable directly; use the functions
`js-exec-register-debugger' and `js-exec-unregister-debugger',
since there may be extra setup required on registration and
unregistration.")

(defun js-exec-register-debugger (debugger)
  "Hooks up DEBUGGER to the evaluator.
There can only be one DEBUGGER in the system at a time, so an
error is signaled if a debugger is already registered.  Check
the value of `js-exec-debugger' to see if one is registered.
DEBUGGER will be called on entry and exit to every execution step.
See `js-debug' for details."
  (if js-exec-debugger
      (error "A JavaScript debugger is already registered."))
  (unless (functionp debugger)
    (error "The debugger must be a callable function."))
  (setq js-exec-debugger debugger))

(defun js-exec-unregister-debugger ()
  "Unhooks a debugger from the evaluator."
  ;; Just a hook in case we need to do more work down the road.
  (setq js-exec-debugger nil))

;;; 8.7 -- Reference Type

(defsubst js-make-reference (base name node)
  "Create an Ecma-262 Reference object for propname NAME on BASE.
Returns a 3-element vector with an identifying tag, BASE, NAME,
and NODE."
;;   (when (or (null base) (null name) (null node))
;;     (error "reference with null param: %s, %s, %s" base name node))
  (vector 'js-ref-tag base name node))

(defsubst js-reference-p (v)
  "Return t if V is a Reference."
  (and (vectorp v)
       (= 4 (length v))
       (eq (aref v 0) 'js-ref-tag)))

(defsubst js-ref-base (ref)
  (aref ref 1))

(defsubst js-ref-prop (ref)
  (aref ref 2))

(defsubst js-ref-node (ref)
  (aref ref 3))

(defsubst js-get-value (ref)
  "Implement 8.7.1 GetValue for possible Reference REF.
If REF is a Reference, we perform the lookup of its prop name on
its base object.  Otherwise we return REF.  If REF is a Reference
and its base object is null, we throw a ReferenceError."
  (if (not (js-reference-p ref))
      ref
    (let ((base (js-ref-base ref)))
      (if (or (null base) (eq base 'null)) ; 'undefined is ok(?)
          (js-reference-error
           (format "%s is not defined" (js-ref-prop ref)))
        (js-get base (js-ref-prop ref))))))

(defsubst js-put-value (ref value &optional node)
  "ECMA 8.7.2 - PutValue.  Set Reference REF to VALUE.
If passed, NODE is the REF node, for error reporting."
  (unless (js-reference-p ref)
    (js-reference-error "Invalid assignment left-hand side" node))
  (let ((base (or (js-ref-base ref)
                  (js-global))))
    (js-put base (js-ref-prop ref) value)))

;;; 14 -- Program

(defsubst js-exec-and-get (n)
  "Invoke `js-get-value' on the result of `js-exec' on node N."
  (js-get-value (js-exec n)))

(defsubst js-exec-and-test (n)
  "Perform a `js-test' of a `js-exec-and-get' on node N."
  (js-test
   (js-get-value
    (js-exec n))))

(defsubst js-exec-kid (n kidnum)
  "Invoke `js-exec' on the KIDNUM'th child of node N."
  (js-exec (js-node-kid n kidnum)))

(defsubst js-exec-kid-getval (n kidnum)
  "Invoke `js-exec' on the KIDNUM'th child of node N.
Calls `js-get-value' on the result and returns it."
  (js-get-value
   (js-exec
    (js-node-kid n kidnum))))

(defsubst js-push-scope (&optional obj)
  "Push a new object (or OBJ, if non-nil) on the scope chain.
Returns the newly-pushed object."
  (let* ((x js-current-context)
         (newscope (or obj (js-make-object))))
    (setf (js-Object-scope newscope) (js-Context-scope x))
    (setf (js-Context-scope x) newscope)
    newscope))

(defsubst js-pop-scope ()
  "Restore x.scope to x.scope.parent for execution context X."
  (let ((x js-current-context))
    (setf (js-Context-scope x)
          (js-Object-scope (js-Context-scope x)))))

(defvar js-function-executors
  '((FUNCTION . js-x-function)
    (SCRIPT . js-x-script)
    (BLOCK . js-x-block)
    (IF . js-x-if)
    (SWITCH . js-x-switch)
    (FOR . js-x-for)
    (WHILE . js-x-while)
    (FOR_IN . js-x-for-in)
    (DO . js-x-do)
    (BREAK . js-x-break-cont)
    (CONTINUE . js-x-break-cont)
    (TRY . js-x-try)
    (THROW . js-x-throw)
    (RETURN . js-x-return)
    (WITH . js-x-with)
    (VAR . js-x-var-decl)
    (CONST . js-x-var-decl)
    (DEBUGGER . js-x-debugger)
    (SEMICOLON . js-x-semicolon)
    (LABEL . js-x-label)
    (COMMA . js-x-comma)
    (ASSIGN . js-x-assign)
    (CONDITIONAL . js-x-conditional)
    (OR . js-x-or)
    (AND . js-x-and)
    (BITWISE_OR . js-x-bitwise-or)
    (BITWISE_XOR . js-x-bitwise-xor)
    (BITWISE_AND . js-x-bitwise-and)
    (EQ . js-x-eq)
    (NE . js-x-ne)
    (STRICT_EQ . js-x-strict-eq)
    (STRICT_NE . js-x-strict-ne)
    (LT . js-x-lt)
    (LE . js-x-le)
    (GT . js-x-gt)
    (GE . js-x-ge)
    (IN . js-x-in)
    (INSTANCEOF . js-x-instanceof)
    (LSH . js-x-lsh)
    (RSH . js-x-rsh)
    (URSH . js-x-ursh)
    (PLUS . js-x-plus)
    (MINUS . js-x-minus)
    (MUL . js-x-mul)
    (DIV . js-x-div)
    (MOD . js-x-mod)
    (DELETE . js-x-delete)
    (VOID . js-x-void)
    (TYPEOF . js-x-typeof)
    (NOT . js-x-not)
    (BITWISE_NOT . js-x-bitwise-not)
    (UNARY_PLUS . js-x-unary-plus)
    (UNARY_MINUS . js-x-unary-minus)
    (INCREMENT . js-x-inc-dec)
    (DECREMENT . js-x-inc-dec)
    (DOT . js-x-dot)
    (INDEX . js-x-index)
    (LIST . js-x-list)
    (CALL . js-x-call)
    (NEW . js-x-new)
    (NEW_WITH_ARGS . js-x-new)
    (ARRAY_INIT . js-x-array-init)
    (OBJECT_INIT . js-x-object-init)
    (NULL . js-x-null)
    (THIS . js-x-this)
    (TRUE . js-x-true)
    (FALSE . js-x-false)
    (IDENTIFIER . js-x-identifier)
    (NUMBER . js-x-node-value)
    (STRING . js-x-node-value)
    (REGEXP . js-x-regexp)
    (GROUP . js-x-group))
  "Evaluation handlers for each AST node type.")

(dolist (spec js-function-executors)
  (put (car spec) 'js-executor (cdr spec)))

(defun js-exec (n)
  "Execute AST node N in current execution context.
The result of execution is stored in the context result slot."
  (let* ((tt (js-node-type n))
         (ex (or (get tt 'js-executor)
                 (error "PANIC: js-exec: unknown node type: %s" tt)))
         result)
    (if js-exec-debugger
        (funcall js-exec-debugger 'enter js-current-context n ex))
    (unwind-protect
        (progn
          (setq result (funcall ex n))
          (if js-exec-debugger
              (funcall js-exec-debugger
                       'exit js-current-context n result)))
      (if js-exec-debugger
          (funcall js-exec-debugger
                   'unwind js-current-context n)))
    (or result 'undefined)))

;;; 11 -- Assignments
;;; 12 -- Statements
;;
;; Defined in the order Narcissus defines them, so they're mingled.

(defun js-x-function (n)
  (let* ((form (js-node-get n 'function-form))
         (name-node (js-node-get n 'name))
         (name (and name-node (js-node-value name-node)))
         (current-scope (js-Context-scope js-current-context))
         (result 'undefined)
         new-scope)
    (when (neq form 'DECLARED_FORM)
      (if (or (null name)
              (eq form 'STATEMENT_FORM))
          (progn
            (setq result (js-function-object n current-scope))
            (if (eq form 'STATEMENT_FORM)
                (js-define current-scope name result t)))  ; {DontDelete}
        (unwind-protect
            (progn
              (setq new-scope (js-push-scope))
              (setq result (js-function-object n new-scope))
              (js-define new-scope name result t t))
          (js-pop-scope))))
    result))

(defsubst js-x-block (n)
  "The block statement.  ECMA 12.1."
  (loop for kid in (js-node-kids n) do
        (js-exec kid)))

(defun js-x-script (n)
  (let* ((x js-current-context)
         (scope (js-Context-scope x))
         (vars scope)  ; variable object
         (is-eval (eq (js-Context-type x) 'EVAL_CODE))
         readonly
         isdefined
         name
         fun)
    (loop for kid in (js-node-kids (js-node-get n 'fun-decls))
          for name = (js-node-value (js-node-get kid 'name))
          for fun = (js-function-object kid scope)
          do
          (js-define vars name fun (not is-eval)))
    (loop for kid in (js-node-kids (js-node-get n 'var-decls))
          for name = (js-node-get kid 'name)
          for readonly = (js-node-get kid 'read-only) ; const
          for isdefined = (js-has-own-property vars name)
          for was-const = (and isdefined
                               (js-prop-read-only-p
                                (js-get-prop-cell vars name)))
          do
          (if (or (and readonly isdefined) ; const redefining var/const
                  was-const) ; var/const redefining const
              (js-type-error (format "Redeclaration of %s %s"
                                     (if was-const "const" "var")
                                     name)
                             kid))
          (if (or readonly (not isdefined))
              (js-define vars name 'undefined (not is-eval) readonly)))
    (js-x-block n)))

(defun js-x-if (n)
  "The if statement.  ECMA 12.5."
  (let (else)
    (if (js-exec-and-test (js-node-get n 'condition))
        (js-exec (js-node-get n 'then-part))
      (if (setq else (js-node-get n 'else-part))
          (js-exec else)))))

(defun js-x-switch (n)
  "The switch statement.  ECMA 12.11."
  (let* ((expr (js-get-value
                (js-exec (js-node-get n 'discriminant))))
         (cases (js-node-kids (js-node-get n 'cases)))
         (default-index (js-node-get n 'default-index)) ; none => -1
         (i 0)
         (j (length cases))
         match-default case label stmts result)
         
    (catch 'js-x-switch-break
      (while t
        (catch 'js-x-switch-continue
          (when (= i j)
            (when (>= default-index 0)
                  (setq i (1- default-index) ; no case matched, do default
                        match-default t)
                  (throw 'js-x-switch-continue nil))
            (throw 'js-x-switch-break nil))

          (setq case (nth i cases))          ; next case (might be default)

          (if (eq (js-node-get case 'type) 'CASE)
              (setq label (js-get-value
                           (js-exec (js-node-get case 'case-label))))
            (if (not match-default)          ; not defaulting, skip for now
                (throw 'js-x-switch-continue nil)
              (setq label expr)))            ; force match to do default

          (if (js-strict-equal-p label expr)
              (while t                       ; this loop exits switch-loop
                (when (setq stmts (js-node-get case 'statements))
                  (if (eq 'BREAK
                          (setq result
                                (catch 'js-break-cont
                                  (progn (js-exec stmts) nil))))
                      (if (eq (js-Context-target js-current-context) n)
                          (throw 'js-x-switch-break nil)
                        ;; otherwise re-throw the break
                        (throw 'js-break-cont result))))
                (if (= (incf i) j)
                    (throw 'js-x-switch-break nil))
                (setq case (nth i cases)))))
        (incf i)))))

(defsubst js-loop-body (n body)
  "Execute BODY of while, do, for, or for-in loop N.
The caller should catch `js-x-loop-break', thrown if the body broke
from this innermost loop.  If a break or continue executes
whose target is an enclosing loop (i.e. its target is not N), then
it re-throws the `js-break-cont'.  If the body executes normally,
or does partial execution via an inner continue whose target is N,
then this function simply returns."
  (let (result target)
    (when (setq result (catch 'js-break-cont
                         (progn (js-exec body) nil)))
      (setq target (js-Context-target js-current-context))
      (if (eq result 'BREAK)
          (if (eq target n)
              (throw 'js-x-loop-break nil)      ; break this loop
            (throw 'js-break-cont 'BREAK))      ; break outer loop
        (unless (eq target n)
          (throw 'js-break-cont 'CONTINUE)))))) ; continue outer loop    

(defun js-x-for (n)
  "The for statement.  ECMA 12.6.3."
  (let ((setup (js-node-get n 'setup))
        (condition (js-node-get n 'condition))
        (update (js-node-get n 'update))
        (body (js-node-get n 'body)))
    (if setup
        (js-get-value (js-exec setup))) ; Ecma: get val and discard it
    (catch 'js-x-loop-break
      (while (or (null condition)
                 (js-exec-and-test condition))
        (js-loop-body n body)
        (if update
            (js-get-value (js-exec update)))))))  ; get val and discard

(defun js-x-while (n)
  "The while statement.  ECMA 12.6.2."
  (let ((body (js-node-get n 'body))
        (condition (js-node-get n 'condition)))
    (catch 'js-x-loop-break
      (while (js-exec-and-test condition)
        (js-loop-body n body)))))

(defun js-x-for-in (n)
  "The for..in statement.  ECMA 12.6.4."      ; for
  (let* ((vardecl (js-node-get n 'var-decl)) ; <var>
         (iter (js-node-get n 'iterator))    ; {iter} (Node)
         (obj-node (js-node-get n 'object))
         (expr (js-exec obj-node))            ; .. in {object}
         (val (js-get-value expr))            ; (dereference)
         (object (js-to-object val expr obj-node))
         (body (js-node-get n 'body)))
    (if vardecl
        (js-exec vardecl))  ; create iter in variable object if necessary
    (catch 'js-x-loop-break
      (dolist (prop (js-for-in object))
        (js-put-value (js-exec iter) prop iter)  ; update iterator
        (js-loop-body n body)))
    'undefined))

(defun js-x-do (n)
  (let ((body (js-node-get n 'body))
        (condition (js-node-get n 'condition)))
    (catch 'js-x-loop-break
      (loop do (js-loop-body n body)
            while (js-exec-and-test condition)))))

(defun js-x-break-cont (n)
  "Throw `js-break-cont' with a value of `break' or `cont'.
Sets the target in the current context to the target node."
  ;; Narcissus sets the target to the label node, not the statement
  ;; it's labeling.  This works for break, but not continue.  I've
  ;; fixed it by resetting the target here to the labeled statement.
  (let* ((target (js-node-get n 'target))
         (stmt (js-node-get target 'statement)))
    (setf (js-Context-target js-current-context)
          (or stmt target))
    (throw 'js-break-cont (js-node-get n 'type))))

(defun js-x-try (n)
  (let* ((clauses (js-node-kids (js-node-get n 'catch-clauses)))
         (num-clauses (length clauses))
         (finally-block (js-node-get n 'finally-block))
         (x js-current-context)
         caught
         found
         exc
         guard)
    (unwind-protect
        (if (zerop num-clauses)
            ;; no catch clauses => let exceptions propagate up
            (js-exec (js-node-get n 'try-block))

          ;; otherwise check any exceptions thrown in try-block
          (when (setq caught
                      (catch 'js-THROW
                        (progn
                          (js-exec (js-node-get n 'try-block))
                          nil)))
            ;; assume no catch clause will match
            (setq exc (js-Context-result x))
            (setf (js-Context-result x) 'undefined)

            ;; look for a catch clause whose guard succeeds
            (loop for c in clauses
                  until found
                  do
                  ;; bind exception obj to specified name in a new scope
                  (js-define (js-push-scope)
                             (js-node-get c 'var-name)
                             exc
                             t)   ; {DontDelete}
                  (unwind-protect
                      (unless  ; ...this clause has a guard and it fails
                          (and (setq guard (js-node-get c 'guard))
                               (not (js-get-value
                                     (js-exec guard))))
                        (js-exec (js-node-get c 'block))
                        (setq found t))
                    ;; always remove the catch-block scope
                    (js-pop-scope)))
            ;; no matching clause found => re-throw exception
            (when (not found)
              (setf (js-Context-result x) exc)
              (throw 'js-THROW caught))))
      ;; finally
      (if finally-block
          (js-exec finally-block)))))

(defun js-x-throw (n)
  (setf (js-Context-result js-current-context)
           (js-get-value (js-exec (js-node-get n 'exception))))
  ;; data value has to be 'throw to distinguish from 'return
  (throw 'js-THROW 'throw))

(defun js-x-return (n)
  (let ((value (js-node-get n 'value)))
    (setf (js-Context-result js-current-context)
          (if (eq value 'undefined)
              value
            (js-get-value (js-exec value))))
    (throw 'js-THROW 'return)))

(defun js-x-with (n)
  (let* ((expr (js-node-get n 'object))
         (ref (js-exec expr))
         (obj (js-to-object (js-get-value ref) ref expr)))
    (js-push-scope obj)
    (unwind-protect
        (js-exec (js-node-get n 'body))
      (js-pop-scope))))

(defun js-x-var-decl (n)
  (let ((kids (js-node-kids n))
        (x js-current-context)
        name
        scope)
    (loop for kid in kids
          for value = (js-node-get kid 'initializer)
          when value
          do
          (setq name (js-node-get kid 'name)
                scope (js-var-scope (js-Context-scope x) name)
                value (js-get-value (js-exec value)))
          (if (eq (js-node-type n) 'CONST)
              (js-define scope name value
                         (neq (js-Context-type x) 'EVAL_CODE)
                         t)
            (js-put scope name value)))))

(defun js-x-debugger (n)
  (error "not yet implemented: %s" (js-node-type n)))

(defun js-x-semicolon (n)
  (let ((expr (js-node-get n 'expression)))
    (if expr
        (setf (js-Context-result js-current-context)
              (js-exec-and-get expr)))))

(defun js-x-label (n)
  (js-exec (js-node-get n 'statement)))

(defun js-x-comma (n)
  (let (v)
    (loop for kid in (js-node-kids n) do
          (setq v (js-exec-and-get kid)))
    v))

(defun js-x-assign (n)
  (let* ((kid (js-node-kid n 0))
         (ref (js-exec kid))
         (op (js-node-get n 'assign-op))
         lhs rhs result)
    (if op
        (setq lhs (js-get-value ref)))
    (setq rhs (js-exec-kid-getval n 1))
    (setq result
          (if op
              (condition-case nil
                  (case op
                    (PLUS (js-op-add lhs rhs))
                    (MINUS (js-op-sub lhs rhs))
                    (MUL (js-op-mul lhs rhs))
                    (DIV (js-op-div lhs rhs))
                    (MOD (js-op-mod lhs rhs))
                    (BITWISE_OR (logior lhs rhs))
                    (BITWISE_XOR (logxor lhs rhs))
                    (BITWISE_AND (logand lhs rhs))
                    (LSH (js-op-lsh lhs rhs))
                    (RSH (js-op-rsh lhs rhs))
                    (URSH (js-op-ursh lhs rhs)))
                (wrong-type-argument "NaN"))
            rhs))
    (js-put-value ref result kid)
    result))

(defun js-x-conditional (n)
  (if (js-test (js-exec-kid-getval n 0))
      (js-exec-kid-getval n 1)
    (js-exec-kid-getval n 2)))

(defun js-x-or (n)
  (let ((v1 (js-exec-kid-getval n 0))
        (v2 (js-exec-kid-getval n 1)))
    (if (js-test v1)
        v1
      v2)))

(defun js-x-and (n)
  (let ((v1 (js-exec-kid-getval n 0))
        (v2 (js-exec-kid-getval n 1)))
    (if (js-test v1)
        v2
      v1)))

(defun js-x-bitwise-or (n)
  (logior (js-exec-kid-getval n 0)
          (js-exec-kid-getval n 1)))

(defun js-x-bitwise-xor (n)
  (logxor (js-exec-kid-getval n 0)
          (js-exec-kid-getval n 1)))

(defun js-x-bitwise-and (n)
  (logand (js-exec-kid-getval n 0)
          (js-exec-kid-getval n 1)))

(defun js-x-eq (n)
  (js-bool (js-equal-p (js-exec-kid-getval n 0)
                       (js-exec-kid-getval n 1))))

(defun js-x-ne (n)
  (js-bool
   (not (js-equal-p (js-exec-kid-getval n 0)
                    (js-exec-kid-getval n 1)))))

(defun js-x-strict-eq (n)
  (js-bool (js-strict-equal-p (js-exec-kid-getval n 0)
                              (js-exec-kid-getval n 1))))

(defun js-x-strict-ne (n)
  (js-bool
   (not (js-strict-equal-p (js-exec-kid-getval n 0)
                           (js-exec-kid-getval n 1)))))

(defun js-x-lt (n)  ;; 11.8.1
  (let* ((v0 (js-exec-kid-getval n 0))
         (v1 (js-exec-kid-getval n 1))
         (result (js-less-p v0 v1)))
    (if (eq result 'undefined)
        'false
      (js-bool result))))

(defun js-x-gt (n)  ;; 11.8.2
  (let* ((v0 (js-exec-kid-getval n 0))
         (v1 (js-exec-kid-getval n 1))
         (result (js-less-p v1 v0)))
    (if (eq result 'undefined)
        'false
      (js-bool result))))

(defun js-x-le (n)  ;; 11.8.3
  (let* ((v0 (js-exec-kid-getval n 0))
         (v1 (js-exec-kid-getval n 1))
         (result (js-less-p v1 v0)))
    (if result   ; t or 'undefined
        'false
      'true)))

(defun js-x-ge (n)  ;; 11.8.4
  (let* ((v0 (js-exec-kid-getval n 0))
         (v1 (js-exec-kid-getval n 1))
         (result (js-less-p v0 v1)))
    (if result   ; t or 'undefined
        'false
      'true)))

(defun js-x-in (n)
  (js-in (js-exec-kid-getval n 0)
         (js-exec-kid-getval n 1)))

(defun js-x-instanceof (n)
  (js-bool (js-instanceof (js-exec-kid-getval n 0)
                          (js-exec-kid-getval n 1))))

(defun js-x-lsh (n)
  (js-op-lsh (js-exec-kid-getval n 0)
             (js-exec-kid-getval n 1)))

(defun js-x-rsh (n)
  (js-op-rsh (js-exec-kid-getval n 0)
             (js-exec-kid-getval n 1)))

(defun js-x-ursh (n)
  (js-op-ursh (js-exec-kid-getval n 0)
              (js-exec-kid-getval n 1)))

(defun js-x-plus (n)
  (js-op-add (js-exec-kid-getval n 0)
             (js-exec-kid-getval n 1)))

(defun js-x-minus (n)
  (js-op-sub (js-exec-kid-getval n 0)
             (js-exec-kid-getval n 1)))

(defun js-x-mul (n)
  (js-op-mul (js-exec-kid-getval n 0)
             (js-exec-kid-getval n 1)))

(defun js-x-div (n)
  (js-op-div (js-exec-kid-getval n 0)
             (js-exec-kid-getval n 1)))

(defun js-x-mod (n)
  (js-op-mod (js-exec-kid-getval n 0)
             (js-exec-kid-getval n 1)))

(defun js-x-delete (n)
  (let ((r (js-exec-kid n 0)))
    (if (not (js-reference-p r))
      'true
      (js-delete (js-ref-base r) (js-ref-prop r)))))
                 
(defun js-x-void (n)
  (js-get-value (js-exec-kid n 0)))

(defun js-x-typeof (n)
  (let ((result (js-exec-kid n 0)))
    (if (js-reference-p result)
        (setq result (if (js-null-p (js-ref-base result))
                         'undefined
                       (js-get (js-ref-base result)
                               (js-ref-prop result)))))
    (js-typeof result)))

(defun js-x-not (n)
  (js-not (js-bool (js-test (js-exec-kid-getval n 0)))))

(defun js-x-bitwise-not (n)
  (lognot (js-exec-kid-getval n 0)))

(defun js-x-unary-plus (n)
  (condition-case nil
      (+ (js-to-number (js-exec-kid-getval n 0)))
    (wrong-type-argument "NaN")))

(defun js-x-unary-minus (n)
  (condition-case nil
      (- (js-exec-kid-getval n 0))
  (wrong-type-argument "NaN")))

(defun js-x-inc-dec (n)
  (let* ((kid0 (js-node-kid n 0))
         (ref (js-exec kid0))
         (num (js-to-number (js-get-value ref)))
         (postfix (js-node-get n 'postfix))
         result)
    (if postfix
        (setq result num))
    (js-put-value ref
                  (if (eq (js-node-type n) 'INCREMENT)
                      (incf num)
                    (decf num))
                  kid0)
    (unless postfix
      (setq result num))
    result))

(defun js-x-dot (n)
  (let* ((kid0 (js-node-kid n 0))
         (r (js-exec kid0))
         (v (js-get-value r))
         (u (js-node-value (js-node-kid n 1))))
    (js-make-reference (js-to-object v r kid0)
                       (format "%s" u)  ; String(u)
                       n)))

(defun js-x-index (n)
  (let* ((kid0 (js-node-kid n 0))
         (r (js-exec kid0))
         (val (js-get-value r))
         (u (js-exec-kid-getval n 1)))
    (js-make-reference (js-to-object val r kid0)
                       (format "%s" u)  ; String(u)
                       n)))

(defun js-x-list (n)
  "Create an arguments list by evaluating the kids of N.
Return value is a plain lisp list of JavaScript objects.
Converted to Arguments object later, when function is called."
  (loop for k in (js-node-kids n)
        collect (js-get-value (js-exec k))))

(defun js-x-call (n)
  (let* ((r (js-exec-kid n 0))     ; reference or function
         (args (js-exec-kid n 1))  ; args list from `js-x-list'
         (f (js-get-value r))      ; function object
         thisobj)
    (if (js-reference-p r)
        (setq thisobj (js-ref-base r)))
    (cond
     ((js-Function-p f)
      (js-Function-call f
                        (if (js-Activation-p thisobj)
                            nil
                          thisobj)
                        (if (eq args 'undefined)
                            nil
                          args)))
     ((eq f 'undefined)
      (if (and (js-object-p thisobj)
               (setq f (js-get thisobj "__noSuchMethod__"))
               (js-Function-p f))
          (js-Function-call f
                            (if (js-Activation-p thisobj)
                                nil
                              thisobj)
                            (list (js-ref-prop r)
                                  (if (eq args 'undefined)
                                      (js-Array--construct--)
                                    (js-Array--construct-- nil args))))
        (js-type-error
         (js-format "Cannot find function %s." (js-ref-prop r))
         (js-node-kid n 0))))
     (t
      (js-type-error
       (js-format "%s is not callable" r) (js-node-kid n 0))))))
       
(defun js-x-new (n)
  (let* ((r (js-exec-kid n 0))
         (f (js-get-value r))
         args)
    (if (eq (js-node-type n) 'NEW_WITH_ARGS)
      (setq args (js-exec-kid n 1)))
    (apply 'js-call-internal
           'js-Internal-construct
           f        ; constructor function
           args)))  ; constructor args

(defun js-x-array-init (n)
  (loop with v = (js-make-Array)
        for count from 0
        for k in (js-node-kids n) do
        (js-put v count (js-exec-kid-getval n count))
        finally do
        (js-put v "length" count)
        finally return v))

(defun js-x-object-init (n)
  (let ((v (js-make-object))
        f name)
    (loop for kid in (js-node-kids n)
          for type = (js-node-type kid) do
          (case type
            (PROPERTY_INIT
             (js-put v
                     (js-node-value (js-node-kid kid 0))
                     (js-get-value
                      (js-exec-kid kid 1))))
            ((GETTER SETTER)
             (setq f (js-function-object
                      kid
                      (js-Context-scope js-current-context))
                   name (js-node-value (js-node-get kid 'name)))
             (if (eq type 'GETTER)
                 (js-define-getter v name f)
               (js-define-setter v name f)))
            (t
             (error "PANIC: unknown node type %s" type))))
    v))

(defun js-x-null (n)
  'null)

(defun js-x-this (n)
  (js-Context-this js-current-context))

(defun js-x-true (n)
  'true)

(defun js-x-false (n)
  'false)

(defun js-x-identifier (n)
  (let ((s (js-Context-scope js-current-context))
        (id (js-node-value n))
        found)
    (while (and s (not (js-has-property s id)))
      (setq s (js-Object-scope s)))
    (js-make-reference s id n)))

(defun js-x-node-value (n)
  (js-node-value n))

(defun js-x-regexp (n)
  (let ((re (js-node-value n))
        pattern flags)
    (if (string-match "/\\(.+\\)/\\([gim]*\\)" re)
        (setq pattern (match-string 1 re)
              flags (match-string 2 re))
      (js-syntax-error
       (js-format "Invalid regexp: '%s'" re)))
    (js-RegExp--construct-- nil (list pattern flags))))

(defun js-x-group (n)
  (js-exec-kid n 0))

(defun js-evaluate (s &optional file line)
  "Top-level evaluation function for global script code.
S is the string to evaluate.  FILE and LINE are currently ignored.
You need to wrap it with a catch for `js-THROW' to handle thrown
exceptions."
  (if (not (string= (js-typeof s) "string"))
      s
    (let ((js-current-context
           (or js-current-context
               (js-context 'GLOBAL_CODE (js-global)))))
      (js-exec (js-parse-string s))
      (js-Context-result js-current-context))))

(defun js-eval-buffer (buf)
  (let (context global)
    (setq global (make-js-Object))
    (setq context (js-context 'GLOBAL_CODE global))
    (let ((js-current-context context)  ; buffer-local
          (js-no-parent-links t))      ; buffer-local
      (js-init-standard-objects global)
      (save-excursion
        (set-buffer buf)
        (js-evaluate
         (buffer-substring-no-properties (point-min)
                                         (point-max)))))))

(provide 'js-exec)

;;; js-exec.el ends here
