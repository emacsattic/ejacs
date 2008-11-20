;;; js-native-function:  implementation of JavaScript Function type

;; Author:  Steve Yegge (steve.yegge@gmail.com)
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

;;; Code:

(require 'js-parse)

(defun js-init-native-Function (object-proto function-proto)
  "ECMA 15.3.2 -- initialize and return a Function Constructor object.
OBJECT-PROTO is the Object.prototype object per 15.2.3.1.
FUNCTION-PROTO is the Function.prototype object per 15.3.3.1."
  (let ((Function (make-js-Function :proto function-proto
                                    :builtin-p t))
        (fop function-proto))

    ;; during bootstrapping, for `js-builtin-function', make the function
    ;; proto lookup find the right object before we've set it properly in
    ;; the global context.
    (flet ((js-get-Function-prototype () function-proto))

      (setf (js-Function-call-slot Function)
            'js-Function-ctor--call--)
      (setf (js-Function-construct-slot Function)
            'js-Function-ctor--construct--)

      (js-define Function "length" 1 t t t) ; 15.3.3
      (js-define Function "prototype" fop t t t) ; 15.3.3.1

      ;; 15.3.4 -- Properties of the Function Prototype Object
      ;; Not to be confused with the user-visible "call" property,
      ;; defined down below.  This is for calling Function.prototype():
      (setf (js-Function-call-slot fop) 'js-Function--no-op--)

      (js-define fop "constructor" Function nil nil t) ; 15.3.4.1

      ;; 15.3.4.2
      (js-define-builtin fop "toString"
                         'js-Function-toString 'no-wrap t nil t)
      ;; 15.3.4.3
      (js-define-builtin fop "apply"
                         'js-Function-apply nil ; wrapped
                         t nil t)
      ;; 15.3.4.4
      (js-define-builtin fop "call"
                         'js-Function-call nil ; wrapped
                         t nil t)

      (js-define-builtin fop "toSource"
                         'js-Function-toSource 'no-wrap t nil t))
    Function))

(defun js-Function--no-op-- (funobj this args)
  "15.3.4 -- when invoked, Function.__proto__() returns 'undefined.
It accepts any arguments and ignores them."
  'undefined)

(defun js-Function-ctor--call-- (obj args)
  "15.3.1.1 -- Function constructor called as a function"
  (funcall 'js-Function-ctor--construct-- nil args))

(defun js-Function-ctor--construct-- (ctor args)
  "15.3.2.1 -- Function Constructor as part of a 'new' expression"
  (let ((params (butlast args))
        (body (car (last args)))
        (js-no-parent-links t)
        ast)
    ;; Just hack a script together and extract the function node.
    (setq params
          (mapconcat 'identity
                     (loop for obj in params
                           for p = (js-to-string obj)
                           collect p)
                     ", "))
    (setq body (js-to-string body))
    (setq ast (js-parse-string
               (concat "var f = function(" params "){" body "}")))
    (setq ast
          (js-node-get
           (car (js-node-kids                ; identifier (name == f)
                 (car (js-node-kids ast))))  ; var statement
           'initializer))                     ; function
    (js-function-object ast (js-global))))

(defun js-Function-toString (funobj args)
  "ECMA 15.3.4.2 -- Function.prototype.toString() method.
Implementation-dependent, but typically you prettyprint the source."
  (js-function-print funobj nil))

(defun js-Function-toSource (funobj args)
  "ECMA extension - prints the source for FUNOBJ on one line."
  (js-function-print funobj t))

(defun js-function-print (funobj &optional one-line)
  "Code common to Function.toSource and Function.toString.
Returns the JavaScript source for FUNOBJ.  ONE-LINE, if non-nil,
causes the result to be a single line of code."
  (let ((slot (js-Function-call-slot funobj))
        name)
    (if (js-Function-builtin-p funobj)
        (if (and (symbolp slot)
                 (setq name (get slot 'js-name)))
            (format "function %s() {\n  [native code]\n}" name)
          (format "[native elisp function `%s']" slot))
      (js-node-to-source
       (js-Function-node funobj)
       one-line))))

(defsubst js-cleanup-thisobj (thisobj)
  "Implement the rules of ECMA-262 15.3.4.4."
  (cond
   ((js-null-p thisobj)
    (js-global))
   ((js-object-p thisobj)
    thisobj)
   (t
    (js-to-object thisobj))))

(defun js-Function--call-- (funobj thisobj args)
  "Internal implementation of [[Call]] for JavaScript function objects.

This is -not- used for calling built-in functions.  `js-Function-call'
and `js-Function-apply' accept a function object and determine whether
to invoke it as a built-in function or a JavaScript function.

This particular implementation expects the Function object instance
FUNOBJ to have a node slot containing the AST node for the function
definition.  Native and host-scripted functions have different [[Call]]
implementations.

THIS is the 'this' object for the function call.  ARGS is the
arguments list, a plain lisp list of JavaScript values."
  (let* ((x js-current-context)
         (x2 (make-js-Context :type 'FUNCTION_CODE))
         (retval 'undefined)
         (global (js-global))
         (scope (or (js-Function-scope funobj) global))
         completion)
    ;; 10.2.3: initialize new execution context from calling context
    (setf (js-Context-this x2) (js-cleanup-thisobj thisobj))
    (setf (js-Context-caller x2) x)
    (setf (js-Context-callee x2) funobj)

    ;; push a new Activation on the front of the scope chain
    (setf (js-Context-scope x2)
           (js-activation-object funobj scope args))

    (let ((js-current-context x2))
      (setq completion
            (catch 'js-THROW
              (progn
                (js-exec
                 (js-node-get (js-Function-node funobj) 'body))
                nil)))
      (if (eq completion 'return)
          (setq retval (js-Context-result x2))
        (when completion
          ;; they may have thrown the error object, or put it in result
          (throw 'js-THROW
                 (setf (js-Context-result x)
                       (if (js-object-p completion)
                           completion
                         (js-Context-result x2)))))))
      retval))

(defun js-Function--construct-- (ctor args)
  "13.2.2 -- Internal function for [[Construct]] invocation of CTOR.
ARGS is the arguments list, a plain lisp list.  If the constructor
function returns an Object, we return it.  Otherwise we return a
new object of type CTOR."
  (let ((o (js-make-object))
        (p (js-get ctor "prototype"))
        v)
    (if (js-object-p p)
        (setf (js-Object-proto o) p))
    ;; else o.__proto__ defaulted to Object.prototype

    (setq v (js-Function--call-- ctor o args))
    (if (js-object-p v)
        v
      o)))

(defun js-Function--has-instance-- (ctor v &rest args)
  "Internal function for [[HasInstance]] invocation on CTOR.
V is the object to check."
  (let (retval o p continue)
    (if (js-primitive-p v)
        nil
      (setq p (js-get ctor "prototype"))
      (if (js-primitive-p p)
          (js-type-error "'prototype' property is not an object"
                         (js-printobj (js-get ctor 'node))))
      (setq continue t
            retval nil)
      (while (and continue (setq o (js-prototype v)))
        (if (eq o p)
            (progn
              (setq retval t)
              (setq continue nil))
          (setq v o)))
      retval)))

(defun js-Function-apply (funobj this args)
  "15.3.4.3 -- Function.prototype.apply.
FUNOBJ is the function object being applied.  THIS is 'this' for the call.
ARGS is the arguments list, a plain lisp list, corresponding to
what args were passed by the user code.  The first argument, (car ARGS),
must be a `js-Array' or `js-Arguments' object, and the remaining args
are ignored."
  (let* ((argArray (car-safe args))
         (arglist
          (cond
           ((js-null-p argArray)
            nil)
           ((js-Array-p argArray)
            (js-Array-mapcar 'identity argArray))
           ((js-Arguments-p argArray)
            (copy-list (js-Arguments-args argArray)))
           (t
            (js-type-error
             (concat "First argument to Function.prototype.apply"
                     " must be an array or arguments object"))))))
    (js-Function-call funobj (js-cleanup-thisobj this) arglist)))

(defun js-Function-call (funobj this args)
  "15.3.4.4 -- Function.prototype.call(thisArg [, arg1 [, arg2, ...]])

This is the user-level Function.prototype.call function.
It is also the entry point for JavaScript evaluator CALL operation.

FUNOBJ is a `js-Function', which could be a built-in function or
a user-defined function.

THISOBJ is the object to be used as the `this' object for the
call.  If it is null, or not an Object, it will be cleaned up
according to the rules in ECMA 15.3.4.4

ARGS is a lisp list of JavaScript values that will be passed as
the arguments to the called function. "
  (if (js-Function-builtin-p funobj)
      ;; Lisp functions that implement JS builtins take (this args).
      ;; Other lisp functions are wrapped in a lambda that throws
      ;; away this and applies the args.
      (funcall (js-Function-call-slot funobj)
               (js-cleanup-thisobj this)
               args)
    (js-Function--call-- funobj this args))) ; scrubs `this' itself

(provide 'js-native-function)

;;; js-native-function.el ends here
