;;; js-runtime.el -- runtime support for ECMAScript interpreter

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

;;; Commentary:
;;
;; This package implements the runtime support described by the
;; ECMA-262 specification, 3rd edition, available at this URL:
;; http://www.ecma-international.org/publications/standards/Ecma-262.htm
;; The parser and interpreter are in separate packages.
;;
;; This package has no features for Emacs users; it is a library for use
;; by Emacs-JavaScript applications such as `js-console'.
;;
;; Some of the algorithms are borrowed from SpiderMonkey and Rhino.
;;
;; Emacs Compatibility:
;;
;; This package only works in GNU Emacs >= 22, for various reasons
;; that include multibyte/utf-8/unicode support.  I have no plans to
;; support older versions of Emacs.
;;
;; ECMA Extensions:
;;
;; This runtime library implements several features not in ECMA-262,
;; including some JavaScript 1.5/1.6 features and a few SpiderMonkey
;; extensions.  The goal is to evolve towards 1.7 compatibility.  I'm
;; not particularly worried about security (this is an editor, not a
;; web browser) so __defineProperty__ is always available.
;;
;; Extensions implemented:
;;  - toSource() method
;;  - get/set syntax for getters/setters in object initializers
;;  - __defineGetter__, __defineSetter__
;;  - __lookupGetter__, __lookupSetter__
;;  - __proto__, __parent__
;;  - __noSuchMethod__
;;  - __defineProperty__ (SpiderMonkey)
;;  - array indexing of strings, e.g. "foobar"[3] => 'b'
;;  - Array:  every, indexOf, filter, forEach, lastIndexOf, map, some
;;
;; Interpreters:
;;
;; You can fire up multiple JavaScript interpreters per Emacs session.
;; However, you can only have one intepreter per buffer, as the
;; interpreter uses buffer-local variables for storing the state of
;; the parser, interpreter, and runtime.
;;
;; Primitive Types:
;;
;; The JavaScript primitive types and values are mapped as follows:
;;
;;   true -> t or 'true
;;   false -> nil or 'false
;;   null -> nil, 'null or "null".
;;   undefined -> 'undefined or "undefined"
;;   string -> primitive lisp string
;;   number -> lisp floating-point value
;;   NaN    -> 0.0e+NaN
;;   +Infinity -> 1.0e+INF
;;   -Infinity -> -1.0e+INF
;;
;; Whenever possible, Lisp nil is used for false/null/undefined, and t
;; for true. The symbols 'null, 'false or 'undefined (or their string
;; representations) are generally only used for disambiguation.
;;
;; Numeric Representation:
;;
;; Emacs integers are 29-bit signed values; i.e., -2**28 to 2**28 - 1.
;; Emacs floating-point numbers are C doubles, hence 64 bits on most
;; machines these days.  We can't achieve ECMA compliance with Emacs
;; integers, but we might do so with Emacs floats.
;;
;; AFAICT, the bitwise and shift operators all work with 32-bit ints
;; in SpiderMonkey/Rhino, and overflow or return nonsense when used
;; with integral values that require greater precision.  The other
;; arithmetic operators all seem to operate with 64-bit precision.
;; Hence, if we represent Numbers and numeric values internally as
;; floats, and do range checks before bitwise/shift ops, we should see
;; reasonable results, possibly excepting overflow/underflow
;; scenarios, but I can live with that.
;;
;; NaN, +Infinity and -Infinity are represented internally by the
;; Emacs floating-point values 0.0e+NaN, 1.0e+INF and -1.0e+INF.
;; -1.0e+INF is not equal/eql/eq/= to either of the other two.  Sadly,
;; 0.0e+NaN and 1.0e+INF are `equal', `eql', `=', and they format the
;; same when printed as strings.  They are both not `eq' to themselves
;; or each other.  Each is '>' than the other.  In a nutshell, there
;; is no combination of equality predicates or other operators than
;; can distinguish the two values.  The only way to distinguish them
;; that I am aware of is to use `format' with "%f" or "%g".  I've made
;; utility functions to smooth over this as much as possible.
;;
;; Emacs does not distinguish +/- zero, so this implementation ignores
;; sections of Ecma-262 that deal specially with -0, always treating it
;; as +0 instead.
;;
;; Built-In Constructors:
;;
;; The built-in classes Object, Function, Array, String, Boolean, etc.
;; are represented as defstructs.  Each has a table of internal
;; methods and properties (a "vtable" of type `js-Internal').  The
;; vtable and Ecma-262 [[Class]] property are stored as symbol props
;; on the cl-struct tags.  The constructors are all initialized on
;; construction of a new interpreter.

;;; TODO:
;; - finish Number.prototype.toString (section 9.8.1)
;; - arguments.caller and exception stack traces
;;     (http://examples.oreilly.com/jscript3/text/7-3.txt)
;; - SpiderMonkey unit-test suite
;; - JS regex engine (just port the Rhino code - easiest)
;; - debugger (and add `debugger' keyword)
;; - E4X
;; - Emacs host objects
;; - unicode identifiers (in SpiderMonkey but not Rhino)
;; - warn on identifiers that are future reserved words
;; - strict-mode

;;; BUGS:
;; - bitwise and shift operators fail for sufficiently large integers
;; - mismatched quotes in console, e.g. "foobar' => lisp eval error

;;; PERFORMANCE:
;; - cache MRU prop name/val in a new slot on js-Object

;;; Code:

(require 'js-parse)
(require 'help-fns)  ; for setting length prop on native functions
(require 'js-util)

(defconst js-obj-plist-type 'hybrid
  "Data structure to use for JS object property lists.
Currently supported types are `alist', `hybrid', and `splay'.
A value of `alist' always uses alists for any number of properties.
A value of `splay' uses a splay tree to hold an object's properties.
A value of `hybrid' uses an alist up to a certain size, then converts
to a hashtable for that object.")

(case js-obj-plist-type
  (hybrid
   (load "js-obj-hybrid"))
  (alist
   (load "js-obj-alist"))
  (splay
   (load "js-obj-splay"))
  (t
   (error "Unknown backing store type: %s" js-obj-plist-type)))

(eval-and-compile
  (require 'cl))     ; need at runtime, e.g. for most-positive-float

(eval-when-compile
  (defun js-exec (n) nil))

(defvar js-current-context nil
  "Current buffer-local JavaScript Execution Context.
An execution context is associated with each buffer running a script.
Each time a new context is entered, we dynamically bind this variable
with `let', creating a context stack that unwinds with the lisp
execution stack.")

;(make-variable-buffer-local 'js-current-context)

;;; Number parsing

(defsubst js-radix-chars (radix)
  "Return the list of valid chars for RADIX.
Returns the ascii char codes, including upper/lowercase."
  (if (<= radix 10)
      (loop for i from ?0 to (+ ?0 radix -1) collect i)
    (append
     (loop for i from ?0 to ?9 collect i)
     (loop for i from ?a to (+ ?a (- radix 11)) collect i)
     (loop for i from ?A to (+ ?A (- radix 11)) collect i))))

(defun js-parse-int (string &optional radix)
  "JavaScript global parseInt function.  ECMA 15.1.2.2.
Returns a lisp integer or integral float value."
  ;; NOTE:  parseInt("Infinity") => NaN  (rhino/squarefree)
  (let ((s (js-to-string string))
        (sign 1)
        (i 0)
        chars )
    ;; trim leading/trailing whitespace
    (if (string-match "^\\s-+\\(.+?\\)\\s-*" s)
        (setq s (match-string 1 s)))

    ;; strip +/- and set sign
    (when (plusp (length s))
      (if (eq (aref s 0) ?-)
          (setq sign -1))
      (if (memq (aref s 0) '(?- ?+))
          (setq s (substring s 1))))

    ;; determine radix
    (setq radix (if radix
                    (truncate (js-to-int32 radix))
                  10))
    (cond
     ((string-match "^0[xX]" s)
      (setq s (substring s 2))
      (setq radix 16))
     ((string-match "^0." s)
      (setq radix 8)))

    ;; trivial rejects
    (if (or (string= s "")
            (not (numberp radix))
            (< radix 2)
            (> radix 36))
        0.0e+NaN
      ;; strip trailing bad chars
      (setq chars (js-radix-chars radix))
      (setq s (apply 'string (loop for c across s
                                   until (not (memq c chars))
                                   collect c)))
      (cond
       ((string= s "")
        0.0e+NaN)
       ;; js-parse-int-internal handles radixes <= 16, but Emacs
       ;; is likely to be faster and more accurate for large numbers.
       ((<= radix 16)
        (condition-case nil
            (string-to-number s)  ; call emacs to parse it
          (error 0.0e+NaN)))
       (t
        (js-parse-int-internal s radix sign))))))

(defun js-parse-int-internal (s radix sign)
  "Parse int from S with RADIX and SIGN.  S must be well-formed."
  (let ((digit-max ?9)
        (lcBound ?a)
        (ucBound ?A)
        (len (length s))
        (end 0)
        (sum 0.0))
    (cond
     ((< radix 10)
      (setq digit-max (+ ?0 radix -1)))
     ((> radix 10)
      (setq lcBound (+ ?a radix -10))
      (setq ucBound (+ ?A radix -10))))
    (catch 'break
      (loop with new-digit = 0
            for c across s do
            (cond
             ((and (<= ?0 c) (<= c digit-max))
              (setq new-digit (- c ?0)))
             ((and (<= ?a c) (< c lcBound))
              (setq new-digit (+ c (- ?a) 10)))
             ((and (<= ?A c) (< c ucBound))
              (setq new-digit (+ c (- ?A) 10)))
             (t
              (throw 'break nil)))
            (incf end)
            (setq sum (+ (* sum radix) new-digit))))
    ;; Rhino has some special handling for rounding inaccuracies in
    ;; the basic algorithm when applied to very large numbers.  I'll
    ;; punt on that for now.
    (if (zerop end)
        0.0e+NaN
      sum)))

(defconst js-plus-minus-fp-regexp
  (concat "^\\([+-]?" js-fp-regexp "\\)")
  "Matches +/- floating-point numbers, but not integers.")

(defconst js-plus-minus-int-regexp
  (concat "^\\([+-]?" js-int-regexp "\\)")
  "Matches +/- JavaScript integers.")

(defun js-parse-float (val)
  "Parse a string as a decimal literal.  ECMA 15.1.2.3.
Currently limited by the capabilities of `string-to-number' and
other problems with Elisp's numeric precision."
  (let ((s (js-to-string val)))
    (if (string-match "^\\s-+\\(.+\\)" s) ; strip leading spaces
        (setq s (match-string 1 s)))
    (if (string-match js-plus-minus-fp-regexp s)
        (string-to-number (match-string 1 s))
      (if (string-match js-plus-minus-int-regexp s)
          (js-parse-int (match-string 1 s))
        0.0e+NaN))))

;;; js2-runtime proper starts here

(defsubst js-bool (expr)
  "Convert a Lisp boolean expression to a JavaScript boolean value.
Turns non-nil to 'true and nil to 'false"
  (if expr 'true 'false))

(defsubst aref-safe (array idx)
  "Return element of ARRAY at index IDX.
Returns nil if IDX is out of bounds, so be careful to use it only
when you know there are no nils in ARRAY.  IDX starts at zero."
  (if (or (minusp idx) (>= idx (length array)))
      nil
    (aref array idx)))

(defsubst js-primitive-p (value)
  (or (memq value '(undefined null true false))
      (stringp value)
      (symbolp value)  ; e.g. property names
      (numberp value)))

(defsubst js-null-p (val)
  "Return t if VAL is nil or JavaScript null or undefined."
  (or (null val)
      (member val '(undefined null))))

(defsubst js-boolean-p (val)
  "Return t if VAL is a primitive boolean or a Boolean object."
  (or (memq val '(t true false))
      (js-Boolean-p val)))

(defsubst js-number-p (val)
  "Return t if VAL is a primitive number or a Number object."
  (or (numberp val)  ; includes 1.0e+INF 0.0e+NaN -1.0e+INF
      (js-Number-p val)))

(defsubst js-string-p (val)
  "Return t if VAL is a primitive string or a String object."
  (or (stringp val)
      (js-String-p val)))

;;; Structs for each of the native object types.
;;
;; These structs hold per-instance data for JavaScript objects.
;; They exist primarily to yield fast access for frequently-accessed
;; internal properties such as an object's prototype or parent scope.
;;
;; These are -not- the same as the built-in type constructors.
;; For example, "Object", a built-in native constructor for objects
;; of type Object, is actually a js-Function, because it's callable
;; (and newable.)  The built-in type constructors typically create
;; an instance of the corresponding struct when invoked with "new".

(defstruct js-Object
  "JavaScript Object object instance"
  proto  ; prototype link
  scope  ; parent scope
  props) ; property list

(defstruct (js-Function (:include js-Object))
  "JavaScript Function object instance"
  call-slot       ; if nil, defaults to the vtable version
  construct-slot  ; ditto
  builtin-p       ; t for built-in function objects (no AST, etc.)
  node)           ; AST node with params and body

(defstruct (js-Array (:include js-Object))
  "JavaScript Array object instance")

(defstruct (js-String (:include js-Object))
  "JavaScript String object instance"
  value)  ; the lisp string object

(defstruct (js-Boolean (:include js-Object))
  "JavaScript Boolean object instance"
  value)

(defstruct (js-Number (:include js-Object))
  "JavaScript Number object instance"
  value)

(defstruct (js-Math (:include js-Object))
  "JavaScript Math object")

(defstruct (js-Date (:include js-Object))
  "JavaScript Date object instance"
  value)  ; time in millis since Epoch

(defstruct (js-RegExp (:include js-Function))
  "JavaScript RegExp object instance"
  pattern    ; regexp source
  flags      ; regexp flags (bit field)
  compiled   ; jsre-compiled-re (byte-compiled regexp)
  elisp)     ; elisp translation of the pattern string

(defstruct (js-Error (:include js-Object))
  "JavaScript Error instance"
  (name "Error"))

(defstruct (js-EvalError
            (:include js-Error (name "EvalError" :read-only t)))
  "ECMA-262 15.11.6.1")

(defstruct (js-RangeError
            (:include js-Error (name "RangeError" :read-only t)))
  "ECMA-262 15.11.6.2")

(defstruct (js-ReferenceError
            (:include js-Error (name "ReferenceError" :read-only t)))
  "ECMA-262 15.11.6.3")

(defstruct (js-SyntaxError
            (:include js-Error (name "SyntaxError" :read-only t)))
  "ECMA-262 15.11.6.4")

(defstruct (js-TypeError
            (:include js-Error (name "TypeError" :read-only t)))
  "ECMA-262 15.11.6.5")

(defstruct (js-URIError
            (:include js-Error (name "URIError" :read-only t)))
  "ECMA-262 15.11.6.6")

(put 'cl-struct-js-EvalError      'js-class 'Error)
(put 'cl-struct-js-RangeError     'js-class 'Error)
(put 'cl-struct-js-ReferenceError 'js-class 'Error)
(put 'cl-struct-js-SyntaxError    'js-class 'Error)
(put 'cl-struct-js-TypeError      'js-class 'Error)
(put 'cl-struct-js-URIError       'js-class 'Error)

;; `defstruct' gives us a unique tag per struct, which we'll use to
;; store class-related information for each Native and Host constructor.

(put 'cl-struct-js-Object   'js-class 'Object)
(put 'cl-struct-js-Function 'js-class 'Function)
(put 'cl-struct-js-Array    'js-class 'Array)
(put 'cl-struct-js-String   'js-class 'String)
(put 'cl-struct-js-Boolean  'js-class 'Boolean)
(put 'cl-struct-js-Number   'js-class 'Number)
(put 'cl-struct-js-Math     'js-class 'Math)
(put 'cl-struct-js-Date     'js-class 'Date)
(put 'cl-struct-js-RegExp   'js-class 'RegExp)
(put 'cl-struct-js-Error    'js-class 'Error)

(defstruct js-Internal
  "A virtual method table for JavaScript internal methods.
The default values of the slots are the functions implementing
the corresponding internal methods for Object.  Native and Host
objects provide their own js-Internal table as a property on the
constructor symbol called `js-vtable'."
  (get           'js-default--get--)           ; [[Get]]
  (put           'js-default--put--)           ; [[Put]]
  (can-put       'js-default--can-put--)       ; [[CanPut]]
  (has-property  'js-default--has-property--)  ; [[HasProperty]]
  (delete        'js-default--delete--)        ; [[Delete]]
  (default-value 'js-default--default-value--) ; [[DefaultValue]]
  (construct     'js-default--construct--)     ; [[Construct]]
  (call          'js-default--call--)          ; [[Call]]
  (has-instance  'js-default--has-instance--)) ; [[HasInstance]]

;; Most of the built-in types don't override the standard internal
;; properties and methods.
(let ((shared-vtable (make-js-Internal)))
  (dolist (sym '(String Boolean Number Math Date RegExp
                 Error EvalError RangeError ReferenceError
                 SyntaxError TypeError URIError))
    (put (intern (concat "cl-struct-js-"
                         (symbol-name sym)))
         'js-vtable
         shared-vtable)))

(defstruct js-Context
  "A JavaScript execution context."
  type
  this
  caller
  callee
  arguments
  scope
  (result 'undefined)
  target)

(defun js-init-scope (obj &optional scope)
  (setf (js-Object-scope obj)
        (or scope
            (if js-current-context
                (js-Context-scope js-current-context)
              (error "No execution context")))))

(defsubst js-init-proto (obj &optional proto)
  (setf (js-Object-proto obj)
        (or proto (js-get-Object-prototype
                   (js-Object-scope obj)))))

(defun js-make-object (&optional scope proto)
  "Create a new JavaScript native Object.

SCOPE is the parent scope for this object, and must be a valid
JavaScript Object.  If omitted, it is set to the Global object from
the current execution context, which is the buffer-local value of
`js-current-context'.  If there is no current context, an error will
be signaled.

PROTO is the Prototype of this object.  If omitted, it will
default to Object.prototype, which is found by traversing the
scope chain from SCOPE to the root Global object."
  (let ((obj (make-js-Object)))
    (js-init-scope obj scope)
    (js-init-proto obj proto)
    obj))

(defalias 'js-object-p 'js-Object-p
  "Return t if OBJ appears to be a valid JavaScript native Object.
Return nil if OBJ is a JavaScript primitive or any other lisp type.")

(defalias 'js-prototype 'js-Object-proto
  "Return the prototype for OBJ.  Returns nil if the prototype is null.")

(defsubst js-class (obj)
  "Return the [[Class]] internal property of OBJ."
  ;; Stored on the cl-struct-js-Foo tag of the vector, for now.
  ;; Wanted a place to put it that was per-class, not per-instance.
  (get (aref obj 0) 'js-class))

(defsubst js-vtable (obj)
  "Return the table of internal method pointers for OBJ."
  (get (aref obj 0) 'js-vtable))

(defun js-printobj (obj)
  "Debugging function - print JavaScript object OBJ.
Return value is a string representation of OBJ suitable for
display in a JavaScript debugging console."
  (let ((s
         (cond
          ;; don't want dependency on js-exec, but need js-reference-p:
          ((and (vectorp obj)
                (eq (aref obj 0) 'js-ref-tag))
           ;; An internal reference is [js-ref-tag BASE NAME NODE].
           ;; BASE is often the global object.  Would be nice to have
           ;; smarter printing of the BASE object.
           (format "%s" (aref obj 2)))
          (t
           (js-to-source obj)))))
    (if (string= s "[object Unknown]")
        (setq s (format "%s" obj)))
    (if (> (length s) 80)
        (concat (substring s 0 77) "...")
      s)))

;;; ECMA 8.6.2 -- default implementations of Internal methods.

(defsubst js-call-internal (method obj &rest args)
  "Call an internal method on OBJ, passing ARGS.
METHOD must be a valid slot accessor for `js-Internals' structs.
OBJ must be a `js-Object', and ARGS are the arguments to the method."
  ;; Functions can have `call' or `construct' instance slots that
  ;; override their vtable entry.
  (cond
   ((and (eq method 'js-Internal-call)
         (js-Function-p obj)
         (js-Function-call-slot obj))
    (funcall (js-Function-call-slot obj) obj args))
   ((and (eq method 'js-Internal-construct)
         (js-Function-p obj)
         (js-Function-construct-slot obj))
    (funcall (js-Function-construct-slot obj) obj args))
   (t
    (apply (funcall method (js-vtable obj)) obj args))))

(defsubst js-get (obj name)
  "Utility for calling the [[Get]] method on OBJ for property NAME.
NAME can be any object, and will be converted to a string by the
actual method definition.  Returns 'undefined if NAME is not found
on OBJ or its prototype chain."
  (cond
   ((js-null-p obj)
    'undefined)
   ;; The __proto__ and __parent__ virtual properties can't go through
   ;; the normal lookup chain.  Rhino handles them by creating a subclass
   ;; of Ref called SpecialRef, created in the IRFactory when the parser
   ;; determines that the name is "__proto__" or "__parent__".  This is
   ;; probably a good long-term solution for us, but since elisp has no
   ;; virtual methods or polymorphic dispatch, we'll have to build our
   ;; own for references.  For now, we just detect special names here
   ;; and in js-put.
   ((equal name "__proto__")
    (js-Object-proto obj))
   ((equal name "__parent__")
    (js-Object-scope obj))
   (t
    (js-call-internal 'js-Internal-get obj name))))

;; `js-default--get--' recursively traverses up the prototype chain.
;; If it finds a getter, it calls it.  Unfortunately, we need to call
;; the getter on the original object, not the current object.  Example:
;; var x = {}; x.__proto__ => walks up to Object.prototype, finds the
;; getter for __proto__.  If it calls it on Object.prototype, it returns
;; null, since Object.prototype has no prototype.  Need to call on x!
;; Rhino handles this by requiring a 'start' parameter to get() as part
;; of the public API, which is pretty confusing.  Even Norris couldn't
;; remember why it was there.  A better solution is to tuck the start
;; object away in a thread-local (or in Emacs' case, buffer-local)
;; variable and check it on each recursion step.  This is that variable:
(defvar js-get-base-obj nil)
;(make-variable-buffer-local 'js-get-base-obj)

(defun js-default--get-- (obj name)
  "Lisp-level [[Get]] implementation for Object, 8.6.2.1.
OBJ must be a js-Object, and NAME must be a lisp string or lisp symbol.
Returns the value of the property named NAME, searching the prototype
chain.  If not found, returns the symbol `undefined'."
  (let ((cell (js-get-prop-cell obj name))
        (js-get-base-obj (or js-get-base-obj obj))
        getter result)
    (cond
     ((null cell)
      ;; recurse up prototype chain to look for result
      (setq result (js-get (js-prototype obj) name))
      (if (neq result 'undefined)
          result
        ;; Rhino seems to permit calling certain Object builtins even
        ;; if the target object's __proto__ link is null.  For these
        ;; we'll fetch the value from Object.prototype.
        (if (and (null (js-Object-proto obj))
                 (member name '("toSource" "toString"
                                "__lookupGetter__"
                                "__lookupSetter__"
                                "__defineGetter__"
                                "__defineSetter__"
                                "__defineProperty__"))) ; others?
            (js-global-get name)
          'undefined)))
     ((js-getter-setter-p cell)
      (if (setq getter (caar cell))
          (js-Function-call getter js-get-base-obj nil)
        'undefined))
     (t
      (first cell)))))

(defsubst js-can-put (obj name)
  "Utility for calling the [[CanPut]] method on OBJ, a js-Object.
NAME can be any object, and will be converted to a string by the
actual method implementation, e.g. `js-Object--can-put--'."
  (if (js-null-p obj)
      nil
    (js-call-internal 'js-Internal-can-put obj name)))

(defun js-default--can-put-- (obj name)
  "Lisp-level implementation of [[CanPut]] for Object, 8.6.2.2.
OBJ must be a js-Object, and NAME must be a lisp string or symbol.
Return t if a `js-put' of NAME on OBJ will succeed, else nil.
Note that this method returns Lisp t/nil, not JavaScript true/false.
Host and Native objects that override it should do the same."
  (let ((cell (js-get-prop-cell obj name))
        proto)
    (if cell
        (not (js-prop-read-only-p cell))
      (setq proto (js-prototype obj))
      (if (null proto)
          t  ; no read-only version in prototype chain => t
        (js-can-put proto name)))))

(defsubst js-put (obj name value)
  "Utility for calling the [[Put]] method on OBJ.
NAME can be any object, and will be converted to a string by the
actual method implementation, e.g. `js-Object--put--'.  VALUE must be
a valid JavaScript value to associate with NAME in OBJ."
  (cond
   ((js-null-p obj)
    (js-type-error (js-format "%s has no properties" obj)))
   ;; See comments about __proto__/__parent__ in `js-put'.
   ((equal name "__proto__")
    (js-Object-setProto obj value))
   ((equal name "__parent__")
    (js-Object-setParent obj value))
   (t
    (js-call-internal 'js-Internal-put obj name value))))

(defun js-default--put-- (obj name value)
  "Lisp-level [[Put]] implementation for Object, 8.6.2.2.
Tries to set local property NAME to value VALUE on object OBJ.

If OBJ has a setter function for NAME, the setter is invoked with
VALUE.  If OBJ has a getter for NAME but no setter, an error is
signaled.  Otherwise, the property is set in the object's property
list.

Excepting setter invocations, the property always goes into the
local plist for OBJ, even if NAME is defined somewhere up the
prototype chain.  Has no effect if the property is declared
read-only, including up the prototype chain.  NAME must be a
symbol or string."
  (let ((setter (js-Object-lookupSetter obj (list name))))
    (if setter
        (js-Function-call setter obj (list value))
      (if (js-can-put obj name)
          (js-define obj name value 'keep-flags)))))

(defsubst js-has-property (obj name)
  "Utility for calling the [[HasProperty]] method on OBJ.
NAME can be any object, and will be converted to a string by the
actual method implementation, e.g. `js-default--has-property--'.
This method returns Lisp t/nil rather than JavaScript true/false."
  (if (js-null-p obj)
      nil
    (js-call-internal 'js-Internal-has-property obj name)))

(defun js-default--has-property-- (obj name)
  "Lisp-level [[HasProperty]] implementation for Object, 8.6.2.4.
If OBJ or its prototype chain has property named NAME, return t,
Otherwise return nil.  OBJ must be a js-Object, and NAME must be a
lisp string or symbol."
  ;; As long as the object has an entry for that property, even if the
  ;; value is undefined, the object is considered to have the property.
  (if (js-get-prop-cell obj name)
      t
    (js-has-property (js-prototype obj) name)))

(defsubst js-delete (obj name)
  "Utility to call [[Delete]] method for OBJ.
If NAME is deleted (or not found), returns the symbol `true',
else returns the symbol `false'.  Native and Host objects that
override this method should also return JavaScript true/false values."
  (if (js-null-p obj)
      'false
    (js-call-internal 'js-Internal-delete obj name)))

(defun js-default--delete-- (obj name)
  "Lisp-level [[Delete]] implementation for Object, 8.6.2.5.

OBJ must be a js-Object, and NAME must be a lisp string or symbol.
This method tries to delete the property named NAME from OBJ.

Note that the [[Delete]] method does not search the prototype chain.
If the property has the `permanent' flag set, or OBJ has no property
named NAME, returns 'false.  Otherwise deletes it and returns 'true."
  (js-prop-delete obj name))

(defsubst js-default-value (obj &optional hint)
  "Utility for calling the [[DefaultValue]] method on OBJ.
Always returns a JavaScript primitive value, or throws a TypeError.
HINT is a symbol such as 'String or 'Number."
  (if (null obj)
      (error "nil is not a JavaScript object")  ; coding error
    (js-call-internal 'js-Internal-default-value obj hint)))

(defun js-default--default-value-- (obj &optional hint)
  "Lisp-level implementation of [[DefaultValue]] on Object, 8.6.2.6.
Always returns a primitive value for the object.  Host objects that
implement their own default-value method must also return primitives.
If provided, hint must be the symbol 'String or 'Number."
  (if (null hint)
      (setq hint (if (js-Date-p obj)
                     'String
                   'Number)))
  (let ((method1 (if (eq hint 'Number) "valueOf" "toString"))
        (method2 (if (eq hint 'Number) "toString" "valueOf"))
        val)
    (loop for m in (list method1 method2)
          for fun = (js-get obj m)
          if (and (js-Function-p fun)
                  (js-primitive-p
                   (setq val (js-Function-call fun obj nil))))
          return val
          finally do
          (js-type-error
           (js-format "No default value for %s, hint %s" obj hint)))))

(defun js-default--construct-- (ctor args)
  "Lisp-level implementation of [[Call]] for non-Function objects."
  (js-type-error (js-format "%s is not a constructor" ctor)))

(defun js-default--call-- (obj args)
  "Lisp-level implementation of [[Call]] for non-Function objects."
  (js-type-error (js-format "%s is not a function" obj)))

(defun js-default--has-instance-- (obj value)
  "Lisp-level implementation of [[HasInstance]] on Object, 11.8.6.
Always throws a TypeError.  This method is overridden by js-Function."
  (js-type-error
   (js-format "%s is not a valid 'instanceof' operand" obj)))

(defsubst js-instanceof (obj ctor)
  "Return t if CTOR [[HasInstance]] OBJ, else nil.  ECMA 11.8.6."
  (if (js-object-p ctor)
      (js-call-internal 'js-Internal-has-instance ctor obj)
    (js-type-error
     (js-format "invalid 'instanceof' operand: %s" ctor))))

(defsubst js-in (val obj)
  "Return 'true if VAL is in OBJ, else 'false.  ECMA 11.8.7."
  (unless (js-object-p obj)
    (js-type-error
     (js-format "invalid 'in' operand: %s" obj)))
  (js-bool (js-has-property obj (js-to-string val))))

(defun js-property-is-enumerable (obj v)
  "Return t if OBJ has a local enumerable property named V, else nil.
This method does not consider objects in the prototype chain."
  (if (or (null obj) (null v))
      nil
    (let ((cell (js-get-prop-cell obj (js-to-string v))))
      (if (null cell)
          nil
        (not (js-prop-dont-enum-p cell))))))

;;; 9 -- Type Conversion (plus a few type predicates)

(defconst js-NaN-string (format "%f" 0.0e+NaN))
(defconst js-pos-INF-string (format "%f" 1.0e+INF))
(defconst js-neg-INF-string (format "%f" -1.0e+INF))

(defconst js-nonbroken-NaN-p (eql 0.0e+NaN 0.0e+NaN)
  "Some C libraries don't let NaN compare `eql' to itself.")

(defun js-NaN-p (val)
  "Return t if VAL is the Number/number NaN (but not +/- Infinity).
VAL can be any lisp object."
  (if (js-Number-p val)
      (setq val (js-Number-value val)))
  (if js-nonbroken-NaN-p
      (eql val 0.0e+NaN)
    (and (eql val 1.0e+INF)     ; fast reject for all but NaN/Infinity
         (string= (format "%f" val)     ; distinguish from 1.0e+INF
                  js-NaN-string))))

(defun js-infinity-p (val)
  "Return t if VAL is the Number/number Infinity (not -Infinity/NaN).
VAL can be any lisp object."
  (if (js-Number-p val)
      (setq val (js-Number-value val)))
  (if js-nonbroken-NaN-p
      (eql val 1.0e+INF)
    (and (eql val 1.0e+INF)     ; fast reject for all but NaN/Infinity
         (string= (format "%f" val)     ; distinguish from 0.0e+NaN
                  js-pos-INF-string))))

(defun js-minus-infinity-p (val)
  "Return t if VAL is the Number/number -Infinity (not Infinity/NaN).
VAL can be any lisp object."
  (if (js-Number-p val)
      (setq val (js-Number-value val)))
  (if js-nonbroken-NaN-p
      (eql val -1.0e+INF)
    (and (eql -1.0e+INF val)
         (string= (format "%f" val) js-neg-INF-string))))
                  
(defun js-infinite-p (val)
  "Return t if VAL is the Number/number +/-Infinity (but not NaN).
VAL can be any lisp object."
  (if (js-Number-p val)
      (setq val (js-Number-value val)))
  (and (or (eql val -1.0e+INF)
           (eql val 1.0e+INF))  ; fast reject for most values
       (or js-nonbroken-NaN-p
           (not (string= (format "%f" val)
                         js-NaN-string)))))

(defun js-finite-p (val)
  "Return t if VAL is a finite Number/number (not +/-Infinity/NaN).
VAL can be any lisp object."
  (if (js-Number-p val)
      (setq val (js-Number-value val)))
  (and (numberp val)
       (not (member val '(1.0e+INF -1.0e+INF 0.0e+NaN)))))

(defun js-Nanfinity-p (val)
  "Return t if VAL is the Number/number NaN or +/- Infinity.
VAL can be any lisp object."
  (and (numberp val)
       (not (js-finite-p val))))

(defun js-integer-p (val)
  "Return t if VAL is an integral Number or primitive number.
VAL can be any lisp object.  Returns nil for Nan or +/-Infinity."
  (if (js-Number-p val)
      (setq val (js-Number-value val)))
  (or (integerp val)
      (and (floatp val)
           (not (js-Nanfinity-p val)) ; or `ffloor' will barf
           (= val (ffloor val)))))

(defun js-typeof (val)
  "Implementation of the typeof operator.  ECMA 11.4.3.
Returns a string representing the type of VAL."
  (cond
   ((memq val '(undefined nil))
    "undefined")
   ((eq val 'null)
    "object")
   ((js-Function-p val)
    "function")
   ((js-object-p val)
    "object")
   ((js-boolean-p val)
    "boolean")
   ((js-number-p val)
    "number")
   ((js-string-p val)
    "string")
   (t
    ;; implementation-dependent, needs EmacsConnect spec
    (format "[Elisp: %s]" val))))

(defsubst js-to-primitive (value &optional preferred-type)
  "Operator to convert VALUE (an Object) to a non-Object type.
If an object is capable of converting to more than one primitive type,
it may use the optional PREFERRED-TYPE to favor that type."
  (if (js-primitive-p value)
      value
    (js-default-value value preferred-type)))

(defun js-to-boolean (val)
  "The ToBoolean operator (9.2) converts VAL to type Boolean.
Returns either 'true or 'false."
  (cond
   ((memq val '(nil undefined null false))
    'false)
   ((memq val '(t true)) 'true)
   ((js-Boolean-p val)
    (js-bool (js-Boolean-value val)))
   ((numberp val)
    (if (js-Number-p val)
        (setq val (js-Number-value val)))
    (js-bool (not (or (zerop val)
                      (js-NaN-p val)))))
   ((stringp val)
    (if (js-String-p val)
        (setq val (js-String-value val)))
    (js-bool (not (zerop (length val)))))
   ((js-object-p val)
    'true)
   (t
    'true)))  ; punt on host objects and native elisp thingies

(defsubst js-not (val)
  "Convert 'true to 'false and vice-versa."
  (if (eq val 'true)
      'false
    'true))

(defsubst js-test (val)
  "Convert a JavaScript boolean value to a Lisp boolean value.
Returns t if `js-to-boolean' of VAL is true, else nil."
  (eq 'true (js-to-primitive (js-to-boolean val))))

(defsubst js-and (x y)
  "Return 'true or 'false based on converting X and Y to boolean"
  (let ((a (js-to-primitive (js-to-boolean x)))
        (b (js-to-primitive (js-to-boolean y))))
    (js-bool (and (eq a 'true) (eq b 'true)))))

(defsubst js-or (x y)
  "Return 'true or 'false based on converting X and Y to boolean"
  (let ((a (js-to-primitive (js-to-boolean x)))
        (b (js-to-primitive (js-to-boolean y))))
    (js-bool (or (eq a 'true) (eq b 'true)))))

(defsubst js-xor (x y)
  "Return 'true or 'false based on converting X and Y to boolean"
  (let ((a (js-to-primitive (js-to-boolean x)))
        (b (js-to-primitive (js-to-boolean y))))
    (js-bool (neq a b))))

(defun js-format-number (num)
  "Format NUM, a primitive number, for script output.
Returns a string that represents the number in JavaScript.  For
instance, turns integral floats into ints, and Lisp NaN/infinity
representations into NaN and Infinity."
  (cond
   ((js-Number-p num)
    (js-format-number (js-Number-value num)))
   ((not (numberp num))
    "NaN")
   ((js-NaN-p num)
    "NaN")
   ((js-infinity-p num)
    "Infinity")
   ((js-minus-infinity-p num)
    "-Infinity")
   ((js-integer-p num)
    (let ((s (number-to-string num)))
      (if (string-match "\\(.+\\)\\.0+" s)
          (match-string 1 s)
        s)))
   (t
    (number-to-string num))))

(defun js-parse-number (val)
  "Return VAL parsed as a Number.  VAL is a lisp string.
Follows the rules in 9.3.1 of Ecma-262.  Returns a primitive
number value."
  ;; Uses Lisp parsing and some heuristics.  Needs work.
  (cond
   ((string= val "NaN")
    0.0e+NaN)
   ((string= val "Infinity")
    1.0e+INF)
   ((string-match "^0[xX]\\(.+\\)" val)
    (string-to-number (match-string 1 val) 16))
   (t
    (string-to-number val))))

(defun js-to-number (val)
  "The ToNumber operator (9.3) converts its argument to type Number.
Returns a lisp numeric representation."
  (cond
   ((eq val 'undefined) 0.0e+NaN)
   ((memq val '(false null)) 0.0)
   ((eq val 'true) 1)
   ((js-boolean-p val)
    (if (memq (js-Boolean-value val) '(t true))
        1.0
      0.0))
   ((numberp val)
    val)
   ((js-Number-p val)
    (js-Number-value val))
   ((stringp val)
    (js-parse-number val))
   ((js-String-p val)
    (js-parse-number (js-String-value val)))
   ((js-object-p val)
    (js-to-number (js-to-primitive val 'Number)))
   (t
    0.0e+NaN)))

(defun js-to-integer (val)
  "Convert VAL to an integral numeric value.
Implements the ToInteger operator from section 9.4.  Note that it
generally returns an integral float value, to obtain sufficient
precision."
  (let ((num (js-to-number val)))  ; always a lisp number
    (cond
     ((js-Nanfinity-p num) num)
     ((zerop num) 0.0)  ; this check must follow NaN check
     (t (* (if (minusp num) -1.0 1.0)
           (ffloor (abs num)))))))

(defun js-to-int32 (val)
  "Implement 9.5: ToInt32 operator.  Convert VAL to signed 32-bit int.
Actual lisp representation returned is a float."
  (let ((num (js-to-uint32 val)))
    (if (>= num (expt 2.0 31))
           (- num (expt 2.0 32))
      num)))

(defun js-to-uint32 (val)
  "Implement 9.6:  ToUint32 operator.  Convert VAL to uint32.
Conversion computes VAL modulo 2**32.  Actual lisp representation
returned is a float value."
  (let ((num (js-to-number val)))
    (if (or (zerop num) (js-Nanfinity-p num))
        0.0
      (mod (* (if (minusp num) -1.0 1.0)
              (ffloor (abs num)))
           (expt 2.0 32)))))

(defun js-to-uint16 (val)
  "Implement 9.7:  ToUint16 operator.  Convert VAL to uint16.
Conversion computes VAL modulo 2**16.  Actual lisp representation
returned is a float value."
  (let ((num (js-to-number val)))
    (if (or (zerop num) (js-Nanfinity-p num))
        0.0
      (mod (* (if (minusp num) -1.0 1.0)
              (ffloor (abs num)))
           (expt 2.0 16)))))

(defun js-to-string (val)
  "Ecma-262 9.8: ToString operator.  Convert VAL to type String.
Doesn't follow the rules exactly: uses elisp `number-to-string'
function for now, until I finish implementing it properly.
Returns a lisp string, not a JavaScript String object."
  (cond
   ((eq val 'null) "null")
   ((eq val 'undefined) "undefined")
   ((js-boolean-p val)
    (symbol-name (js-to-boolean val)))  ; "true"/"false"
   ((js-number-p val)
    (if (js-Number-p val)
        (setq val (js-Number-value val)))
    (cond
     ((js-NaN-p val) "NaN")
     ((js-infinity-p val) "Infinity")
     ((js-minus-infinity-p val) "-Infinity")
     ((zerop val) "0")
     (t
      (number-to-string val))))
   ((stringp val) val)
   ((symbolp val) (symbol-name val))
   ((js-String-p val)
    (js-String-value val))
   ((js-Object-p val)
    (js-to-string (js-to-primitive val)))
   (t
    (js-type-error
     (js-format "js-to-string: Invalid ECMA value type: %s" val)))))

(defun js-to-object (val &optional ref node)
  "Ecma-262 9.9:  ToObject operator.  Convert VAL to type Object.
If REF is a Reference, VAL should be `js-get-value' on REF,
otherwise VAL should be the same object as REF.  If passed, NODE
is the node whose execute result was REF."
  (cond
   ((js-Object-p val)
    val)
   ((js-boolean-p val)
    (js-Boolean--construct-- nil (list val)))
   ((js-number-p val)
    (js-Number--construct-- nil (list val)))
   ((stringp val)
    (js-String--construct-- nil (list val)))
   ((and (not (memq val '(undefined null)))
         (js-object-p val))
    val)
   (t
    (js-type-error
     (cond
      ((memq val '(undefined null))
       (format "%s has no properties" val))
      (t
       (js-format "%s (type %s) has no properties" ref
                  (js-typeof val)) node))))))

;;; 11 -- Expressions

(defun js-add-numbers (a b &optional subtract)
  "11.6.3 -- Additive operators (+, -) to Numbers.
A and B can be any JavaScript types, and are first converted
via `js-to-number'.  Optional SUBTRACT specifies to compute
the difference A - B.  Returns a lisp numeric representation."
  (let* ((x (js-to-number a))
         (y (if subtract
                (- (js-to-number b))
              (js-to-number b)))
         (inf+x (js-infinity-p x))
         (inf+y (js-infinity-p y))
         (inf-x (and (not inf+x) (js-minus-infinity-p x)))
         (inf-y (and (not inf+y) (js-minus-infinity-p y))))
    (cond
     ((or (js-NaN-p x) (js-NaN-p y))
      0.0e+NaN)
     ((or (and inf+x inf-y)
          (and inf-x inf+y))
      0.0e+NaN)
     ((or (and inf+x inf+y)
          (and inf-x inf-y))
      x)
     ((or inf+x inf-x) x)
     ((or inf+y inf-y) y)
     (t
      (+ x y)))))

(defun js-less-p (x y)
  "ECMA 11.8.5 -- Abstract Relational Comparison Algorithm.
Returns t if X < Y, nil if !(X < Y), or 'undefined if at least
one of X or Y is NaN.  X and Y can be any JavaScript value."
  (let ((px (js-to-primitive x 'Number))
        (py (js-to-primitive y 'Number))
        nx ny)
    (if (and (stringp px)
             (stringp py))
        ;; TODO:  check if Emacs string< follows 11.8.5
        (string< px py)
      (setq nx (js-to-number px)
            ny (js-to-number py))
      (cond
       ((js-NaN-p nx) 'undefined)
       ((js-NaN-p ny) 'undefined)
       ((= nx ny) nil)
       ((js-infinity-p nx) nil)
       ((js-infinity-p ny) t)
       ((js-minus-infinity-p ny) nil)
       ((js-minus-infinity-p nx) t)
       (t
        (< nx ny))))))

(defun js-equal-p (x y)
  "Abstract equality comparison algorithm (==).  ECMA 11.9.3.
Returns t if x == y, nil if x != y."
  (let ((tx (intern (js-typeof x)))
        (ty (intern (js-typeof y))))
    (if (eq tx ty)
        (case tx
          ((undefined null) t)
          (number
           (cond
            ((or (js-NaN-p x) (js-NaN-p y))
             nil)
            ((and (numberp x) (numberp y))
             (= x y))  ; correctly handles +/- 1.0e+INF
            (t nil)))  ; step 10 (unreachable?)
          (string
           (string= x y))
          (boolean
           (eq (js-to-boolean x)   ; convert to 'true / 'false
               (js-to-boolean y)))
          (t
           ;; new Number(3) != new Number(3)
           (or (and (js-null-p x) (js-null-p y))
               (eq x y)))) ; same lisp object reference is OK
      ;; cases where Type(x) != Type(y)
      (cond
       ((or (and (eq tx 'string) (eq ty 'number))
            (and (eq tx 'number) (eq ty 'string)))
        (condition-case nil
            (= (js-to-number x) (js-to-number y))
          (wrong-type-argument nil)))
       ((eq tx 'boolean)
        (js-equal-p (js-to-number x) y))
       ((eq ty 'boolean)
        (js-equal-p x (js-to-number y)))
       ((and (memq tx '(string number))
             (eq ty 'object))
        (js-equal-p x (js-to-primitive y)))
       ((and (eq tx 'object)
             (memq ty '(string number)))
        (js-equal-p (js-to-primitive x) y))
       (t
        nil)))))

(defun js-strict-equal-p (x y)
  "Abstract strict equality (===) comparison.  ECMA 11.9.6.
Returns t if items are ===, else nil."
  (let ((tx (intern (js-typeof x)))
        (ty (intern (js-typeof y))))
    (if (neq tx ty)
        nil
      (case tx
        (undefined t)
        (number
         (if (or (js-NaN-p x) (js-NaN-p y))
             nil
           (= x y)))
        (string
         (string= x y))
        (boolean
         (eq (js-to-boolean x)    ; convert to 'true / 'false
             (js-to-boolean y)))
        (t
         (eq x y))))))  ; must be same object

;; The three shift operators don't produce the same results as Rhino
;; when the result or operands are greater than most-positive-fixnum.
;; Need to come up with a way to make them broken in the same way.

(defsubst js-op-lsh (v1 v2)
  (let ((lhs (truncate (js-to-uint32 v1)))
        (rhs (truncate (js-to-uint32 v2))))
    (setq rhs (logand rhs #x1F))  ; mask all but low 5 bits
    (lsh lhs rhs)))

(defsubst js-op-rsh (v1 v2)
  (let ((lhs (truncate (js-to-uint32 v1)))
        (rhs (truncate (js-to-uint32 v2))))
    (setq rhs (logand rhs #x1F))
    (ash lhs (- rhs))))

(defsubst js-op-ursh (v1 v2)
  (let ((lhs (truncate (js-to-uint32 v1)))
        (rhs (truncate (js-to-uint32 v2))))
    (setq rhs (logand rhs #x1F))
    (lsh lhs (- rhs))))

(defsubst js-op-add (v1 v2)
  (let ((lhs (js-to-primitive v1))
        (rhs (js-to-primitive v2)))
    (if (or (js-string-p lhs) (js-string-p rhs))
        (concat (js-to-string lhs) (js-to-string rhs))
      (js-add-numbers lhs rhs))))

(defsubst js-op-sub (v1 v2)
  (let ((lhs (js-to-primitive v1))
        (rhs (js-to-primitive v2)))
    (js-add-numbers lhs rhs t))) ; t to subtract

(defsubst js-op-mul (v1 v2)
  (condition-case nil
      (* v1 v2)
    (wrong-type-argument "NaN")))
  
(defsubst js-op-div (v1 v2)
  (condition-case nil
      (/ v1 v2)
    (wrong-type-argument "NaN")))
  
(defsubst js-op-mod (v1 v2)
  (condition-case nil
      (mod v1 v2)
    (wrong-type-argument "NaN")))

;;; 10 -- Execution Contexts

(defstruct (js-Activation (:include js-Object))
  "Activation object for calling a JavaScript function.
The arguments object is defined as a DontDelete property of the object."
  function  ; js-Function
  args)     ; lisp list

(put 'cl-struct-js-Activation 'js-class 'Activation)
(put 'cl-struct-js-Activation 'js-vtable (make-js-Internal))

(defstruct (js-Arguments (:include js-Object))
  "The 'arguments' object for a JavaScript function call.
The `args' slot of a `js-Arguments' struct initially points to the
`args' slot (a lisp list) of its associated activation record.  Any
modification of its elements triggers creation of a copy."
  activation  ; js-Activation
  length      ; original arg-list length (immutable)
  modified-p  ; t if args has been modified (get/put/delete)
  args)       ; lisp list

(put 'cl-struct-js-Arguments 'js-class 'Arguments)
(put 'cl-struct-js-Arguments 'js-vtable
     (let ((vtable (make-js-Internal)))
       (setf (js-Internal-put vtable) 'js-Arguments--has-property--)
       (setf (js-Internal-put vtable) 'js-Arguments--get--)
       (setf (js-Internal-put vtable) 'js-Arguments--put--)
       (setf (js-Internal-put vtable) 'js-Arguments--delete--)
       vtable))

(defsubst js-var-scope (scope name)
  "Return scope in which NAME is defined, starting at SCOPE.
If NAME is not defined anywhere, returns the top-level scope."
  (unless (js-object-p scope)
    (error "%s is not a JavaScript Object" scope))
  (loop with s = scope
        for parent = (js-Object-scope s)
        until (or (null parent)
                  (js-has-own-property s name))
        do
        (setq s parent)
        finally return s))

(defsubst js-get-top-level (scope)
  (unless (js-object-p scope)
    (error "%s is not a JavaScript Object" scope))
  (let ((parent))
    (while (setq parent (js-Object-scope scope))
      (setq scope parent))
    scope))

(defun js-context (type scope)
  "Create a new Execution Context of type TYPE.
TYPE can be 'FUNCTION_CODE, 'EVAL_CODE, or 'GLOBAL_CODE.
SCOPE is the current value of 'this'.  The scope chain is set
to the topmost parent scope of SCOPE."
  (unless (memq type '(GLOBAL_CODE EVAL_CODE FUNCTION_CODE))
    (error "Invalid context type: %s" type))
  (make-js-Context :type type
                   :this scope
                   :scope (js-get-top-level scope)))

(defsubst js-function-param-count (funobj)
  "Return the number of formal parameters specified for FUNOBJ."
  (length (js-node-get (js-Function-node funobj) 'params)))

(defun js-activation-object (func scope args)
  "Create an Activation object for calling a JavaScript function.

FUNC is the function object being called.  Its AST node should have
a `params' property that is a lisp list of any formal parameter names.

SCOPE is the scope in which FUNC was defined, and becomes the parent
scope for the new activation object.

ARGS is a lisp list of the args to this invocation of the function."
  (let ((activation (make-js-Activation :function func
                                        :scope scope
                                        :args args))
        (tmp args))
    ;; define formal parameters in activation object
    (loop for prop in (js-node-kids
                       (js-node-get
                        (js-Function-node func) 'params)) do
          (js-define activation
                     (js-node-value prop)
                     (or (car-safe tmp)
                         'undefined)
                     t)  ; {DontDelete} - 10.2.3
          (setq tmp (cdr-safe tmp)))

    ;; don't init "arguments" property if a param/var has same name
    (unless (js-has-property activation "arguments")
      (js-define activation "arguments"
                 (js-arguments-object activation)
                 t))
    activation))

(defun js-arguments-object (activation)
  "10.1.8 -- create arguments object for a JavaScript function call.
ACTIVATION is the activation record for the call."
  (let* ((parent (js-Activation-scope activation))
         (args (js-Activation-args activation))
         (func (js-Activation-function activation))
         ;; Ecma-262 10.1.8 says the [[Prototype]] is Object.prototype.
         ;; The activation record's prototype is also Object.prototype.
         (arguments (make-js-Arguments :scope parent
                                       :args args
                                       :proto (js-prototype parent)))
         (count (length args)))
    (js-define arguments "callee" func nil nil t)  ; {DontEnum}

    ;; User-visible length prop, which they can do whatever with.
    ;; Since it's not an Array, it doesn't change the actual arguments.
    (js-define arguments "length" count nil nil t)

    ;; This has to be after "callee" and "length" have been created.
    (loop for i from 0
          for arg in args do
          (js-define arguments (number-to-string i) arg nil nil t))

    ;; internal length property, which never changes
    (setf (js-Arguments-length arguments) count)

    arguments)) ; return the new Arguments object

(defsubst js-arguments-deleted-p (value)
  (eq value 'js-NOT-FOUND))

(defsubst js-arguments-arg-deleted (a i)
  "Return t if arg at index I was deleted from arguments obj A."
  (js-arguments-deleted-p (nth i (js-Arguments-args a))))

(defsubst js-arguments-clone-args (a)
  "Copy the arguments args list if it has not yet been modified."
  (unless (js-Arguments-modified-p a)
    (setf (js-Arguments-args a) (copy-list (js-Arguments-args a))
          (js-Arguments-modified-p a) t)))

(defsubst js-arguments-index-p (a i)
  "Return t if index I is an index for the args list for arguments A."
  (and (integerp i)
       (plusp i)
       (< i (js-Arguments-length a))))

(defun js-Arguments--has-property-- (a name)
  "[[HasProperty]] for arguments object A, for property named NAME."
  (let ((i (js-to-number name)))
    (or (and (js-arguments-index-p a i)
             (not (js-arguments-arg-deleted a i)))
        ;; otherwise ask "superclass"
        (js-default--has-property-- a name))))

(defun js-Arguments--get-- (a name)
  "Implement arguments obj sharing props with activation object.
If NAME is a property name corresponding to an index between 0 and
the arg-list length, we do sharing, otherwise we delegate to our
superclass, Object."
  (let ((i (js-to-number name))
        v f argname activation)
    (cond
     ((js-arguments-index-p a i)
      (setq v (nth i (js-Arguments-args a)))
      (when (and (not (js-arguments-deleted-p v))
                 (js-shared-with-activation-p a i))
        (setq activation (js-Arguments-activation a)
              f (js-Activation-function activation)
              argname (js-function-param-or-var-name f i)
              v (js-get activation argname)))
      v)
     (t  ; else ask "superclass"
      (js-default--get-- a name)))))

(defun js-Arguments--put-- (a name v)
  "Implement arguments obj sharing props with activation object."
  (let ((i (js-to-number name)))
    (catch 'return
      (if (and (js-arguments-index-p a i)
               (not (js-arguments-arg-deleted a i)))
          (if (js-shared-with-activation-p a i)
              (let* ((activation (js-Arguments-activation a))
                     (argname (js-function-param-or-var-name
                               (js-Activation-function activation) i)))
                (js-call-internal 'js-Internal-put activation argname v)
                (throw 'return nil))
            (js-arguments-clone-args a)
            (setcar (nthcdr i (js-Arguments-args a)) v)
            (throw 'return 'nil))
        ;; else ask superclass
        (js-default--put-- a name v)))))

(defun js-Arguments--delete-- (a name)
  "Implement arguments obj sharing props with activation object."
  (let ((i (js-to-number name)))
    (if (and (js-arguments-index-p a i)
             (not (js-arguments-arg-deleted a i)))
        (progn
          (js-arguments-clone-args a)
          (setcar (nthcdr i (js-Arguments-args a)) 'js-NOT-FOUND))
      ;; otherwise call superclass
      (js-default--delete-- a name))))

(defun js-shared-with-activation-p (arguments index)
  (let* ((activation (js-Arguments-activation arguments))
         (func (js-Activation-function activation))
         (count (js-function-param-count func))
         argname)
    (if (< index count)
        ;; Make sure the arg is not hidden by a later arg with same name,
        ;; as hidden args are not shared with the activation object.
        (if (< index (1- count))
            (loop with argname = (js-function-param-or-var-name
                                  func index)
                  for i from (1+ index) below count
                  if (string= argname
                              (js-function-param-or-var-name func i))
                  return nil
                  finally return t)
          t)
      t)
    nil))

(defun js-function-param-or-var-name (funobj index)
  "Get parameter or variable name.
If INDEX is less than the number of formal parameters, return the name
of the corresponding parameter.  Otherwise return the name of the local
variable at INDEX, where locals are indexed starting after last param,
or nil if not found."
  (let* ((node (js-Function-node funobj))
         (params (js-node-get node 'params))
         (pcount (length params)))
    (if (< index pcount)
        (nth index params)
      (js-node-get (nth (- index pcount)
                         (js-node-get
                          (js-node-get node 'body)
                          'var-decls))
                    'name))))

;;; 13 -- Function Definition

(defun js-function-object (node scope)
  "ECMA-262 13.2 -- Create a JavaScript native Function Object.
NODE is the AST node, which contains the formal parameter list and
the function body.  SCOPE is the current scope chain."
  (let ((fobj (make-js-Function))
        (proto (js-make-object)))  ; user-visible "prototype" property
    (js-init-scope fobj scope)
    (js-init-proto fobj (js-get-Function-prototype scope))
    (setf (js-Function-node fobj) node)
    (js-define fobj "length" (length (js-node-get node 'params)) t t t)
    (js-define proto "constructor" fobj nil nil t)
    (js-define fobj "prototype" proto t)
    fobj))

(defun js-call-method (obj method &rest args)
  "Utility method for invoking a JavaScript method from elisp.

OBJ is the Object on which METHOD is defined.  METHOD is a string
naming a method on OBJ. ARGS are the arguments to the method.

This function does not work for invoking internal methods such
as [[Get]] or [[HasInstance]] - use `js-call-internal' for those."
  (let ((f (js-get obj method))
        slot)
    (if (not (js-null-p f))
        (js-Function-call f obj args)
      (js-type-error
       (js-format "%s is not a method on %s" method obj)))))

;; Don't change the parameter list - or at any rate, make sure it
;; follows the spec, since this is the eval() called by JS code.
(defun js-eval (obj args)
  "Evaluate S and return the result.  ECMA 15.1.2.1"
  (if (not (js-string-p (car args)))
      (car args)
    (let ((s (car args))
          (x js-current-context)
          (x2 (js-context 'EVAL_CODE obj)))
      (when x
        (setf (js-Context-this x2)   (js-Context-this x))
        (setf (js-Context-caller x2) (js-Context-caller x))
        (setf (js-Context-callee x2) (js-Context-callee x))
        (setf (js-Context-scope x2)  (js-Context-scope x)))

      ;; use dynamic binding to make a stack of contexts
      (let ((js-current-context x2)
            e result)
        (setq e
              (catch 'js-THROW
                (progn
                  (js-exec (js-parse-string s))
                  (setq result (js-Context-result x2))
                  nil)))
        (if e
            (progn
              (setf (js-Context-result x)
                    (js-Context-result x2))
              (throw 'js-THROW e))
          result)))))

(defun js-to-source (obj)
  "Return formatted JavaScript source for recreating OBJ.
If OBJ is an Object, invokes OBJ.toSource().  Also handles
primitives and lisp objects."
  (cond
   ((js-Object-p obj)
    (js-call-method obj "toSource" 'undefined))
   ((stringp obj)
    (concat "\"" (replace-regexp-in-string "\"" "\\\"" obj nil t)
            "\""))
   ((numberp obj)
    (let ((result (number-to-string obj)))
      (if (and (js-integer-p obj)
               (string-match "\\(.+?\\)\\.0$" result))
          (match-string 1 result)
        result)))
   ((memq obj '(undefined nil))
    "")
   ((eq obj 'false)
    "false")
   ((eq obj 'null)
    "null")
   ((memq obj '(t true))
    "true")
   ((symbolp obj)  ; could be a property name from exec'ing for..in
    (concat "\""
            (replace-regexp-in-string "\"" "\\\"" (symbol-name obj) nil t)
            "\""))
   (t
    "[object Unknown]")))

;;; 15.1.3 - URI handling

(defun js-decodeURI (obj args)
  "ECMA 15.1.3.1"
  (js-decode (js-to-string (car args)) t))

(defun js-decodeURIComponent (obj args)
  "ECMA 15.1.3.2"
  (js-decode (js-to-string (car args)) nil))

(defun js-encodeURI (obj args)
  "ECMA 15.1.3.3"
  (js-encode (js-to-string (car args)) t))

(defun js-encodeURIComponent (obj args)
  "ECMA 15.1.3.4"
  (js-encode (js-to-string (car args)) nil))

(defconst js-uri-decode-reserved
  (mapcar 'string-to-char (split-string ";/?:@&=+$,#" "" t))
  "uriReserved chars from ECMA-262 URI grammar")

(defconst js-hex-chars
  (apply #'vector
         (mapcar 'string-to-char
                 (split-string "0123456789ABCDEF" "" t)))
  "Vector of the chars 0-9 and a-z")

(defun js-encode-unescaped-p (c &optional full-uri)
  (or (and (<= ?A c) (<= c ?Z))
      (and (<= ?a c) (<= c ?z))
      (and (<= ?0 c) (<= c ?9))
      (memq c '(?- ?_ ?. ?! ?~ ?* ?' ?\( ?\)))
      (and full-uri (memq c js-uri-decode-reserved))))

(defun js-encode (s &optional full-uri)
  "ECMA-262, 15.1.3 -- hidden Encode function.  S is a lisp string."
  (let ((len (length s))
        V C2 result)
    (loop for k from 0
          for C across s do
          (if (js-encode-unescaped-p C full-uri)
              (push C result)
            (if (and (<= #xD800 C) (<= C #xDFFF))
                (js-uri-error (format "Invalid URI char '%s'" C)))
            (if (or (< C #xD800) (< #xDBFF C))
                (setq V (string C))
              (if (= (incf k) len)
                  (js-uri-error "Invalid URI"))
              (setq C2 (aref s k))
              (unless (and (<= #xDC00 C2) (<= C2 #xDFFF))
                (js-uri-error "Invalid URI"))
              (setq V (string (+ (lsh (- C #xD800) 10)
                                 (- C2 #xDC00)
                                 #x10000))))
            (loop for ch across (encode-coding-string V 'utf-8)
                  for d = (logand #xff ch) do
                  (push ?% result)
                  (push (aref js-hex-chars (lsh d -4)) result)
                  (push (aref js-hex-chars (logand d #xf)) result)))
          finally
          return (if result
                     (apply #'string (nreverse result))
                   ""))))

(defun js-decode (s &optional full-uri)
  "ECMA-262, 15.1.3 -- hidden Decode function. S is a lisp string.
This implementation is pretty lazy, and doesn't signal errors for most
malformed URI sequences, e.g. %xx where xx is not two hex digits.
FULL-URI is ignored, since I couldn't think of a case where it matters.
Even in Firefox I think you just have to know which one to call."
  (let (result encoded tmp)
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (while (re-search-forward "\\(.+?\\)\\(%[[:xdigit:]]\\{2\\}\\)+"
                                nil t)
        (unless (string= "" (match-string 1))
          (push (match-string 1) result))
        (goto-char (match-end 1))
        (while (looking-at "%\\([[:xdigit:]]\\{2\\}\\)")
            (push (string-to-number (match-string 1) 16) tmp)
            (goto-char (match-end 0)))
        (push (decode-coding-string
               (apply #'string (nreverse tmp)) 'utf-8) result)
        (setq tmp nil))
      (unless (eobp)
        (push (buffer-substring-no-properties (point) (point-max))
              result))
      (mapconcat 'identity (nreverse result) ""))))

;;; Built-in function support:
;;
;; Unlike function objects defined in JavaScript, built-in functions
;; like parseInt don't have AST nodes and don't use execution
;; contexts, activation objects, the scope chain, etc.  But they are
;; function objects (i.e., instances of Function), and they need a
;; [[Call]] internal property to be callable.
;;
;; So we define a `js-builtin-function' that produces a function object
;; that can call the native elisp implementation.

(defun js-define-builtin (obj name sym &optional no-wrap &rest flags)
  "Make a lisp function a callable JavaScript function on OBJ.

Creates a function object using `js-builtin-function' and defines
it as a property named NAME on OBJ.  SYM is an `fboundp' symbol
whose `symbol-function' is the lisp function to call.

NO-WRAP specifies that the object has the correct signature for
being called by the evaluator, namely (thisobj &rest args).

If SYM has a symbol-property `js-length', it will be set as the
length of the function object created.

NAME will be set as a symbol-property named `js-name' on SYM,
overwriting any existing `js-name' property on SYM.

The new function property is created with the default flags
{DontEnum}, unless flags PERMANENT, READ-ONLY and/or DONT-ENUM
are passed explicitly."
  (let ((numflags (length flags))
        dd ro de)
    (unless (js-object-p obj)
      (error "%s must be a JavaScript object" obj))
    (if (symbolp name)
        (setq name (symbol-name name)))
    (unless (stringp name)
      (error "%s is not a valid property name" name))
    (setq dd (if (plusp numflags)
                 (first flags)
               nil))
    (setq ro (if (> numflags 1)
                 (second flags)
               nil))
    (setq de (if (> numflags 2)
                 (third flags)
               t))
    (put sym 'js-name name)  ; do this before `js-builtin-function'
    (js-define obj name
               (js-builtin-function sym no-wrap)
               dd ro de)))

(defun js-builtin-function (func &optional no-wrap)
  "Create a function object for lisp function FUNC.

FUNC is any symbol that is `fboundp'.  An attempt is made to
determine its arity for setting the `length' property.  The arity
is currently defined as the number of required arguments, though
it would behoove JavaScript callers to check the actual function
documentation to see the actual parameter list.

If NO-WRAP is non-nil, then FUNC must have the proper signature for
being called directly by the evaluator:  (funobj thisobj args)

  FUNOBJ is the function object being called
  THISOBJ is 'this' for the call
  ARGS is the array of arguments

If FUNC has a symbol property named `js-length', its value will
become the `length' property of the returned function object.

If FUNC has a symbol property named `js-name', its value will
become the user-visible print name of the function (in JavaScript).

The object returned will not have a callable [[Construct]] function."
  (unless (fboundp func)
    (error "%s is not a callable lisp function" func))
  (unless (symbolp func)
    (error "%s is not an `fbound' symbol" func))
  (let ((fobj (make-js-Function :proto (js-get-Function-prototype)
                                :builtin-p t))
        fname)
    (js-define fobj "length" (if no-wrap
                                 (or (get func 'js-length) 0)
                               (js-native-function-arity func))
               t t t)

    (setf (js-Function-construct-slot fobj) nil) ; override inherited one
    (setf (js-Function-call-slot fobj) (if no-wrap
                                           func
                                         (js-elisp-wrapper func)))

    ;; record the name of the function we're wrapping, for toString()
    (if (setq fname (get func 'js-name))
        (put (js-Function-call-slot fobj) 'js-name fname))
    fobj))

(defun js-elisp-wrapper (func &optional intern)
  "Create a function that can invoke FUNC from the JavaScript evaluator.

FUNC is a symbol naming the elisp function for the wrapper to call.
The return value is a symbol whose function definition is a lambda
that takes arguments in the form expected by the JavaScript evaluator:

	(thisobj args)

where `args' is the list of arguments for the elisp function to call.
The thisobj argument is used for JavaScript functions, but is not
needed for elisp functions.
Example:

  (setq x (js-elisp-wrapper 'logior))
    => G58832   ; or some such generated name
  (funcall x nil '(1 2 4))
    => 7

You do not need to assign the result to a variable in order to call it:

  (funcall (js-elisp-wrapper 'mod) nil '(5 3))
    => 2

If INTERN is non-nil, the return value is an interned symbol whose name
is FUNC prefixed by `js-native--', which is defined as a function with
the JavaScript calling conventions.  Hence:

  (js-elisp-wrapper 'concat)
    => js-native--concat
  (js-native--concat t '(\"foo\" \"bar\"))
    => \"foobar\"

The first parameter is ignored and can be any value.  FUNC can be
any symbol whose definition is a normal function, a byte-compiled
function, or a subr.  E.g.:

  (js-elisp-wrapper '+ t)
    => js-native--+
  (js-native--+ nil '(1 2 3 4))
    => 10

`js-elisp-wrapper' will not currently work with special forms or
macros.  Hence, this will fail

  (funcall (js-elisp-wrapper 'and) nil '(t t))
    => error:  invalid-function #<subr and>

because `and' is a built-in special form that behaves like a macro and
cannot be invoked with `apply'.  This is an implementation restriction
that I will fix at some point.

Normally you do not call `js-elisp-wrapper', since it is typically only
useful for setting the __call__ or __construct__ property of a JS Object.
This happens automatically if you use `js-builtin-function', a utility
for producing JavaScript host objects that wrap elisp functions."
  (let ((sym (if intern
                 (intern (concat "js-native--" (symbol-name func)))
               (gensym))))
    (fset sym
          `(lambda (this args)
             (apply #',func this args)))
    sym))

(defun js-elisp-lambda (fun)
  "Takes FUN, a lambda (thisobj args), and makes it callable.
Returns a JavaScript Function object that calls the lambda."
  (let ((sym (gensym)))
    (fset sym fun)
    (js-builtin-function sym t)))

(defun js-native-function-arity (func)
  "Return an estimated value for the length property for FUNC.
The length property in JavaScript is a hint as to the number of
expected arguments, and is usually the count of named parameters.
To make it semantically similar, we try to count the number of
named parameters for the Lisp function, including optional and rest
parameters."
  (let ((defn (indirect-function func))
        result min max)
    (if (subrp defn)
        (progn
          (setq result (subr-arity defn)
                min (car-safe result)
                max (cdr-safe result))
          (cond
           ((numberp max)
            max)
           ((numberp min)
            (if (eq max 'many)
                (1+ min)     ; add one for &rest (`many')
              min))          ; `unevalled' is special form; guess min
           (t 0)))           ; punt, no idea
      ;; otherwise, `help-function-arglist' is a lifesaver
      (count-if-not (lambda (arg)
                      (= (aref (symbol-name arg) 0) ?&))
                    (help-function-arglist defn)))))

(defsubst js-el-name-to-js-name (sym)
  "Convert an emacs-lisp function name to a JavaScript identifier.
SYM is the symbol bound to the native function to invoke."
  ;; simplest approach for now
  (replace-regexp-in-string "[-]" "_" (symbol-name sym)))

(defun js-elisp-wrapper-tosrc (sym)
  "Quick-and-dirty toString for elisp function bound to SYM."
  (let ((arglist (help-function-arglist sym)))
    (concat "function "
            (js-el-name-to-js-name sym)
            (if (eq t arglist)
                "()"
              (prin1-to-string
               (remove-if (lambda (arg)
                            (= (aref (symbol-name arg) 0) ?&))
                          arglist)))
            " {\n  [native code]\n}")))

;;; ECMA 15 -- Native ECMAScript Objects

(defun js-create-interpreter ()
  "Utility function for bootstrapping a JavaScript interpreter.

It returns a `js-Context' object that should be set as the value
of `js-current-context' in the target buffer.  The context
created has a Global object created as the global scope for all
JavaScript executed in the buffer.  It is initialized with the
standard JavaScript runtime objects.

If BUF is specified (as an existing buffer or buffer name), the
new global context will be set as `js-current-context' for that
buffer.  An error will be signaled if `js-current-context' is
non-nil, to avoid accidentally overwriting an existing
interpreter in BUF."
  (let* ((global (js-make-global-object))
         (context (js-context 'GLOBAL_CODE global)))
    (let ((js-current-context context))
      (js-init-standard-objects global)
      (setf (js-Object-proto global)
            (js-get-Object-prototype global)))
    (setq js-current-context context)
    context))

(defstruct (js-Emacs (:include js-Object))
  "JavaScript prototype object for Emacs global objects.
This is where the host objects for Emacs are defined.")

(put 'cl-struct-js-Emacs 'js-class 'Emacs)
(put 'cl-struct-js-Emacs 'js-vtable (make-js-Internal))

(defun js-make-global-object (&optional object-proto)
  "ECMA 15.1 -- The Global Object.
Creates and returns an object suitable for use as the Global Object
in the execution of an ECMAScript program.  Does not initialize the
standard objects.  The `scope' slot is set to nil, since this is the
top-level scope.  The `proto' slot is set to an object that provides
access to a set of built-in Emacs host objects.

For bootstrapping reasons, you must pass the value of Object.prototype
as OBJECT-PROTO if you want this object to inherit indirectly from
Object.prototype.  If you omit OBJECT-PROTO, the returned object will
not have the standard methods such as `toString' or `hasOwnProperty'.
If you cannot pass in OBJECT-PROTO, you can always set it later by
setting the prototype of this object to Object.prototype:

  (setf (js-Object-proto my-global)
        (js-get-Object-prototype my-global))

where `my-global' is the object created by `js-make-global-object'."
  (let ((global (make-js-Emacs :scope nil)))
    (js-define global "emacs" global t t nil)
    (if object-proto
        (setf (js-Object-proto global) object-proto))
    global))

(defun js-find-scope (&optional scope-or-context)
  "Look in SCOPE-OR-CONTEXT for a scope (a JavaScript Object)."
  (cond
   ((null scope-or-context)
    (if js-current-context
        (js-Context-scope js-current-context)
      (error "No scope or execution context available")))
   ((js-Object-p scope-or-context)
    scope-or-context)
   ((js-Context-p scope-or-context)
    (or (js-Context-scope scope-or-context)
        (error "No 'scope property in context %x"
               scope-or-context)))
   (t
    (error "Not a scope or context: %s" scope-or-context))))

(defsubst js-global (&optional scope-or-context)
  "Return the current Global object, optionally from SCOPE-OR-CONTEXT.
SCOPE-OR-CONTEXT is either a js-Object (scope) or a js-Context.
If omitted, looks in `js-current-context'."
  (js-get-top-level (js-find-scope scope-or-context)))

(defun js-global-get (name &optional scope-or-context)
  "Fetch the value of NAME on the Global object in SCOPE-OR-CONTEXT
Returns 'undefined if not present"
  (let ((global (js-global scope-or-context)))
    (or (js-get global name)
        'undefined)))

(defun js-get-Object-prototype (&optional scope-or-context)
  "Returns the Object.prototype object from SCOPE-OR-CONTEXT.
If you pass an execution context, uses its 'scope property.
Follows the scope chain to the top global object to get at the
built-in Object constructor prototype.  If SCOPE-OR-CONTEXT is
nil, `js-current-context' must be active, or an error is signaled."
  (js-get
   (js-global-get "Object" scope-or-context)
   "prototype"))

(defun js-get-Function-prototype (&optional scope-or-context)
  "Returns the Function.prototype object from SCOPE-OR-CONTEXT.
Has same semantics as `js-get-Object-prototype'."
  (js-get
   (js-global-get "Function" scope-or-context)
   "prototype"))

(defun js-init-standard-objects (&optional scope)
  "Populates a top-level scope suitable for use as a Global object.
Installs the definitions of all the JavaScript built-in Objects.
SCOPE is an optional Object, created with `js-make-object', to be
populated.  Otherwise a fresh Object will be created."
  (let*
      (;; 15.2.3.1 - default [[Prototype]] for JavaScript objects.
       (Object-proto (make-js-Object)) ; also handles 15.2.4

       (Global (or scope (js-make-global-object Object-proto)))

       ;; 15.3.4 - [[Prototype]] for built-in functions/constructors
       (Function-proto (make-js-Function :builtin-p t)))

    (unless (js-object-p Global)
      (error "%s is not a JavaScript Object" Global))

    (flet ((jsprop (name value &optional dd ro de)
                     (js-define Global name value dd ro de))
           (jsfunc (name sym)
                   (js-define-builtin Global name sym
                                      t  ; no-wrap (correct signature)
                                      nil nil t)))  ; {DontEnum}

      ;; 15.2 - 15.11 -- the builtin class constructors
      (dolist (ctor (mapcar 'symbol-name
                            '(Object Function Array String Boolean
                              Number Math Date RegExp Error)))
        (jsprop ctor
                (funcall (intern (concat "js-init-native-" ctor))
                         Object-proto Function-proto)
                nil nil t))  ; {DontEnum}

      (js-init-native-errors Object-proto
                             Function-proto
                             (js-get (js-get Global "Error") "prototype")
                             Global)

      ;; 8.5 and 15.1.1 - global NaN, Infinity, undefined
      (jsprop "NaN" 0.0e+NaN t nil t)  ; {DontEnum, DontDelete} (15.1.1.1)
      (jsprop "Infinity" 1.0e+INF t nil t)
      (jsprop "undefined" 'undefined t nil t)

      ;; 15.1.2  Function properties of the Global Object
      (jsfunc "eval"       'js-eval)
      (jsfunc "parseInt"   'js-parseInt)
      (jsfunc "parseFloat" 'js-parseFloat)
      (jsfunc "isNaN"      'js-isNaN)
      (jsfunc "isFinite"   'js-isFinite)
      (jsfunc "encodeURI"  'js-encodeURI)
      (jsfunc "decodeURI"  'js-decodeURI)
      (jsfunc "encodeURIComponent" 'js-encodeURIComponent)
      (jsfunc "decodeURIComponent" 'js-decodeURIComponent))

    (js-add-host-objects Global)

    Global))

(defun js-parseInt (obj args)
  "ECMA 15.1.2.2."
  (js-parse-int (first args)
                (second args)))  ; radix

(defun js-parseFloat (obj args)
  "ECMA 15.1.2.3."
  (js-parse-float (car args)))

(defun js-isNaN (obj args)
  "Return 'true if VAL is NaN, else 'false."
  (js-bool (js-NaN-p (car args))))

(defun js-isFinite (obj args)
  "Return 'true if VAL is a finite Number or primitive number.
Returns false for non-numbers, +/- Infinity, and NaN."
  (let ((val (car args)))
    (js-bool (and (js-number-p val)
                  (not (js-Nanfinity-p val))))))

;; I'm a little iffy on the file organization and code structure
;; right now, but it'll get worked out eventually.

;; Set the default internal method implementations for all objects.
(put 'cl-struct-js-Object 'js-vtable (make-js-Internal))

;; Set the default internal method implementations for Functions.
;; They're the same as the defaults for all objects, except for [[Call]],
;; [[Construct]], and [[HasInstance]].
(let ((vtable (make-js-Internal)))
  (setf (js-Internal-call vtable) 'js-Function--call--)
  (setf (js-Internal-construct vtable) 'js-Function--construct--)
  (setf (js-Internal-has-instance vtable) 'js-Function--has-instance--)
  (put 'cl-struct-js-Function 'js-vtable vtable))

;; Override the standard [[Put]] method for Arrays.
(put 'cl-struct-js-Array 'js-vtable
     (let ((vtable (make-js-Internal)))
       (setf (js-Internal-put vtable) 'js-Array--put--) ; 15.2.3.1
       vtable))

;; Override the standard [[Get/Put]] methods for Strings to enable
;; Array indexing to work for reads (but not writes).  This doesn't
;; seem to be in ECMA, but everyone supports it.
(put 'cl-struct-js-String 'js-vtable
     (let ((vtable (make-js-Internal)))
       (setf (js-Internal-get vtable) 'js-String--get--)
       (setf (js-Internal-put vtable) 'js-String--put--)
       vtable))

(require 'js-native-object)   ; 15.2 -- Object
(require 'js-native-function) ; 15.3 -- Function
(require 'js-native-array)    ; 15.4 -- Array
(require 'js-native-string)   ; 15.5 -- String
(require 'js-native-boolean)  ; 15.6 -- Boolean
(require 'js-native-number)   ; 15.7 -- Number
(require 'js-native-math)     ; 15.8 -- Math
(require 'js-native-date)     ; 15.9 -- Date
(require 'js-native-regexp)   ; 15.10 -- RegExp
(require 'js-native-error)    ; 15.11 -- Error
(require 'js-host)

(provide 'js-runtime)

;;; js-runtime.el ends here
