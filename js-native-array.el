;;; js-native-array:  implementation of JavaScript Array type

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

(defun js-init-native-Array (obj-proto func-proto)
  (let ((Array (make-js-Function :proto func-proto))
        (array-proto (make-js-Array :proto obj-proto))) ; 15.4.4

    ;; 15.4.3 (note that "length" here is required by ECMA spec)
    (js-define Array "length" 1 t t t)  ; function arity
    (js-define Array "prototype" array-proto t t t)   ; 15.4.3.1

    ;; 15.4.4 - properties of the Array Prototype object
    (js-define array-proto "length" 0 t t t)
    (js-define array-proto "constructor" Array nil nil t) ; 15.4.4.1

    ;; 15.4.1, 15.4.2 - call is same as construct
    (setf (js-Function-call-slot Array) 'js-Array--construct--)
    (setf (js-Function-construct-slot Array) 'js-Array--construct--)

    (flet ((array-builtin (name)
             (js-define-builtin
              array-proto name
              (intern (concat "js-Array-" name))
              t ; have callable signature
              nil nil t)))  ; {DontEnum}
      (array-builtin "toString")       ; 15.4.4.2
      (array-builtin "toLocaleString") ; 15.4.4.3
      (array-builtin "concat")         ; 15.4.4.4
      (array-builtin "every")          ; JavaScript 1.6
      (array-builtin "indexOf")        ; JavaScript 1.6
      (array-builtin "filter")         ; JavaScript 1.6
      (array-builtin "forEach")        ; JavaScript 1.6
      (array-builtin "join")           ; 15.4.4.5
      (array-builtin "lastIndexOf")    ; JavaScript 1.6
      (array-builtin "map")            ; JavaScript 1.6
      (array-builtin "pop")            ; 15.4.4.6
      (array-builtin "push")           ; 15.4.4.7
      (array-builtin "reverse")        ; 15.4.4.8
      (array-builtin "shift")          ; 15.4.4.9
      (array-builtin "slice")          ; 15.4.4.10
      (array-builtin "some")           ; JavaScript 1.6
      (array-builtin "sort")           ; 15.4.4.11
      (array-builtin "splice")         ; 15.4.4.12
      (array-builtin "toSource")       ; extension
      (array-builtin "unshift"))       ; 15.4.4.13

    ;; Change the default arity on some of the methods, per Ecma-262.
    (js-put (js-get array-proto "concat") "length" 1)  ; 15.4.4.4
    (js-put (js-get array-proto "join") "length" 1)    ; 15.4.4.5
    (js-put (js-get array-proto "push") "length" 1)    ; 15.4.4.7
    (js-put (js-get array-proto "slice") "length" 2)   ; 15.4.4.10
    (js-put (js-get array-proto "splice") "length" 2)  ; 15.4.4.12
    (js-put (js-get array-proto "unshift") "length" 1) ; 15.4.4.13

    Array))

;; Array utilities

(defsubst js-make-Array (&rest args)
  "Utility to create a new Array using ARGS.
Invokes the built-in Array constructor function to create the Array."
  (js-Array--construct-- nil args))

(defsubst js-length (obj)
  "Return the length prop of OBJ, which need not be an array.
The length is returned as a number.  If the length property does not
exist, or is not a positive integral value, returns 0."
  (let ((num (js-parse-int (js-get obj "length"))))
    (if (and (js-integer-p num) (plusp num))
        num
      0)))

(defun js-Array-mapcar (func array)
  "Utility to call FUNC on every element of ARRAY.
FUNC is a lisp function that takes an Array element as its only
argument.  Returns the result as a lisp list."
  (loop for i from 0 below (js-length array)
        for item = (js-get array (number-to-string i))
        collect (funcall func item)))

(defsubst js-aref (array n)
  "Return the Nth element of ARRAY.  N can be a string or number."
  (if (numberp n)
      (setq n (number-to-string n)))
  (js-get array n))

(defsubst js-aset (array n elt)
  "Set the Nth element of ARRAY to ELT.  N is a string or number."
  (if (numberp n)
      (setq n (number-to-string n)))
  (js-put array n elt))

;; Array methods
;; TODO:  nail down semantics of array indices and re-check all these.

(defun js-Array--construct-- (&optional funobj args)
  "ECMA 15.4.2.1 and 15.4.2.2 -- Array Constructor.
ARRAY param is ignored.  ARGS is the arguments to new Array()."
  (let* ((ctor (or funobj (js-global-get "Array")))
         (obj (make-js-Array :proto (js-get ctor "prototype")))
         len)
    (if (/= 1 (length args))  ; includes nil/0 case
        ;; 15.4.2.1 - new Array ( [ item0 [ , item 1 ... ] ] )
        (loop for arg in args
              for i from 0 do
              (js-put obj (number-to-string i) arg)
              finally do
              (js-define obj "length" (length args) t nil t))
      (if (js-number-p (setq len (first args)))
          (if (= len (js-to-uint32 (first args)))
              (js-define obj "length" len t nil t)
            (js-range-error "Invalid array length: %s" (first args)))
        (js-define obj "length" 1 t nil 1)
        (js-put obj "0" (first args))))
    obj))

;; TODO:  handle +/- Infinity as array indices (as strings)
(defun js-Array--put-- (array &rest args)
  "ECMA 15.4.5.1 -- [[Put]] for Array objects"
  (let ((p (js-to-string (first args)))
        v i cell)
    (when (js-can-put array p)
      (setq v (second args))
      (cond
       ((string= p "length")
        (setq i (js-to-uint32 v))  ; can throw RangeError
        (unless (= i (js-to-number v))
          (js-range-error
           (js-format "Unable to set array length to %s" v)))
        ;; TODO: This is really expensive, and could be
        ;; improved by using a different internal data structure for
        ;; holding indexed properties of arrays.
        (loop for n from (1+ i) below (js-length array) do
              (if (setq cell (js-get-prop-cell
                              array (number-to-string n)))
                  (delete cell array)))
        ;; We have to truncate to int before assigning length, or exprs
        ;; like a[a.length-1] will look for a float index like a[1.0].
        ;; This loses precision for lengths of over most-positive-fixnum
        ;; (about 268 million on my box).  For true Ecma compliance we
        ;; need to support up to 32-bit lengths, but I doubt it'll become
        ;; an issue any time soon.
        (js-default--put-- array "length" (truncate i)))
       (t
        (js-default--put-- array p v)
        (setq i (js-parse-int p))
        (unless (or (js-NaN-p i)
                    (minusp i)
                    (< i (js-length array)))
          (js-default--put-- array "length" (1+ i))))))))

(defun js-Array-toString (array args)
  "ECMA 15.4.4.2 -- Array.prototype.toString"
  (unless (js-Array-p array)
    (js-type-error (js-format "%s is not an Array" array)))
  (js-Array-join array nil))

(defun js-Array-toLocaleString (array args)
  "ECMA 15.4.4.3 -- Array.prototype.toLocaleString"
  (unless (js-Array-p array)
    (js-type-error (js-format "%s is not an Array" array)))
  ;; TODO - can Emacs determine Locale-specific list separator?
  (let ((sep ","))
    (mapconcat (lambda (item)
                 (if (js-null-p item)
                     ""
                   (js-call-method (js-to-object item)
                                   "toLocaleString")))
               (js-Array-mapcar 'identity array)
               sep)))

(defun js-Array-concat (target args)
  "ECMA 15.4.4.4 -- Array.prototype.concat
TARGET and each member of ARGS are added to the result array.
If TARGET is an Array, its elements are added, else TARGET is added.
For each arg in ARGS, if it has a length property, its elements are
added, else the arg is added."
  (let ((result (js-make-Array))
        (Array (js-global-get "Array"))
        (index 0)
        (slot "0"))
    (dolist (obj (cons target args) result)
      (if (or (and (eq obj target)
                   (js-instanceof obj Array))      ; target is Array
              (and (neq obj target)
                   (js-object-p obj)
                   (js-has-property obj "length")))  ; arg has "length"
          (loop for i from 0 below (js-length obj)
                for n = (number-to-string i)
                for item = (js-get obj n) do
                (js-aset result slot item)
                (setq slot (number-to-string (incf index))))
        (js-aset result slot obj)
        (setq slot (number-to-string (incf index)))))))

(defun js-Array-join (array args)
  "ECMA 15.4.4.5 -- Array.prototype.join (separator).
ARRAY is the object this method is being invoked on.  ARGS is the
arguments.  Only the first argument (separator) is used.  If omitted,
a single comma is used as the separator."
  (let ((sep (if args (js-to-string (car args)) ",")))
    (mapconcat 'identity
               (js-Array-mapcar (lambda (item)
                                  (if (js-null-p item)
                                      ""
                                    (js-to-string item)))
                                array)
               sep)))

(defun js-Array-pop (array args)
  "ECMA 15.4.4.6 -- Array.prototype.pop()"
  (let* ((len (js-length array))              ; step 2
         (last (number-to-string (1- len))))  ; step 6
    (if (zerop len)                           ; step 3
        (progn
          (js-aset array "length" 0)          ; step 4
          'undefined)                         ; step 5
      (prog1                                  ; step 10
          (js-get array last)                 ; step 7
        (js-delete array last)                ; step 8
        (js-aset array "length" (1- len)))))) ; step 9

(defun js-Array-push (array args)
  "ECMA 15.4.4.7 -- append ARGS to ARRAY and return new length."
  (loop for n from (js-length array)
        for a in args do
        (js-aset array n a)
        finally do
        (js-put array "length" n)
        finally return n))

(defun js-Array-reverse (array ignored)
  "ECMA 15.4.4.8 -- reverse order of elements in ARRAY."
  (let* ((len (js-length array))
         (mid (floor (/ len 2))))
    (loop for i from 0 below mid
          for j = (- len i 1)
          for t1 = (js-aref array i)
          for t2 = (js-aref array j) do
          (js-aset array i t2)
          (js-aset array j t1)
          finally return array)))

(defun js-Array-shift (array ignored)
  "ECMA 15.4.4.9 -- remove and return first element of ARRAY."
  (let ((len (js-length array)))
    (if (zerop len)
        (progn
          (js-put array "length" 0)
          'undefined)
      (prog1
          (js-get array "0")
        (loop for k from 1 below len
              for temp = (js-aref array k) do
              (js-aset array (1- k) temp)
              finally do
              (js-put array "length" (1- len)))))))

(defsubst js-to-slice-index (value length)
  (if (minusp value)
      (if (minusp (+ value length))
          0
        (+ value length))
    (if (> value length)
        length
      value)))

(defun js-Array-slice (array args)
  "ECMA 15.4.4.10 -- Array.prototype.slice(start, end)"
  (let* ((result (js-make-Array))
         (len (js-length array))
         (beg (if (first args)
                    (js-to-integer (first args))
                  0))
         (end (if (second args)
                  (js-to-integer (second args))
                len)))
    (setq beg (js-to-slice-index beg len)
          end (js-to-slice-index end len))
    (loop for i from beg below end
          for tmp = (js-aref array i) do
          (js-aset result (- i beg) tmp))
    result))

(defun js-Array-sort (array args)
  "ECMA 15.4.4.11 -- Array.prototype.sort(opt_fn)"
  (let ((js-comparefn (first args))
        items)
    ;; elisp has no closures; using an inner function instead
    (defun js-array-sorter (o1 o2)
      (cond
       ;; cases where array didn't have the property
       ((and (null o1) (null o2)) nil)
       ((null o1) t)
       ((null o2) nil)

       ;; cases where the property value was undefined
       ((and (eq o1 'undefined) (eq o2 'undefined)) nil)
       ((eq o1 'undefined) t)
       ((eq o2 'undefined) nil)

       ;; convert user comparator (0, 1, -1) to t/nil
       (js-comparefn
        (plusp (js-Function-call js-comparefn nil (list o1 o2))))
       (t
        ;; lexicographic sort by default
        (string< (js-to-string o1)
                 (js-to-string o2)))))

    ;; move array elements into a lisp list
    (loop for i from 0 below (js-length array)
          for si = (js-to-string i)
          for item = (js-get array si) do
          (push item items)
          (js-delete array si))

    ;; call the built-in C function
    (setq items (sort items 'js-array-sorter))

    ;; copy sorted elements back in and return the array
    (loop for item in items
          for i from 0 do
          (js-aset array i item)
          finally return array)))

(defun js-Array-splice (array args)
  "ECMA 15.4.4.12 -- Array.prototype.splice()"
  (let* ((result (js-make-Array))
         (len (js-length array))
         beg end count delta)
    (if (null args)
        result
      ;; first arg => starting index
      (setq beg (js-to-slice-index (pop args) len))

      ;; second arg => count
      (setq count (if (null args)
                      (- len beg)
                    ;; 0 <= count <= (len - beg)
                    (min (max (js-to-integer (pop args))
                              0)
                         (- len beg)))
            end (+ beg count))

      ;; put any elements to remove into result
      (if (plusp count)
          (loop for last from beg below end
                for temp = (js-aref array last) do
                (js-aset result (- last beg) temp)))

      ;; find direction up/down for copy, and make room for argv
      (setq delta (- (length args) count))
      (if (plusp delta)
          (loop for last from (1- len) downto end
                for temp = (js-aref array last) do
                (js-aset array (+ last delta) temp))
        (if (minusp delta)
            (loop for last from end below len
                  for temp = (js-aref array last) do
                  (js-aset array (+ last delta) temp))))

      ;; copy from argv into the hole to complete the splice
      (loop for arg in args
            for i from beg do
            (js-aset array i arg))

      (js-put array "length" (+ len delta))
      result)))

(defun js-Array-unshift (array args)
  "ECMA 15.4.4.13 -- Array.prototype.unshift(item, ...)"
  (let ((len (js-length array))
        (argc (length args)))
    (if (zerop argc)
        len
      ;; slide up the elements to make room for new args
      (if (plusp len)
          (loop for last from (1- len) downto 0
                for tmp = (js-aref array last) do
                (js-aset array (+ last argc) tmp)))
      ;; copy in args to array bottom
      (loop for arg in args
            for i from 0 do
            (js-aset array i arg))
      ;; set and return new length
      (js-put array "length" (+ len argc))
      (+ len argc))))

;;; JavaScript 1.6 Array methods

(defun js-Array-indexOf (array args)
  "JavaScript 1.6 -- Array.indexOf(element, [fromIndex])
See http://tinyurl.com/qcaff for spec."
  (let ((len (js-length array))
        (item (first args))
        (beg (if (second args)
                  (js-to-number (second args))
                0))
        (continue t))
    (setq beg
          (if (minusp beg)
              (ceiling beg)
            (floor beg)))
    (if (minusp beg)
        (setq beg (+ beg len)))
    (loop for i from beg below len
          for value = (js-aref array i)
          if (and value
                  (js-strict-equal-p value item))
          return i
          finally return -1)))

(defun js-Array-lastIndexOf (array args)
  "JavaScript 1.6 -- Array.lastIndexOf(element, [fromIndex]).
See http://tinyurl.com/r33j9 for spec."
  (let ((len (js-length array))
        (item (first args))
        (beg (if (second args)
                 (js-to-number (second args))
               0.0e+NaN)))
    (if (js-NaN-p beg)
        (setq beg (1- len))
      (unless (integerp beg)
        (setq beg (if (minusp beg)
                       (ceiling beg)
                     (floor beg))))
      (if (minusp beg)
          (incf beg len)
        (if (>= beg len)
            (setq beg (1- len)))))
    (loop for i from beg above -1
          for value = (js-aref array i)
          if (and value
                  (js-strict-equal-p value item))
          return i
          finally return -1)))

(defsubst js--verify-callback (callback method)
  "Throw TypeError if CALLBACK is not a JavaScript function.
METHOD is the calling method name."
  (unless (string= (js-typeof callback) "function")
    (js-type-error
     (js-format "First arg to '%s' must be a function" method))))

(defsubst js--call-predicate (callback thisobj &rest args)
  "Call CALLBACK, a JavaScript callback, and convert result to t/nil."
  (js-test (js-Function-call callback thisobj args)))

(defun js-Array-every (array args)
  "JavaScript 1.6 -- Array.every(callback, [thisobj])
See http://tinyurl.com/3y39n5 for spec.  Returns a JavaScript boolean
value, either 'true or 'false."
  (let ((fun (first args))
        (this (second args)))
    (js--verify-callback fun "every")
    (loop for i from 0 below (js-length array)
          for item = (js-aref array i)
          if (and item
                  (not (js--call-predicate fun this item i array)))
          return 'false
          finally return 'true)))

(defun js-Array-filter (array args)
  "JavaScript 1.6 -- Array.filter(callback, [thisobj])
See http://tinyurl.com/3aoh3j for spec."
  (let ((fun (first args))
        (this (second args))
        (result (js-make-Array)))
    (js--verify-callback fun "filter")
    (loop for i from 0 below (js-length array)
          for value = (js-aref array i)
          if (and value
                  (js--call-predicate fun this value i array))
          collect value into yesses
          finally (funcall 'js-Array-push result yesses)
          finally return result)))

(defun js-Array-forEach (array args)
  "JavaScript 1.6 -- Array.forEach(callback, [thisobj])
See http://tinyurl.com/ds8lo for spec."
  (let ((fun (first args))
        (this (second args)))
    (js--verify-callback fun "forEach")
    (loop for i from 0 below (js-length array)
          for value = (js-aref array i)
          if value
          do (js-Function-call fun this (list value i array))
          finally return 'undefined)))

(defun js-Array-map (array args)
  "JavaScript 1.6 -- Array.map(callback, [thisobj])
See http://tinyurl.com/ypqjmj for spec."
  (let ((fun (first args))
        (this (second args))
        (result (js-make-Array)))
    (js--verify-callback fun "map")
    (loop for i from 0 below (js-length array)
          for item = (js-aref array i)
          if item
          collect (js-Function-call fun this (list item i array))
          into results
          finally (funcall 'js-Array-push result results)
          finally return result)))

(defun js-Array-some (array args)
  "JavaScript 1.6 -- Array.some(callback, [thisobj])
See http://tinyurl.com/3ddyom for spec."
  (let ((fun (first args))
        (this (second args)))
    (js--verify-callback fun "some")
    (loop for i from 0 below (js-length array)
          for item = (js-aref array i)
          if (and item
                  (js--call-predicate fun this item i array))
          return 'true
          finally return 'false)))

(defun js-Array-toSource (array args)
  "Return formatted source for ARRAY."
  (let ((result '()))
    (push "[" result)
    (loop with len = (js-length array)
          for i from 0 below len
          for item = (js-aref array i) do
          (push (js-to-source item) result)
          (if (< i (1- len))
              (push ", " result)))
    (push "]" result)
    (mapconcat 'identity (nreverse result) "")))

(provide 'js-native-array)

;;; js-native-array.el ends here
