;;; js-obj-alist.el -- alist implementation of JS Object type

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
;; All JavaScript data values (including functions) derive from the
;; base Object class, which provides a property list interface.
;;
;; This package implements the property list as a simple alist
;; whose keys are symbols (the interned property names).
;;
;; It might make sense to change it to use a struct containing
;; the alist, so it can keep track of the list size in a separate
;; field.

;;; Code:

(require 'js-defs)

(defsubst js-get-prop-cell (obj name)
  "Return the internal representation of a property and its flags.
OBJ is a js-Object, and NAME is a symbol or string.

The return value is a cons whose car is the value, and whose cdr
is the bitwise-OR'd flags, or 0 if none of the four flags are set.
Returns nil if the property does not exist on the object."
  (cdr (assq (js-obj-key name) (js-Object-props obj))))

(defun js-define-prop (obj name val getter setter flags)
  "Common implementation for setting props, getters and setters.

This function will not invoke an existing setter; that check has to
happen at a higher level.

If FLAGS is the symbol 'keep-flags, the cell retains its original
flags, or present, else the flags are initialized to zero.

FLAGS should have `js-prop-getter' or `js-prop-setter' set if GETTER
or SETTER is non-nil, respectively."
  (let* ((sym (js-obj-key name))
         ;; cell structure is (name (value . flags)),
         ;; or for getter/setter, (name ((getter . setter) . flags))
         (cell (cdr (assq sym (js-Object-props obj)))))  ; can be nil
    (if cell
        (js-prop-update-cell cell val getter setter flags)
      (push (js-prop-new-cell sym val getter setter
                              (if (numberp flags)
                                  flags
                                0))
            (js-Object-props obj)))
    'undefined))

(defsubst js-has-own-property (obj name)
  "Return non-nil if OBJ has local property NAME, else return nil.
NAME is a string or symbol.  When returning the result to JavaScript,
it must be converted to a boolean with `js-bool' or equivalent."
  (js-get-prop-cell obj name))

(defun js-for-in (obj)
  "Returns a lisp list of the names of the enumerable props of OBJ.
Used by the for..in statement evaluator.  Returns the string names,
not symbols, because the runtime doesn't handle raw symbols as JS
values (except special ones like 'null and 'undefined)."
  (loop for (name . value) in (js-Object-props obj)
        unless (js-prop-dont-enum-p value)
        collect (symbol-name name)))
    
(defun js-prop-delete (obj name)
  "Delete the (local) property from OBJ named NAME.
Implements the semantics of Ecma 8.6.2.5.  OBJ is a js-Object
whose property list was set by this package.  NAME is the
property key, normally a Lisp symbol or string."
  (let ((curval (js-get-prop-cell obj name)))
    (cond
     ((null curval) 'true)
     ((js-prop-permanent-p curval) 'false)
     (t
      ;; This delete-cell function needs to be pushed down a layer.
      (setf (js-Object-props obj)
            (delete-if (lambda (prop)
                         (string= (car prop) name))
                       (js-Object-props obj)))
      'true))))

(defun js-visit-props (obj visitor)
  "Visit the property list of OBJ with VISITOR.
VISITOR is a function that takes (key value), where key is the
prop name as a string.  VISITOR must NOT modify the property list
of OBJ while iterating."
  (loop for (key . value) in (js-Object-props obj) do
        (funcall visitor (symbol-name key) value)))

(defsubst js-prop-count (obj)
  "Return the number of properties in the plist of OBJ."
  (length (js-Object-props obj)))

(provide 'js-obj-alist)

;;; js-obj-alist.el ends here
