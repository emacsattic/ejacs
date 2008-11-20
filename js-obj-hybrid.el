;;; js-obj-hybrid.el -- alist implementation of JS Object type

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
;; This package implements the property list as an alist that converts
;; into a hashtable when the list grows large enough.

;;; Code:

(require 'js-defs)

(eval-when-compile
  (require 'cl))

(defconst js-obj-hybrid-conversion-size 30
  "Size at which to convert from an alist to a hashtable representation.
This value should be chosen in conjunction with
`js-obj-hybrid-initial-table-size'.")

;; 65 is the default size, according to the `make-hash-table' docs.
;; It seems like a reasonable choice for us.  The table will rehash when
;; we exceed 52 properties on the object.

(defconst js-obj-hybrid-initial-table-size 65
  "Initial size for `make-hash-table'.

The table will not rehash until the number of properties on an object
exceeds the product of this number and the :rehash-threshold for the table,
which defaults to 0.8.  That product should not be smaller than
`js-obj-hybrid-conversion-size' or the table will automatically rehash
when created.

The choice for this value can have a significant impact on the overall
speed and memory usage of the JavaScript interpreter.")

(defstruct js-plist
  "Container for an object property list represented as an alist."
  size
  props)  ; alist

(defsubst js-get-prop-cell (obj name)
  "Return the internal representation of a property and its flags.
OBJ is a js-Object, and NAME is a symbol or string.
The return value is a cons whose car is the value, and whose cdr
is the bitwise-OR'd flags, or 0 if none of the four flags are set.
Returns nil if the property does not exist on the object."
  (let ((plist (js-Object-props obj)))
    (cond
     ((null plist) nil)
     ((js-plist-p plist)
      (cdr (assq (js-obj-key name)
                 (js-plist-props plist))))
     (t
      (gethash (js-obj-key name) plist)))))

(defun js-define-prop (obj name val getter setter flags)
  "Common implementation for setting props, getters and setters.

This function will not invoke an existing setter; that check has to
happen at a higher level.

If FLAGS is the symbol 'keep-flags, the cell retains its original
flags, or present, else the flags are initialized to zero.

FLAGS should have `js-prop-getter' or `js-prop-setter' set if GETTER
or SETTER is non-nil, respectively."
  (let ((key (js-obj-key name))
        (plist (js-Object-props obj))  ; nil, alist, or hashtable
        cell hash
        (new-flags (if (eq flags 'keep-flags)
                       0
                     flags))) ; flags to use for new prop cell
    (cond
     ;; no properties yet
     ((null plist)
      (setf (js-Object-props obj)
            (make-js-plist :size 1
                           :props (list (js-prop-new-cell
                                         key val
                                         getter setter
                                         new-flags)))))
     ;; alist
     ((js-plist-p plist)
      (if (setq cell (cdr (assq key (js-plist-props plist))))
          ;; replace existing value, possibly inheriting flags
          (js-prop-update-cell cell val getter setter flags)
        ;; otherwise add a new prop cell
        (if (>= (js-plist-size plist) js-obj-hybrid-conversion-size)
            ;; threshold exceeded: convert to hashtable
            (progn
              (setq hash (make-hash-table
                          :size js-obj-hybrid-initial-table-size
                          :test 'eq))
              (dolist (pair (js-plist-props plist))
                (puthash (car pair) (cdr pair) hash))
              (puthash key
                       (js-prop-new-cell-value val getter setter new-flags)
                       hash)
              (setf (js-Object-props obj) hash)) ; plist will be gc'ed

          ;; else no conversion yet; prepend new value to alist
          (push (js-prop-new-cell key val getter setter new-flags)
                (js-plist-props plist))
          (incf (js-plist-size plist)))))

     ;; hashtable
     (t
      (if (setq cell (gethash key plist))
          (js-prop-update-cell cell val getter setter flags)
        (puthash key
                 (js-prop-new-cell-value val getter setter new-flags)
                 plist))))
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
  (let ((plist (js-Object-props obj)))
    (cond
     ((null plist)
      nil)
     ((js-plist-p plist)
      (loop for (name . value) in (js-plist-props plist)
            unless (js-prop-dont-enum-p value)
            collect (symbol-name name)))
     (t
      (loop for name being the hash-keys of plist
            using (hash-values value)
            unless (js-prop-dont-enum-p value)
            collect (symbol-name name))))))

(defun js-prop-delete (obj name)
  "Delete the (local) property from OBJ named NAME.
Implements the semantics of Ecma 8.6.2.5.  OBJ is a js-Object
whose property list was set by this package.  NAME is the
property key, normally a Lisp symbol or string."
  (let ((plist (js-Object-props obj))
        cell key)
    (cond
     ((null plist)
      'true)

     ;; alist
     ((js-plist-p plist)
      (setq cell (js-get-prop-cell obj name))
      (cond
       ((null cell) 'true)
       ((js-prop-permanent-p cell) 'false)
       (t
        (setf (js-plist-props plist)
              (delete-if (lambda (prop)
                           (string= (car prop) name))
                         (js-plist-props plist)))
        'true)))

     ;; hashtable
     (t
      (setq key (js-obj-key name)
            cell (gethash key plist))
      (cond
       ((null cell) 'true)
       ((js-prop-permanent-p cell) 'false)
       (t
        (remhash key plist)
        'true))))))

(defun js-visit-props (obj visitor)
  "Visit the property list of OBJ with VISITOR.
VISITOR is a function that takes (key value), where key is the
prop name as a string.  VISITOR must NOT modify the property list
of OBJ while iterating."
  (let ((plist (js-Object-props obj)))
    (cond
     ((null plist)
      nil)
     ((js-plist-p plist)
      (loop for (key . value) in (js-plist-props plist) do
        (funcall visitor (symbol-name key) value)))
     (t
      (maphash (lambda (k v)
                 (funcall visitor (symbol-name k) v))
               plist)))))

(defsubst js-prop-count (obj)
  "Return the number of properties in the plist of OBJ."
  (let ((plist (js-Object-props obj)))
    (cond
     ((null plist) nil)
     ((js-plist-p plist)
      (js-plist-size plist))
     (t
      (hash-table-count plist)))))

(provide 'js-obj-hybrid)

;;; js-obj-hybrid.el ends here
