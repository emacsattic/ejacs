;;; js-obj-splay.el -- splay-tree implementation of JS Object type

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
;; Optimizing property storage is a tricky proposition.  Different
;; schemes yield varying results, depending on how properties are
;; distributed across objects.  When objects have only a handful of
;; properties, an association list is probably the ideal data
;; structure.  Objects can often have hundreds of properties (e.g. if
;; many functions are defined on a Prototype object), in which case a
;; hashing scheme probably yields the fastest access, but at the cost
;; of a large vector per object with >= 50% wasted space.  (Also,
;; Emacs' built-in hashtable class is expensive to create, requiring
;; 10x-15x more time than creating a small alist.)
;;
;; This package encapsulates creation of (and access to) JavaScript
;; objects for the `js-runtime' package, so we can experiment with
;; different allocation schemes.
;;
;; The current scheme is a splay tree, but it will take some profiling
;; to know for sure how it compares to custom hashing.
;;
;; The basic benefits of a splay tree are:
;;   - amortized log(N) access with no explicit tree balancing
;;   - amortized log(M) access if only M nodes are being accessed
;;     - implies O(1) access on repeated accesses to the same property
;;   - no consing except on insertion of a new node
;;   - good use of the heap:  many small nodes rather than a big block
;;   - inherently sorted (could help when accessing indexed props)
;;   - simple implementation
;;
;; The basic downsides of a splay tree are:
;;   - the splay operation is a high per-access constant factor
;;     - may outweigh any potential benefits relative to hashing
;;   - poor concurrency support:  iterators are affected by reads
;;     - Emacs is not terribly concurrent, but JS 2.0 may be
;;
;; It may make sense to support int values as keys, by providing a
;; custom splay-tree comparator function -- even if not for all
;; objects, it still might work well for Arrays.
;;
;; Due to circular dependencies with `js-runtime.el', this package
;; cannot be used by itself.  It's separate from `js-runtime' to make
;; it easier to keep the data structure details from leaking through
;; the property-list API.

;;; Code:

(require 'js-defs)
(require 'splay-tree)

(defsubst js-get-prop-cell (obj name)
  "Return the internal representation of a property and its flags.
OBJ is a js-Object, and NAME is a symbol or string.
The return value is a cons whose car is the value, and whose cdr
is the bitwise-OR'd flags, or 0 if none of the four flags are set.
Returns nil if the property does not exist on the object."
  (let ((sym (js-obj-key name))
        (plist (js-Object-props obj)))
    (and plist
         (cdr (splay-tree-find plist sym)))))

(defun js-define-prop (obj name val getter setter flags)
  "Common implementation for setting props, getters and setters.

This function will not invoke an existing setter; that check has to
happen at a higher level.

If FLAGS is the symbol 'keep-flags, the cell retains its original
flags, or present, else the flags are initialized to zero.

FLAGS should have `js-prop-getter' or `js-prop-setter' set if GETTER
or SETTER is non-nil, respectively."
  (let* ((tree (or (js-Object-props obj)
                   (setf (js-Object-props obj) (make-splay-tree))))
         (cell (splay-tree-find tree name))
         (new-flags (if (eq flags 'keep-flags)
                        (cdr cell)
                      flags)))
  (splay-tree-insert
   tree
   (js-obj-key name)
   (js-prop-new-cell-value val getter setter new-flags))
  'undefined))

(defsubst js-has-own-property (obj name)
  "Return non-nil if OBJ has local property NAME, else return nil.
NAME is a string or symbol.  When returning the result to JavaScript,
it must be converted to a boolean with `js-bool' or equivalent."
  (js-get-prop-cell obj name))

;; Note that we can use a tree iterator to retrieve the enumerable
;; property names without modifying the tree, since it's an atomic
;; operation.  The actual for..in code could modify the tree while
;; iterating, so it has to fetch each property separately.

(defun js-for-in (obj)
  "Returns a lisp list of the names of the enumerable props of OBJ.
Used by the for..in statement evaluator.  Returns the string names,
not symbols, because the runtime doesn't handle raw symbols as JS
values (except special ones like 'null and 'undefined)."
  (let ((result '()))
    ;; Don't use `splay-tree-filter', since it builds up an alist
    ;; of cells that we'd have to traverse again to extract the keys.
    (splay-tree-inorder-walk
     (js-Object-props obj)
     (function (lambda (key value)
                 (unless (js-prop-dont-enum-p value)
                   (push (symbol-name key) result)))))
    result))
    
(defun js-prop-delete (obj name)
  "Delete the (local) property from OBJ named NAME.
Implements the semantics of Ecma 8.6.2.5.  OBJ is a js-Object
whose property list was set by this package.  NAME is the
property key, normally a Lisp symbol or string."
  (let ((curval (js-get-prop-cell obj name)))  ; splays to root
    (cond
     ((null curval) 'true)
     ((js-prop-permanent-p curval) 'false)
     (t
      (splay-tree-delete (js-Object-props obj) name)
      'true))))

(defun js-visit-props (obj visitor)
  "Visit the property list of OBJ with VISITOR.
VISITOR is a function that takes (key value), and must NOT
modify the property list of OBJ while iterating."
  (let ((tree (js-Object-props obj)))
    (and tree
         (splay-tree-inorder-walk tree visitor))))

(defsubst js-prop-count (obj)
  "Return the number of properties in the plist of OBJ."
  (let ((plist (js-Object-props obj)))
    (if plist
        (splay-tree-size plist)
      0)))

(provide 'js-obj-splay)

;;; js-obj-splay.el ends here
