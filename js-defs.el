;;; js-defs.el -- shared code for js-obj backend implementations

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
;; A property cell has the following structure, regardless of the
;; data structure for holding the cells:
;;
;;  (name (value . flags))
;;
;; If the property is a getter or setter, it has this structure instead:
;;
;;  (name ((getter . setter) . flags))
;;
;; NAME is a symbol or a number.  Typically code passes in a string
;; for the property name, and it's converted to a symbol before being
;; used as the property key.
;;
;; FLAGS is always a fixnum, the bitwise-or of flags for the property.
;; The supported flags are defined at the top of this file, js-prop-*.
;;
;; VALUE can be any Lisp value.  If GETTER and/or SETTER is present,
;; they will be callable js-Function objects.

;;; Code:

(defconst js-prop-dont-enum #x1
  "Property attribute for non-enumerable properties.
Props with this flag are not enumerated with the for-in construct.")

(defconst js-prop-read-only #x2
  "Property flag that makes properties immutable.")

(defconst js-prop-permanent #x4
  "Property cannot be deleted.")

;; Not yet supported, but here to keep flag values same as SpiderMonkey's.
(defconst js-prop-exported #x8
  "Property is exported from object.")

(defconst js-prop-getter #x10
  "Property holds getter function.")

(defconst js-prop-setter #x20
  "Property holds setter function.")

(defconst js-getter-or-setter
  (logior js-prop-getter js-prop-setter))

(defsubst js-prop-read-only-p (cell)
  (plusp (logand (cdr cell) js-prop-read-only)))

(defsubst js-prop-permanent-p (cell)
  (plusp (logand (cdr cell) js-prop-permanent)))

(defsubst js-prop-dont-enum-p (cell)
  (plusp (logand (cdr cell) js-prop-dont-enum)))

(defsubst js-convert-flags (permanent read-only dont-enum)
  (logior (if permanent js-prop-permanent 0)
          (if read-only js-prop-read-only 0)
          (if dont-enum js-prop-dont-enum 0)))

(defsubst js-obj-key (name)
  "Convert NAME to a property key.
NAME is a symbol, string, or number.  Returns a symbol.
Numbers are self-quoting, so they already function as symbols."
  (cond
   ((symbolp name) name)
   ((numberp name) name)
   ((stringp name) (intern name))
   (t
    (error "Invalid property name: %s" name))))

(defsubst js-getter-setter-p (cell)
  "Return t if CELL has `js-prop-getter' or `js-prop-setter' flag set.
CELL is the result of calling `js-get-prop-cell' on a js-Object."
  (plusp (logand (cdr cell) js-getter-or-setter)))

(defun js-define (obj name val &optional permanent read-only dont-enum)
  "Set a property named NAME on a JS object OBJ to value VALUE.

This function will only set local properties on OBJ - the
function will never create or modify properties on the prototype
chain of OBJ.  Always sets the value, overwriting any previous
value and flags for the property.  If you pass the symbol `keep-flags'
as the PERMANENT parameter, all the flags will be inherited rather
than overwritten, if the property already exists.

Always returns the value 'undefined, since this is often in the tail
position in code that returns values to JavaScript."
  (js-define-prop obj name val nil nil
                  (if (eq permanent 'keep-flags)
                      'keep-flags
                    (js-convert-flags permanent read-only dont-enum))))

(defun js-define-getter (obj name getter
                             &optional permanent read-only dont-enum)
  "Define a GETTER function for property named NAME on OBJ."
  (js-define-prop obj name nil getter nil
                  (logior (js-convert-flags permanent read-only dont-enum)
                          js-prop-getter)))

(defun js-define-setter (obj name setter
                             &optional permanent read-only dont-enum)
  "Define a SETTER function for property named NAME on OBJ."
  (js-define-prop obj name nil nil setter
                  (logior (js-convert-flags permanent read-only dont-enum)
                          js-prop-setter)))

(defsubst js-lookup-getter-or-setter (obj name setterp)
  "Return the (local) getter/setter function on OBJ for NAME.
Does not search up the prototype chain for OBJ.

The return value is as follows:
  - a function if the getter/setter was found
  - `undefined' if the getter/setter definitely does not exist
  - nil if we might need to search up the prototype chain for OBJ

Return value is the symbol `undefined' when OBJ has a local property
cell for NAME, but it is either not a getter/setter pair, or the
specified slot is empty.

SETTERP is t to look up a setter; otherwise looks for a getter."
  (let ((cell (js-get-prop-cell obj name)))  ; (value . flags)
    (if (null cell)
        nil
      (or (and (js-getter-setter-p cell)  ; ((getter . setter) . flags)
               (if setterp
                   (cdar cell)
                 (caar cell)))
          'undefined))))

(defun js-lookup-getter (obj name)
  (js-lookup-getter-or-setter obj name nil))

(defun js-lookup-setter (obj name)
  (js-lookup-getter-or-setter obj name t))

(defsubst js-prop-new-cell-value (val getter setter flags)
  "Create the cdr of a new prop cell."
  (unless (numberp flags)
    (error "Flag value %s must be a number." flags))
  (cons (cond
         (getter
          (cons getter nil))
         (setter
          (cons nil setter))
         (t val))
        flags))

(defsubst js-prop-new-cell (sym val getter setter flags)
  "Create new prop cell named SYM with VAL, GETTER or SETTER and FLAGS."
  (cons sym (js-prop-new-cell-value val getter setter flags)))

(defsubst js-prop-update-cell (cell val getter setter flags)
  "Update existing prop CELL with VAL, GETTER or SETTER and FLAGS.
If FLAGS is the symbol 'keep-flags, the flags are unchanged."
  (let (was-gs gs-pair)
    ;; if new getter/setter, see if previous value was getter/setter
    (when (or getter setter)
      (setq was-gs (js-getter-setter-p cell)) ; w/ prev flags
      (if was-gs
          (setq gs-pair (car cell))))   ; (getter . setter)
    (cond
     (getter
      (if was-gs
          (setcar gs-pair getter)
        (setcar cell (cons getter nil))))
     (setter
      (if was-gs
          (setcdr gs-pair setter)
        (setcar cell (cons nil setter))))
     (t
      (setcar cell val)))
    ;; setting flags is the same for normal, getter, or setter
    (if (numberp flags)
      (setcdr cell flags))))

(provide 'js-defs)

;;; js-defs.el ends here
