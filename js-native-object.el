;;; js-native-object:  implementation of JavaScript Object type

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

(defun js-init-native-Object (object-proto function-proto)
  "ECMA 15.2.2 -- initialize and return an Object Constructor object.
Also initializes OBJECT-PROTO, which is Object.prototype per 15.2.3.1.
FUNCTION-PROTO is the [[Prototype]] object for built-in functions and
constructors, per 15.3.3.1."
  (let ((Object (make-js-Function :proto function-proto))) ; 15.2.3

    ;; `js-op-fn' defines a built-in function on Object.prototype.
    ;; All the functions are no-wrap - they have the correct signature.
    (flet ((js-get-Function-prototype () function-proto) ; bootstrapping
           (js-op-fn (name sym)
                     (js-define-builtin object-proto name sym t)))
      ;; 15.2.1, 15.2.2
      (setf (js-Function-call-slot Object) 'js-Object--call--)
      (setf (js-Function-construct-slot Object) 'js-Object--construct--)

      ;; 15.2.3, 15.2.3.1
      (js-define Object "length" 1 t t t)
      (js-define Object "prototype" object-proto t t t)

      ;; 15.2.4.1
      (js-define object-proto "constructor" Object nil nil t)

      ;; 15.2.4.2 - 15.2.4.7
      (js-op-fn "toString" 'js-Object-toString)
      (js-op-fn "toLocaleString" 'js-Object-toLocaleString)
      (js-op-fn "valueOf" 'js-Object-valueOf)
      (js-op-fn "hasOwnProperty" 'js-Object-hasOwnProperty)
      (js-op-fn "isPrototypeOf" 'js-Object-isPrototypeOf)
      (js-op-fn "propertyIsEnumerable" 'js-Object-propertyIsEnumerable)

      ;; ECMA B.2
      (js-op-fn "escape" 'js-Object-escape)
      (js-op-fn "unescape" 'js-Object-unescape)

      ;; ECMA extensions
      (js-op-fn "toSource" 'js-Object-toSource)
      (js-op-fn "__defineGetter__" 'js-Object-defineGetter)
      (js-op-fn "__defineSetter__" 'js-Object-defineSetter)
      (js-op-fn "__lookupGetter__" 'js-Object-lookupGetter)
      (js-op-fn "__lookupSetter__" 'js-Object-lookupSetter)
      (js-op-fn "__defineProperty__" 'js-Object-defineProperty)

    Object)))

(defun js-Object--call-- (obj args)
  "ECMA 15.2.1.1 - Object Constructor called as a Function"
  (let ((value (first args)))
    (if (js-null-p value)
        (js-make-object)
      (js-to-object value))))

(defun js-Object--construct-- (ctor args)
  "ECMA 15.2.2.1 - Object Constructor called in a `new' expression"
  (let ((value (first args)))
    (cond
     ((js-null-p value)  ; argument not specified (or Null/Undefined)
      (js-make-object :proto (js-get ctor "prototype")))
     ((js-object-p value)
      value)
     ((or (js-string-p value)
          (js-boolean-p value)
          (js-number-p value))
      (js-to-object value))
     (t
      (error "Host objects not yet implemented: %s" value)))))

(defun js-Object-toSource (obj args)
  "ECMA extension - print formatted source code for OBJ"
  (let ((result '())
        (len (js-prop-count obj))
        (i 0))
    (push "{" result)
    (js-visit-props
     obj
     (lambda (name value)
       (unless (js-prop-dont-enum-p value)
         (push (if (symbolp name) (symbol-name name) name) result)
         (push ": " result)
         (push (js-to-source (car value)) result)
         (when (< (incf i) len)
           (push ", " result)))))
    (push "}" result)
    (mapconcat 'identity (nreverse result) "")))

(defun js-Object-toString (obj args)
  "ECMA 15.2.4.2 -- Object.prototype.toString"
  (concat "[object " (symbol-name (js-class obj)) "]"))

(defun js-Object-toLocaleString (obj args)
  "ECMA 15.4.2.3 -- Object.prototype.toLocaleString"
  (js-Object-toString obj args))

(defun js-Object-valueOf (obj args)
  "ECMA 15.2.4.4 -- Object.prototype.valueOf"
  obj)

(defun js-Object-hasOwnProperty (obj args)
  "ECMA 15.2.4.5 -- Object.prototype.hasOwnProperty"
  (let ((v (first args)))
    (if v
        (js-bool
         (js-has-own-property obj (js-to-string v)))
      'false)))

(defun js-Object-isPrototypeOf (obj args)
  "ECMA 15.2.4.6 -- Object.prototype.isPrototypeOf"
  (let ((v (first args)))
    (if (not (js-object-p v))
        'false
      (loop while v
            if (eq v obj) return 'true
            else do (setq v (js-prototype v))
            finally return 'false))))

(defun js-Object-propertyIsEnumerable (obj args)
  "ECMA 15.2.4.7 -- Object.prototype.propertyIsEnumerable"
  (js-bool (js-property-is-enumerable obj (first args))))

(defconst js-global-escape-chars
  (split-string
   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789@_+-./\""
   "" t)
  "69 nonblank characters that are not escaped by escape()")

(defun js-Object-escape (obj args)
  "ECMA B.2.1 -- escape (string)"
  (let ((s (js-to-string (first args)))
        result)
    (loop for c across s do
          (cond
           ((member (string c) js-global-escape-chars)
            (push (string c) result))
           ((< c 256)
            (push (format "%%%X" c) result))
           (t
            (push (format "%%u%.4X" c) result)))
          finally return (apply #'concat (nreverse result)))))

(defun js-Object-unescape (obj args)
  "ECMA B.2.2 -- unescape (string)"
  (let* ((s (first args))
         (first-escape (string-match "%" s))
         (len (length s))
         k start end c result)
    (if (null first-escape)
        s
      ;; start with result containing reversed chars up to first-escape
      (setq result
            (mapcar #'string-to-char
                    (nreverse
                     (split-string (substring s 0 first-escape) "" t))))
      (setq k first-escape)
      (while (< k len)
        (setq c (aref s k))
        (incf k)
        (when (and (= c ?%) (/= k len))
          (if (= (aref s k) ?u)
              (setq start (+ k 1)
                    end (+ k 5))
            (setq start k
                  end (+ k 2)))
          (when (<= end len)
            (loop with x = 0
                  for i from start below end do
                  (setq x (js-hex-digit-to-int (aref s i) x))
                  finally do
                  (if (>= x 0)
                      (setq c x
                            k end)))))
        (push c result))
      (apply #'string (nreverse result)))))

(defun js-Object-defineGetter (obj args)
  "Create a getter function on OBJ."
  (let ((name (first args))
        (getter (second args)))
    (unless (js-Function-p getter)
      (js-type-error "Invalid getter usage"))
    (js-define-getter obj name getter)))

(defun js-Object-defineSetter (obj args)
  "Create a setter function on OBJ."
  (let ((name (first args))
        (setter (second args)))
    (unless (js-Function-p setter)
      (js-type-error "Invalid setter usage"))
    (js-define-setter obj name setter)))

(defun js-Object-lookupGetter (obj args)
  "Find getter function on OBJ for specified name.."
  (let* ((name (js-to-string (first args)))
         (result (js-lookup-getter obj name))
         proto)
    (cond
     ((eq result 'undefined) nil)
     (result result)
     (t
      (if (setq proto (js-prototype obj))
          (js-Object-lookupGetter proto args)
        nil)))))

(defun js-Object-lookupSetter (obj args)
  "Find setter function on OBJ for specified name."
  (let* ((name (js-to-string (first args)))
         (result (js-lookup-setter obj name))
         proto)
    (cond
     ((eq result 'undefined) nil)
     (result result)
     (t
      (if (setq proto (js-prototype obj))
          (js-Object-lookupSetter proto args)
        nil)))))

(defun js-Object-defineProperty (obj args)
  "Set a property on OBJ, optionally including property flags.
ARGS is (NAME VALUE &optional PERMANENT READONLY DONTENUM)."
  (js-define obj
             (first args)
             (second args)
             (js-test (third args))
             (js-test (fourth args))
             (js-test (fifth args))))

(defun js-Object-setProto (obj proto)
  "ECMA extension for setting object internal prototype link."
  (setf (js-Object-proto obj)
        (if (js-null-p proto)
            nil
          (js-to-object proto))))

(defun js-Object-setParent (obj scope)
  "ECMA extension for setting __parent__ (i.e. [[Scope]])."
  (setf (js-Object-scope obj)
        (if (js-null-p scope)
            nil
          (js-to-object scope))))

(provide 'js-native-object)

;;; js-native-object.el ends here
