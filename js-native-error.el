;;; js-native-error:  implementation of JavaScript Error type

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

(require 'js-parse)  ; for `js-error-helper'

(defun js-init-native-Error (obj-proto func-proto)
  "ECMA 15.11 - initialize built-in native error constructors."
  (let ((Error (make-js-Function :proto func-proto))
        (error-proto (make-js-Error :proto obj-proto))) ; 15.11.4

    ;; 15.11.1 -- call is the same as construct
    (setf (js-Function-call-slot Error) 'js-Error--construct--)
    (setf (js-Function-construct-slot Error) 'js-Error--construct--)

    (js-define Error "length" 1 t t t) ; 15.11.3
    (js-define Error "prototype" error-proto t t t)  ; 15.11.3.1

    ;; 15.11.4 -- Properties of the Error Prototype Object
    (js-define error-proto "constructor" Error nil nil t) ; 15.11.4.1
    (js-define error-proto "name" "Error")                ; 15.11.4.2
    (js-define error-proto "message" "")                  ; 15.11.4.3
    (js-define-builtin error-proto "toString"             ; 15.11.4.4
                       'js-Error-toString 'no-wrap
                       t nil t)
    (js-define-builtin error-proto "toSource"             ; extension
                       'js-Error-toSource 'no-wrap
                       t nil t)
    Error))

(defun js-init-native-errors (obj-proto func-proto error-proto global)
  "ECMA 15.11.7 - NativeError constructors"
  (dolist (ctor '(EvalError RangeError ReferenceError
                  SyntaxError TypeError URIError))
    (let* ((ctor-obj (make-js-Function :proto func-proto))
           (ctor-name (symbol-name ctor))
           (ctor-call (intern (concat "js-" ctor-name "--construct--")))
           (proto (make-js-Error :proto error-proto))) ; 15.11.7.7

      (setf (js-Function-call-slot ctor-obj) ctor-call)
      (setf (js-Function-construct-slot ctor-obj) ctor-call)

      (setf (js-Object-proto ctor-obj) func-proto) ; 15.11.7.5
      (js-define ctor-obj "length" 1 t t t)        ; 15.11.7.5
      (js-define ctor-obj "prototype" proto t t t) ; 15.11.7.6

      (js-define global ctor-name ctor-obj nil nil t)  ; {DontEnum}

      (js-define proto "constructor" ctor-obj) ; 15.11.7.8
      (js-define proto "name" ctor-name)  ; 15.11.7.9
      (js-define proto "message" ""))))   ; 15.11.7.10

(defun js-Error--construct-- (ctor args)
  "ECMA 15.11.2 -- the Error constructor."
  (funcall 'js-construct-error "Error" args))

(defun js-EvalError--construct-- (ctor args)
  (funcall 'js-construct-error "EvalError" args))

(defun js-RangeError--construct-- (ctor args)
  (funcall 'js-construct-error "RangeError" args))

(defun js-ReferenceError--construct-- (ctor args)
  (funcall 'js-construct-error "ReferenceError" args))

(defun js-SyntaxError--construct-- (ctor args)
  (funcall 'js-construct-error "SyntaxError" args))

(defun js-TypeError--construct-- (ctor args)
  (funcall 'js-construct-error "TypeError" args))

(defun js-URIError--construct-- (ctor args)
  (funcall 'js-construct-error "URIError" args))

(defun js-construct-error (type &optional msg file line)
  (let ((e (funcall (intern (concat "make-js-" type)))))
    (setf (js-Object-proto e) (js-get
                               (js-global-get "Error")
                               "prototype"))  ; 15.11.2.1
    (if msg (js-define e "message" msg)) ; 15.11.2.1
    (if file (js-define e "file" file))  ; nonstandard
    (if line (js-define e "line" line))  ; nonstandard
    e))

(defun js-Error-toString (errobj args)
  "ECMA 15.11.4.4 -- implementation-defined string"
  (let ((msg (js-get errobj "message")))
    (if (and msg (not (string= msg "")))
        (concat (js-Error-name errobj) ": " msg)
      (js-Error-name errobj))))

(defun js-Error-toSource (errobj args)
  "ECMA extension - return source for creating ERROBJ."
  (let ((msg (js-get errobj "message")))
    (concat "(new " (js-Error-name errobj) "("
            (if msg (js-to-source msg) "")
            ", \"\"))")))

;;; Error utilities

(defun js-eval-error (&optional msg node lineno)
  "Signal a EvalError."
  (js-error-helper "EvalError" msg node lineno))

(defun js-type-error (&optional msg node lineno)
  "Signal a TypeError with MSG.
NODE, if specified, is the associated AST node, for line info."
  (js-error-helper "TypeError" msg node lineno))

(defun js-range-error (&optional msg node lineno)
  "Signal a RangeError with MSG in FILENAME."
  (js-error-helper "RangeError" msg node lineno))

(defun js-reference-error (&optional msg node lineno)
  "Signal a ReferenceError."
  (js-error-helper "ReferenceError" msg node lineno))

(defun js-syntax-error (&optional msg node lineno)
  "Signal a SyntaxError."
  (js-error-helper "SyntaxError" msg node lineno))

(defun js-uri-error (&optional msg node lineno)
  "Signal a URIError."
  (js-error-helper "URIError" msg node lineno))

(defun js-error-helper (type &optional msg node lineno)
  (let (file line)
    (setq line
          (cond
           (lineno lineno)
           ((js-node-p node)
            (js-node-get node 'line))
           (t nil)))
    (setq file (if (js-node-p node)
                   (js-node-buffer node)))
    (setf (js-Context-result js-current-context)
          (js-construct-error type msg file line))
    (throw 'js-THROW 'throw)))

(provide 'js-native-error)

;;; js-native-error.el ends here
