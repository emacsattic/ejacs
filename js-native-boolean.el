;;; js-native-boolean:  implementation of JavaScript Boolean type

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

(defun js-init-native-Boolean (obj-proto func-proto)
  (let ((Boolean (make-js-Function :proto func-proto))
        (bool-proto (make-js-Boolean :proto obj-proto)))

    ;; 15.6 -- Boolean constructor
    (setf (js-Function-call-slot Boolean)
          'js-Boolean--call--)
    (setf (js-Function-construct-slot Boolean)
          'js-Boolean--construct--)
    (js-define Boolean "length" 1 t t t)                   ; 15.6.3
    (js-define Boolean "prototype" bool-proto t t t)       ; 15.6.3.1

    ;; 15.6.4 -- properties of the Boolean Prototype Object
    (js-define bool-proto "constructor" Boolean nil nil t) ; 15.6.4.1
    (js-define-builtin bool-proto "toString"               ; 15.6.4.2
                       'js-Boolean-toString
                       t  ; have callable signature
                       nil nil t)
    (js-define-builtin bool-proto "valueOf"                ; 15.6.4.3
                       'js-Boolean-valueOf
                       t  ; have callable signature
                       nil nil t)
    (js-define-builtin bool-proto "toSource"               ; extension
                       'js-Boolean-toSource
                       t  ; have callable signature
                       nil nil t)
    Boolean))

(defun js-Boolean--call-- (ctor args)
  "15.6.1.1 -- Boolean constructor called as a function"
  (js-to-boolean (first args)))

(defun js-Boolean--construct-- (&optional funobj args)
  "15.6.2.1 -- Boolean constructor called in `new' expression."
  (let* ((ctor (or funobj (js-global-get "Boolean")))
         (result (make-js-Boolean :proto (js-get ctor "prototype"))))
    (setf (js-Boolean-value result)
          (js-to-boolean (first args)))
    result))

(defun js-Boolean-toString (this args)
  "15.6.4.2 -- Boolean.prototype.toString"
  (unless (js-boolean-p this)
    (js-type-error
     "Boolean.prototype.toString called on incompatible object"))
  (if (memq (if (js-Boolean-p this)
                (js-Boolean-value this)
              this)
            '(t true))
      "true"
    "false"))

(defun js-Boolean-valueOf (this args)
  "15.6.4.3 -- Boolean.prototype.valueOf"
  (unless (js-boolean-p this)
    (js-type-error
     "Boolean.prototype.valueOf called on incompatible object"))
  (if (js-Boolean-p this)
      (js-Boolean-value this)
    this))

(defun js-Boolean-toSource (this args)
  "Return JavaScript source for generating THIS Boolean object."
  (concat "(new Boolean("
          (js-Boolean-toString this args)
          "))"))

(provide 'js-native-boolean)

;;; js-native-boolean.el ends here
