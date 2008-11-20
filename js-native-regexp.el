;;; js-native-regexp:  implementation of JavaScript RegExp type

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

(require 'jsre)      ; regexp engine

(defun js-init-native-RegExp (obj-proto func-proto)
  (let ((RegExp (make-js-Function :proto func-proto))
        (re-proto (make-js-RegExp :proto obj-proto)))

    ;; 15.10.4 - RegExp constructor
    (setf (js-Function-call-slot RegExp) 'js-RegExp--call--)
    (setf (js-Function-construct-slot RegExp) 'js-RegExp--construct--)
    (js-define RegExp "length" 2 t t t)            ; 15.10.5
    (js-define RegExp "prototype" re-proto t t t)  ; 15.10.5.1

    ;; 15.10.6 -- Properties of RegExp prototype
    (js-define re-proto "constructor" RegExp)  ; enumerable!
    (js-define-builtin re-proto "exec" 'js-RegExp-exec t nil nil t)
    (js-define-builtin re-proto "test" 'js-RegExp-test t nil nil t)
    (js-define-builtin re-proto "toString"
                       'js-RegExp-toString t nil nil t)
    (js-define-builtin re-proto "toSource"
                       'js-RegExp-toSource t nil nil t)
    RegExp))

(defconst js-regex-global #x1)
(defconst js-regex-ignore-case #x2)
(defconst js-regex-multiline #x4)

(defun js-regex-global-p (regex)
  (plusp (logand (js-RegExp-flags regex) js-regex-global)))

(defun js-regex-ignore-case-p (regex)
  (plusp (logand (js-RegExp-flags regex) js-regex-ignore-case)))

(defun js-regex-multiline-p (regex)
  (plusp (logand (js-RegExp-flags regex) js-regex-multiline)))

(defun js-RegExp--call-- (ctor args)
  "15.10.3.1 -- RegExp constructor called as a function."
  (let ((pattern (first args))
        (flags (second args)))
    (if (and (js-RegExp-p pattern)
             (null flags))
        pattern
      (js-RegExp--construct-- ctor args))))

(defun js-RegExp--construct-- (&optional funobj args)
  "15.10.4.1 -- RegExp constructor called in a `new' expression."
  (let* ((ctor (or funobj (js-global-get "RegExp")))
         (regex (make-js-RegExp :proto (js-get ctor "prototype")))
         (pattern (first args))
         (flagspec (second args))
         (flags 0)
         g i m)
    (if (js-RegExp-p pattern)
        (if (null flags)
            (setq pattern (js-RegExp-pattern pattern)
                  flags (js-RegExp-flags pattern))
          (js-type-error
           (concat "Can't supply flags when constructing "
                   "one RegExp from another")))
      (setq pattern (if pattern (js-to-string pattern) "")
            flagspec (if flagspec (js-to-string flagspec) "")))
    (loop for c across flagspec do
          (case c
            (?g (if g
                    (js-syntax-error "duplicate flag: g")
                  (setq flags (logior js-regex-global flags)
                        g t)))
            (?i (if i
                    (js-syntax-error "duplicate flag: i")
                  (setq flags (logior js-regex-ignore-case flags)
                        i t)))
            (?m (if m
                    (js-syntax-error "duplicate flag: m")
                  (setq flags (logior js-regex-multiline flags)
                        m t)))
            (t
             (js-syntax-error (format "illegal flag: %c" c)))))
    (setf (js-RegExp-flags regex) flags
          (js-RegExp-pattern regex) pattern)
    (js-define regex "global" (js-bool g) t t t)
    (js-define regex "ignoreCase" (js-bool i) t t t)
    (js-define regex "multiline" (js-bool m) t t t)
    (js-define regex "lastIndex" 0 t nil t)
    (js-regex-compile regex)
    (js-define regex "source" (js-RegExp-pattern regex) t t t)
    regex))

(defun js-regex-compile (regex)
  "Compile the pattern for REGEX, a `js-RegExp' struct.
The pattern and flags fields should be set before calling.
The compiled `jsre-compiled-re' is written into REGEX."
  (let* ((re (jsre-compile (js-RegExp-pattern regex)
                           (js-RegExp-flags regex)))
         (elisp (jsre-compiled-re-elisp re)))
    (setf (js-RegExp-compiled regex) re)
    (when elisp
      (setf (js-RegExp-elisp regex) elisp)
      ;; Make elisp version accessible from JS code, e.g. /(foo)/.__elisp__
      (js-define regex "__elisp__" elisp nil nil t))))

(defun js-RegExp-exec (regex args)
  "15.10.6.2 -- RegExp.prototype.exec(string)
Returns an Array object containing the match results, or nil."
  (unless (js-RegExp-p regex)
    (js-type-error "Method 'exec' called on incompatible object."))
  (let ((s (js-to-string (or (first args) "")))
        (i (truncate (js-to-integer (js-get regex "lastIndex"))))
        (case-fold-search (js-regex-ignore-case-p regex))
        (result (js-Array--construct--))
        (pattern (js-RegExp-elisp regex)))
    (or pattern (error "Couldn't construct elisp regexp for %s"
                       (js-RegExp-pattern regex)))
    (unless (js-regex-global-p regex)
      (setq i 0))
    (if (or (minusp i)
            (> i (length s))
            (not (string-match pattern s i)))
        (progn
          (js-put regex "lastIndex" 0)
          nil)
      (save-match-data
        (if (js-regex-global-p regex)
            (js-put regex "lastIndex" (match-end 0)))
        (js-put result "index" (match-beginning 0))
        (js-put result "input" (match-string 0 s)))
      (loop for i from 0 below (/ (length (match-data)) 2)
            do (js-put result
                       (number-to-string i)
                       (match-string i s))
            finally do
            (js-put result "length" i)
            finally return result))))

(defun js-RegExp-test (regex args)
  "15.10.6.3 -- RegExp.prototype.test(string)."
  (js-bool (not (null (js-RegExp-exec regex args)))))

(defun js-RegExp-toString (regex args)
  "15.10.6.4 -- RegExp.prototype.toString"
  (unless (js-RegExp-p regex)
    (js-type-error "Method 'toString' called on incompatible object."))
  (concat "/" (js-RegExp-pattern regex) "/"
          (if (js-regex-global-p regex) "g" "")
          (if (js-regex-ignore-case-p regex) "i" "")
          (if (js-regex-multiline-p regex) "m" "")))

(defun js-RegExp-toSource (regex args)
  "Return JS source for REGEX."
  (js-RegExp-toString regex args))

(provide 'js-native-regexp)

;;; js-native-regexp.el ends here
