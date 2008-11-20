;;; js-native-number:  implementation of JavaScript Number type

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

;; Establish random seed from current time and pid.
;; Done in a defvar so it only happens once per Emacs session.
(defvar js-random-seeded-p (not (not (random t))))

(defun js-init-native-Number (obj-proto func-proto)
  (let ((Number (make-js-Function :proto func-proto))
        (num-proto (make-js-Number :proto obj-proto)))

    ;; 15.7.1 -- Number constructor
    (setf (js-Function-call-slot Number)
          'js-Number--call--)
    (setf (js-Function-construct-slot Number)
          'js-Number--construct--)
    (js-define Number "length" 1 t t t)            ; 15.7.3
    (js-define Number "prototype" num-proto t t t) ; 15.7.3.1

    (cl-float-limits)  ; establish most/least-positive-float
    (js-define Number "MAX_VALUE" most-positive-float t t t);
    (js-define Number "MIN_VALUE" least-positive-float t t t);
    (js-define Number "NaN" 0.0e+NaN t t t)
    (js-define Number "POSITIVE_INFINITY" 1.0e+INF t t t)
    (js-define Number "NEGATIVE_INFINITY" -1.0e+INF t t t)

    ;; 15.7.4 -- Number Prototype Object
    (js-define num-proto "constructor" Number nil nil t)
    (dolist (name '("toString" "toLocaleString" "valueOf" "toSource"
                    "toFixed" "toExponential" "toPrecision"))
      (js-define-builtin num-proto name
                         (intern (concat "js-Number-" name))
                         t  ; callable signature
                         nil nil t))
    (dolist (name '("toExponential" "toFixed" "toPrecision"))
      (js-define (js-get num-proto name) "length" 1 t t t))

    Number))

(defun js-Number--call-- (ctor args)
  "15.7.1.1 -- Number constructor called as a function"
  (js-to-number (first args)))

(defun js-Number--construct-- (&optional funobj args)
  "15.7.2.1 -- Number constructor called in `new' expression."
  (let* ((ctor (or funobj (js-global-get "Number")))
         (result (make-js-Number :proto (js-get ctor "prototype"))))
    (setf (js-Number-value result)
          (js-to-number (first args)))
    result))

(defsubst js-check-number (thisobj method)
  (unless (js-number-p thisobj)
    (js-type-error
     (format "Number.prototype.%s called on incompatible object"
             method))))
     
(defun js-Number-toString (this args)
  "15.7.4.2 - Number.prototype.toString(radix)"
  (js-check-number this "toString")
  (let ((radix (car args))
        (value (if (js-Number-p this)
                   (js-Number-value this)
                 this)))
    (if (null radix)
        (setq radix 10)
      (unless (and (integerp radix)
                   (>= radix 2)
                   (<= radix 36))
        (js-eval-error
         (js-format "illegal radix %s" radix))))
    (if (= radix 10)
        (number-to-string value)
      (js-eval-error "only radix 10 is supported"))))

(defun js-Number-toLocaleString (this args)
  "15.7.4.3 - Number.prototype.toLocaleString(radix)"
  (js-Number-toString this args))

(defun js-Number-valueOf (this args)
  "15.7.4.3 - Number.prototype.valueOf(radix)"
  (js-check-number this "valueOf")
  (if (js-Number-p this)
      (js-Number-value this)
    this))

(defun js-Number-toFixed (this args)
  "15.7.4.5 - Number.prototype.valueOf(fractionDigits)"
  (js-check-number this "toFixed")
  (let ((x (js-Number-value this))
        (f (if (first args)
               (truncate (js-to-integer (first args)))
             0))
        result)
    (cond
     ((or (minusp f) (> f 20))
      (js-range-error
       (format "Precision %d out of range." f)))
     ((js-NaN-p x) "NaN")
     ((>= (abs x) 1e21)
      (js-Number-toString this args))  ; Ecma 9.8.1
     (t
      (setq result (format (format "%%.%df" f) x))))
    result))

(defun js-Number-toExponential (this args)
  "15.7.4.6 - Number.prototype.toExponential(fractionDigits)"
  (js-check-number this "toExponential")
  (let ((x (js-Number-value this))
        (fraction-digits (if (first args)
                             (truncate (js-to-integer (first args)))))
        tmp format-string result pair s m)

    ;; When precision is unspecified, Emacs defaults to a max precision
    ;; of 6, and SpiderMonkey/Rhino default to max 14.  Use a heuristic
    ;; to determine the right precision: the number of digits up to any
    ;; trailing zeroes.  We may need to trim more zeroes off later, due
    ;; to problems with Emacs' (format) on large floats with fractions.
    ;; E.g. (format "%f" 2473901162496889991098.0005), which formats as
    ;; "2473901162496890200000.000000".  But this heuristic covers most
    ;; common cases.
    (unless fraction-digits
      (setq tmp (format "%f" x))
      (when (string-match "^\\([0-9]+?\\)0*\\.0+$" tmp)
        (setq tmp (match-string 1 tmp)
              fraction-digits (min 14 (1- (length tmp))))))
    (cond
     ((js-NaN-p x) "NaN")
     ((js-infinity-p x) "Infinity")
     ((js-minus-infinity-p x) "-Infinity")
     ((and fraction-digits
           (or (minusp fraction-digits)
               (> fraction-digits 20)))
      (js-range-error
       (format "Precision %d out of range." fraction-digits)))
     (t
      (setq format-string (if fraction-digits
                              (format "%%.%de" fraction-digits)
                            "%e")
            result (format format-string x)
            pair (split-string result "e")
            s (car pair)
            m (cadr pair))

      (unless fraction-digits
        ;; if fraction digits are all zeroes, don't use decimal point
        (if (string-match "^\\([0-9]\\)\\.0*$" s)
            (setq s (match-string 1 s)))
        ;; remove trailing zeroes from fraction
        (if (string-match "^\\([0-9].[0-9]*?\\)0+$" s)
            (setq s (match-string 1 s))))

      ;; strip leading zeroes from exponent (but not all three)
      (if (string-match "^\\([+-]\\)00?\\([0-9]+\\)$" m)
          (setq m (concat (match-string 1 m) (match-string 2 m))))

      ;; Fix trailing zero problem when we computed the precision.
      ;; See comments above.
      (unless (first args)
        (if (string-match "^\\([^.]+\\.[0-9]+?\\)0+$" s)
            (setq s (match-string 1 s))))

      (concat s "e" m)))))

(defun js-Number-toPrecision (this args)
  "15.7.4.7 - Number.prototype.toPrecision(precision)"
  (js-check-number this "toPrecision")
  (let ((x (js-Number-value this))
        (p (if (first args)
               (truncate (js-to-integer (first args)))))
        tmp)
    (cond
     ((null p)
      (js-Number-toString this args))
     ((js-NaN-p x) "NaN")
     ((js-infinity-p x) "Infinity")
     ((js-minus-infinity-p x) "-Infinity")
     ((or (< p 1) (> p 21))
      (js-range-error
       (format "Precision %d out of range." p)))
     ((zerop x)
      (if (= p 1)
          "0"
        (concat "0." (make-string (1- p) "0"))))
     (t
      ;; This probably has edge cases that don't work quite right,
      ;; but it's accurate most of the time, afaict.
      (format (format "%%.%dg" p) x)))))

(defun js-Number-toSource (this args)
  "Return JavaScript source for generating THIS Number object."
  (js-check-number this "toSource")
  (concat "(new Number("
          (js-Number-toString this args)
          "))"))

(provide 'js-native-number)

;;; js-native-number.el ends here
