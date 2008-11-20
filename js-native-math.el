;;; js-native-math:  implementation of JavaScript Math type

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

(defun js-init-native-Math (obj-proto func-proto)
  (let ((Math (make-js-Math :proto obj-proto)))

    (js-define Math "E" e t t t)                ; 15.8.1.1
    (js-define Math "LN10" (log 10) t t t)      ; 15.8.1.2
    (js-define Math "LN2" (log 2) t t t)        ; 15.8.1.3
    (js-define Math "LOG2E" (log e 2) t t t)    ; 15.8.1.4
    (js-define Math "LOG10E" (log10 e) t t t)   ; 15.8.1.5
    (js-define Math "PI" pi t t t)              ; 15.8.1.6
    (js-define Math "SQRT1_2" (sqrt 0.5) t t t) ; 15.8.1.7
    (js-define Math "SQRT2" (sqrt 2) t t t)     ; 15.8.1.8

    (dolist (f '(abs acos asin atan atan2 ceil cos exp floor
                 log max min pow random round sin sqrt tan
                 toSource))
      (js-define-builtin Math (symbol-name f)
                         (intern (concat "js-Math-" (symbol-name f)))
                         t  ; proper signature
                         nil nil t))
    (js-define (js-get Math "max") "length" 2 t t t)
    (js-define (js-get Math "min") "length" 2 t t t)

    Math))

(defun js-Math-toSource (obj args)
  "toSource() implementation for Math builtin"
  "Math")

;; Emacs likes to throw errors where JavaScript returns NaN, so we
;; have to wrap all the math functions to handle the boundary cases.

(defun js-Math-abs (obj args)
  "15.8.2.1 -- abs(x)"
  (abs (js-to-number (car args))))  ; handles NaN/infinity properly

(defun js-Math-acos (obj args)
  "15.8.2.2 -- acos(x)"
  (let ((x (js-to-number (car args))))
    (cond
     ((js-NaN-p x) 0.0e+NaN)
     ((> x 1) 0.0e+NaN)
     ((< x -1) 0.0e+NaN)
     (t (acos x)))))

(defun js-Math-asin (obj args)
  "15.8.2.3 -- asin(x)"
  (let ((x (js-to-number (car args))))
    (cond
     ((js-NaN-p x) 0.0e+NaN)
     ((> x 1) 0.0e+NaN)
     ((< x -1) 0.0e+NaN)
     (t (asin x)))))

(defun js-Math-atan (obj args)
  "15.8.2.4 -- atan(x)"
  (let ((x (js-to-number (car args))))
    (if (js-NaN-p x)
        0.0e+NaN
      (atan x))))

(defun js-Math-atan2 (obj args)
  "15.8.2.5 -- atan2(x)"
  (let* ((y (js-to-number (first args)))
         (x (js-to-number (second args)))
         (py (plusp y))
         (px (plusp x))
         (my (minusp y))
         (mx (minusp x))
         (zy (zerop y))
         (zx (zerop x))
         (inf+y (js-infinity-p y))
         (inf+x (js-infinity-p x))
         (inf-y (and (not inf+y) (js-minus-infinity-p y)))
         (inf-x (and (not inf+x) (js-minus-infinity-p x)))
         (finy (not (or inf+y inf-y)))
         (finx (not (or inf+x inf-x))))
    (cond
     ((or (js-NaN-p y) (js-NaN-p x)) 0.0e+NaN)
     ((and py zx) (/ pi 2))
     ((and zy zx) 0)
     ((and zy mx) pi)
     ((and zy px) 0)
     ((and my zx) (/ pi -2))
     ((and py finy inf+x) 0)
     ((and py finy inf-x) pi)
     ((and my finy inf+x) 0)
     ((and my finy inf-x) (- pi))
     ((and inf+y finx) (/ pi 2))
     ((and inf-y finx) (/ pi -2))
     ((and inf+y inf+x) (/ pi 4))
     ((and inf+y inf-x) (/ (* 3 pi) 4))
     ((and inf-y inf+x) (/ pi -4))
     ((and inf-y inf-x) (/ (* 3 pi) -4))
     (t (atan (/ y x))))))

(defun js-Math-ceil (obj args)
  "15.8.2.6 -- ceil(x)"
  (let ((x (js-to-number (car args))))
    (if (js-Nanfinity-p x)
        x
      (fceiling x))))

(defun js-Math-cos (obj args)
  "15.8.2.7 -- cos(x)"
  (let ((x (js-to-number (car args))))
    (if (js-Nanfinity-p x)
        0.0e+NaN
      (cos x))))

(defun js-Math-exp (obj args)
  "15.8.2.8 -- exp(x)"
  (let ((x (js-to-number (car args))))
    (cond
     ((js-NaN-p x) 0.0e+NaN)
     ((js-infinity-p x) 1.0e+INF)
     ((js-minus-infinity-p x) 0)
     (t (exp x)))))

(defun js-Math-floor (obj args)
  "15.8.2.9 -- floor(x)"
  (let ((x (js-to-number (car args))))
    (if (js-Nanfinity-p x)
        x
      (ffloor x))))

(defun js-Math-log (obj args)
  "15.8.2.10 -- log(x)"
  (let ((x (js-to-number (car args))))
    (cond
     ((js-NaN-p x) 0.0e+NaN)
     ((minusp x) 0.0e+NaN)
     (t (log x)))))

(defun js-Math-max (obj args)
  "15.8.2.11 -- max(val1 [, val2 [, ...]])"
  (let ((values (mapcar 'js-to-number args)))
    (cond
     ((null values) -1.0e+INF)
     ((find-if 'js-NaN-p values) 0.0e+NaN)
     (t (apply 'max values)))))

(defun js-Math-min (obj args)
  "15.8.2.12 -- min(val1 [, val2 [, ...]])"
  (let ((values (mapcar 'js-to-number args)))
    (cond
     ((null values) 1.0e+INF)
     ((find-if 'js-NaN-p values) 0.0e+NaN)
     (t (apply 'min values)))))

(defun js-Math-pow (obj args)
  "15.8.2.13 -- pow(x, y)"
  (let* ((x (js-to-number (first args)))
         (y (js-to-number (second args)))
         (px (plusp x))
         (py (plusp y))
         (mx (minusp x))
         (my (minusp y))
         (zx (zerop x))
         (zy (zerop y))
         (absx (abs x))
         (oipy (and (integerp y) (oddp y)))
         (inf+x (js-infinity-p x))
         (inf+y (js-infinity-p y))
         (inf-x (and (not inf+x) (js-minus-infinity-p x)))
         (inf-y (and (not inf+y) (js-minus-infinity-p y)))
         (finx (not (or inf+x inf-x)))
         (finy (not (or inf+y inf-y))))
    (cond
     (zy 1)
     ((or (js-NaN-p y) (js-NaN-p x)) 0.0e+NaN)
     ((and (> absx 1) inf+y) 1.0e+INF)
     ((and (> absx 1) inf-y) 0)
     ((and (= absx 1) (or inf+y inf-y)) 0.0e+NaN)
     ((and (< absx 1) inf+y) 0)
     ((and (< absx 1) inf-y) 1.0e+INF)
     ((and inf+x py) 1.0e+INF)
     ((and inf+x mx) 0)
     ((and inf-x py oipy) -1.0e+INF)
     ((and inf-x py) 1.0e+INF)
     ((and inf-x my) 0)
     ((and zx py) 0)
     ((and zx my) 1.0e+INF)
     ((and zx py) 0)
     ((and zx my oipy) -1.0e+INF)
     ((and zx my) 1.0e+INF)
     ((and zx finx finy (not (integerp y))) 0.0e+NaN)
     (t (expt (float x) y)))))

(defun js-Math-random (obj args)
  "15.8.2.14 -- random()"
  (abs (/ (float (random)) most-positive-fixnum)))

(defun js-Math-round (obj args)
  "15.8.2.15 -- round(x)"
  (let ((x (js-to-number (car args))))
    (if (js-Nanfinity-p x)
        x
      (fround x))))

(defun js-Math-sin (obj args)
  "15.8.2.16 -- sin(x)"
  (let ((x (js-to-number (car args))))
    (if (js-Nanfinity-p x)
        0.0e+NaN
      (sin x))))

(defun js-Math-sqrt (obj args)
  "15.8.2.17 -- sqrt(x)"
  (let ((x (js-to-number (car args))))
    (cond
     ((js-NaN-p x) 0.0e+NaN)
     ((minusp x) 0.0e+NaN)
     (t (sqrt x)))))

(defun js-Math-tan (obj args)
  "15.8.2.18 -- tan(x)"
  (let ((x (js-to-number (car args))))
    (if (js-NaN-p x)
        0.0e+NaN
      (tan x))))

(provide 'js-native-math)

;;; js-native-math.el ends here
