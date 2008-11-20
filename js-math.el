;;; js-math.el -- emulates JavaScript arithmetic functions with calc library

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
;; The `js-runtime' library uses Emacs 64-bit floats to represent numbers,
;; which usually yields the same results as math in SpiderMonkey and Rhino.
;;
;; Emacs provides floating-point versions of ceiling, floor, and many of
;; the other built-in functions used by (or provided by) JavaScript.
;; However, some are missing -- e.g., the shift functions lsh, rsh, ash --
;; and truncating the arguments to Emacs integers yields wrong results
;; for larger operands.
;;
;; Emacs 22 bundles Dave Gillespie's world-class `calc' mathematics package.
;; Calc provides lisp-level programmability, so we can use it to obtain as
;; much precision as we need.  The easiest way to do this is with `defmath',
;; a macro that automatically converts Emacs built-in functions and data
;; types to Calc's representations.
;;
;; There are a few problems to solve.  One is restricting Calc's precision
;; to that specified by ECMA.  Another passing numbers into and out of our
;; defmath functions. A third problem is emulating the mistakes:  for instance,
;; (7 << 29) yields -536870912 in JavaScript.  We should try to fake the same
;; integer overflow/underflow produced by the C library functions.
;;
;; Eventually I'd like to structure things so that calc is only loaded when
;; a script actually needs the additional power it provides.  Initially, though,
;; I've got to focus on compatibility with ECMA-262 and SpiderMonkey, so I'll
;; worry about the autoloads later.

;;; Code:

;; Lots of work to do - this is just a placeholder.
;; The results of these functions do not fake C lib overflow/underflow.

;; Since these -particular- functions can be implemented with multiplication
;; and division, I'll leave them as examples.

(defmath js-lsh (x y)
  "Left-shift X by Y.
This macro produces the function `calcFunc-js-lsh'.
The return value is a string containing the result."
  (calc-eval (lsh x y)))

(defmath js-rsh (x y)
  "Right-shift X by Y.
This macro produces the function `calcFunc-js-rsh'.
The return value is a string containing the result."
  (calc-eval (ash x (- y))))

(defmath js-ursh (x y)
  "Unsigned -shift X by Y.
This macro produces the function `calcFunc-js-ursh'.
The return value is a string containing the result.
The result does not fake integer overflow or underflow, so its precision
is currently too high for properly emulating the standard C math library."
  (calc-eval (lsh x (- y))))

(provide 'js-math)

;;; js-math.el ends here
