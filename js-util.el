;;; js-util.el -- utilities shared by various js packages

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
    
(defmacro ++ (place &optional x)
  "Increment PLACE by X (1 by default).
PLACE may be a symbol, or any generalized variable allowed by `setf'.
The return value is the original value of PLACE."
  (if (symbolp place)
      (list 'prog1 place
            (list 'setq place (if x
                                  (list '+ place x)
                                (list '1+ place))))
    (list 'prog1 place
          (list 'callf '+ place (or x 1)))))

(defmacro -- (place &optional x)
  "Decrement PLACE by X (1 by default).
PLACE may be a symbol, or any generalized variable allowed by `setf'.
The return value is the original value of PLACE."
  (if (symbolp place)
      (list 'prog1 place
            (list 'setq place (if x
                                  (list '- place x)
                                (list '1- place))))
    (list 'prog1 place
          (list 'callf '- place (or x 1)))))

(defsubst xor (arg1 arg2)
  "Compute the logical-XOR of ARG1 and ARG2.
Both arguments must evaluate to t or nil."
  (not (eq arg1 arg2)))

(defun js-format (format-string &rest args)
  "Like `format', but prints short versions of JavaScript Objects.
The default printing for defstruct trees is inescapably verbose."
  (apply 'format format-string (mapcar 'js-printobj args)))

(defun js-hex-digit-to-int (c acc)
  "If C is a hex digit, return accumulator * 16 plus C.
Otherwise return -1.  C is a character, not a string."
  (let ((value
         (cond
          ((and (>= c ?0) (<= c ?9))
           (- c ?0))
          ((and (>= c ?A) (<= c ?F))
           (- c (- ?A 10)))
          ((and (>= c ?a) (<= c ?f))
           (- c (- ?a 10)))
          (t -1))))
    (if (minusp value)
        value
      (logior value (lsh acc 4)))))

(provide 'js-util)
