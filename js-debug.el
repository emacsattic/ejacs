;;; js-debug.el -- a source-level debugger for EmacsScript

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
;; An incurably dodgy debugger interface.
;; Uses the simplest workable approach:
;;  - debugger is global, and globally on or off
;;  - js-exec calls debugger (if present) on every entry/exit

;;; Code:

(require 'js-exec)

(defun js-debug ()
  "Toggle the JS debugger on or off."
  (interactive)
  (if js-exec-debugger
      (progn
        (js-exec-unregister-debugger)
        (message "JS debugging disabled"))
    (js-exec-register-debugger #'js-debug-callback)
    (message "JS debugging enabled")))

(defun js-debug-callback (op context node &rest args)
  "Debugger callback for JS evaluator.
OP is the operation being signaled to the debugger.
It can be any of:

  'enter - about to execute an AST node
  'exit  - finished executing an AST node
  'unwind - the stack is unwinding from an exceptino

CONTEXT is the current execution context.
NODE is the current AST node.
ARGS is dependent on OP:

  'enter - (EXECUTOR) - the evaluator function about to execute
  'exit  - (RESULT)   - the result of the evaluation
  'unwind - nil       - no extra arguments"
  (case op
    (enter (js-debug-exec context node (car args)))
    (exit (js-debug-exit context node (car args)))
    (unwind (js-debug-unwind context node))
    (t
     (error "Unknown debugger op: %s" op))))

;;; Plan:
;;
;; - integrate with console for the shell
;; - maintain user breakpoint list (file/line/enabled/code-to-run)
;; - step-over sets a breakpoint "after" current expression
;;   (simplest approach => next line)
;; - step-into sets a "stop after next exec" global flag
;; - call `recursive-edit' to "pause" when breakpoint is hit
;; - pull up source file and line if available, highlight w/ overlay
;;   - put source file in special minor-mode

(defun js-debug-exec (context node executor)
  (read-string (format "Executing %s " executor)))

(defun js-debug-exit (context node executor)
  nil)

(defun js-debug-unwind (context node)
  nil)

(provide 'js-debug)
