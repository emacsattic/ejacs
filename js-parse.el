;;; js-parse.el -- a JavaScript/EcmaScript parser

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
;; The parser was originally a port of Brendan Eich's Narcissus.

;;; Code

(eval-when-compile
  '(require 'cl))

(defconst js-version "1.5"
  "Version of JavaScript supported.")

;;; User customization options.

(defvar js-recover-from-parse-errors t
  "Non-nil to continue parsing after a syntax error.

In recovery mode, the AST will be built in full, and any error
nodes will be flagged with appropriate error information.  If
this flag is nil, a syntax error will result in an error being
signaled.

The variable is automatically buffer-local, because different
modes that use the parser will need different settings.")

(make-variable-buffer-local 'js-recover-from-parse-errors)

(defsubst neq (expr1 expr2)
  "Return (not (eq expr1 expr2))."
  (not (eq expr1 expr2)))

;;; definitions and lookup tables

(defconst js-keywords
  '(break
    case catch const continue
    debugger default delete do
    else enum
    false finally for function
    if in instanceof
    new null
    return
    switch
    this throw true try typeof
    var void
    while with)
  "JavaScript keywords.")

(defconst js-tokens
  '(END CONDITIONAL UNARY_PLUS UNARY_MINUS)
  "Special token names.")

(defconst js-nonterminals
  '(SCRIPT BLOCK LABEL FOR_IN CALL NEW_WITH_ARGS INDEX
    ARRAY_INIT OBJECT_INIT PROPERTY_INIT GETTER SETTER
    GROUP LIST)
  "Nonterminal tree type codes in JavaScript's AST structure.")

(defconst js-terminals '(IDENTIFIER NUMBER STRING REGEXP)
  "Terminal tree node type codes in JavaScript's AST structure.")

;; Operator and punctuator mapping from token to tree node type name.
;; NB: superstring tokens (e.g., ++) must come before their substring token
;; counterparts (+ in the example), so that the opRegExp regular expression
;; synthesized from this list makes the longest possible match.
(defconst js-op-names
  '(("\n" . NEWLINE)
    (";" . SEMICOLON)
    ("," . COMMA)
    ("::" . COLONCOLON)
    ("?" . HOOK)
    (":" . COLON)
    ("||" . OR)
    ("&&" . AND)
    ("|" . BITWISE_OR)
    ("^" . BITWISE_XOR)
    ("&" . BITWISE_AND)
    ("===" . STRICT_EQ)
    ("==" . EQ)
    ("=" . ASSIGN)
    ("!==" . STRICT_NE)
    ("!=" . NE)
    ("<<" . LSH)
    ("<=" . LE)
    ("<" . LT)
    (">>>" . URSH)
    (">>" . RSH)
    (">=" . GE)
    (">" . GT)
    ("++" . INCREMENT)
    ("--" . DECREMENT)
    ("+" . PLUS)
    ("-" . MINUS)
    ("*" . MUL)
    ("/" . DIV)
    ("%" . MOD)
    ("!" . NOT)
    ("~" . BITWISE_NOT)
    (".." . DOTDOT)
    ("." . DOT)
    ("@" . XMLATTR)
    ("[" . LEFT_BRACKET)
    ("]" . RIGHT_BRACKET)
    ("{" . LEFT_CURLY)
    ("}" . RIGHT_CURLY)
    ("(" . LEFT_PAREN)
    (")" .  RIGHT_PAREN))
  "Operator and punctuator mapping from token to tree node by type name")

(defconst js-assign-ops
  '("|" "^" "&" "<<" ">>" ">>>" "+" "-" "*" "/" "%")
  "JavaScript assignment operators")

(defvar js-token-names (make-hash-table)
  "Mapping from AST node type symbols to token names, for error messages.")

(loop for (k . v) in js-op-names
      do (puthash v k js-token-names))

(dolist (name-list (list js-tokens js-nonterminals js-terminals))
  (loop for s in name-list
        do (puthash s (symbol-name s) js-token-names)))

(defvar js-keyword-names (make-hash-table :test 'equal)
  "JavaScript keywords by name, mapped to their symbols.")

(loop for k in js-keywords
      do (puthash (symbol-name k)                    ; e.g. instanceof
                  (intern (upcase (symbol-name k)))  ; INSTANCEOF
                  js-keyword-names))

(defconst js-op-precedence-alist
  '((SEMICOLON . 0)
    (COMMA . 1)
    (ASSIGN . 2) (HOOK . 2) (COLON . 2) (CONDITIONAL . 2)
    ;; The above all have to have the same precedence, see bug 330975.
    (OR . 4)
    (AND . 5)
    (BITWISE_OR . 6)
    (BITWISE_XOR . 7)
    (BITWISE_AND . 8)
    (EQ . 9) (NE . 9) (STRICT_EQ . 9) (STRICT_NE . 9)
    (LT . 10) (LE . 10) (GT . 10) (GE . 10) (IN . 10) (INSTANCEOF . 10)
    (LSH . 11) (RSH . 11) (URSH . 11)
    (PLUS . 12) (MINUS . 12)
    (MUL . 13) (DIV . 13) (MOD . 13)
    (DELETE . 14) (VOID . 14) (TYPEOF . 14)
    (NOT . 14) (BITWISE_NOT . 14) (UNARY_PLUS . 14) (UNARY_MINUS . 14)
    (INCREMENT . 15) (DECREMENT . 15) ;; postfix
    (NEW . 16)
    (DOT . 17))
  "JavaScript operator precedence table")

(defconst js-op-arity-alist
  '((COMMA . -2)
    (ASSIGN . 2)
    (CONDITIONAL . 3)
    (OR . 2)
    (AND . 2)
    (BITWISE_OR . 2) (BITWISE_XOR . 2) (BITWISE_AND . 2)
    (EQ . 2) (NE . 2) (STRICT_EQ . 2) (STRICT_NE . 2)
    (LT . 2) (LE . 2) (GT . 2) (GE . 2) (IN . 2) (INSTANCEOF . 2)
    (LSH . 2) (RSH . 2) (URSH . 2)
    (PLUS . 2) (MINUS . 2)
    (MUL . 2) (DIV . 2) (MOD . 2)
    (DELETE . 1) (VOID . 1) (TYPEOF . 1)
    (NOT . 1) (BITWISE_NOT . 1) (UNARY_PLUS . 1) (UNARY_MINUS . 1)
    (INCREMENT . 1) (DECREMENT . 1)
    (NEW . 1) (NEW_WITH_ARGS . 2) (DOT . 2) (INDEX . 2) (CALL . 2)
    (ARRAY_INIT . 1) (OBJECT_INIT . 1) (GROUP . 1))
  "JavaScript operator arity table")

(defsubst js-op-arity (op)
  (or (cdr-safe (assq op js-op-arity-alist)) 0))

(defsubst js-op-precedence (op)
  (or (cdr-safe (assq op js-op-precedence-alist)) 0))

;;; Lexical scanner and parser.

(defconst js-operator-regexp
  (let ((re '()))
    (dolist (op js-op-names
                (mapconcat 'identity (nreverse re) "\\|"))
      (unless (string= (car op) "\n")
        ;; backslash-escape the characters []?|^&(){}+-*/.\
        (push (replace-regexp-in-string
               ;; ']' must come first in a [...] alternative, and '-' last
               ;; Literal |, (, ), { and } aren't escaped in elisp regexps.
               ;; Elisp special chars are `*', `+', `?', `[', `^', `$', `\'.
               ;; No need to escape `-', `/', or `&', as Narcissus does.
               "\\([][?^+*.]\\)" "\\\\\\1" (car op))
              re))))
  "Regexp to recognize operators and punctuators (except newline)")

;/^\d+\.\d*(?:[eE][-+]?\d+)?|^\d+(?:\.\d*)?[eE][-+]?\d+|^\.\d+(?:[eE][-+]?\d+)?/
(defconst js-fp-regexp
  (concat
   "[0-9]+\\.[0-9]*"                       ; ^\d+\.\d*
   "\\(?:[eE][-+]?[0-9]+\\)?"              ; (?:[eE][-+]?\d+)?
   "\\|[0-9]+\\(?:\\.[0-9]*\\)?"           ; |^\d+(?:\.\d*)?
   "[eE][-+]?[0-9]+"                       ; [eE][-+]?\d+
   "\\|\\.[0-9]+\\(?:[eE][-+]?[0-9]+\\)?") ; |^\.\d+(?:[eE][-+]?\d+)?/
  "Regexp to match floating point literals (but not integer literals)")

(defconst js-int-regexp
  "0[xX][0-9a-fA-F]+\\|0[0-7]*\\|[0-9]+"
  "Regexp for matching integer literals in decimal/hex/octal")

(defconst js-identifier-regexp
  "[$A-Za-z_][$A-Za-z0-9_]*"
  "Regexp for matching JavaScript identifiers and keywords")

(defconst js-string-regexp
  (concat
   "\"\\(?:\\\\\\(?:.\\|\n\\)\\|[^\"\n]\\)*\""  ; double-quoted variety
   "\\|"
   "'\\(?:\\\\\\(?:.\\|\n\\)\\|[^'\n]\\)*'")    ; single-quoted variety
  "Match JavaScript single- or double-quoted string literals.
Trickier to get right in elisp, e.g. to match x\\\Xx for any substitution
of single/double quotes for xX, or embedded backquoted newlines, because
'.' doesn't match a newline in elisp, and also because everything needs
to be double-escaped in elisp regexps.  E.g. \\\\ matches a single backquote
char in the final string.")

(defconst js-re-regexp
  "/\\(?:\\\\/\\|[^/\n]\\)+/[gim]*"
  "Regexp for matching JavaScript regular expression literals.")

(defconst js-comment-regexp
  "/\\(?:\\*\\(?:.\\|\n\\)*?\\*/\\|/.*\\)"
  "Match single-line and multi-line JavaScript comments.")

(defconst js-ws-comment-regexp
  (concat "\\([ \t]+\\)\\|\\(" js-comment-regexp "\\)")
  "Matches spaces, tabs, and comments.")

(defconst js-ws-crlf-comment-regexp
  (concat "\\([ \t\r\n]+\\)\\|\\(" js-comment-regexp "\\)")
  "Matches spaces, tabs, newlines, and comments.")

(defconst js-num-id-string-regexp
  (concat
   "\\(" js-fp-regexp  "\\)"
   "\\|\\(" js-int-regexp "\\)"
   "\\|\\(" js-identifier-regexp "\\)"
   "\\|\\(" js-string-regexp "\\)")
  "Regexp to match any of several token types.")

(defconst js-master-regexp
  (concat
   "\\(" js-fp-regexp  "\\)"
   "\\|\\(" js-int-regexp "\\)"
   "\\|\\(" js-identifier-regexp "\\)"
   "\\|\\(" js-string-regexp "\\)"
   "\\|\\(" js-re-regexp "\\)"
   "\\|\\(" js-operator-regexp "\\)")
  "Regexp to match any of several token types.")

(defconst js-master-regexp-no-re
  (concat
   "\\(" js-fp-regexp  "\\)"
   "\\|\\(" js-int-regexp "\\)"
   "\\|\\(" js-identifier-regexp "\\)"
   "\\|\\(" js-string-regexp "\\)"
   "\\|\\(" js-operator-regexp "\\)")
  "Regexp to match any of several token types.")

;; compiler context:  per-buffer parsing state
(defvar js-in-function nil)
(make-variable-buffer-local 'js-in-function)

(defvar js-stmt-stack nil)
(make-variable-buffer-local 'js-stmt-stack)

(defvar js-fun-decls nil)
(make-variable-buffer-local 'js-fun-decls)

(defvar js-var-decls nil)
(make-variable-buffer-local 'js-var-decls)

(defvar js-comments nil)
(make-variable-buffer-local 'js-comments)

(defvar js-bracket-level 0)
(make-variable-buffer-local 'js-bracket-level)

(defvar js-curly-level 0)
(make-variable-buffer-local 'js-curly-level)

(defvar js-paren-level 0)
(make-variable-buffer-local 'js-paren-level)

(defvar js-hook-level 0)
(make-variable-buffer-local 'js-hook-level)

(defvar js-ecma-strict-mode nil)
(make-variable-buffer-local 'js-ecma-strict-mode)

(defvar js-in-for-loop-init nil)
(make-variable-buffer-local 'js-in-for-loop-init)

;; lexer/tokenizer vars

(defvar js-cursor 1
  "Current lexer position")
(make-variable-buffer-local 'js-cursor)

(defvar js-token-ring (make-vector 4 nil)
  "Ring-buffer for 3-token lookahead")
(make-variable-buffer-local 'js-token-ring)

(defvar js-token-index 0
  "Index into lookahead buffer")
(make-variable-buffer-local 'js-token-index)

(defvar js-lookahead 0
  "Current number of tokens that have been prefetched")
(make-variable-buffer-local 'js-lookahead)

(defvar js-scan-newlines nil
  "True to stop at newlines when skipping whitespace.")
(make-variable-buffer-local 'js-scan-newlines)

(defvar js-scan-operand t
  "True when the expression parser expects an operand.")
(make-variable-buffer-local 'js-scan-operand)

(defvar js-current-line 1)
(make-variable-buffer-local 'js-current-line)

(defvar js-parse-hook nil
  "Callback or callback list for receiving parsing progress.")
(make-variable-buffer-local 'js-parsing-callback)

(defvar js-is-eval-code nil
  "True if we're evaluating code in a string.
If non-nil, the tokenizer will record the token text, and the AST nodes
will record their source text.  Off by default for IDE modes, since the
text is available in the buffer.")
(make-variable-buffer-local 'js-is-eval-code)

(defvar js-no-parent-links nil
  "Non-nil to exclude parent links from the AST.
Most applications, including the IDE and the evaluator, need the
parent links.  Leaving them out can make the tree easier to read.")
(make-variable-buffer-local 'js-no-parent-links)

(defsubst js-new-token ()
  "Make a blank token"
  (list (cons 'type nil)
        (cons 'start 0)
        (cons 'end 0)
        (cons 'line 0)
        (cons 'value nil)
        (cons 'assign-op nil)))

(defsubst js-clear-token (token)
  "Clear out the values for existing token TOKEN."
  ;; These are the only values that don't get auto-reset.
  (setcdr (assoc 'value token) nil)
  (setcdr (assoc 'assign-op token) nil))

(defsubst js-token-type (token)
  (cdr (assq 'type token)))

(defsubst js-token-start (token)
  (cdr (assq 'start token)))

(defsubst js-token-end (token)
  (cdr (assq 'end token)))

(defsubst js-token-line (token)
  (cdr (assq 'line token)))

(defsubst js-token-value (token)
  (or (cdr-safe (assq 'value token))
      (buffer-substring-no-properties (js-token-start token)
                                      (js-token-end token))))

(defsubst js-token-assign-op (token)
  (cdr-safe (assq 'assign-op token)))

(defsubst js-scan-error (msg)
  "Signal an error when the lexer encounters an unknown token."
  (signal 'js-scan-error
          (list msg js-cursor)))

(put 'js-scan-error 'error-conditions
     '(error scan-error js-scan-error))
(put 'js-scan-error 'error-message "Syntax error")

(defsubst js-parse-syntax-error (msg &optional node)
  "Signal a syntax error or parse error.
Syntax errors are signaled when we don't want to recover.
Otherwise we signal a parse error, which contains different data."
  (if js-recover-from-parse-errors
      (signal 'js-parse-parse-error
              (list msg node))
  (signal 'js-parse-syntax-error
          (list msg
                (line-number-at-pos js-cursor)
                (save-excursion
                  (goto-char js-cursor)
                  (current-column))
                (js-eobp)))))

(put 'js-parse-syntax-error 'error-conditions
     '(error syntax-error js-parse-syntax-error))
(put 'js-parse-syntax-error 'error-message "Syntax error")

(put 'js-parse-parse-error 'error-conditions
     '(errors parse-error js-parse-parse-error))
(put 'js-parse-parse-error 'error-message "Parse error")

(defsubst js-unscan ()
  (if (= 4 (incf js-lookahead))
      (error "PANIC: too much lookahead!"))
  (setq js-token-index
        (logand 3 (1- js-token-index))))

(defsubst js-current-token (&optional n)
  "Return token at the current token index, or index N if specified."
  (and js-token-ring
       (aref js-token-ring (or n js-token-index))))

(defsubst js-peek ()
  "Return the type of the next token without consuming it."
  (if (plusp js-lookahead)
      (js-token-type
       (js-current-token (logand 3 (+ js-token-index
                                       js-lookahead))))
    (prog1
        (js-scan)
      (js-unscan))))

(defsubst js-eobp ()
  "Return t if we've reached the end of the parse region."
  (eq (js-peek) 'END))

(defsubst js-match (tt)
  "Consume and return next token if it matches type TT, a symbol."
  (or (eq tt (js-scan))
      (progn (js-unscan) nil)))

(defsubst js-must-match (tt)
  "Match next token to token type TT, or signal a syntax error."
  (if (not (js-match tt))
      (js-parse-syntax-error
       (format "Missing %s" (downcase (symbol-name tt))))
    (js-current-token)))

(defsubst js-peek-same-line ()
  (prog2
      (setq js-scan-newlines t)
      (js-peek)
    (setq js-scan-newlines nil)))

(defsubst js-current-value ()
  "Return value field of current token in the lexer."
  (js-token-value (js-current-token)))

(defsubst js-advance-cursor ()
  "Advance cursor across last match, possibly advancing current line.
If the current line has changed, records the new line number and
notifies any parsing callbacks.  Uses match data from last regexp search."
  (let ((end (match-end 0))
        (count 0))
    (goto-char (match-beginning 0))
    (while (search-forward "\n" end t)
      (incf count))
    (goto-char (setq js-cursor end))
    (when (plusp count)
      (setq js-current-line (+ count js-current-line))
      (if js-parse-hook  ; currently only single callback supported
          (funcall js-parse-hook js-current-line)))))

(defun js-scan ()
  "Match and consume the next token, returning token type.
The token is stored in a ring buffer so it can be fetched again
by calling `js-current-token'."
  (catch 'js-ret
    (let (k op type group end)
      (while (plusp js-lookahead)
        (setq js-lookahead (1- js-lookahead)
              js-token-index (logand 3 (1+ js-token-index))
              k (js-current-token))
        (when (or js-scan-newlines
                  (neq (js-token-type k) 'NEWLINE))
          (throw 'js-ret (js-token-type k))))
      (goto-char js-cursor)
      (while (looking-at (if js-scan-newlines
                             js-ws-comment-regexp
                           js-ws-crlf-comment-regexp))
        (unless js-is-eval-code
          (if (match-beginning 2)
              (js-record-comment)))
        (js-advance-cursor))
      (setq js-token-index (logand 3 (1+ js-token-index))
            k (js-current-token))
      (if k
          (js-clear-token k)
        (aset js-token-ring js-token-index
              (setq k (js-new-token))))
      (if (eobp)
          (throw 'js-ret (setcdr (assq 'type k) 'END)))
      (setcdr
       (assq 'type k)
       (if (looking-at (if js-scan-operand
                           js-master-regexp
                         js-master-regexp-no-re))
           (cond
            ((or (match-beginning 1)    ; float
                 (match-beginning 2))   ; int
             'NUMBER)
            ((match-beginning 3)        ; identifier
             (gethash (match-string 3)
                      js-keyword-names ; e.g. 'BREAK
                      'IDENTIFIER))
            ((match-beginning 4)        ; string literal
             'STRING)
            ((and js-scan-operand (match-beginning 5))
             'REGEXP)
            (t                          ; operator
             (setq group (if js-scan-operand 6 5)
                   op (match-string group)
                   type (cdr (assoc op js-op-names)))
             (if (and (member op js-assign-ops)
                      (save-excursion
                        (goto-char (match-end group))
                        (= (following-char) ?=)))
                 (progn
                   (setcdr (assq 'assign-op k) type)
                   (setq end (1+ (match-end group)))
                   'ASSIGN)
               (setcdr (assq 'assign-op k) nil)
               (if (and js-scan-operand
                        (memq type '(PLUS MINUS)))
                   (if (eq type 'PLUS)
                       'UNARY_PLUS
                     'UNARY_MINUS)
                 type))))
         (js-parse-syntax-error "Illegal token")))

      (setcdr (assq 'start k) (match-beginning 0))
      (setcdr (assq 'end k)
              (setq js-cursor (or end (match-end 0))))
      (setcdr (assq 'line k) js-current-line)
      (setq type (js-token-type k))
      (js-record-token-value k type)
      type)))

;; This function (recording token value) needs work:
;;  - some more tokens need special parsing (e.g. RegExp)
;;  - some tokens shouldn't save their text (never requested)
;;  - should be symbol-handler-dispatch rather than big switch
;;  - some tokens are redundantly saving the text of all their kids
;; Should be possible to save a lot of space by just recording the
;; entire evaluated text "somewhere", and only recording separate
;; values for tokens that need to parse the value.  Maybe just save
;; the whole region on the root node?

(defun js-record-token-value (k tt)
  "Records the text of token K in the token.  TT is token type."
  (let ((value (js-current-value)))
    (setcdr (assq 'value k)
            (cond
             ((eq tt 'NUMBER)
              (if (match-beginning 1)
                  (js-parse-float value)
                (js-parse-int value)))
             ((eq tt 'STRING)
              ;; The string literal "\\" represents a 1-char JavaScript string
              ;; containing exactly one backslash.  The literal is four characters
              ;; long (two quotes, two backslashes), and is represented as a lisp
              ;; string by "\"\\\\\"".  We need one level of evaluation in order
              ;; to turn backslash character escape sequences into the characters
              ;; they represent.  Narcissues uses eval(); we do it by hand.
              (js-scan-string value))
             ;; I've nowhere to record pattern + flags in token, so I
             ;; pull it apart later in the evaluator.  I know.  Lame.
             ((eq tt 'REGEXP)
              value)
             (t
              (if js-is-eval-code
                  value
                nil))))))

(defun js-scan-string (s)
  "Read a sequence of characters S as a JavaScript string.
S should be quoted with either single- or double-quote chars.
Returns a Lisp string representing the chars read from S."
  (let ((quote-char (aref s 0))  ; " or '
        (i 0)
        c result tmp val)
    (setq c (aref s (incf i)))
    (while (/= c quote-char)
      (catch 'loop
        (when (= c ?\\)
          ;; We've hit an escaped character
          (setq c (aref s (incf i)))
          (cond
           ((= c ?b) (setq c 8))
           ((= c ?f) (setq c 12))
           ((= c ?n) (setq c 10))
           ((= c ?r) (setq c 13))
           ((= c ?t) (setq c 9))
           ((= c ?v) (setq c 11))
           ((= c ?u)
            ;; Get 4 hex digits; if the u escape is not
            ;; followed by 4 hex digits, use 'u' + the
            ;; literal character sequence that follows.
            (setq tmp (substring s (incf i) (1+ (incf i 3))))
            (if (string-match "[0-9a-fA-F]\\{4\\}" tmp)
                (setq c (read (concat "?\\u" tmp)))
              (setq c ?u)
              (decf i 3)))
           ((= c ?x)
            (setq tmp (substring s (incf i) (1+ (incf i))))
            (if (string-match "[0-9a-fA-F][0-9a-fA-F]" tmp)
                (setq c (read (concat "?\\x" tmp)))
              (setq c ?x)
              (decf i)))
           ((= c ?\n)
            ;; Remove line terminator after escape to follow
            ;; SpiderMonkey and C/C++
            (setq c (aref s (incf i))))
           (t
            (when (and (<= ?0 c) (< c ?8))
              (setq val (- c ?0))
              (setq c (aref s (incf i)))
              (when (and (<= ?0 c) (< c ?8))
                (setq val (- (+ (* 8 val) c) ?0))
                (setq c (aref s (incf i)))
                (when (and (<= ?0 c) (< c ?8) (<= val ?\037))
                  ;; c is 3rd char of octal sequence only
                  ;; if the resulting val <= 0377
                  (setq val (- (+ (* 8 val) c) ?0))
                  (setq c (aref s (incf i)))))
              (decf i)                  ; ungetChar(c)
              (setq c val)))))
        (push c result)
        (setq c (aref s (incf i)))))
    (mapconcat 'string (nreverse result) "")))

(defvar js--node-counter 0 "Internal counter")

(defun js-node (&optional type kids parent)
  "Create a new JavaScript AST node optional TYPE and child list KIDS.
A parse tree node is an alist whose first three elements have cars
`type', `start', and `end'.  A node may also optionally have a
`kids' element whose cdr is a list of type-specific child nodes.
The remaining elements, if any, are named properties added with
`js-node-put'.  The order only matters for the `type' property,
which must be first."
  (let* ((k (js-current-token))
         (n (list
             (cons 'type (or type (if k (js-token-type k) 'BLOCK)))
             (cons 'start (if k (js-token-start k) 0))
             (cons 'end (if k (js-token-end k) 0))
             (cons 'parent parent)))
         (ptr kids))
    (incf js--node-counter)
    (while ptr
      (js-node-push n (car ptr))
      (setq ptr (cdr ptr)))
    (if (and k js-is-eval-code)
        (js-node-put n 'value (js-token-value k)))
    n))

(defsubst js-node-p (n)
  "Heuristic for checking if a value is an AST node."
  (and (consp n)
       (consp (car n))
       (eq (caar n) 'type)))

(defun js-node-put (n p v &optional s)
  "Store named property P with V in js-node N, and return N.
P must be a symbol.  Replaces existing value if P is present.
Otherwise, places the new cons cell at the end of the list,
by destructively modifying the last element's cdr.  Returns N.
If V is an js-node, sets V's parent to N, unless S (suppress)
is non-nil."
  (unless (or s js-no-parent-links)
    (if (js-node-p v)
        (js-node-put v 'parent n t)))
  (let ((cell (assq p n)))
    (if cell
        (progn (setcdr cell v) n)
      (nconc n (list (cons p v))))))

(defsubst js-node-get (n p)
  "Retrieve named property P from AST node N."
  (cdr-safe (assq p n)))

(defsubst js-node-type (n)
  "Return the node type of N, a symbol."
  (cdr (assq 'type n)))

(defsubst js-node-start (n)
  (or (cdr (assq 'start n)) 0))

(defsubst js-node-end (n)
  (or (cdr (assq 'end n)) 0))

(defsubst js-node-set-start (n start)
  (setcdr (assq 'start n) start))

(defsubst js-node-set-end (n end)
  (setcdr (assq 'end n) end))

(defsubst js-node-value (n)
  (or (js-node-get n 'value)
    (buffer-substring-no-properties (js-node-start n)
                                    (js-node-end n))))

(defsubst js-node-line (n)
  "Fetch the source line number at the start of node N.
This is O(n) in the length of the source buffer; use prudently."
  (line-number-at-pos (js-node-start n)))

(defsubst js-node-kids (n)
  "Return the list of child nodes for node N.
This is any nodes that have been pushed into the 'kids list."
  (cdr-safe (assq 'kids n)))

(defsubst js-node-kid (n i)
  "Return child I of node N, or nil if there aren't that many."
  (nth i (js-node-kids n)))

(defsubst js-node-parent (n)
  "Return the parent node of N, or nil if it has no parent."
  (cdr-safe (assq 'parent n)))

(defun js-node-root (n)
  "Return the root of the AST containing N.
If N has no parent pointer, returns N."
  (let ((parent (js-node-parent n)))
    (if parent
        (js-node-root parent)
      n)))

(defun js-node-buffer (n)
  "Return the buffer associated with AST N.
Returns nil if the buffer is not set as a property on the root
node, or if parent links were not recorded during parsing."
  (let ((root (js-node-root n)))
    (if root
        (js-node-get root 'buffer))))

(defun js-node-push (n kid)
  "Push js-node KID onto the end of js-node N's child list.
Always use `js-node-push' when adding operands to an expression,
to make sure N's start and end are updated.  Return value is KID.
KID is always added to the -end- of the 'kids list.  Always sets
the 'parent property of KID to N."
  (let ((kids (assq 'kids n)))
    (setcdr (assq 'start n)
            (min (js-node-start n) (js-node-start kid)))
    (setcdr (assq 'end n)
            (max (js-node-end n) (js-node-end kid)))
    (if kids
        (setcdr kids (nconc (cdr kids) (list kid)))
      (js-node-put n 'kids (list kid) t))
    (unless js-no-parent-links
      (js-node-put kid 'parent n))
    kid))

(defun js-script (&optional in-function)
  "Parse a top-level script or a function body."
  ;; dynamically bind a new compiler context for the call
  (let ((js-in-function in-function)
        (js-stmt-stack nil)
        (js-fun-decls nil)
        (js-var-decls nil)
        (js-comments nil)
        (js-bracket-level 0)
        (js-curly-level 0)
        (js-paren-level 0)
        (js-hook-level 0)
        (js-in-for-loop-init nil)
        n)
    ;; not in `let' clause so that js-in-function is bound
    (setq n (js-statements))
    (js-node-put n 'type 'SCRIPT)
    ;; the fun and var decl nodes already have parent pointers.
    ;; not much point in pointing these grouping nodes upwards.
    (if js-fun-decls
        (js-node-put n 'fun-decls
                      (js-node 'FUN_DECLS js-fun-decls n) t))
    (if js-var-decls
        (js-node-put n 'var-decls
                      (js-node 'VAR_DECLS js-var-decls n) t))
    ;; the comment node parent pointers all go to the 'comments,
    ;; since there's no other good place to hook them in.
    (if js-comments
        (js-node-put n 'comments
                      (js-node 'COMMENTS js-comments n)))
    n))

(defun js-record-comment ()
  "Record a comment.  Uses the match-data from the last scan.
The match data is preserved across the call."
  (let ((n (js-node 'COMMENT))
        (beg  (match-beginning 0)))
    (js-node-set-start n beg)
    (js-node-set-end n (match-end 0))
    (js-node-put n 'format (if (save-excursion
                                  (goto-char (1+ beg))
                                  (= (following-char) ?*))
                                'multi-line
                              'single-line))
    (push n js-comments)))

;; workaround for a byte-compilation warning about valid code
(defsubst js-pop-stmt-stack ()
  (with-no-warnings
    (pop js-stmt-stack)))

(defsubst js-nest (node func &optional end)
  "Statement stack and nested statement handler.
NODE is the ast node.  FUNC is some function to call.
END is a token type."
  (prog2
      (push node js-stmt-stack)
      (funcall func)
    (js-pop-stmt-stack)
    (if end (js-must-match end))))

(defun js-statements ()
  (let ((n (js-node 'BLOCK)))
    (push n js-stmt-stack)
    (while (and (not (js-eobp))
                (neq (js-peek) 'RIGHT_CURLY))
      (js-node-push n (js-stmt)))
    (pop js-stmt-stack))) ; return n

(defsubst js-block ()
  (prog2
      (js-must-match 'LEFT_CURLY)
      (js-statements)
    (js-must-match 'RIGHT_CURLY)))

(defsubst js-paren-expr ()
  (prog2
      (js-match 'LEFT_PAREN)
      (js-expr)
    (js-match 'RIGHT_PAREN)))

;; Certain statement types don't need the automatic
;; semicolon-insertion magic.
(dolist (sym '(DO  ; handles decision in js-do-stmt
               LEFT_CURLY SEMICOLON NEWLINE FUNCTION
               IF FOR TRY WHILE SWITCH WITH
               IDENTIFIER)) ; handled manually
  (put sym 'js-no-magic-semi t))

(defvar js-statement-parsers
  '((BREAK . js-break-or-cont)
    (CATCH . js-bad-catch)
    (CONST . js-variables)
    (CONTINUE . js-break-or-cont)
    (DEBUGGER . js-node)
    (DO . js-do-stmt)
    (FINALLY . js-bad-finally)
    (FOR . js-for-stmt)
    (FUNCTION . js-named-defun)
    (IDENTIFIER . js-label-or-expr)
    (IF . js-if-stmt)
    (LEFT_CURLY . js-block-stmt)
    (NEWLINE . js-semicolon)
    (RETURN . js-return-stmt)
    (SEMICOLON . js-semicolon)
    (SWITCH . js-switch-stmt)
    (THROW . js-throw-stmt)
    (TRY . js-try-stmt)
    (VAR . js-variables)
    (WHILE . js-while-stmt)
    (WITH . js-with-stmt))
  "List of parsers for statement token types.")

(dolist (spec js-statement-parsers)
  (put (car spec) 'js-parser (cdr spec)))

(defun js-stmt ()
  "Parse one JavaScript statement and return its AST node."
  (let* ((tt (js-scan))
         (parser (get tt 'js-parser))
         n)
    (condition-case err
        (progn
          (setq n (if parser
                      (funcall parser)
                    (js-stmt-expr)))
          (unless (get tt 'js-no-magic-semi)
            (js-insert-magic-semicolon))
          n)
      (parse-error
       (js-recover err)))))

(defun js-insert-magic-semicolon ()
  "Possibly treat newline as a statement terminator.
Only happens for certain statement types."
  (when (= js-current-line
           (js-token-line (js-current-token)))
    (if (memq (js-peek-same-line)
              '(END NEWLINE SEMICOLON RIGHT_CURLY))
        (js-match 'SEMICOLON)
      (js-parse-syntax-error "Missing ; before statement"))))

(defsubst js-block-stmt ()
  "Parse a block of statements after a { has been matched.
Returns the AST node for the statement block."
  (prog1
      (js-statements)
    (js-must-match 'RIGHT_CURLY)))

(defsubst js-semicolon ()
  "Parse a semicolon and return its AST node."
  (js-node 'SEMICOLON))

(defsubst js-bad-catch ()
  (js-parse-syntax-error "catch without preceding try"))

(defsubst js-bad-finally ()
  (js-parse-syntax-error "finally without preceding try"))

(defsubst js-label-or-expr ()
  "Parse a label or expression statement after matching IDENTIFIER.
If the identifier is followed by a colon, it's parsed as a label."
  (let (tt)
    (setq js-scan-operand nil
          tt (js-peek)
          js-scan-operand t)
    (if (eq tt 'COLON)
        (js-stmt-label)
      (prog1
          (js-stmt-expr)
        (js-insert-magic-semicolon)))))

(defun js-do-stmt ()
  "Parse a do-statement and return its AST node."
  (let ((n (js-node)))
    (js-node-put n 'is-loop t)
    (js-node-put n 'body (js-nest n #'js-stmt 'WHILE))
    (js-node-put n 'while-start
                  (js-token-start (js-current-token)))
    (js-node-put n 'condition (js-paren-expr))
    ;; http://bugzilla.mozilla.org/show_bug.cgi?id=238945
    (if js-ecma-strict-mode
        (js-match 'SEMICOLON)
      (js-insert-magic-semicolon))
    n))

(defun js-stmt-expr ()
  "Parse an expression in the place of a statement.
Returns an AST node whose type is 'SEMICOLON, like Narcissus."
  (let ((n (js-node 'SEMICOLON))
        e)
    (js-unscan)
    (setq e (js-expr))
    (js-node-put n 'expression e)
    (js-node-set-end n (js-node-end e))
    n))

(defun js-if-stmt ()
  "Parse an if-statement and return its AST node."
  (let ((n (js-node))
        n2 start)
    (js-node-put n 'condition (js-paren-expr))
    (push n js-stmt-stack)
    (js-node-put n 'then-part (js-stmt))
    (when (js-match 'ELSE)
      (setq start (js-token-start (js-current-token)))
      (setq n2 (js-stmt))
      (js-node-put n2 'else-start start)
      (js-node-put n 'else-part n2))
    (pop js-stmt-stack)))

(defun js-return-stmt ()
  "Parse a return-statement and return its AST node."
  (unless js-in-function
    (js-parse-syntax-error "Invalid return"))
  (let ((n (js-node))
        (tt (js-peek-same-line)))
    (if (memq tt '(END NEWLINE SEMICOLON RIGHT_CURLY))
        ;; Narcissus returns "return" here (a mistake, I think)
        (js-node-put n 'value 'undefined)
      (js-node-put n 'value (js-expr)))
    n))

(defun js-for-stmt ()
  "Parse a for-loop or for-in loop and return its AST node."
  (let ((n (js-node))
        tt n2)
    (js-node-put n 'is-loop t)
    (js-must-match 'LEFT_PAREN)
    (when (neq (setq tt (js-peek)) 'SEMICOLON)
      (letf ((js-in-for-loop-init t))
        (setq n2 (if (memq tt '(VAR CONST))
                     (prog2
                         (js-scan) ; skip over var/const kwd
                         (js-variables))
                   (js-expr)))))
    (if (and n2 (js-match 'IN))
        (progn
          (js-node-put n 'type 'FOR_IN)
          (if (neq (js-node-type n2) 'VAR)
              (js-node-put n 'iterator n2)
            (if (/= 1 (length (js-node-kids n2)))
                (js-parse-syntax-error "Invalid for..in left-hand side"))
            (js-node-put n 'iterator (car (js-node-kids n2)) t)
            (js-node-put n 'var-decl n2))
          (js-node-put n 'object (js-expr)))
      ;; regular for-loop
      (js-node-put n 'setup n2)
      (js-must-match 'SEMICOLON)
      (unless (eq (js-peek) 'SEMICOLON)
        (js-node-put n 'condition (js-expr)))
      (js-must-match 'SEMICOLON)
      (unless (eq (js-peek) 'RIGHT_PAREN)
        (js-node-put n 'update (js-expr))))

    (js-must-match 'RIGHT_PAREN)
    (js-node-put n 'body (js-nest n #'js-stmt)))) ; returns n

(defun js-try-stmt ()
  "Parse a try-statement and return its AST node."
  (let ((n (js-node))
        n2 n3 catch-clauses)
    (js-node-put n 'try-block (js-block))
    (while (js-match 'CATCH)
      (setq n2 (js-node))
      (js-must-match 'LEFT_PAREN)
      (js-node-put n2 'var-name
                    (js-token-value (js-must-match 'IDENTIFIER)))
      (when (js-match 'IF)
        (if js-ecma-strict-mode
            (js-parse-syntax-error "Illegal catch guard"))
        (if (and (plusp (length catch-clauses))
                 (not (js-node-get (car catch-clauses) 'guard)))
            (js-parse-syntax-error "Guarded catch after unguarded"))
        (js-node-put n2 'guard-kwd (js-node))
        (js-node-put n2 'guard (js-expr)))
      (js-must-match 'RIGHT_PAREN)
      (js-node-put n2 'block (js-block))
      (push n2 catch-clauses))  ; push n2 onto catch-clauses list
    (unless js-no-parent-links
      (loop for clause in catch-clauses do
            (js-node-put clause 'parent n)))
    (js-node-put n 'catch-clauses
                  (js-node 'CATCH_CLAUSES (reverse catch-clauses)))
    (when (js-match 'FINALLY)
      (setq n3 (js-node))
      (js-node-put (setq n2 (js-block)) 'kwd n3)
      (js-node-put n 'finally-block n2))
    (and (null catch-clauses)
         (not (js-node-get n 'finally-block))
         (js-parse-syntax-error "Invalid try statement"))
    n))

(defun js-switch-stmt ()
  "Parse a switch statement and return its AST node."
  (let ((n (js-node))
        tt n2 cases statements)
    (js-must-match 'LEFT_PAREN)
    (js-node-put n 'discriminant (js-expr))
    (js-must-match 'RIGHT_PAREN)
    (js-node-put n 'default-index -1)
    (push n js-stmt-stack)
    (js-must-match 'LEFT_CURLY)

    (while (neq (setq tt (js-scan)) 'RIGHT_CURLY)
      (unless (memq tt '(DEFAULT CASE))
       (js-parse-syntax-error "Invalid switch case"))
      (and (eq tt 'DEFAULT)
           (plusp (js-node-get n 'default-index))
           (js-parse-syntax-error "More than one switch default"))
      (setq n2 (js-node))
      (if (eq tt 'DEFAULT)
          (js-node-put n 'default-index (length cases))
        (js-node-put n2 'case-label (js-expr 'COLON)))
      (js-must-match 'COLON)
      (js-node-put n2 'statements (setq statements (js-node 'BLOCK)))
      (while (not (memq (setq tt (js-peek))
                        '(CASE DEFAULT RIGHT_CURLY)))
        (js-node-push statements (js-stmt)))
      (push n2 cases))

    (js-node-put n 'cases (js-node 'CASES (nreverse cases)))
    (pop js-stmt-stack)))

(defun js-while-stmt ()
  "Parse a while statement and return its AST node."
  (let ((n (js-node)))
    (js-node-put n 'is-loop t)
    (js-node-put n 'condition (js-paren-expr))
    (js-node-put n 'body (js-nest n #'js-stmt)))) ; returns n

(defun js-throw-stmt ()
  "Parse a throw statement and return its AST node."
  (let ((n (js-node)))
    (js-node-put n 'exception (js-expr))))

(defun js-with-stmt ()
  "Parse a with-statement and return its AST node."
  (let ((n (js-node)))
    (js-node-put n 'object (js-paren-expr))
    (js-node-put n 'body (js-nest n #'js-stmt))))

(defun js-break-or-cont ()
  "Parse a break or continue statement and return its AST node.
We find the target statement of the break or continue: either
a label or the innermost enclosing switch or loop."
  (let* ((n (js-node))
         (tt (js-node-type n))
         (ss js-stmt-stack)
         label)
    (when (eq (js-peek-same-line) 'IDENTIFIER)
      (js-scan)
      (js-node-put n 'label (setq label (js-current-value))))

    ;; move ss to point to the target statement in the stmt stack
    (if label
        (progn
          (while (and ss
                      (not (string= (js-node-get (car ss) 'label)
                                    label)))
            (setq ss (cdr ss)))
          (unless ss
            (js-parse-syntax-error "Label not found")))

      ;; no label:  search for enclosing loop or switch
      (while (and ss
                  (not (or (js-node-get (car ss) 'is-loop)
                           (and (eq tt 'BREAK)
                                (eq (js-node-type (car ss))
                                    'SWITCH)))))
        (setq ss (cdr ss)))
      (unless ss (js-parse-syntax-error
                  (format "Invalid %s" (downcase (symbol-name tt))))))

    ;; save the target statement
    (js-node-put n 'target (car ss))))  ; returns n

(defun js-stmt-label ()
  "Parse a statement label and return its AST node."
  (let ((label (js-current-value))
        (start (js-token-start
                (js-current-token))) ; beginning of label
        (node js-stmt-stack)
        n stmt)
    (while node
      (if (string= label (js-node-get (car node) 'label))
          (js-parse-syntax-error
           (format "Duplicate label: %s" label)))
      (setq node (cdr node)))
    (js-scan)
    (prog1 (setq n (js-node 'LABEL))
      (js-node-set-start n start)
      (js-node-put n 'label label)
      ;; Give the labeled stmt a 'label prop with its enclosing label.
      ;; I don't see how else break/continue to labels can work; Narcissus
      ;; seems to be broken (by inspection - it's hard to test.)
      (js-node-put n 'statement (setq stmt (js-nest n #'js-stmt)))
      (js-node-put stmt 'label n))))
       
(defsubst js-named-defun ()
  "Parse/return AST node for a named, declared inner/top-level function."
  (js-defun t))

(defun js-defun (require-name &optional function-form)
  "Parse a function definition and return its AST node.

REQUIRE-NAME is nil for anonymous functions.

FUNCTION-FORM should be:
 'DECLARED_FORM for function foo(){...} in global or function scope
 'STATEMENT_FORM for function foo(){...} inside any kind of statement
 'EXPRESSED_FORM for function rvalues
If not specified, it chooses the appropriate form."
  (let ((f (js-node))
        (form (or function-form
                  (if (> (length js-stmt-stack) 1)
                      'STATEMENT_FORM
                    'DECLARED_FORM)))
        tt params)
    (if (neq (js-node-type f) 'FUNCTION)
        (js-node-put f 'type
                      (if (string= (js-node-value f) "get")
                          'GETTER 'SETTER)))
    (if (js-match 'IDENTIFIER)
        (js-node-put f 'name (js-node))
      (if require-name
          (js-parse-syntax-error "Missing function identifier")))
    (js-must-match 'LEFT_PAREN)
    (while (neq (setq tt (js-scan)) 'RIGHT_PAREN)
      (if (neq tt 'IDENTIFIER)
          (js-parse-syntax-error "Missing formal parameter"))
      (push (js-node) params)
      (if (neq (js-peek) 'RIGHT_PAREN)
          (js-must-match 'COMMA)))
    (if params
        (js-node-put f 'params (js-node 'PARAMS (nreverse params))))
    (js-must-match 'LEFT_CURLY)
    (js-node-put f 'body (js-script t))
    (js-must-match 'RIGHT_CURLY)
    (js-node-set-end f (js-token-end (js-current-token)))

    (js-node-put f 'function-form form)
    (if (eq form 'DECLARED_FORM)
        (setq js-fun-decls (nconc js-fun-decls (list f))))
    f))

(defun js-variables ()
  "Parse a variable declaration list"
  (let ((n (js-node))
        (continue t)
        n2)
  (while continue
    (js-must-match 'IDENTIFIER)
    (setq n2 (js-node))
    (js-node-put n2 'name (js-node-value n2))
    (when (js-match 'ASSIGN)
      (if (js-token-assign-op (js-current-token))
          (js-parse-syntax-error "Invalid variable initialization"))
      (js-node-put n2 'initializer (js-expr 'COMMA)))
    (if (eq (js-node-type n) 'CONST)
        (js-node-put n2 'read-only t))
    (js-node-push n n2)
    ;; should really accumulate in a list, then nreverse+nconc
    (setq js-var-decls (nconc js-var-decls (list n2)))
    (setq continue (js-match 'COMMA)))
    n))

;;; Expression parser

(eval-when-compile
  (defun js-reduce () nil))

;; to refactor this function, move operators/operands into defvars
(defun js-expr (&optional stop)
  (let ((bl js-bracket-level)
        (cl js-curly-level)
        (pl js-paren-level)
        (hl js-hook-level)
        n n2 id tt kid ; temps
        operators ; stack order is reversed from the Narcissus version
        operands) ; ditto

    ;; NOTE:  our operators/operands stacks push/pop at the list head!
    (defun js-reduce ()
      (catch 'return
        (let* ((n (pop operators))
               (op (js-node-type n))
               (arity (js-op-arity op))
               left end)
          (when (= arity -2)
            ;; Flatten left-associative trees.
            (setq left (and (>= (length operands) 2) (second operands)))
            (when (and left (eq (js-node-type left) op))
              (js-node-push left (pop operands))
              (throw 'return left))
            (setq arity 2))

          ;; remove arity operands from stack top and push onto operator node
          ;; in the order in which they were scanned
          (dolist (a (nreverse (loop repeat arity collect (pop operands))))
            (js-node-push n a))

          ;; Include closing bracket or postfix operator in [start,end).
          (setq end (js-token-end (js-current-token)))
          (if (< (js-node-end n) end)
              (js-node-set-end n end))

          (push n operands)
          n)))

    (catch 'break-loop
      (while (neq (setq tt (js-scan)) 'END)
        (and (eq tt stop)
             (= js-bracket-level bl)
             (= js-curly-level cl)
             (= js-paren-level pl)
             (= js-hook-level hl)
             (throw 'break-loop nil))
        (cond
          ((eq tt 'SEMICOLON) ;; NB: cannot be empty, Statement handled that.
           (throw 'break-loop nil))

          ((memq tt '(ASSIGN HOOK COLON))
           (if js-scan-operand
               (throw 'break-loop nil))
           ;; Use >, not >=, for right-associative ASSIGN and HOOK/COLON.
           (while (and operators
                       (or (> (js-op-precedence
                               (js-node-type (car operators)))
                              (js-op-precedence tt))
                           (and (eq tt 'COLON)
                                (eq (js-node-type (car operators))
                                    'ASSIGN))))
             (js-reduce))
           (cond
            ((eq tt 'COLON)
             (setq n (car-safe operators))
             (if (neq (js-node-type n) 'HOOK)
                 (js-parse-syntax-error "Invalid label"))
             (js-node-put n 'type 'CONDITIONAL)
             (decf js-hook-level))
            (t
             (push (js-node) operators)
             (if (eq tt 'ASSIGN)
                 (js-node-put (car operators) 'assign-op
                                (js-token-assign-op (js-current-token)))
               (incf js-hook-level))))
           (setq js-scan-operand t))

          ((memq tt '(IN
                      COMMA
                      ;; Treat comma as left-associative so reduce can
                      ;; fold left-heavy COMMA trees into a single array.
                      OR AND
                      BITWISE_OR BITWISE_XOR BITWISE_AND
                      EQ NE STRICT_EQ STRICT_NE
                      LT LE GE GT
                      INSTANCEOF
                      LSH RSH URSH
                      PLUS MINUS
                      MUL DIV MOD
                      DOT))
           (and (eq tt 'IN)
                ;; An in operator should not be parsed if we're parsing the
                ;; head of a for (...) loop, unless it is in the then part of
                ;; a conditional expression, or parenthesized somehow.
                js-in-for-loop-init
                (zerop js-hook-level)
                (zerop js-bracket-level)
                (zerop js-curly-level)
                (zerop js-paren-level)
                (throw 'break-loop nil))
           ;; fall-through cases
           (if js-scan-operand
               (throw 'break-loop nil))
           (while (and operators (>= (js-op-precedence
                                      (js-node-type (car operators)))
                                     (js-op-precedence tt)))
             (js-reduce))
           (cond
            ((eq tt 'DOT)
             (js-must-match 'IDENTIFIER)
             (push (js-node 'DOT (list (pop operands) (js-node)))
                   operands))
            (t
             (push (js-node) operators)
             (setq js-scan-operand t))))

          ((memq tt '(DELETE VOID TYPEOF NOT BITWISE_NOT
                      UNARY_PLUS UNARY_MINUS NEW))
           (unless js-scan-operand
             (throw 'break-loop nil))
           (push (js-node) operators))

          ((memq tt '(INCREMENT DECREMENT))
           (if js-scan-operand
               (push (js-node) operators) ;; prefix inc or dec
             ;; Use >, not >=, so postfix has higher precedence than prefix.
             (while (and operators (> (js-op-precedence
                                       (js-node-type (car operators)))
                                      (js-op-precedence tt)))
               (js-reduce))
             (setq n (js-node tt))
             (js-node-push n (pop operands))
             (js-node-put n 'postfix t)
             (push n operands)))

          ((eq tt 'FUNCTION)
           (unless js-scan-operand
             (throw 'break-loop nil))
           (push (js-defun nil 'EXPRESSED_FORM) operands)
           (setq js-scan-operand nil))

          ((memq tt '(NULL THIS TRUE FALSE IDENTIFIER NUMBER STRING REGEXP))
           (unless js-scan-operand
             (throw 'break-loop nil))
           (push (js-node) operands)
           (setq js-scan-operand nil))

          ((eq tt 'LEFT_BRACKET)
           (cond
            (js-scan-operand
             ;; Array initialiser.  Parse using recursive descent, as the
             ;; sub-grammar here is not an operator grammar.
             (setq n (js-node 'ARRAY_INIT))
             (catch 'break-while
               (while (neq (setq tt (js-peek)) 'RIGHT_BRACKET)
                 (if (eq tt 'COMMA)
                     (progn
                       (js-scan)
                       (js-node-push n nil))
                   (js-node-push n (js-expr 'COMMA))
                   (unless (js-match 'COMMA)
                     (throw 'break-while nil)))))
             (js-must-match 'RIGHT_BRACKET)
             (push n operands)
             (setq js-scan-operand nil))
            (t
             ;; Property indexing operator
             (push (js-node 'INDEX) operators)
             (setq js-scan-operand t)
             (incf js-bracket-level))))

          ((eq tt 'RIGHT_BRACKET)
           (if (or js-scan-operand
                   (= bl js-bracket-level))
               (throw 'break-loop nil))
           (while (neq (js-node-type (js-reduce)) 'INDEX)
             'continue)
           (decf js-bracket-level))

          ((eq tt 'LEFT_CURLY)
           (unless js-scan-operand
             (throw 'break-loop nil))
           ;; Object initialiser.  As for array initialisers (see above),
           ;; parse using recursive descent.
           (incf js-curly-level)
           (setq n (js-node 'OBJECT_INIT))
           (catch 'object-init
             (unless (js-match 'RIGHT_CURLY)
               (loop
                do
                (setq tt (js-scan))
                (cond
                 ((and (member (js-current-value) '("get" "set"))
                       (eq (js-peek) 'IDENTIFIER))
                  (if js-ecma-strict-mode
                      (js-parse-syntax-error "Illegal property accessor"))
                  (js-node-push n (js-defun t 'EXPRESSED_FORM)))
                 (t
                  (cond
                    ((memq tt '(IDENTIFIER NUMBER STRING))
                     (setq id (js-node)))
                    ((eq tt 'RIGHT_CURLY)
                     (if js-ecma-strict-mode
                         (js-parse-syntax-error "Illegal trailing ,"))
                     (throw 'object-init nil))
                    (t
                     (js-parse-syntax-error "Invalid property name")))
                  (js-must-match 'COLON)
                  (setq n2 (js-node 'PROPERTY_INIT))
                  (js-node-push n2 id)
                  (js-node-push n2 (js-expr 'COMMA))
                  (js-node-push n n2)))
                until
                (not (js-match 'COMMA)))
               (js-must-match 'RIGHT_CURLY)))
           (push n operands)
           (setq js-scan-operand nil)
           (decf js-curly-level))

          ((eq tt 'RIGHT_CURLY)
           (and (not js-scan-operand)
                (/= cl js-curly-level)
                (error "PANIC:  right curly botch"))
           (throw 'break-loop nil))

          ((eq tt 'LEFT_PAREN)
           (catch 'break-switch
             (if js-scan-operand
                 (push (js-node 'GROUP) operators)
               (while (and operators
                           (> (js-op-precedence
                               (js-node-type (car operators)))
                              (js-op-precedence 'NEW)))
                 (js-reduce))
               ;; Handle () now, to regularize the n-ary case for n > 0.
               ;; We must set scanOperand in case there are arguments and
               ;; the first one is a regexp or unary+/-.
               (setq n (car operators))
               (setq js-scan-operand t)
               (when (js-match 'RIGHT_PAREN)
                 (if (and n (eq (js-node-type n) 'NEW))
                     (progn
                       (pop operators)
                       (js-node-push n (pop operands)))
                   (setq n (js-node 'CALL
                                          (list (pop operands)
                                                (js-node 'LIST)))))
                 (push n operands)
                 (setq js-scan-operand nil)
                 (throw 'break-switch nil))

               (if (and n (eq (js-node-type n) 'NEW))
                   (js-node-put n 'type 'NEW_WITH_ARGS)
                 (push (js-node 'CALL) operators)))
             (incf js-paren-level)))

          ((eq tt 'RIGHT_PAREN)
           (if (or js-scan-operand
                   (= pl js-paren-level))
               (throw 'break-loop nil))
           (while (not (memq (setq tt (js-node-type (js-reduce)))
                             '(GROUP CALL NEW_WITH_ARGS)))
             'continue)
           (when (neq tt 'GROUP)
             (setq n (car operands))
             (setq kid (second (js-node-kids n)))
             (if (neq (js-node-type kid) 'COMMA)  ; 1 arg => put in own LIST
                 (setcar (nthcdr 1 (js-node-kids n))
                         (js-node 'LIST (list kid)))
               (js-node-put kid 'type 'LIST)))    ; change COMMA to LIST
           (decf js-paren-level))

          ;; Automatic semicolon insertion means we may scan across a newline
          ;; and into the beginning of another statement.  If so, break out of
          ;; the while loop and let the t.scanOperand logic handle errors.
          (t
           (throw 'break-loop nil)))))

    (if (/= hl js-hook-level)
        (js-parse-syntax-error "Missing : after ?"))
    (if (/= pl js-paren-level)
        (js-parse-syntax-error "Missing ) in parenthetical"))
    (if (/= bl js-bracket-level)
        (js-parse-syntax-error "Missing ] in index expression"))
    (if js-scan-operand
        (js-parse-syntax-error "Missing operand"))

    (setq js-scan-operand t)
    (js-unscan)
    (while (plusp (length operators))
      (js-reduce))
    (pop operands)))

(defsubst js-init-lexer (&optional cb)
  (setq js-cursor 1
        ;; clear this out to avoid getting cruft from previous parse
        js-token-ring (make-vector 4 nil)
        js-token-index 0
        js-lookahead 0
        js-scan-newlines nil
        js-scan-operand t
        js-current-line 1
        js-parse-hook cb))

(defun js-parse (buf &optional cb)
  "Tells the parser to parse a region of JavaScript.

BUF is a buffer or buffer name containing the code to parse.
Call `narrow-to-region' first to parse only part of the buffer.

The returned AST root node is given some additional properties:
  `node-count' - total number of nodes in the AST
  `buffer' - BUF.  The buffer it refers to may change or be killed,
             so the value is not necessarily reliable.

An optional callback CB can be specified to report parsing
progress.  If `(functionp CB)' returns t, it will be called with
the current line number once before parsing begins, then again
each time the lexer reaches a new line number.

CB can also be a list of the form `(symbol cb ...)' to specify
multiple callbacks with different criteria.  Each symbol is a
criterion keyword, and the following element is the callback to
call

  :line  - called whenever the line number changes
  :token - called for each new token consumed

The list of criteria could be extended to include entering or
leaving a statement, an expression, or a function definition."
  (if (and cb (not (functionp cb)))
      (error "criteria callbacks not yet implemented"))
  (let ((inhibit-point-motion-hooks t)
        (js--node-counter 0)
        ;; This is a recursive-descent parser, so give it a big stack.
        (max-lisp-eval-depth (max max-lisp-eval-depth 1500))
        (case-fold-search nil)
        ast)
    (save-excursion
      (set-buffer buf)
      (js-init-lexer cb)
      (setq ast
            (js-script))  ; parse
      (unless (js-eobp)
        (js-parse-syntax-error "Syntax error"))
      (js-node-put ast 'node-count js--node-counter)
      (js-node-put ast 'buffer buf)

      ;; Clean up, for now.  Might want to define macros for
      ;; js-with-new-{lexer|context} around forms.
      (js-init-lexer)
      ast)))

(defun js-parse-no-recover (buf &optional cb)
  "Do an `js-parse' but always throw syntax errors.
BUF and CB are the same as for `js-parse'.  This function
temporarily sets `js-recover-from-parse-errors' to t around
the call to `js-parse'."
  (letf ((js-recover-from-parse-errors nil))
    (js-parse buf cb)))

(defun js-parse-string (s)
  "Parses a string S of JavaScript.
The string S is saved in a property called `text' on the root
of the returned AST."
  (let ((js-is-eval-code t)
        (context js-current-context)
        ast)
    (with-temp-buffer
      ;; copy buffer-local context var into new temp buffer
      (setq js-current-context context)
      (insert s)
      (setq ast (js-parse (current-buffer)))
      (js-node-put ast 'buffer nil)
      (js-node-put ast 'text s))))

(defun js-parse-incremental (&optional parser)
  "Try parsing a statement at the current buffer position.
PARSER is the parsing function to try.  Defaults to `js-stmt'."
  (js-init-lexer)
  (save-excursion
    (letf* ((js-cursor (point))
            (js-current-line (line-number-at-pos js-cursor)))
      (funcall (or parser #'js-stmt)))))

(defun js-recover (err)
  "Try to recover from a parse error.
ERR is the list returned by `js-parse-parse-error'.
We return an AST error node, find a re-sync point, and set
the lexer there so we can keep parsing."
  ;; for now, just whine and try to recover
  (let* ((error-symbol (first err))
         (msg (second err))
         (node (third err))
         (n (js-node 'ERROR))
         (start (or (and node (js-node-start node))
                    (point)))
         (end (point)))
    (if msg (js-node-put n 'msg msg))
    (if node (js-node-push n node))
    (when (re-search-forward "[;}\n]" nil t)
      (setq end (point)))
    (js-node-set-start n start)
    (js-node-set-end n end)
    n))

;;; AST visiting and pretty-printing support

(defun js-node-all-props (node &optional predicate)
  "Return a list of indexed and named properties of NODE.
Props will be sorted by indexed first, then named.  Returns a
list of (id . value) pairs.  PREDICATE is an optional function
that takes a key and value, and returns non-nil if it should be
included in the results.  The default predicate skips 'kids
and 'parent."
  (let ((test (or predicate
                  (lambda (k v)
                    (not (memq k '(kids parent)))))))
    (nconc
     (loop for i from 0
           for kid in (js-node-kids node)
           collect (cons i kid) into kids
           finally return (sort* kids #'< :key #'car))
     (loop for (k . v) in node
           if (funcall test k v)
           collect (cons (symbol-name k) v) into p
           finally return (sort* p #'string< :key #'car)))))

(defsubst js-node-child-nodes (node)
  "Return a list of all the child nodes of NODE."
  (js-node-all-props node (lambda (k v)
                             (js-node-p v))))

(defun js-node-visit-subtree (node v &rest args)
  "Call visitor function V on all js-node children of NODE.
ARGS are any arguments to pass to V.  The visitor function V will
take a node, followed by any user-specified ARGS.  It should
return a non-nil value to continue processing.  If V returns nil
for a given node, that node's children will not be processed."
  (let ((seen (make-hash-table)))
    (prog1
        (js-node-visit-1 node seen v args)
      (clrhash seen)))) ; let gc clean up

(defun js-node-visit-1 (n seen v &rest args)
  (unless (gethash n seen)
    (puthash n t seen)
    (loop for (name . n2) in (js-node-child-nodes n) do
          (if (apply v n2 args)
              (dolist (kid (js-node-child-nodes n2))
                (js-node-visit-1 n2 seen v args))))))

(defun js-print-ast (ast)
  "Return a pretty string representation of AST.
Can be fairly large, but is usually more compact and readable than
the default output of the lisp printer.

Simple example usage, printing the AST of a 1-character script:
    (js-print-ast
       (js-parse-string \"3\"))

The string returned by this example:
{
    type: SCRIPT,
    start: 0,
    end: 2,
    0: {
        type: SEMICOLON,
        start: 1,
        end: 2,
        expression: {
            type: NUMBER,
            start: 1,
            end: 2,
            value: 3
        },
        value: 3
    },
    buffer: nil,
    node-count: 3,
    text: 3
}"
  (with-output-to-string
     (js-node-to-s-internal ast 0 (make-hash-table))))

(defun js-node-to-string (node)
  "Print the AST starting at NODE into a temp buffer.

Follows the Narcissus toString() representation to some extent, to aid
with debugging.

You can also just (insert (pp node)) to see the lisp forms, although
it can be up to twice as long because it re-prints all the subtrees
for var-decls and fun-decls.

Unless you bound `js-is-eval-code' to t during parsing, the AST will
not have recorded the text values of the nodes, and you will need to
wrap this call with a `set-buffer' to the JavaScript source buffer."
  (let ((buf (get-buffer-create "*js-output*")))
    (save-excursion
      (set-buffer buf)
      (toggle-read-only -1)
      (erase-buffer)
      (insert
       (js-node-to-s-internal node 0 (make-hash-table))
       buf)
      (pop-to-buffer buf))))

(defsubst js-spacer (indent &optional prop beg end)
  (if beg (princ beg))
  (princ "\n")
  (princ (make-string (* 4 indent) ? ))
  (if prop
      (princ (concat (if (numberp prop)
                         (number-to-string prop)
                       prop)
                     ": ")))
  (if end (princ end)))

(defun js-node-to-s-internal (node n seen)
  (let ((a (js-node-all-props node)))
    ;; print type, start, and end first, for easier reading
    (js-spacer (incf n) "type" "{"
                (prin1-to-string (js-node-type node)))
    (js-spacer n "start" ","
                (prin1-to-string (js-node-start node)))
    (js-spacer n "end" ","
                (prin1-to-string (js-node-end node)))
    (loop for (id . val) in a
          unless (member id '("type" "start" "end"))
          do
          (js-spacer n id ",")
          (cond
           ((js-node-p val)
            (if (gethash val seen)
                (progn
                  (princ (js-node-type val))
                  (princ " at line ")
                  (princ (js-node-line val)))
              (puthash val t seen)
              (js-node-to-s-internal val n seen)))
           (t
            (princ val))))
    (js-spacer (decf n) nil nil "}")))

;;; Implementation of toSource (i.e. printing AST nodes as JavaScript)
;;
;; (E)lisp makes it a royal pain in the arse to (a) pass in a list
;; and (b) push something on the front, because you also have to
;; (c) return it and (d) setq the variable in the caller.  Fuggers.
;; I'm using dynamic scoping to eliminate steps (c) and (d).
;; The alternative is to use my own list structure with a dummy header,
;; and push-front items after the header.  So dynamic scoping it is.
;; toSource() probably isn't used often enough to warrant something nicer.

(defvar js-indent-level 2
  "How many spaces to indent each nesting level")

(eval-when-compile
  (defvar result nil))

(defun js-node-to-source (node &optional one-line)
  "Return JavaScript source for NODE.
If ONE-LINE is non-nil, newlines are stripped from the result."
  (let ((result '()))
    (js-nts node 0)
    (setq result (mapconcat 'identity (nreverse result) ""))
    (if one-line
        (js-node-flatten-source result)
      result)))

;; This is lame, but I hate the idea of special-casing the whitespace
;; at every point in js-nts below, so I'll have to think of a better way.
(defun js-node-flatten-source (s)
  "Convert S to a single source line."
  (setq s (replace-regexp-in-string "\\([{(]\\)[ \t\r\n]+" "\\1" s))
  (setq s (replace-regexp-in-string "[ \t\r\n]+\\([})]\\)" "\\1" s))
  (setq s (replace-regexp-in-string "\n" "" s))
  (setq s (replace-regexp-in-string " +" " " s))
  s)

(defsubst js-source-indent (amt)
  (make-string (* js-indent-level amt) ? ))

(defsubst js-nts-add (obj)
  (push obj result))

(defsubst js-nts-add-node (obj indent)
  (js-nts obj indent))

(defsubst js-nts-add-kid (obj kidnum indent)
  (js-nts (js-node-kid obj kidnum) indent))

(defsubst js-nts-add-kids (obj indent sep &optional last-sep)
  (if (js-node-kids obj)
      (loop with kids = (js-node-kids obj)
            with len = (length kids)
            for i from 0
            for kid in kids do
            (js-nts kid indent)
            (if (or last-sep (< i (1- len)))
                (js-nts-add sep)))
    (js-nts obj indent)
    (if last-sep
        (js-nts-add sep))))

(defsubst js-nts-indent (s n)
  "Indent string S by a multiple of N space chars.
If S is nil, just pushes the whitespace."
  (unless (or (zerop n)
              ;; bit of a hack - don't indent unless we just
              ;; printed a newline
              (and (car result)
                   (/= (aref (car result)
                             (1- (length (car result))))
                       ?\n)))
    (push (js-source-indent n) result))
  (if s (push s result)))
      
(defun js-nts (node n)
  "Collect NTS (node-to-source) for NODE at depth N.
Pushes the pieces onto the dynamic variable `result'."
  (let ((tt (js-node-type node))
        tmp)
    (case tt
      ((FUNCTION GETTER SETTER)
       (js-nts-function-to-source node n))
      ((SCRIPT BLOCK)
       (loop for kid in (js-node-kids node) do
             (js-nts kid n)))
      (IF
       (js-nts-indent "if (" n)
       (js-nts (js-node-get node 'condition) n)
       (js-nts-add ") {\n")
       (js-nts-add-kids (js-node-get node 'then-part) (1+ n) "\n" t)
       (js-nts-indent "}" n)
       (when (setq tmp (js-node-get node 'else-part))
         (js-nts-add " else {\n")
         (js-nts-add-kids tmp (1+ n) "\n" t)
         (js-nts-indent "}" n)))
      (SWITCH
       (js-nts-indent "switch (" n)
       (js-nts (js-node-get node 'discriminant) n)
       (js-nts-add ") {\n")
       (loop for case in (js-node-kids (js-node-get node 'cases))
             for type = (js-node-get case 'type) do
             (js-nts-indent (if (eq type 'CASE)
                                (format "case %s" ; in case it's a number
                                        (js-node-value
                                         (js-node-get case 'case-label)))
                              "default")
                            (1+ n))
             (js-nts-add ":\n")
             (loop for kid in (js-node-kids
                               (js-node-get case 'statements))
                   do
                   (js-nts kid (+ 2 n))
                   (js-nts-add "\n")))
       (js-nts-indent "}" n))
      (FOR
       (js-nts-indent "for (" n)
       (js-nts (setq tmp (js-node-get node 'setup)) n)
       (if (eq (js-node-type tmp) 'VAR)  ; adds semi for us
           (js-nts-add " ")
         (js-nts-add "; "))
       (js-nts (js-node-get node 'condition) n)
       (js-nts-add "; ")
       (js-nts (js-node-get node 'update) n)
       (js-nts-add ") {\n")
       (js-nts-add-kids (js-node-get node 'body) (1+ n) "\n" t)
       (js-nts-indent "}" n))
      (WHILE
       (js-nts-indent "while (" n)
       (js-nts (js-node-get node 'condition) n)
       (js-nts-add ") {\n")
       (js-nts-add-kids (js-node-get node 'body) (1+ n) "\n" t)
       (js-nts-indent "}" n))
      (FOR_IN
       (js-nts-indent "for (" n)
       (if (js-node-get node 'var-decl)
           (js-nts-add "var "))
       (js-nts (js-node-get node 'iterator) n)
       (js-nts-add " in ")
       (js-nts (js-node-get node 'object) n)
       (js-nts-add ") {\n")
       (js-nts-add-kids (js-node-get node 'body) (1+ n) "\n" t)
       (js-nts-indent "}" n))
      (DO
       (js-nts-indent "do {\n" n)
       (js-nts-add-kids (js-node-get node 'body) (1+ n) "\n" t)
       (js-nts-indent "} while (" n)
       (js-nts (js-node-get node 'condition) 0)
       (js-nts-add ");"))
      ((BREAK CONTINUE)
       (js-nts-indent (downcase (symbol-name tt)) n)
       (when (and (setq tmp (js-node-get node 'target))
                  (eq (js-node-type tmp) 'LABEL))
         (js-nts-add " ")
         (js-nts-add (js-node-get tmp 'label)))
       (js-nts-add ";"))
      (TRY
       (js-nts-indent "try {\n" n)
       (js-nts-add-kids (js-node-get node 'try-block) (1+ n) "\n" t)
       (js-nts-indent "}" n)
       (loop with clauses = (js-node-kids
                             (js-node-get node 'catch-clauses))
             with num-clauses = (length clauses)
             with finally-block = (js-node-get node 'finally-block)
             with guard = nil
             for i from 0
             for c in clauses do
             (js-nts-add " catch (")
             (js-nts-add (js-node-get c 'var-name))
             (when (setq guard (js-node-get c 'guard))
               (js-nts-add " if ")
               (js-nts guard 0))
             (js-nts-add ") {\n")
             (js-nts-add-kids (js-node-get c 'block) (1+ n) "\n" t)
             (js-nts-indent "}" n)
             finally do
             (if finally-block
                 (progn
                   (js-nts-add " finally {\n")
                   (js-nts-add-kids finally-block (1+ n) "\n" t)
                   (js-nts-indent "}\n" n))
               (js-nts-add "\n"))))
      (THROW
       (js-nts-indent "throw " n)
       (js-nts (js-node-get node 'exception) n)
       (js-nts-add ";"))
      (RETURN
       (js-nts-indent "return" n)
       (when (neq (setq tmp (js-node-value node)) 'undefined)
         (js-nts-add " ")
         (js-nts tmp n))
       (js-nts-add ";"))
      (WITH
       (js-nts-indent "with (" n)
       (js-nts (js-node-get node 'object) 0)
       (js-nts-add ") {\n")
       (js-nts-add-kids (js-node-get node 'body) (1+ n) "\n" t)
       (js-nts-indent "}" n))
      ((VAR CONST)
       (js-nts-indent (downcase (symbol-name tt)) n)
       (js-nts-add " ")
       (loop with kids = (js-node-kids node)
             with len = (length kids)
             for kid in kids
             for i from 0
             for value = (js-node-get kid 'initializer) do
             (js-nts-add (js-node-get kid 'name))
             (when value
               (js-nts-add " = ")
               (js-nts value n))
             (if (< i (1- len))
                 (js-nts-add ", ")))
       (js-nts-add ";"))
      (DEBUGGER
       (js-nts-indent "debugger;" n))
      (SEMICOLON
       (js-nts-indent nil n)
       (js-nts (js-node-get node 'expression) n)
       (js-nts-add ";"))
      (LABEL
       (js-nts-indent nil (1- n))                  ; outdent
       (js-nts-add (js-node-get node 'label))
       (js-nts-add ":\n")
       (js-nts (js-node-get node 'statement) n))
      (COMMA
       (js-nts-add-kids node n ", "))
      (ASSIGN
       (js-nts-add-kid node 0 n)
       (js-nts-add " ")
       (if (setq tmp (js-node-get node 'assign-op))
           (js-nts-add (gethash tmp js-token-names)))
       (js-nts-add "=")
       (js-nts-add " ")
       (js-nts-add-kid node 1 n))
      (CONDITIONAL
       (js-nts-add-kid node 0 n)
       (js-nts-add " ? ")
       (js-nts-add-kid node 1 n)
       (js-nts-add " : ")
       (js-nts-add-kid node 2 n))
      ((AND OR BITWISE_OR BITWISE_XOR BITWISE_AND
            EQ NE STRICT_EQ STRICT_NE LT LE GT GE
            LSH RSH URSH PLUS MINUS MUL DIV MOD)
       (js-nts-add-kid node 0 n)
       (js-nts-add " ")
       (js-nts-add (gethash tt js-token-names))
       (js-nts-add " ")
       (js-nts-add-kid node 1 n))
      ((IN INSTANCEOF)
       (js-nts-add-kid node 0 n)
       (js-nts-add " ")
       (js-nts-add (downcase (symbol-name tt)))
       (js-nts-add " ")
       (js-nts-add-kid node 1 n))
      ((DELETE VOID TYPEOF)
       (js-nts-add (downcase (symbol-name tt)))
       (js-nts-add " ")
       (js-nts-add-kid node 0 n))
      ((NOT BITWISE_NOT)
       (js-nts-add (gethash tt js-token-names))
       (js-nts-add-kid node 0 n))
      (UNARY_PLUS
       (js-nts-add "+")
       (js-nts-add-kid node 0 n))
      (UNARY_MINUS
       (js-nts-add "-")
       (js-nts-add-kid node 0 n))
      ((INCREMENT DECREMENT)
       (setq tmp (if (eq tt 'INCREMENT) "++" "--"))
       (unless (js-node-get node 'postfix) (js-nts-add tmp))
       (js-nts-add-kid node 0 n) ; could push many strings
       (when (js-node-get node 'postfix) (js-nts-add tmp)))
      (DOT
       (js-nts-add-kid node 0 n)
       (js-nts-add ".")
       (js-nts-add-kid node 1 n))
      (INDEX
       (js-nts-add-kid node 0 n)
       (js-nts-add "[")
       (js-nts-add-kid node 1 n)
       (js-nts-add "]"))
      (CALL
       (js-nts-add-kid node 0 n)
       (js-nts-add "(")
       (js-nts-add-kids (js-node-kid node 1) n ", ")
       (js-nts-add ")"))
      ((NEW NEW_WITH_ARGS)
       (js-nts-add "new ")
       (js-nts-add-kid node 0 n)
       (js-nts-add "(")
       (when (eq tt 'NEW_WITH_ARGS)
         (js-nts-add-kids (js-node-kid node 1) n ", "))
       (js-nts-add ")"))
      (ARRAY_INIT
       (js-nts-add "[")
       (if (js-node-kids node)
           (js-nts-add-kids node n ", "))
       (js-nts-add "]"))
      (OBJECT_INIT
       (js-nts-add "{")
       (loop with kids = (js-node-kids node)
             with len = (length kids)
             for i from 0
             for k in kids
             for tt = (js-node-type k)
             if (eq tt 'PROPERTY_INIT) do
               (js-nts-add-kid k 0 n)
               (js-nts-add ": ")
               (js-nts-add-kid k 1 n)
             else do
               (js-nts-add-kid k 0 n)              ; getter/setter
             end
             if (< i (1- len))
             do (js-nts-add ", "))
       (js-nts-add "}"))
      ((NULL THIS TRUE FALSE)
       (js-nts-add (downcase (symbol-name tt))))
      (IDENTIFIER
       (js-nts-add (js-node-value node)))
      (NUMBER
       (js-nts-add (number-to-string (js-node-value node))))
      (STRING
       (js-nts-add "\"")
       (js-nts-add (replace-regexp-in-string
             "\"" "\\\"" (js-node-value node) nil t))
       (js-nts-add "\""))
      (REGEXP
       (js-nts-add (js-node-value node)))
      (GROUP
       (js-nts-add "(")
       (js-nts-add-kid node 0 n)
       (js-nts-add ")"))
      (t
       (format "unknown node type: %s" tt)))))

(defun js-nts-function-to-source (node n)
  "Returns the source for NODE as a list of strings, reversed.
N is the current indent level."
  (let* ((form (js-node-get node 'function-form))
         (declared (eq form 'DECLARED_FORM))
         (expressed (eq form 'EXPRESSED_FORM))
         (statement (eq form 'STATEMENT_FORM))
         (type (js-node-type node))
         (id (js-node-get node 'name))
         (name (if id (js-node-value id) "")))
    (if declared
        (js-nts-indent nil n))
    (case type
      (FUNCTION
       (js-nts-add "function "))
      (GETTER
       (js-nts-add "get "))
      (SETTER
       (js-nts-add "set ")))
    (if name
        (js-nts-add name))
    (js-nts-add "(")
    (loop with kids = (js-node-kids (js-node-get node 'params))
          with len = (length kids)
          for i from 0
          for kid in kids do
          (js-nts-add (js-node-value kid))
          (if (< i (1- len))
              (js-nts-add ", ")))
    (js-nts-add ") {\n")
    (js-nts-add-kids (js-node-get node 'body) (1+ n) "\n" t)
    (js-nts-indent "}" n)))

(provide 'js-parse)

;;; js-parse.el ends here
