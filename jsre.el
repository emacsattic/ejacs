;;; jsre.el -- ECMAScript-compliant regular expressions

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
;; This engine is a port of the Mozilla Rhino regular expression code,
;; which Norris Boyd ported from Brendan Eich's SpiderMonkey regexp
;; engine.
;;
;; The engine supports two modes of operation:
;;  - compiling to custom bytecode for later interpretation
;;  - translating the input regexp into an emacs-lisp regexp string
;;
;; As far as I can tell, most JavaScript regexps can be rewritten as
;; equivalent elisp regexps, with the following exceptions:
;;
;;   - zero-width positive/negative lookahead assertions
;;   - \D, \S or \W (negated classes) in a [...] character class
;;
;; An error is thrown if you try to generate an elisp regexp with
;; one of these unsupported features.

;;; Bugs:
;;
;; Elisp conversion problems:
;;  - double-check fixing up escapes inside char classes
;;  - make a unit-test suite just for elisp conversion (lots of cases)

;;; TODO:
;; - make a global cache for the compiled regexps
;; - finish wiring up js-runtime to call jsre-execute-re-bytecode
;; - test jsre-execute-re-bytecode thoroughly
;; - permit matching against a buffer
;; - see about compiling directly to elisp bytecode
;; - run elp profiler on the code

;;; Code:

(eval-and-compile
  (require 'cl))

(require 'js-util)

(defconst JSREG_GLOB #x1  "g flag: global")
(defconst JSREG_FOLD #x2 "i flag: fold case")
(defconst JSREG_MULTILINE #x4 "m flag: multiline")

;; type of match to perform
(defconst JSRE_TEST 0)
(defconst JSRE_MATCH 1)
(defconst JSRE_PREFIX 2)

(defconst jsre-msg-bad-quant "Invalid quantifier %s")
(defconst jsre-msg-bad-range "Invalid range in character class.")
(defconst jsre-msg-max-lt-min "Maximum %s less than minimum")
(defconst jsre-msg-overlarge-backref "Overly large back reference %s")
(defconst jsre-msg-overlarge-max "Overly large maximum %s")
(defconst jsre-msg-overlarge-min "Overly large minimum %s")
(defconst jsre-msg-trail-backslash "Trailing \\ in regular expression.")
(defconst jsre-msg-unmatched-right-paren "unmatched ) in regular expression.")
(defconst jsre-msg-unterm-class "Unterminated character class %s")
(defconst jsre-msg-unterm-paren "Unterminated parenthetical %s")

(defconst REOP_EMPTY          0  "Match rest of input against rest of r.e.")
(defconst REOP_ALT            1  "alternative subexpressions in kid and next")
(defconst REOP_BOL            2  "beginning of input (or line if multiline)")
(defconst REOP_EOL            3  "end of input (or line if multiline)")
(defconst REOP_WBDRY          4  "match '' at word boundary")
(defconst REOP_WNONBDRY       5  "match '' at word non-boundary")
(defconst REOP_QUANT          6  "quantified atom: atom{1,2}")
(defconst REOP_STAR           7  "zero or more occurrences of kid")
(defconst REOP_PLUS           8  "one or more occurrences of kid")
(defconst REOP_OPT            9  "optional subexpression in kid")
(defconst REOP_LPAREN         10 "left paren bytecode: kid is u.num'th sub-regexp")
(defconst REOP_RPAREN         11 "right paren bytecode")
(defconst REOP_DOT            12 "stands for any character")
(defconst REOP_CCLASS         13 "character class: [a-f]")
(defconst REOP_DIGIT          14 "match a digit char: [0-9]")
(defconst REOP_NONDIGIT       15 "match a non-digit char: [^0-9]")
(defconst REOP_ALNUM          16 "match an alphanumeric char: [0-9a-z_A-Z]")
(defconst REOP_NONALNUM       17 "match a non-alphanumeric char: [^0-9a-z_A-Z]")
(defconst REOP_SPACE          18 "match a whitespace char")
(defconst REOP_NONSPACE       19 "match a non-whitespace char")
(defconst REOP_BACKREF        20 "back-reference (e.g., \1) to a parenthetical")
(defconst REOP_FLAT           21 "match a flat string")
(defconst REOP_FLAT1          22 "match a single char")
(defconst REOP_JUMP           23 "for deoptimized closure loops")
(defconst REOP_DOTSTAR        24 "optimize .* to use a single opcode")
(defconst REOP_ANCHOR         25 "like .* but skips left context to unanchored r.e.")
(defconst REOP_EOLONLY        26 "$ not preceded by any pattern")
(defconst REOP_UCFLAT         27 "flat Unicode string; len immediate counts chars")
(defconst REOP_UCFLAT1        28 "single Unicode char")
(defconst REOP_UCCLASS        29 "Unicode character class, vector of chars to match")
(defconst REOP_NUCCLASS       30 "negated Unicode character class")
(defconst REOP_BACKREFi       31 "case-independent REOP_BACKREF")
(defconst REOP_FLATi          32 "case-independent REOP_FLAT")
(defconst REOP_FLAT1i         33 "case-independent REOP_FLAT1")
(defconst REOP_UCFLATi        34 "case-independent REOP_UCFLAT")
(defconst REOP_UCFLAT1i       35 "case-independent REOP_UCFLAT1")
(defconst REOP_ANCHOR1        36 "first-char discriminating REOP_ANCHOR")
(defconst REOP_NCCLASS        37 "negated 8-bit character class")
(defconst REOP_DOTSTARMIN     38 "ungreedy version of REOP_DOTSTAR")
(defconst REOP_LPARENNON      39 "non-capturing version of REOP_LPAREN")
(defconst REOP_RPARENNON      40 "non-capturing version of REOP_RPAREN")
(defconst REOP_ASSERT         41 "zero width positive lookahead assertion")
(defconst REOP_ASSERT_NOT     42 "zero width negative lookahead assertion")
(defconst REOP_ASSERTTEST     43 "sentinel at end of assertion child")
(defconst REOP_ASSERTNOTTEST  44 "sentinel at end of !assertion child")
(defconst REOP_MINIMALSTAR    45 "non-greedy version of *")
(defconst REOP_MINIMALPLUS    46 "non-greedy version of +")
(defconst REOP_MINIMALOPT     47 "non-greedy version of ?")
(defconst REOP_MINIMALQUANT   48 "non-greedy version of {}")
(defconst REOP_ENDCHILD       49 "sentinel at end of quantifier child")
(defconst REOP_CLASS          50 "character class with index")
(defconst REOP_REPEAT         51 "directs execution of greedy quantifier")
(defconst REOP_MINIMALREPEAT  52 "directs execution of non-greedy quantifier")
(defconst REOP_SHY_GROUP      53 "non-capturing paren group")
(defconst REOP_END            54)

(defvar jsre-byte-codes (make-hash-table :test 'eq)
  "Lookup table from jsre bytecode values to their names.")
(dolist (sym (apropos-internal "^REOP_"))
  (puthash (symbol-value sym) sym jsre-byte-codes))

(defstruct jsre-compiled-re  ; RECompiled
  source            ; {String} locked source string, sans //
  (paren-count 0)   ; {int} number of parenthesized submatches
  (flags 0)         ; {int} flags
  program           ; {byte[]} regular expression bytecode
  (class-count 0)   ; {int} count [...] bitmaps
  class-list        ; {jsre-charset[]} list of [...] bitmaps
  (anchor-char -1)  ; {char} if >= 0, then re starts with this literal char
  elisp)            ; {string} source rewritten as elisp regexp, if possible

(defstruct jsre-node  ; RENode
  op          ; {byte}  r.e. op bytecode

  next        ; {jsre-node} next in concatenation order
  kid         ; {jsre-node} first operand
  kid2        ; {jsre-node} second operand

  ;; could be a number
  num         ; {int}
  ;; or a parenthesis index
  paren-index ; {int}

  ;; or a range
  min         ; {int}
  max         ; {int}

  paren-count ; {int}
  greedy      ; {boolean}

  ;; or a character class
  start-index ; {int}
  kidlen      ; {int} length of string at kid, in chars
  bmsize      ; {int} bitmap size, based on max char code
  index       ; {int} index into class list

  ;; or a literal sequence
  chr         ; {char} of one character
  length      ; {int} or many (via the index)
  flat-index) ; {int} which is -1 if not sourced

;; RE Compiler context vars:  buffer-local temp compiler state

(defvar jsre-state-cpbegin        ; string
  "The source pattern for the regexp being compiled.")
(make-variable-buffer-local 'jsre-state-cpbegin)

(defvar jsre-state-cpend          ; int
  "Length of the regexp source string.")
(make-variable-buffer-local 'jsre-state-cpend)

(defvar jsre-state-cp             ; int
  "Current index into the source string.")
(make-variable-buffer-local 'jsre-state-cp)

(defvar jsre-state-flags          ; int
  "Flag spec (as a bitfield) for regexp being compiled.")
(make-variable-buffer-local 'jsre-state-flags)

(defvar jsre-state-paren-count)   ; int
(make-variable-buffer-local 'jsre-state-paren-count)

(defvar jsre-state-paren-nesting) ; int
(make-variable-buffer-local 'jsre-state-paren-nesting)

(defvar jsre-state-class-count    ; int
  "Number of [...] char classes encountered so far.")
(make-variable-buffer-local 'jsre-state-class-count)

(defvar jsre-state-prog-length    ; int
  "Estimated bytecode length")
(make-variable-buffer-local 'jsre-state-prog-length)

(defvar jsre-state-result)        ; jsre-node
(make-variable-buffer-local 'jsre-state-result)

(defstruct jsre-progstate  ; REProgState
  previous   ; previous state in stack
  min        ; current quantifier min
  max        ; current quantifier max
  index      ; progress in text
  cont-op    ; continuation op
  cont-pc    ; continuation pc
  backtrack) ; used by ASSERT_ to recover state

(defstruct jsre-btdata  ; REBackTrackData
  "Don't use this directly; call `jsre-make-backtrack-data'"
  previous
  cont-op          ; where to backtrack to
  cont-pc
  last-paren
  parens           ; parenthesis captures
  cp               ; char buffer index
  state-stack-top) ; state of op that backtracked

(defstruct jsre-gdata  ; REGlobalData
  multiline            ; {boolean}
  regexp               ; {jsre-compiled-re}  the RE in execution
  last-paren           ; {int} highest paren set so far
  skipped              ; {int} chars skipped anchoring this RE
  cp                   ; {int} char buffer index
  parens               ; {long[]} parens captures
  state-stack-top      ; {jsre-progstate} stack of state of current ancestors
  backtrack-stack-top) ; {jsre-btdata} last matched-so-far position

(defsubst jsre-gdata-parens-index (gdata i)
  "Get start of parenthesis capture contents. -1 for empty."
  (elt (jsre-gdata-parens gdata) i))

(defsubst jsre-gdata-parens-length (gdata i)
  "Get length of parenthesis capture contents."
  (ash (elt (jsre-gdata-parens gdata) i) -32))  ; (parens[i] >>> 32)

(defsubst jsre-gdata-set-parens (gdata i index length)
  ;; parens[i] = ((long)index & 0xffffffffL) | ((long)length << 32)
  (aset (jsre-gdata-parens gdata)
        i
        (logior (logand index #xffffffff)
                (lsh length 32))))

(defun jsre-make-backtrack-data (global-data op pc)
  "Construct a new jsre-btdata"
  (let ((data (make-jsre-btdata)))
    (setf (jsre-btdata-previous data)
          (jsre-gdata-backtrack-stack-top global-data))
    (setf (jsre-btdata-cont-op data) op)
    (setf (jsre-btdata-cont-pc data) pc)
    (setf (jsre-btdata-last-paren data) (jsre-gdata-last-paren global-data))
    (if (jsre-gdata-parens global-data)
        (setf (jsre-btdata-parens data)
              (copy-sequence (jsre-gdata-parens global-data))))
    (setf (jsre-btdata-cp data) (jsre-gdata-cp global-data))
    (setf (jsre-btdata-state-stack-top data)
          (jsre-gdata-state-stack-top global-data))
    data))

(defstruct jsre-charset
  "Holds a bitmap representation of a class from a regexp.
There's a list of these referenced by the class-list field in
the jsre-compiled-re struct.  The initial state has start-index
set to the offset in the original regexp source of the beginning
of the class contents.  The first use of the class converts the
source representation into a bitmap."
  length
  start-index
  str-length
  converted ; {boolean}
  sense     ; {boolean}
  bits)     ; {bool-vector}

(defun jsre-init-compiler-state (src len flags)
  "Initialize all the buffer-locals for compiling a regexp.
SRC is the regexp source, LEN is its length, and FLAGS are
the regexp flags, as a bitfield."
  (setq jsre-state-cpbegin src
        jsre-state-cp 0
        jsre-state-cpend len
        jsre-state-flags flags
        jsre-state-paren-count 0
        jsre-state-class-count 0
        jsre-state-prog-length 0
        jsre-state-paren-nesting 0
        jsre-state-result nil))

(defun jsre-error (msg &optional value)
  "Signal a lisp error with MSG.
If VALUE is non-nil, uses MSG as a format string."
  (if value
      (setq msg (format msg value)))
  (error msg))

(defsubst jsre-get-index (array pc)
  (logior (lsh (logand (aref array pc) #xFF) 8)
          (logand (aref array (1+ pc)) #xFF)))

(defsubst jsre-done ()
  "Return t if jsre-state-cp >= jsre-state-cpend"
  (>= jsre-state-cp jsre-state-cpend))

(defsubst jsre-peek (src)
  "Return current char."
  (aref src jsre-state-cp))

(defsubst jsre-scan (src)
  "Return current char and (then) increment buffer pointer"
  (aref src (++ jsre-state-cp)))

;; Port of static NativeRegExp.compileRE
(defun jsre-compile (str global &optional flat)
  "Return a compiled regexp from STR.
GLOBAL is an optional flags string (or int flags).
FLAT is non-nil for a flat match; i.e. no metacharacters."
  (let ((re (make-jsre-compiled-re))
        (len (length str))
        (flags 0)
        end-pc)
    (setf (jsre-compiled-re-source re) str)
    (when global
     (if (stringp global)
         (loop for c across global do
               (setq flags (logior flags
                                   (cond
                                    ((= c ?g) JSREG_GLOB)
                                    ((= c ?i) JSREG_FOLD)
                                    ((= c ?m) JSREG_MULTILINE)
                                    (t
                                     (jsre-error
                                      (concat "Invalid regexp flag: "
                                              (string c))))))))
       (setq flags global)))
    (setf (jsre-compiled-re-flags re) flags)

    (jsre-init-compiler-state str len flags)

    (catch 'done  ; enable early return from first conditional below
      (if (and flat (plusp len))
          (progn
            (setq jsre-state-result (make-jsre-node :op REOP_FLAT
                                                    :chr (aref str 0)
                                                    :length len
                                                    :flat-index 0))
            (incf jsre-state-prog-length 5))
        (unless (jsre-parse-disjunction)
          (throw 'done nil))) ; early return (parse failed)

      ;; initialize bytecode array to -1 (invalid op)
      (setf (jsre-compiled-re-program re)
            (make-vector (1+ jsre-state-prog-length) -1))

      (unless (zerop jsre-state-class-count)
        (setf (jsre-compiled-re-class-list re)
              (loop repeat jsre-state-class-count
                    vconcat (list (make-jsre-charset))))
        (setf (jsre-compiled-re-class-count re) jsre-state-class-count))

      ;; generate bytecode
      (setq end-pc (jsre-emit-re-bytecode re 0 jsre-state-result))
      (setf (aref (jsre-compiled-re-program re) end-pc) REOP_END)

      (setf (jsre-compiled-re-paren-count re)
            jsre-state-paren-count)

      ;; if re starts with literal, init anchor-char accordingly
      (case (aref (jsre-compiled-re-program re) 0)
        ((REOP_UCFLAT1 REOP_UCFLAT1i)
         (setf (jsre-compiled-re-anchor-char re)
               (jsre-get-index (jsre-compiled-re-program re) 1)))
        ((REOP_FLAT1 REOP_FLAT1i)
         (setf (jsre-compiled-re-anchor-char re)
               (logand #xff (aref (jsre-compiled-re-program re) 1))))
        ((REOP_FLAT REOP_FLATi)
         (setf (jsre-compiled-re-anchor-char re)
               (aref (jsre-compiled-re-source re)
                     (jsre-get-index (jsre-compiled-re-program re) 1)))))

      ;; try to generate a compatible elisp regexp
      (setf (jsre-compiled-re-elisp re)
            (ignore-errors
              (jsre-generate-elisp jsre-state-result)))
      re)))

(defsubst jsre-digit-p (c)
  (and (<= ?0 c) (<= c ?9)))

;; TODO:  fix for Unicode
(defsubst jsre-letter-p (c)
  (or (and (<= ?a c) (<= c ?z))
      (and (<= ?A c) (<= c ?Z))))

;; TODO:  fix for Unicode
(defsubst jsre-word-p (c)
  "Return t if the specified character C is a letter, digit, or underscore."
  (or (jsre-digit-p c)
      (and (<= ?a c) (<= c ?z))
      (and (<= ?A c) (<= c ?Z))
      (= c ?_)))

(defsubst jsre-line-term-p (c)
  (or (= c ?\n) (= c ?\r)
      (= c #x2028)
      (= c #x2029)))

;; TODO:  fix for Unicode
(defsubst jsre-re-whitespace-p (c)
  (memq c '(#x20 #x09 ?\n ?\r
            #x2028 #x2029
            #x0C #x0B #xA0)))

(defsubst jsre-to-hex-digit (c)
  (cond
   ((< c ?0) -1)
   ((<= c ?9) (- c ?0))
   (t
    (setq c (logior c #x20))
    (if (and (<= ?a c) (<= c ?f))
        (+ c (- ?a) 10)
      -1))))

;; regexp:   altern
;;           altern '|' regexp
(defun jsre-parse-disjunction ()
  "Topmost production of top-down regexp grammar.
A regular expression is one or more alternatives separated by a
vertical bar."
  (catch 'done
    (unless (jsre-parse-alternative)
      (throw 'done nil))
    (let ((source jsre-state-cpbegin)
          (index jsre-state-cp)
          alt-result)
      (when (and (/= index (length source))
                 (= (aref source index) ?|))
        (incf jsre-state-cp)
        (setq alt-result (make-jsre-node :op REOP_ALT))
        (setf (jsre-node-kid alt-result) jsre-state-result)
        (unless (jsre-parse-disjunction)
          (throw 'done nil))
        (setf (jsre-node-kid2 alt-result) jsre-state-result)
        (setq jsre-state-result alt-result)
        ;; ALT, <next>, ..., JUMP, <end> ... JUMP <end>
        (incf jsre-state-prog-length 9))
      t)))

;; altern:   item
;;           item altern
(defun jsre-parse-alternative ()
  "An alternative is one or more items concatenated together.
Returns t if parse succeeded, else nil."
  (let ((head-term nil)
        (tail-term nil)
        (src jsre-state-cpbegin))
    (catch 'break
      (while t
        (when (or (jsre-done)
                  (= (jsre-peek src) ?|)
                  (and
                   (/= 0 jsre-state-paren-nesting)
                   (= (jsre-peek src) ?\))))
          (setq jsre-state-result
                (if (null head-term)
                    (make-jsre-node :op REOP_EMPTY)
                  head-term))
          (throw 'break t))
        (unless (jsre-parse-term)
          (throw 'break nil))
        (cond
         ((null head-term)
          (setq head-term jsre-state-result))
         ((null tail-term)
          (setf (jsre-node-next head-term) jsre-state-result)
          (setq tail-term jsre-state-result)
          (while (jsre-node-next tail-term)
            (setq tail-term (jsre-node-next tail-term))))
         (t
          (setf (jsre-node-next tail-term) jsre-state-result)
          (setq tail-term (jsre-node-next tail-term))
          (while (jsre-node-next tail-term)
            (setq tail-term (jsre-node-next tail-term)))))))))

(defun jsre-calculate-bitmap-size (target src index end)
  "Calculate the total size of the bitmap required for a class expression.
TARGET is a jsre-node.  SRC is a string.  INDEX and END are ints."
  (let ((range-start 0)
        (max 0)
        (in-range nil)
        i c n
        digit nDigits
        local-max
        cu cd)
    (catch 'done
      (setf (jsre-node-bmsize target) 0)

      (if (= index end)
          (throw 'done t))

      (if (= (aref src index) ?^)
          (incf index))

      (catch 'break
        (while (/= index end)
          (catch 'continue
            (setq local-max 0
                  nDigits 2)
            (if (/= (aref src index) ?\\)
                (setq local-max (aref src (++ index)))
              ;; escaped character inside char class
              (setq c (aref src (incf index)))
              (incf index)
              (case c
                (?b (setq local-max #x8))
                (?f (setq local-max #xC))
                (?n (setq local-max #xA))
                (?r (setq local-max #xD))
                (?t (setq local-max #x9))
                (?v (setq local-max #xB))
                (?c
                 (if (and (< (1+ index) end)
                          (jsre-letter-p (aref src (1+ index))))
                     (setq local-max (logand #x1F (aref src (++ index))))
                   (setq local-max ?\\)))
                ((?u ?x)
                 (if (= c ?u)
                     (incf nDigits 2))
                 (setq n 0)
                 (loop for i from 0 below nDigits
                       while (< index end) do
                       (setq c (aref src (++ index))
                             n (js-hex-digit-to-int c n))
                       (when (minusp n)
                         ;; Back off to accepting the original '\'
                         ;; as a literal
                         (decf index (1+ i))
                         (setq n ?\\)
                         (return)))  ; just breaks the inner loop
                 (setq local-max n))
                (?d
                 (when in-range
                   (jsre-error jsre-msg-bad-range)
                   (throw 'done nil))
                 (setq local-max ?9))
                ((?D ?s ?S ?w ?W)
                 (when in-range
                   (jsre-error jsre-msg-bad-range)
                   (throw 'done nil))
                 (setf (jsre-node-bmsize target) 65535)
                 (throw 'done t))
                ((?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7)
                 ;; Non-ECMA extension - decimal escapes (here, octal)
                 ;; are supposed to be an error inside class ranges,
                 ;; but are supported for backwards compatibility.
                 (setq n (- c ?0))
                 (setq c (aref src index))
                 (when (and (<= ?0 c) (<= c ?7))
                   (incf index)
                   (setq n (+ (* 8 n) (- c ?0)))
                   (setq c (aref src index))
                   (when (and (<= ?0 c) (<= c ?7))
                     (incf index)
                     (setq i (+ (* 8 n) (- c ?0)))
                     (if (<= i #o377)
                         (setq n i)
                       (decf index))))
                 (setq local-max n))
                (t
                 (setq local-max c))))

            (if in-range
                (progn
                  (when (> range-start local-max)
                    (jsre-error jsre-msg-bad-range)
                    (throw 'done nil))
                  (setq in-range nil))
              (when (and (< index (1- end))
                         (= (aref src index) ?-))
                (incf index)
                (setq in-range t)
                (setq range-start local-max)
                (throw 'continue nil)))

            (when (/= 0 (logand jsre-state-flags JSREG_FOLD))
              (setq cu (upcase local-max)
                    cd (downcase local-max)
                    local-max (if (>= cu cd) cu cd)))

            (if (> local-max max)
                (setq max local-max))))  ; end while

        (setf (jsre-node-bmsize target) max)
        t))))

;;  item:       assertion               An item is either an assertion or
;;              quantatom               a quantified atom.
;;
;;  assertion:  '^'                     Assertions match beginning of string
;;                                      (or line if the class static property
;;                                      RegExp.multiline is true).
;;              '$'                     End of string (or line if the class
;;                                      static property RegExp.multiline is
;;                                      true).
;;              '\b'                    Word boundary (between \w and \W).
;;              '\B'                    Word non-boundary.
;;
;;  quantatom:  atom                    An unquantified atom.
;;              quantatom '{' n ',' m '}'
;;                                      Atom must occur between n and m times.
;;              quantatom '{' n ',' '}' Atom must occur at least n times.
;;              quantatom '{' n '}'     Atom must occur exactly n times.
;;              quantatom '*'           Zero or more times (same as {0,}).
;;              quantatom '+'           One or more times (same as {1,}).
;;              quantatom '?'           Zero or one time (same as {0,1}).
;;
;;              any of which can be optionally followed by '?' for ungreedy
;;
;;  atom:       '(' regexp ')'          A parenthesized regexp (what matched
;;                                      can be addressed using a backreference,
;;                                      see '\' n below).
;;              '.'                     Matches any char except '\n'.
;;              '[' classlist ']'       A character class.
;;              '[' '^' classlist ']'   A negated character class.
;;              '\f'                    Form Feed.
;;              '\n'                    Newline (Line Feed).
;;              '\r'                    Carriage Return.
;;              '\t'                    Horizontal Tab.
;;              '\v'                    Vertical Tab.
;;              '\d'                    A digit (same as [0-9]).
;;              '\D'                    A non-digit.
;;              '\w'                    A word character, [0-9a-z_A-Z].
;;              '\W'                    A non-word character.
;;              '\s'                    A whitespace character, [ \b\f\n\r\t\v].
;;              '\S'                    A non-whitespace character.
;;              '\' n                   A backreference to the nth (n decimal
;;                                      and positive) parenthesized expression.
;;              '\' octal               An octal escape sequence (octal must be
;;                                      two or three digits long, unless it is
;;                                      0 for the null character).
;;              '\x' hex                A hex escape (hex must be two digits).
;;              '\c' ctrl               A control character, ctrl is a letter.
;;              '\' literalatomchar     Any character except one of the above
;;                                      that follow '\' in an atom.
;;              otheratomchar           Any character not first among the other
;;                                      atom right-hand sides.

(defsubst jsre-set-result (op &optional prog-inc)
  "Set jsre-state-result to a new jsre-node for OP.
Increments jsre-state-prog-length by PROG-INC, or 1 if omitted.
Returns the new node."
  (prog1
      (setq jsre-state-result (make-jsre-node :op op))
    (incf jsre-state-prog-length (or prog-inc 1))))

(defun jsre-do-flat (c)
  (let ((result (jsre-set-result REOP_FLAT 3)))
    (setf (jsre-node-chr result) c
          (jsre-node-length result) 1
          (jsre-node-flat-index result) -1)))

(defun jsre-get-decimal-value (c max-value overflow-msg)
  "Parse a number from the regexp stream.
C is the last scanned character.
If parsed number exceeds MAX-VALUE, errors with OVERFLOW-MSG,
which should contain a placeholder for formatting a number."
  (let ((overflow nil)
        (start jsre-state-cp)
        (src jsre-state-cpbegin)
        (value (- c ?0))
        digit)
    (catch 'break
      (while (not (jsre-done))
        (setq c (jsre-peek src))
        (unless (jsre-digit-p c)
          (throw 'break nil))
        (unless overflow
          (setq digit (- c ?0))
          (if (< value (/ (- max-value digit) 10))
              (setq value (+ (* value 10) digit))
            (setq overflow t
                  value max-value)))
        (incf jsre-state-cp)))
    (if overflow
        (jsre-error (format overflow-msg
                            (string-to-number
                             (substring src start (- jsre-state-cp start))))))
    value))

;; Copy/pasted in Rhino; factored out here as an inline function.
(defsubst jsre-parse-octal-escape (src)
  (let ((num 0)
        c tmp)
    (catch 'break
      (while (not (jsre-done))
        (setq c (jsre-peek src))
        (if (and (>= c ?0) (<= c ?7))
            (progn
              (incf jsre-state-cp)
              (setq tmp (+ (* 8 num) (- c ?0)))
              (if (> tmp #o377)
                  (throw 'break nil))
              (setq num tmp))
          (throw 'break nil))))
    num))

(defun jsre-parse-term ()
  (let (src c n nDigits paren-base-count
        num tmp term term-start ocp result
        hasQ min max left-curl)
    (catch 'done
      (setq src              jsre-state-cpbegin
            c                (jsre-scan src)
            nDigits          2
            paren-base-count jsre-state-paren-count
            ocp              jsre-state-cp)
      (catch 'break-switch
        (case c
          ;; assertions and atoms
          (?^
           (jsre-set-result REOP_BOL 1)
           (throw 'done t))
          (?$
           (jsre-set-result REOP_EOL 1)
           (throw 'done t))
          (?\\
           (when (jsre-done)
             (jsre-error jsre-msg-trail-backslash)
             (throw 'done nil))
           (setq c (jsre-scan src))
           (case c
             ;; assertion escapes
             (?b
              (jsre-set-result REOP_WBDRY 1)
              (throw 'done t))
             (?B
              (jsre-set-result REOP_WNONBDRY 1)
              (throw 'done t))

             ;; octal escape
             (?0
              ;; Under 'strict' ECMA 3, we interpret \0 as NUL and don't
              ;; accept octal.  Until we have a strict-mode, we'll just
              ;; behave the old way for compatibility reasons.
              ;; http://bugzilla.mozilla.org/show_bug.cgi?id=141078
              (setq num (jsre-parse-octal-escape src)
                    c num)
              (jsre-do-flat c))

             ;; decimal escape
             ((?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
              (setq term-start (1- jsre-state-cp))
              (setq num (jsre-get-decimal-value c #xFFFF
                                                jsre-msg-overlarge-backref))

              ;; n > 9 && > count of parens => treat as octal instead
              (if (and (> num 9) (> num jsre-state-paren-count))
                  (progn
                    (setq jsre-state-cp term-start)
                    (setq num (jsre-parse-octal-escape src)
                          c num)
                    (jsre-do-flat c))
                ;; otherwise it's a back-reference
                (setq jsre-state-result (make-jsre-node :op REOP_BACKREF
                                                        :paren-index (1- num)))
                (incf jsre-state-prog-length 3)))

             ;; control escape
             (?f (jsre-do-flat (setq c #xC)))
             (?n (jsre-do-flat (setq c #xA)))
             (?r (jsre-do-flat (setq c #xD)))
             (?t (jsre-do-flat (setq c #x9)))
             (?v (jsre-do-flat (setq c #xB)))

             ;; control letter
             (?c
              (if (and (< (1+ jsre-state-cp) jsre-state-cpend)
                       (jsre-letter-p (aref src (1+ jsre-state-cp))))
                  (setq c (logand #x1F (jsre-scan src)))
                ;; else back off to accepting original '\' as a literal
                (decf jsre-state-cp)
                (setq c ?\\))
              (jsre-do-flat c))

             ;; Unicode or hex escape sequence
             ((?u ?x)
              (if (= c ?u) (incf nDigits 2))
              (setq n 0)
              (loop for i from 0 below nDigits
                    while (not (jsre-done))
                    do
                    (setq c (jsre-scan src)
                          n (js-hex-digit-to-int c n))
                    (when (minusp n)
                      ;; back off to accepting original 'u'/'x' as literal
                      (decf jsre-state-cp (+ i 2))
                      (setq n (jsre-scan src))
                      (return)))  ; breaks this loop
              (setq c n)
              (jsre-do-flat c))

             ;; Character class escapes
             (?d (jsre-set-result REOP_DIGIT))
             (?D (jsre-set-result REOP_NONDIGIT))
             (?s (jsre-set-result REOP_SPACE))
             (?S (jsre-set-result REOP_NONSPACE))
             (?w (jsre-set-result REOP_ALNUM))
             (?W (jsre-set-result REOP_NONALNUM))

             ;; Identity escape
             (t
              (setq jsre-state-result
                    (make-jsre-node :op REOP_FLAT
                                    :chr c
                                    :length 1
                                    :flat-index (1- jsre-state-cp)))
              (incf jsre-state-prog-length 3))))

          ;; (end giant case for '\\' characters)

          (?\(
           (setq result nil
                 term-start jsre-state-cp)
           (if (and
                (< (1+ jsre-state-cp) jsre-state-cpend)
                (= (jsre-peek src) ??)
                (memq (setq c (aref src (1+ jsre-state-cp)))
                      '(?= ?! ?:)))
               (progn
                 (incf jsre-state-cp 2)
                 (cond
                  ((= c ?=)
                   ;; ASSERT, <next>, ... ASSERTTEST
                   (setq result (jsre-set-result REOP_ASSERT 4)))
                  ((= c ?!)
                   ;; ASSERTNOT, <next>, ... ASSERTNOTTEST
                   (setq result (jsre-set-result REOP_ASSERT_NOT 4)))
                  ((= c ?:)
                   ;; Rhino doesn't record anything for shy groups.
                   ;; It just falls through to the parse-disjunction below,
                   ;; which happens within incremented paren nesting.  If
                   ;; the shy group is followed by a quantifier, it's pushed
                   ;; down to be the kid of the quantifier node, which gives
                   ;; it implicit parenthesization for the bytecode.  We need
                   ;; to record the shy group for elisp regexp generation.
                   ;; LPAREN, <index>, ... RPAREN, <index>  (may be incorrect)
                   (setq result (jsre-set-result REOP_SHY_GROUP 6)))))
             ;; else LPAREN, <index>, ... RPAREN, <index>
             (setq result (jsre-set-result REOP_LPAREN 6))
             (setf (jsre-node-paren-index result)
                   (++ jsre-state-paren-count)))

           (incf jsre-state-paren-nesting)
           (unless (jsre-parse-disjunction)  ; parse expr inside parens
             (throw 'done nil))
           (when (or (jsre-done)
                     (/= ?\) (jsre-peek src)))
             (jsre-error jsre-msg-unterm-paren "")
             (throw 'done nil))
           (incf jsre-state-cp)  ; consume close-paren
           (decf jsre-state-paren-nesting)
           (when result
             (setf (jsre-node-kid result) jsre-state-result)
             (setq jsre-state-result result)))

          (?\)
           (jsre-error jsre-msg-unmatched-right-paren)
           (throw 'done nil))

          (?\[
           (setq jsre-state-result (make-jsre-node :op REOP_CLASS)
                 term-start jsre-state-cp)
           (setf (jsre-node-start-index jsre-state-result) term-start)
           (catch 'break-while
             (while t
               (when (jsre-done)
                 (jsre-error jsre-msg-unterm-class "")
                 (throw 'done nil))
               (if (= (jsre-peek src) ?\\)
                   (incf jsre-state-cp)
                 (when (= (jsre-peek src) ?\])
                   (setf (jsre-node-kidlen jsre-state-result)
                         (- jsre-state-cp term-start))
                   (throw 'break-while nil)))
               (incf jsre-state-cp)))
           (setf (jsre-node-index jsre-state-result)
                 (++ jsre-state-class-count))
           ;; Call calculateBitmapSize now as we want any errors it finds
           ;; to be reported during the parse phase, not at execution.
           (unless (jsre-calculate-bitmap-size
                    jsre-state-result src term-start (++ jsre-state-cp))
             (throw 'done nil))
           (incf jsre-state-prog-length 3))  ; CLASS, <index>

          (?.
           (jsre-set-result REOP_DOT 1))

          ((?* ?+ ??)
           (jsre-error jsre-msg-bad-quant
                       (string (aref src (1- jsre-state-cp))))
           (throw 'done nil))  ; this is redundant w/ jsre-error, yes?

          (t
           (setq jsre-state-result
                 (make-jsre-node :op REOP_FLAT
                                 :chr c
                                 :length 1
                                 :flat-index (1- jsre-state-cp)))
           (incf jsre-state-prog-length 3))))

      (setq term jsre-state-result)
      (if (jsre-done)
          (throw 'done t))

      ;; check for a quantifier following the last scanned char
      (case (jsre-peek src)
        (?+
         (setq jsre-state-result (make-jsre-node :op REOP_QUANT
                                                 :min 1
                                                 :max -1))
         ;; <PLUS>, <parencount>, <parenindex>, <next> ... <ENDCHILD>
         (incf jsre-state-prog-length 8)
         (setq hasQ t))
        (?*
         (setq jsre-state-result (make-jsre-node :op REOP_QUANT
                                                 :min 0
                                                 :max -1))
         ;; <STAR>, <parencount>, <parenindex>, <next> ... <ENDCHILD>
         (incf jsre-state-prog-length 8)
         (setq hasQ t))
        (??
         (setq jsre-state-result (make-jsre-node :op REOP_QUANT
                                                 :min 0
                                                 :max 1))
         ;; <OPT>, <parencount>, <parenindex>, <next> ... <ENDCHILD>
         (incf jsre-state-prog-length 8)
         (setq hasQ t))

        (?{
         ;; balance '}'
         (setq min 0
               max -1
               left-curl jsre-state-cp)

         ;; For Perl etc. compatibility, if quantifier does not match
         ;; \{\d+(,\d*)?\} exactly back off from it being a quantifier,
         ;; and chew it up as a literal atom next time instead.

         (setq c (aref src (incf jsre-state-cp)))
         (when (jsre-digit-p c)
           (incf jsre-state-cp)
           (setq min (jsre-get-decimal-value c #xFFFF jsre-msg-overlarge-min))
           (setq c (jsre-peek src))
           (if (/= c ?,)
               (setq max min)
             (setq c (aref src (incf jsre-state-cp)))
             (when (jsre-digit-p c)
               (incf jsre-state-cp)
               (setq max (jsre-get-decimal-value
                          c #xFFFF jsre-msg-overlarge-max))
               (setq c (jsre-peek src))
               (when (> min max)
                 (jsre-error jsre-msg-max-lt-min (string (jsre-peek src)))
                 (throw 'done nil))))
           ;; balance '{'
           (when (= c ?})
             (setq jsre-state-result (make-jsre-node :op REOP_QUANT
                                                     :min min
                                                     :max max))
             ;; QUANT, <min>, <max>, <parencount>,
             ;; <parenindex>, <next> ... <ENDCHILD>
             (incf jsre-state-prog-length 12)
             (setq hasQ t)))
         (unless hasQ
           (setq jsre-state-cp left-curl))))

      (unless hasQ
        (throw 'done t))

      (incf jsre-state-cp)
      (setf (jsre-node-kid jsre-state-result) term)
      (setf (jsre-node-paren-index jsre-state-result) paren-base-count)
      (setf (jsre-node-paren-count jsre-state-result)
            (- jsre-state-paren-count paren-base-count))
      (if (and (not (jsre-done)) (= (jsre-peek src) ??))
          (progn
            (incf jsre-state-cp)
            (setf (jsre-node-greedy jsre-state-result) nil))
        (setf (jsre-node-greedy jsre-state-result) t))
      t)))

(defun jsre-generate-elisp (n)
  "Attempts to convert RE into an emacs-lisp regular expression.
N is the root node of the parse (sub)tree to generate.
Returns the elisp regexp if it could be translated.
Signals an error if we couldn't generate an elisp version."
  (let ((src jsre-state-cpbegin)
        (case-fold-search nil)
        next-alt tmp min max
        op kid greedy
        result minus)
    (while n
      (setq op (gethash (jsre-node-op n) jsre-byte-codes))
      (cond
       ((eq op 'REOP_BOL)
        (push "^" result))
       ((eq op 'REOP_EOL)
        (push "$" result))
       ((eq op 'REOP_DOT)
        (push "." result))
       ((eq op 'REOP_WBDRY)
        (push "\\b" result))
       ((eq op 'REOP_WNONBDRY)
        (push "\\B" result))
       ((eq op 'REOP_DIGIT)
        (push "[0-9]" result))
       ((eq op 'REOP_NONDIGIT)
        (push "[^0-9]" result))
       ((eq op 'REOP_SPACE)
        (push "\\s-" result))
       ((eq op 'REOP_NONSPACE)
        (push "\\S-" result))
       ((eq op 'REOP_ALNUM)
        (push "[0-9a-z_A-Z]" result))
       ((eq op 'REOP_NONALNUM)
        (push "[^0-9a-z_A-Z]" result))
       ((eq op 'REOP_ALT)
        (setq next-alt (jsre-node-kid2 n))
        (push (jsre-generate-elisp (jsre-node-kid n)) result)
        (push "\\|" result)
        (push (jsre-generate-elisp next-alt) result))
       ((eq op 'REOP_FLAT)
        (setq tmp (substring src
                             (jsre-node-flat-index n)
                             (+ (jsre-node-flat-index n)
                                (jsre-node-length n))))
        (if (member tmp '("[" "]" "?" "."))
            (push "\\" result))
        (push tmp result))
       ((memq op '(REOP_LPAREN REOP_SHY_GROUP))
        (push "\\(" result)
        (if (eq op 'REOP_SHY_GROUP)
            (push "?:" result))
        (push (jsre-generate-elisp (jsre-node-kid n)) result)
        (push "\\)" result))
       ((eq op 'REOP_BACKREF)
        (push "\\" result)
        (push (number-to-string (jsre-node-paren-index n)) result))
       ((eq op 'REOP_ASSERT)
        (error "elisp regexps do not support ?= or ?! assertions"))
       ((eq op 'REOP_QUANT)
        (setq min (jsre-node-min n)
              max (jsre-node-max n)
              greedy (jsre-node-greedy n))
        (push (jsre-generate-elisp (jsre-node-kid n)) result)
        (cond
         ((and (= min 1) (= max -1))
          (push "+" result))
         ((and (= min 0) (= max -1))
          (push "*" result))
         ((and (= min 0) (= max 1))
          (push "?" result))
         ((= min max)
          (push (format "\\{%d\\}" min) result))
         (t
          (push (format "\\{%d,%d\\}" min max) result)))
        (unless greedy
          (push "?" result)))
      ((eq op 'REOP_CLASS)
        (push "[" result)
        ;; The parser treats the whole class as a string, so we
        ;; have to go in and fix up non-elisp escapes manually.
        ;; Need to figure out the complete list.
        (setq tmp (substring src
                             (jsre-node-start-index n)
                             (+ (jsre-node-start-index n)
                                (jsre-node-kidlen n))))
        ;; If there's a \\] in the char class, it has to become ] and be
        ;; the first character in the class, e.g. []f] to match ] or f.
        (when (string-match "\\\\]" tmp)
          (setq tmp (replace-regexp-in-string "\\\\]" "" tmp t t))
          (push "]" result))
        ;; To include - it has to be first or last, so we make it last,
        ;; since ] will be first if present.
        (when (string-match "\\\\-" tmp)
          (setq tmp (replace-regexp-in-string "\\\\-" "" tmp t t))
          (setq minus t))
        ;; [ . and / are not escaped in char classes.
        (setq tmp (replace-regexp-in-string "\\\\\\([.[/]\\)" "\\1" tmp))
        ;; perl5 escapes
        (setq tmp (replace-regexp-in-string "\\\\d" "0-9" tmp t t))
        (setq tmp (replace-regexp-in-string "\\\\s" " \t" tmp t t))
        (setq tmp (replace-regexp-in-string "\\\\w" "0-9a-z_A-Z" tmp t t))
        ;; How do we handle "negative" escapes like \D, \S, \W?
        ;; E.g. var re = /[^3\D]/ -- not sure if there's a way to
        ;; express this in elisp with a single class.  For now, punt.
        (if (string-match "\\\\[DSW]" tmp)
            (error "cannot convert inverted character escape"))
        (push tmp result)
        (when minus
          (push "-" result)
          (setq minus nil))
        (push "]" result))
       (t
        (error "Unhandled RE op: %d" (jsre-node-op n)))) ; end cond
      (setq n (jsre-node-next n))) ; end while
    (apply #'concat (nreverse result))))

(defconst JSRE-OFFSET-LEN 2)
(defconst JSRE-INDEX-LEN 2)

(defsubst jsre-add-index (array pc index)
  (assert (>= index 0))
  (if (> index #xFFFF)
      (error "Regexp too complex"))
  (aset array pc (lsh index -8))  ;; (index >> 8)
  (aset array (1+ pc) index)
  (+ pc 2))

(defalias 'jsre-get-offset 'jsre-get-index)

(defsubst jsre-resolve-forward-jump (array from pc)
  (assert (not (> from pc)))
  (jsre-add-index array from (- pc from)))

(defun jsre-emit-re-bytecode (re pc n)
  "Creates the bytecode array for compiled regexp RE.
PC is the initial array offset.  N is the parse tree root node.
Returns the end-pc (i.e. the length of the array).

The `program' field of RE should be a preallocated vector big
enough to hold the generated bytecode."
  (let (next
        next-alt
        next-alt-fixup
        next-term-fixup
        (fold-case (/= 0 (logand jsre-state-flags JSREG_FOLD)))
        (program (jsre-compiled-re-program re))
        op) ; vector
    (while n
      (aset program (++ pc) (jsre-node-op n))
      (setq op (jsre-node-op n))
      (cond
       ((eq op 'REOP_EMPTY)
        (decf pc))

       ((eq op 'REOP_ALT)
         (setq next-alt (jsre-node-kid2 n)
               next-alt-fixup pc) ;; address of next alternate
         (incf pc JSRE-OFFSET-LEN)
         (setq pc (jsre-emit-re-bytecode re pc (jsre-node-kid n)))
         (aset program (++ pc) 'REOP_JUMP)
         (setq next-term-fixup pc) ;; address of following term
         (incf pc JSRE-OFFSET-LEN)
         (jsre-resolve-forward-jump program next-alt-fixup pc)
         (setq pc (jsre-emit-re-bytecode re pc next-alt))

         (aset program (++ pc) 'REOP_JUMP)
         (setq next-alt-fixup pc)
         (incf pc JSRE-OFFSET-LEN)

         (jsre-resolve-forward-jump program next-term-fixup pc)
         (jsre-resolve-forward-jump program next-alt-fixup pc))

       ((eq op 'REOP_FLAT)
         ;; Consecutize FLAT's if possible.
         (when (/= (jsre-node-flat-index n) 1)
           (while (and (setq next (jsre-node-next n))
                       (= (jsre-node-op next) 'REOP_FLAT)
                       (= (+ (jsre-node-flat-index n)
                             (jsre-node-length n))
                          (jsre-node-flat-index next)))
             (incf (jsre-node-length n)
                   (jsre-node-length next))
             (setf (jsre-node-next n)
                   (jsre-node-next next))))

         (if (and (/= (jsre-node-flat-index n) 1)
                  (> (jsre-node-length n) 1))
             (progn
               (aset program (1- pc)
                     (if fold-case 'REOP_FLATi 'REOP_FLAT))
               (setq pc (jsre-add-index
                         program pc (jsre-node-flat-index n)))
               (setq pc (jsre-add-index
                         program pc (jsre-node-length n))))
           (if (< (jsre-node-chr n) 256)
               (progn
                 (aset program (1- pc)
                       (if fold-case 'REOP_FLATi 'REOP_FLAT))
                 (aset program (++ pc) (jsre-node-chr n)))
             (aset program (1- pc)
                   (if fold-case 'REOP_UCFLATi 'REOP_UCFLAT))
             (setq pc (jsre-add-index program pc (jsre-node-chr n))))))

       ((eq op 'REOP_LPAREN)
         (setq pc (jsre-add-index program pc (jsre-node-paren-index n)))
         (setq pc (jsre-emit-re-bytecode re pc (jsre-node-kid n)))
         (aset program (++ pc) 'REOP_RPAREN)
         (setq pc (jsre-add-index program pc (jsre-node-paren-index n))))

       ((eq op 'REOP_BACKREF)
         (setq pc (jsre-add-index program pc (jsre-node-paren-index n))))

       ((eq op 'REOP_ASSERT)
         (setq next-term-fixup pc)
         (incf pc JSRE-OFFSET-LEN)
         (setq pc (jsre-emit-re-bytecode re pc (jsre-node-kid n)))
         (aset program (++ pc) 'REOP_ASSERTTEST)
         (jsre-resolve-forward-jump program next-term-fixup pc))

       ((eq op 'REOP_ASSERT_NOT)
         (setq next-term-fixup pc)
         (incf pc JSRE-OFFSET-LEN)
         (setq pc (jsre-emit-re-bytecode re pc (jsre-node-kid n)))
         (aset program (++ pc) 'REOP_ASSERTNOTTEST)
         (jsre-resolve-forward-jump program next-term-fixup pc))

       ((eq op 'REOP_QUANT)
         (cond
          ((and (zerop (jsre-node-min n))
                (= -1 (jsre-node-max n)))
           (aset program (1- pc)
                 (if (jsre-node-greedy n) 'REOP_STAR 'REOP_MINIMALSTAR)))
          ((and (zerop (jsre-node-min n))
                (= 1 (jsre-node-max n)))
           (aset program (1- pc)
                 (if (jsre-node-greedy n) 'REOP_OPT 'REOP_MINIMALOPT)))
          ((and (= 1 (jsre-node-min n))
                (= -1 (jsre-node-max n)))
           (aset program (1- pc)
                 (if (jsre-node-greedy n) 'REOP_PLUS 'REOP_MINIMALPLUS)))
          (t
           (if (not (jsre-node-greedy n))
               (aset program (1- pc) 'REOP_MINIMALQUANT)
             (setq pc (jsre-add-index program pc (jsre-node-min n)))
             ;; max can be -1 which addIndex does not accept
             (setq pc (jsre-add-index program pc (1+ (jsre-node-max n)))))))

         (setq pc (jsre-add-index program pc (jsre-node-paren-count n)))
         (setq pc (jsre-add-index program pc (jsre-node-paren-index n)))
         (setq next-term-fixup pc)
         (incf pc JSRE-OFFSET-LEN)
         (setq pc (jsre-emit-re-bytecode re pc (jsre-node-kid n)))
         (aset program (++ pc) 'REOP_ENDCHILD)
         (jsre-resolve-forward-jump program next-term-fixup pc))

       ((eq op 'REOP_CLASS)
         (setq pc (jsre-add-index program pc (jsre-node-index n)))
         (setf (aref (jsre-compiled-re-class-list re)
                     (jsre-node-index n))
               (make-jsre-charset
                :length (jsre-node-bmsize n)
                :start-index (jsre-node-start-index n)
                :str-length (jsre-node-kidlen n))))) ; end case

      (setq n (jsre-node-next n)))      ; end while
    pc))

(defsubst jsre-push-prog-state (gdata
                                min
                                max
                                backtrack-last-to-save
                                cont-pc
                                cont-op)
  (setf (jsre-gdata-state-stack-top gdata)
        (make-jsre-progstate
         :previous (jsre-gdata-state-stack-top gdata)
         :min min
         :max max
         :index (jsre-gdata-cp gdata)
         :backtrack backtrack-last-to-save
         :cont-op cont-op
         :cont-pc cont-pc)))

(defsubst jsre-pop-prog-state (gdata)
  (let ((state (jsre-gdata-state-stack-top gdata)))
    (setf (jsre-gdata-state-stack-top gdata)
          (jsre-progstate-previous state))
    state))

(defsubst jsre-push-backtrack-state (gdata op target)
  (setf (jsre-gdata-backtrack-stack-top gdata)
        (jsre-make-backtrack-data gdata op target)))

(defsubst jsre-flat-n-matcher (gdata
                               match-chars
                               length
                               chars
                               end
                               &optional fold-case)
  "Match consecutive literal characters.
Optional FOLD-CASE argument does case-insensitive compare."
  (let* ((regexp (jsre-gdata-regexp gdata))
         (source (jsre-compiled-re-source regexp)))
    (if (> (+ (jsre-gdata-cp gdata) length) end)
        nil
      (loop repeat length
            for pos from match-chars
            for cp from (jsre-gdata-cp gdata)
            for c1 = (aref source pos)
            for c2 = (aref chars cp)
            if fold-case
              if (/= (upcase c1) (upcase c2)) return nil end
            else
              if (/= c1 c2) return nil end
            finally do (incf (jsre-gdata-cp gdata) length)
            finally return t))))

;;  1. Evaluate DecimalEscape to obtain an EscapeValue E.
;;  2. If E is not a character then go to step 6.
;;  3. Let ch be E's character.
;;  4. Let A be a one-element RECharSet containing the character ch.
;;  5. Call CharacterSetMatcher(A, false) and return its Matcher result.
;;  6. E must be an integer. Let n be that integer.
;;  7. If n=0 or n>NCapturingParens then throw a SyntaxError exception.
;;  8. Return an internal Matcher closure that takes two arguments, a State x
;;     and a Continuation c, and performs the following:
;;     1. Let cap be x's captures internal array.
;;     2. Let s be cap[n].
;;     3. If s is undefined, then call c(x) and return its result.
;;     4. Let e be x's endIndex.
;;     5. Let len be s's length.
;;     6. Let f be e+len.
;;     7. If f>InputLength, return failure.
;;     8. If there exists an integer i between 0 (inclusive) and len (exclusive)
;;        such that Canonicalize(s[i]) is not the same character as
;;        Canonicalize(Input [e+i]), then return failure.
;;     9. Let y be the State (f, cap).
;;     10. Call c(y) and return its result.

(defun jsre-backref-matcher (gdata paren-index chars end)
  (let (len
        (paren-content (jsre-gdata-parens-index gdata paren-index)))
    (cond
     ((= paren-content -1)
      t)
     ((> (+ (jsre-gdata-cp gdata)
            (setq len (jsre-gdata-parens-length gdata paren-index)))
         end)
      nil)
     (t
      (catch 'return
        (progn
          (if (/= 0 (logand (jsre-compiled-re-flags (jsre-gdata-regexp gdata))
                            JSREG_FOLD))
              (dotimes (i len)
                (if (/= (upcase (aref chars (+ i paren-content)))
                        (upcase (aref chars (+ i (jsre-gdata-cp gdata)))))
                    (throw 'return nil)))
            (dotimes (i len)
              (if (/= (aref chars (+ i paren-content))
                      (aref chars (+ i (jsre-gdata-cp gdata))))
                  (throw 'return nil))))
          (incf (jsre-gdata-cp gdata) len)
          t))))))

(defsubst jsre-add-character-to-charset (charset c)
  (assert (<= c (jsre-charset-length charset)))
  (aset (jsre-charset-bits charset) c t))

;; probably smarter to use a char-table for this...
(defsubst jsre-add-char-range-to-charset (charset c1 c2)
  "Add a character range, C1 to C2 (inclusive) to CHARSET."
  (assert (and (< c2 (jsre-charset-length charset))
               (<= c1 c2)))
  (loop with bits = (jsre-charset-bits charset)
        for i from c1 to c2 do
        (aset bits i t)))

(defun jsre-process-charset (gdata charset)
  "Compile the source of the class into a jsre-charset."
  (unless (jsre-charset-converted charset)
    (jsre-process-charset-impl gdata charset)
    (setf (jsre-charset-converted charset) t)))

(defun jsre-process-charset-impl (gdata charset)
  (let* ((src (jsre-charset-start-index charset))
         (end (+ src (jsre-charset-str-length charset)))
         (range 0)
         this-ch byte-length
         c n nDigits digit
         i in-range range-start
         (regexp (jsre-gdata-regexp gdata))
         (source (jsre-compiled-re-source regexp))
         (fold-case
          (/= 0 (logand (jsre-compiled-re-flags regexp) JSREG_FOLD))))
    (setf (jsre-charset-sense charset) t)
    (setq byte-length (1+ (/ (jsre-charset-length charset) 8)))
    (setf (jsre-charset-bits charset) (make-bool-vector byte-length nil))
    (unless (= src end)
      (when (= ?^ (aref source src))
        (setf (jsre-charset-sense charset) nil)
        (incf src))
      (while (/= src end)
        (catch 'continue
          (setq nDigits 2)
          (case (aref source src)
            (?\\
             (incf src)
             (setq c (aref source (++ src)))
             (case c
               (?b (setq this-ch #x8))
               (?f (setq this-ch #xC))
               (?n (setq this-ch #xA))
               (?r (setq this-ch #xD))
               (?t (setq this-ch #x9))
               (?v (setq this-ch #xB))
               (?c
                (if (and (< (1+ src) end)
                         (jsre-word-p (aref source (1+ src))))
                    (setq this-ch (logand #x1F (aref source (++ src))))
                  (decf src)
                  (setq this-ch ?\\)))
               ((?u ?x)
                (if (= c ?u)
                    (incf nDigits 2))
                (setq n 0)
                (loop for i from 0
                      while (and (< i nDigits) (< src end)) do
                      (progn
                        (setq c (aref source (++ src))
                              digit (jsre-to-hex-digit c))
                        (when (minusp digit) ; conversion failed
                          ;; back off to accepting original '\' as a literal
                          (decf src (1+ i))
                          (setq n ?\\)
                          (return))     ; just breaks the inner loop
                        (setq n (logior (lsh n 4) digit))))
                (setq this-ch n))
               ((?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7)
                ;; Non-ECMA extension - decimal escapes (in this case,
                ;; octal) are supposed to be an error inside class ranges,
                ;; but are supported here for backwards compatibility.
                (setq n (- c ?0))
                (setq c (aref source src))
                (when (and (<= ?0 c) (<= c ?7))
                  (incf src)
                  (setq n (* 8 (+ n (- c ?0))))
                  (setq c (aref source src))
                  (when (and (<= ?0 c) (<= c ?7))
                    (incf src)
                    (setq i (* 8 (+ n (- c ?0))))
                    (if (<= i #o377)
                        (setq n i)
                      (decf src))))
                (setq this-ch n))
               (?d
                (jsre-add-char-range-to-charset charset ?0 ?9)
                (throw 'continue nil))  ; don't need range processing
               (?D
                (jsre-add-char-range-to-charset charset 0 (1- ?0))
                (jsre-add-char-range-to-charset charset (1+ ?9)
                                                (jsre-charset-length charset))
                (throw 'continue nil))
               (?s
                (loop for i from (jsre-charset-length charset) downto 0 do
                      (if (jsre-re-whitespace-p i)
                          (jsre-add-character-to-charset charset i)))
                (throw 'continue nil))
               (?S
                (loop for i from (jsre-charset-length charset) downto 0 do
                      (unless (jsre-re-whitespace-p i)
                        (jsre-add-character-to-charset charset i)))
                (throw 'continue nil))
               (?w
                (loop for i from (jsre-charset-length charset) downto 0 do
                      (if (jsre-word-p i)
                          (jsre-add-character-to-charset charset i)))
                (throw 'continue nil))
               (?W
                (loop for i from (jsre-charset-length charset) downto 0 do
                      (unless (jsre-word-p i)
                        (jsre-add-character-to-charset charset i)))
                (throw 'continue nil))
               (otherwise
                ;; ends inner switch on char following '\'
                (setq this-ch c))))
             (otherwise
              (setq this-ch (aref source (++ src)))))  ; end outer switch

          (if in-range
              (progn
                (if fold-case
                    (progn
                      (jsre-add-char-range-to-charset charset
                                                      (upcase range-start)
                                                      (upcase this-ch))
                      (jsre-add-char-range-to-charset charset
                                                      (downcase range-start)
                                                      (downcase this-ch)))
                  (jsre-add-char-range-to-charset charset
                                                  range-start
                                                  this-ch))
                (setq in-range nil))
            (if fold-case
                (progn
                  (jsre-add-character-to-charset charset (upcase this-ch))
                  (jsre-add-character-to-charset charset (downcase this-ch)))
              (jsre-add-character-to-charset charset this-ch))
            (when (and (< src (1- end))
                       (= (aref source src) ?-))
              (incf src)
              (setq in-range t
                    range-start this-ch))))))))

(defun jsre-class-matcher (gdata charset ch)
  "Initialize the character set if this is the first call.
Test the bit - if the ^ flag was specified, non-inclusion is a success."
  (unless (jsre-charset-converted charset)
    (jsre-process-charset gdata charset))
  (let ((bit-set (or (zerop (jsre-charset-length charset))
                     (or (> ch (jsre-charset-length charset))
                         (aref (jsre-charset-bits charset) ch))))
        (sense (jsre-charset-sense charset)))
    (not (eq sense bit-set))))

(defun jsre-execute-re-bytecode (gdata chars end)
  (let* ((pc 0)
         (regexp (jsre-gdata-regexp gdata))
         (flags (jsre-compiled-re-flags regexp))
         (multiline (or (jsre-gdata-multiline gdata)
                        (/= 0 (logand flags JSREG_MULTILINE))))
         (fold-case (/= 0 (logand flags JSREG_FOLD)))
         (program (jsre-compiled-re-program regexp))
         (cc-pc 0)        ; current-continuation-pc
         (cc-op 'REOP_END) ; current-continuation-op
         (result nil)
         (op (aref program (++ pc)))
         cp offset length match-ch next-pc next-op index
         test-op state paren-index cap-index paren-count
         min max greedy new-min new-max btdata)
    (catch 'return
      (while t
        (catch 'continue
          (setq cp (jsre-gdata-cp gdata))
          (cond
           ((eq op 'REOP_EMPTY)
            (setq result t))

           ((eq op 'REOP_BOL)
            (if (/= 0 cp)
                (setq result (and multiline
                                  (jsre-line-term-p (aref chars (1- cp)))))
              (setq result t)))

           ((eq op 'REOP_EOL)
            (if (/= end cp)
                (setq result (and multiline
                                  (jsre-line-term-p (aref chars (1- cp)))))
              (setq result t)))

           ((eq op 'REOP_WBDRY)
            (setq result
                  (xor
                   (or (zerop cp)
                       (not (jsre-word-p (aref chars (1- cp)))))
                   (not
                    (and (< cp end)
                         (jsre-word-p (aref chars cp)))))))

           ((eq op 'REOP_WNONBDRY)
            (setq result
                  (xor
                   (or (zerop cp)
                       (not (jsre-word-p (aref chars (1- cp)))))
                   (and (< cp end)
                        (jsre-word-p (aref chars cp))))))

           ((eq op 'REOP_DOT)
            (if (setq result
                      (and (/= cp end)
                           (not (jsre-line-term-p (aref chars cp)))))
                (incf (jsre-gdata-cp gdata))))

           ((or (eq op 'REOP_DIGIT)
                (eq op 'REOP_NONDIGIT))
            (if (setq result
                      (and (/= cp end)
                           (xor 'REOP_NONDIGIT
                                (jsre-digit-p (aref chars cp)))))
                (incf (jsre-gdata-cp gdata))))

            ((or (eq op 'REOP_SPACE)
                 (eq op 'REOP_NONSPACE))
             (if (setq result
                       (and (/= cp end)
                            (xor 'REOP_NONSPACE
                                 (jsre-re-whitespace-p (aref chars cp)))))
                 (incf (jsre-gdata-cp gdata))))

            ((or (eq op 'REOP_ALNUM)
                 (eq op 'REOP_NONALNUM))
             (if (setq result
                       (and (/= cp end)
                            (xor 'REOP_NONALNUM
                                 (jsre-word-p (aref chars cp)))))
                 (incf (jsre-gdata-cp gdata))))

            ((or (eq op 'REOP_FLAT)
                 (eq op 'REOP_FLATi))
             (setq offset (jsre-get-index program pc))
             (incf pc JSRE-INDEX-LEN)
             (setq length (jsre-get-index program pc))
             (incf pc JSRE-INDEX-LEN)
             (setq result
                   (jsre-flat-n-matcher gdata offset length chars end
                                        (eq op 'REOP_FLATi))))

            ((or (eq op 'REOP_FLAT1)
                 (eq op 'REOP_FLAT1i))
             (setq match-ch (logand #xFF (aref program (++ pc))))
             (if (setq result
                       (and (/= cp end)
                            (if (eq op 'REOP_FLAT1)
                                (= (aref chars cp) match-ch)
                              (= (upcase (aref chars cp)) ; REOP_FLAT1i
                                 (upcase match-ch)))))
                 (incf (jsre-gdata-cp gdata))))

            ((or (eq op 'REOP_UCFLAT1)
                 (eq op 'REOP_UCFLAT1i))
             (setq match-ch (aref program pc))
             (incf pc JSRE-INDEX-LEN)
             (if (setq result
                       (and (/= cp end)
                            (if (eq op 'REOP_UCFLAT1)
                                (= (aref chars cp) match-ch)
                              (= (upcase (aref chars cp)) match-ch))))
                 (incf (jsre-gdata-cp gdata))))

           ((eq op 'REOP_ALT)
             (jsre-push-prog-state gdata 0 0 nil cc-pc cc-op)
             (setq next-pc (+ pc (jsre-get-offset program pc)))
             (setq next-op (aref program (++ next-pc)))
             (jsre-push-backtrack-state gdata next-op next-pc)
             (incf pc JSRE-INDEX-LEN)
             (setq op (aref program (++ pc)))
             (throw 'continue nil))

           ((eq op 'REOP_JUMP)
             (setq state (jsre-pop-prog-state gdata)
                   cc-pc (jsre-progstate-cont-pc state)
                   cc-op (jsre-progstate-cont-op state)
                   offset (jsre-get-offset program pc))
             (incf pc offset)
             (setq op (aref program (++ pc)))
             (throw 'continue nil))

           ((eq op 'REOP_LPAREN)
             (setq paren-index (jsre-get-index program pc))
             (incf pc JSRE-INDEX-LEN)
             (jsre-gdata-set-parens gdata paren-index (jsre-gdata-cp gdata) 0)
             (setq op (aref program (++ pc)))
             (throw 'continue nil))

           ((eq op 'REOP_RPAREN)
             (setq paren-index (jsre-get-index program pc))
             (incf pc JSRE-INDEX-LEN)
             (setq cap-index (jsre-gdata-parens-index gdata paren-index))
             (jsre-gdata-set-parens gdata
                                    paren-index
                                    cap-index
                                    (- (jsre-gdata-cp gdata) cap-index))
             (if (> paren-index (jsre-gdata-last-paren gdata))
                 (setf (jsre-gdata-last-paren gdata) paren-index))
             (setq op (aref program (++ pc)))
             (throw 'continue nil))

           ((eq op 'REOP_BACKREF)
             (setq paren-index (jsre-get-index program pc))
             (incf pc JSRE-INDEX-LEN)
             (setq result (jsre-backref-matcher gdata paren-index chars end)))

           ((eq op 'REOP_CLASS)
             (setq index (jsre-get-index program pc))
             (incf pc JSRE-INDEX-LEN)
             (setq result
                   (and (/= (jsre-gdata-cp gdata) end)
                        (jsre-class-matcher
                         gdata
                         (aref (jsre-compiled-re-class-list regexp) index)
                         (aref chars (jsre-gdata-cp gdata))))))

            ((or (eq op 'REOP_ASSERT)
                 (eq op 'REOP_ASSERT_NOT))
             (jsre-push-prog-state gdata 0 0
                                   (jsre-gdata-backtrack-stack-top gdata)
                                   cc-pc cc-op)
             (setq test-op
                   (if (eq op 'REOP_ASSERT)
                       'REOP_ASSERTTEST
                     'REOP_ASSERTNOTTEST))
             (jsre-push-backtrack-state gdata test-op
                                        (+ pc (jsre-get-offset program pc)))
             (incf pc JSRE-INDEX-LEN)
             (setq op (aref program (++ pc)))
             (throw 'continue nil))

            ((or (eq op 'REOP_ASSERTTEST)
                 (eq op 'REOP_ASSERTNOTTEST))
             (setq state (jsre-pop-prog-state gdata))
             (setf (jsre-gdata-cp gdata) (jsre-progstate-index state))
             (setf (jsre-gdata-backtrack-stack-top gdata)
                   (jsre-progstate-backtrack state))
             (setq cc-pc (jsre-progstate-cont-pc state)
                   cc-op (jsre-progstate-cont-op state))
             (if result
                 (setq result (eq op 'REOP_ASSERTTEST))
               (setq result (not (eq op 'REOP_ASSERTTEST)))))

            ((memq op '(REOP_STAR
                        REOP_PLUS
                        REOP_OPT
                        REOP_QUANT
                        REOP_MINIMALSTAR
                        REOP_MINIMALPLUS
                        REOP_MINIMALOPT
                        REOP_MINIMALQUANT))
             (cond
              ((or (eq op 'REOP_STAR)
                   (eq op 'REOP_MINIMALSTAR))
               (setq greedy (eq op 'REOP_STAR)
                     min 0
                     max -1))
              ((or (eq op 'REOP_PLUS)
                   (eq op 'REOP_MINIMALPLUS))
               (setq greedy (eq op 'REOP_PLUS)
                     min 1
                     max -1))
              ((or (eq op 'REOP_OPT)
                   (eq op 'REOP_MINIMALOPT))
               (setq greedy (eq op 'REOP_OPT)
                     min 0
                     max 1))
              ((or (eq op 'REOP_QUANT)
                   (eq op 'REOP_MINIMALQUANT))
               (setq greedy (eq op 'REOP_QUANT)
                     min (jsre-get-offset program pc)
                     pc (+ pc JSRE-INDEX-LEN)
                     ;; See comments in emitREBytecode for "- 1" reason
                     max (1- (jsre-get-offset program pc))
                     pc (+ pc JSRE-INDEX-LEN))))
             (jsre-push-prog-state gdata min max nil cc-pc cc-op)
             (cond
              (greedy
               (setq cc-op 'REOP_REPEAT
                     cc-pc pc)
               (jsre-push-backtrack-state gdata 'REOP_REPEAT pc)
               ;; Step over <parencount>, <parenindex> & <next>
               (incf pc (* 3 JSRE-INDEX-LEN)))
              ((/= 0 min)
               (setq cc-op 'REOP_MINIMALREPEAT
                     cc-op pc)
               ;; <parencount> <parenindex> & <next>
               (incf pc (* 3 JSRE-INDEX-LEN)))
              (t
               (jsre-push-backtrack-state gdata 'REOP_MINIMALREPEAT pc)
               (jsre-pop-prog-state gdata)
               (incf pc (* 2 JSRE-INDEX-LEN))
               (incf pc (jsre-get-offset program pc))))
             (setq op (aref program (++ pc)))
             (throw 'continue nil))

           ((eq op 'REOP_ENDCHILD)
             ;; Use the current continuation
             (setq pc cc-pc
                   op cc-op)
             (throw 'continue nil))

           ((eq op 'REOP_REPEAT)
             (setq state (jsre-pop-prog-state gdata))
             (catch 'break-switch-case
               (if (not result)
                   ;; There's been a failure; see if we have enough children.
                   (progn
                     (setq result (zerop (jsre-progstate-min state)))
                     (setq cc-pc (jsre-progstate-cont-pc state)
                           cc-op (jsre-progstate-cont-op state))
                     (incf pc (* 2 JSRE-INDEX-LEN)) ; <parencount> & <parenindex>
                     (incf pc (jsre-get-offset program pc))
                     (throw 'break-switch-case nil))
                 (when (and (zerop (jsre-progstate-min state))
                            (= cp (jsre-progstate-index state)))
                   ;; matched an empty string; that'll get us nowhere
                   (setq result nil
                         cc-pc (jsre-progstate-cont-pc state)
                         cc-op (jsre-progstate-cont-op state))
                   (incf pc (* 2 JSRE-INDEX-LEN))
                   (incf pc (jsre-get-offset program pc))
                   (throw 'break-switch-case nil))
                 (setq new-min (jsre-progstate-min state)
                       new-max (jsre-progstate-max state))
                 (if (/= 0 new-min) (decf new-min))
                 (if (/= -1 new-max) (decf new-max))
                 (when (zerop new-max)
                   (setq result t
                         cc-pc (jsre-progstate-cont-pc state)
                         cc-op (jsre-progstate-cont-op state))
                   (incf pc (* 2 JSRE-INDEX-LEN))
                   (incf pc (jsre-get-offset program pc))
                   (throw 'break-switch-case nil))
                 (jsre-push-prog-state gdata new-min new-max nil
                                       (jsre-progstate-cont-pc state)
                                       (jsre-progstate-cont-op state))
                 (setq cc-op 'REOP_REPEAT
                       cc-pc pc)
                 (jsre-push-backtrack-state gdata 'REOP_REPEAT pc)
                 (setq paren-count (jsre-get-index program pc))
                 (incf pc JSRE-INDEX-LEN)
                 (setq paren-index (jsre-get-index program pc))
                 (incf pc (* 2 JSRE-INDEX-LEN))
                 (setq op (aref program (++ pc)))
                 (dotimes (k paren-count)
                   (jsre-gdata-set-parens gdata (+ k paren-index) -1 0)))
               (throw 'continue nil)))

           ((eq op 'REOP_MINIMALREPEAT)
             (setq state (jsre-pop-prog-state gdata)
                   max (jsre-progstate-max state)
                   min (jsre-progstate-min state))
             (catch 'break-switch-case
               (if (not result)
                   ;; Non-greedy failure - try to consume another child.
                   (if (or (= max -1) (plusp max))
                       (progn
                         (jsre-push-prog-state gdata min max nil
                                               (jsre-progstate-cont-pc state)
                                               (jsre-progstate-cont-op state))
                         (setq cc-op 'REOP_MINIMALREPEAT
                               cc-pc pc)
                         (setq paren-count (jsre-get-index program pc))
                         (incf pc JSRE-INDEX-LEN)
                         (setq paren-index (jsre-get-index program pc))
                         (incf pc (* 2 JSRE-INDEX-LEN))
                         (dotimes (k paren-count)
                           (jsre-gdata-set-parens gdata (+ k paren-index) -1 0))
                         (setq op (aref program (++ pc)))
                         (throw 'continue nil))
                     ;; Don't need to adjust pc since we're going to pop.
                     (setq cc-pc (jsre-progstate-cont-pc state)
                           cc-op (jsre-progstate-cont-op state))
                     (throw 'break-switch-case nil))
                 ;; else have result...
                 (when (and (zerop min)
                            (= cp (jsre-progstate-index state)))
                   ;; Matched an empty string; that'll get us nowhere.
                   (setq result nil
                         cc-pc (jsre-progstate-cont-pc state)
                         cc-op (jsre-progstate-cont-op state))
                   (throw 'break-switch-case nil))
                 (setq new-min (jsre-progstate-min state)
                       new-max (jsre-progstate-max state))
                 (if (/= 0 new-min) (decf new-min))
                 (if (/= -1 new-max) (decf new-max))
                 (jsre-push-prog-state gdata new-min new-max nil
                                       (jsre-progstate-cont-pc state)
                                       (jsre-progstate-cont-op state))
                 (if (/= 0 new-min)
                     (progn
                       (setq cc-op 'REOP_MINIMALREPEAT
                             cc-pc pc)
                       (setq paren-count (jsre-get-index program pc))
                       (incf pc JSRE-INDEX-LEN)
                       (setq paren-index (jsre-get-index program pc))
                       (incf pc (* 2 JSRE-INDEX-LEN))
                       (dotimes (k paren-count)
                         (jsre-gdata-set-parens gdata (+ paren-index k) -1 0)))
                   (setq cc-op (jsre-progstate-cont-op state)
                         cc-pc (jsre-progstate-cont-pc state))
                   (jsre-push-backtrack-state gdata 'REOP_MINIMALREPEAT pc)
                   (jsre-pop-prog-state gdata)
                   (incf pc (* 2 JSRE-INDEX-LEN))
                   (incf pc (jsre-get-offset program pc)))
                 (setq op (aref program (++ pc)))
                 (throw 'continue nil))))

           ((eq op 'REOP_END)
             (throw 'return t))

            (t
             (error "invalid regexp bytecode"))) ; end outermost switch

          (if result
              (setq op (aref program (++ pc)))
            ;; If the match failed and there's a backtrack option, take it.
            ;; Otherwise this is a complete and utter failure.
            (unless (setq btdata (jsre-gdata-backtrack-stack-top gdata))
              (throw 'return nil))

            (setf (jsre-gdata-backtrack-stack-top gdata)
                  (jsre-btdata-previous btdata))

            (setf (jsre-gdata-last-paren gdata)
                  (jsre-btdata-last-paren btdata))

            ;; XXX: If backTrackData will no longer be used, then
            ;; there is no need to clone backTrackData.parens
            (if (jsre-btdata-parens btdata)
                (setf (jsre-gdata-parens gdata)
                      (copy-sequence (jsre-btdata-parens btdata))))

            (setf (jsre-gdata-cp gdata) (jsre-btdata-cp btdata))

            (setf (jsre-gdata-state-stack-top gdata)
                  (jsre-btdata-state-stack-top btdata))

            (setf cc-op (jsre-progstate-cont-op
                         (jsre-gdata-state-stack-top gdata)))

            (setf cc-pc (jsre-progstate-cont-pc
                         (jsre-gdata-state-stack-top gdata)))

            (setq pc (jsre-btdata-cont-pc btdata)
                  op (jsre-btdata-cont-op btdata))))))))

(defun jsre-match-regexp (gdata re chars start end multiline)
  "Match compiled regexp RE against string CHARS.
Return t if match is successful, else nil."
  (let ((paren-count (jsre-compiled-re-paren-count re))
        (anchor-ch (jsre-compiled-re-anchor-char re))
        (fold-case (/= 0 (logand (jsre-compiled-re-flags re) JSREG_FOLD)))
        match-ch
        result)
    (setf (jsre-gdata-parens gdata) (if (/= 0 paren-count)
                                        (make-vector paren-count 0)
                                      nil))
    (setf (jsre-gdata-backtrack-stack-top gdata) nil
          (jsre-gdata-state-stack-top gdata) nil
          (jsre-gdata-multiline gdata) multiline
          (jsre-gdata-regexp gdata) re
          (jsre-gdata-last-paren gdata) 0)
    (catch 'return
      ;; have to include the position beyond the last character
      ;; in order to detect end-of-input/line condition
      (loop for i from start upto end do
            ;; If the first node is a literal match, step the index into
            ;; the string until that match is made, or fail if it can't be
            ;; found at all.
            (when (>= anchor-ch 0)
              (catch 'break
                (while t
                  (if (= i end)
                      (throw 'return nil))
                  (setq match-ch (aref chars i))
                  (if (or (= match-ch anchor-ch)
                          (and fold-case
                               (= (upcase match-ch) (upcase anchor-ch))))
                      (throw 'break nil))
                  (incf i))))
            (setf (jsre-gdata-cp gdata) i)
            (dotimes (j paren-count)
              (jsre-gdata-set-parens gdata j -1 0))
            (setq result (jsre-execute-re-bytecode gdata chars end))
            (setf (jsre-gdata-backtrack-stack-top gdata) nil
                  (jsre-gdata-state-stack-top gdata) nil)
            (when result
              (setf (jsre-gdata-skipped gdata) (- i start))
              (throw 'return t)))
      nil)))

;; instance data from org.mozilla.javascript.regexp.RegExpImpl
(defstruct jsre-match-data
  "Stores information about the last match attempt."
  ;; input string to match (perl $_)
  input

  ;; whether input contains newlines (perl $*)
  multiline

  ;; vector of `jsre-substring';
  ;; last set of parens matched (perl $1, $2)
  parens

  ;; last string matched (perl $&)
  last-match

  ;; last paren matched (perl $+)
  last-paren

  ;; input to left of last match (perl $`)
  left-context

  ;; input to right of last match (perl $')
  right-context)

;; port of org.mozilla.javascript.regexp.SubString
;; TODO(stevey):  don't create the chars array (use buffer chars)
;; In fact, get rid of this silly Rhino artifact altogether, and
;; just store the lc-index, lc-length, rc-index, rc-length.
(defstruct
  (jsre-substring
   (:constructor new-jsre-substring (char-array index &optional length)))
  char-array    ; pointer to the parent string
  (index 0)     ; start index
  (length 0))   ; substring length

(defvar jsre-empty-substring (make-jsre-substring)
  "Singleton empty `jsre-substring' object.")

(defsubst jsre-substring-to-string (subst)
  "Convert SUBST, a `jsre-substring', to a string."
  (if (null (jsre-substring-char-array subst))
      ""
    (let ((index (jsre-substring-index subst)))
      (substring (jsre-substring-char-array subst)
                  index
                  (+ index (jsre-substring-length subst))))))

;; port of org.mozilla.javascript.regexp.NativeRegexp.executeRegExp
(defun jsre-execute-regexp (re match-data str indexp match-type)
  "Invoke RE on STR at INDEXP and construct JavaScript result.
RE is a `jsre-compiled-re' struct with containing compiled bytecode.
STR is the input string.
INDEXP is a 1-element vector (in/out parameter) of start index.
MATCH-DATA contains information about the last match attempt.
MATCH-TYPE is operation to perform:  'test, 'match or 'prefix.
Returns a js-Array"
  (let ((gdata (make-jsre-gdata))  ; state tracking for current match
        (start (aref indexp 0))
        (end (length str))
        matches index i
        match-len ep scope match-str
        result obj
        (paren-count (jsre-compiled-re-paren-count re)))
    (if (> start end)
        (setq start end))
    ;; Call the recursive matcher to do the real work.
    (setq matches (jsre-match-regexp
                   gdata re str start end
                   (jsre-match-data-multiline match-data)))
    (cond
     ((not matches)
      (if (neq match-type 'prefix)
          'null
        'undefined))
     (t
      (setq index (jsre-gdata-cp gdata)
            i index
            indexp i
            match-len (- i (+ start (jsre-gdata-skipped gdata)))
            ep index
            index (- index match-len))
      (if (eq match-type 'test)
          ;; Testing for a match and updating cx.regExpImpl: don't allocate
          ;; an array object; do return true.
          (setq result t
                obj nil)
        ;; The array returned on match has element 0 bound to the matched
        ;; string, elements 1 through re.parenCount bound to the paren
        ;; matches, an index property telling the length of the left context,
        ;; and an input property referring to the input string.
        (setq scope (js-global)
              result (js-make-Array)
              obj result
              match-str (substring str index (+ index match-len)))
        (js-put obj "0" match-str))

      (if (zerop paren-count)
          (progn
            (setf (jsre-match-data-parens match-data) nil
                  (jsre-match-data-last-paren match-data) jsre-empty-substring))
        (setf (jsre-match-data-parens match-data)
              (make-vector paren-count 0))
        (loop with parsub = nil
              for num from 0 below paren-count
              for cap-index = (jsre-gdata-parens-index gdata num)
              do
              (if (/= cap-index -1)
                  (progn
                    (setq parsub (new-jsre-substring
                                  str
                                  cap-index
                                  (jsre-gdata-parens-length gdata num)))
                    (aset (jsre-match-data-parens match-data)
                          num
                          parsub)
                    (unless (eq match-type 'test)
                      (js-put obj (1+ num) (jsre-substring-to-string parsub))))
                (if (neq match-type 'test)
                    (js-put obj (1+ num) 'undefined)))
              finally
              (setf (jsre-match-data-last-paren match-data) parsub)))

      (when (/= match-type 'test)
        ;; Define the index and input properties last for better for/in loop
        ;; order (so they come after the elements).
        (js-put obj "index" (+ start (jsre-gdata-skipped gdata)))
        (js-put obj "input" str))

      (when (null (jsre-match-data-last-match match-data))
        (setf (jsre-match-data-last-match match-data) (make-jsre-substring))
        (setf (jsre-match-data-left-context match-data) (make-jsre-substring))
        (setf (jsre-match-data-right-context match-data) (make-jsre-substring)))

      (let ((data (jsre-match-data-last-match match-data)))
        (setf (jsre-substring-char-array data) str
              (jsre-substring-index data) index
              (jsre-substring-length data) match-len))

      (let ((data (jsre-match-data-left-context match-data)))
        (setf (jsre-substring-char-array data) str
              (jsre-substring-index data) 0
              (jsre-substring-length data) (+ start (jsre-gdata-skipped gdata))))

      (let ((data (jsre-match-data-right-context match-data)))
        (setf (jsre-substring-char-array data) str
              (jsre-substring-index data) ep
              (jsre-substring-length data) (- end ep)))

      ;; return a pair (indexp is an "in/out" parameter)
      (cons result indexp)))))


(provide 'jsre)

;;; jsre.el ends here
