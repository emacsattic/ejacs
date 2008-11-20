;;; js-native-string:  implementation of JavaScript String type

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

(defun js-init-native-String (obj-proto func-proto)
  (let ((String (make-js-Function :proto func-proto)) ; 15.5.3
        (string-proto (make-js-String :proto obj-proto
                                      :value ""))) ; 15.5.4
    ;; 15.5 -- String constructor
    (setf (js-Function-call-slot String) 'js-String--call--)
    (setf (js-Function-construct-slot String) 'js-String--construct--)
    (js-define String "length" 1 t t t)  ; 15.5.3
    (js-define String "prototype" string-proto t t t) ; 15.5.3.1
    (js-define-builtin String "fromCharCode"
                       'js-String-fromCharCode t nil nil t) ; 15.5.3.2
    (js-define (js-get String "fromCharCode") "length" 1)

    ;; 15.5.4 -- String.prototype
    (js-define string-proto "constructor" String nil nil t)
    (dolist (name '(toString valueOf charAt charCodeAt concat
                    indexOf lastIndexOf localeCompare match
                    replace search slice split substr substring
                    toLowerCase toLocaleLowerCase toSource
                    toUpperCase toLocaleUpperCase))
      (js-define-builtin string-proto (symbol-name name)
                         (intern (concat "js-String-"
                                         (symbol-name name)))
                         t ; have callable signature
                         nil nil t))
    (dolist (name '("concat" "lastIndexOf"))
      (js-define (js-get string-proto name) "length" 1 t t t))
    (dolist (name '("slice" "split" "substring" "substr"))
      (js-define (js-get string-proto name) "length" 2 t t t))

    String))

(defsubst js-string-index (s arg)
  "Return t if ARG can be an (elisp) string index for S.
Returns the index, or nil if it's not a valid index."
  (let (index)
    (when (string-match "^[0-9]+$" arg)
      (setq index (string-to-number arg))
      (if (and (not (minusp index))
               (<= index (length s))
               (<= index most-positive-fixnum))
          index
        nil))))

(defun js-String--get-- (this arg)
  "Allow array indexing to work on strings.  Not part of ECMA."
  (let* ((s (js-to-string this))
         (index (js-string-index s arg)))
    (if index
        (string (aref s index))
      (js-default--get-- this arg))))

(defun js-String--put-- (this name val)
  "Prevent array indexing from assigning to strings."
  (let* ((s (js-to-string this))
         (index (js-string-index s name)))
    (unless index
      (js-default--put-- this name val))))

(defun js-String--call-- (ctor args)
  "15.5.1.1 -- String Constructor called as a function"
  (if (js-null-p (first args))
      ""
    (js-to-string (first args))))

(defun js-String--construct-- (&optional funobj args)
  "15.5.2.1 -- String constructor called in `new' expression.
FUNOBJ is the String constructor function, and can be omitted."
  (let* ((ctor (or funobj (js-global-get "String")))
         (result (make-js-String :proto (js-get ctor "prototype")))
         (val (funcall 'js-String--call-- ctor args)))
    (setf (js-String-value result) val)
    (js-define result "length" (length val) t t t) ; 15.5.5.1
    result))

(defun js-String-fromCharCode (thisobj args)
  "15.5.3.2 -- String.fromCharCode"
  (mapconcat 'string (mapcar 'js-to-uint16 args) ""))

;; I don't much care for this format, but it seems to be the standard.
(defun js-String-toSource (s args)
  (concat "(new String(\"" (js-String-value s) "\"))"))

(defun js-String-toString (s args)
  "15.5.4.2 - String.prototype.toString"
  (js-String-valueOf s args))

(defun js-String-valueOf (s args)
  "15.5.4.3 - String.prototype.valueOf"
  (unless (js-string-p s)
    (js-type-error (js-format "%s is not a String")))
  (if (stringp s)
      s
    (js-String-value s)))

(defun js-String-charAt (s args)
  "15.5.4.4 - String.prototype.charAt(pos)"
  (let ((result (js-String-charCodeAt s (car-safe args))))
    (if (js-NaN-p result)
        ""
      (string result))))

(defun js-String-charCodeAt (s args)
  "15.5.4.5 - String.prototype.charCodeAt"
  (let* ((target (js-to-string s))
         (pos (or (first args) 0))
         (ix (js-to-integer pos))
         (len (length target)))
    (if (or (minusp ix) (>= ix len))
        'NaN
       (aref target ix))))

(defun js-String-concat (s args)
  "15.5.4.6 - String.prototype.concat"
  (mapconcat 'js-to-string (cons s args) ""))

(defun js-String-indexOf (s args)
  "15.5.4.7 - String.prototype.indexOf"
  (let ((target (js-to-string s))
        (what (js-to-string (first args)))
        (beg (if (second args)
                 (js-to-integer (second args))
               0))
        (case-fold-search nil))
    (if (minusp beg)
        (setq beg 0))
    (if (> beg (length target))
        -1
      (if (string-match (regexp-quote what) target beg)
          (match-beginning 0)
        -1))))

(defun js-String-lastIndexOf (s args)
  "15.5.4.8 - String.prototype.lastIndexOf"
  (let ((target (js-to-string s))
        (what (regexp-quote (js-to-string (first args))))
        (pos (if (second args)
                 (js-to-number (second args))
               0.0e+NaN))
        (result -1)
        (continue t)
        (case-fold-search nil))
    (setq pos
          (if (js-NaN-p pos)
              1.0e+INF
            (js-to-integer pos)))
    (while (and continue
                (string-match what target (1+ result)))
      (if (<= (match-beginning 0) pos)
          (setq result (match-beginning 0))
        (setq continue nil)))
    result))

(defun js-String-localeCompare (s args)
  "15.5.4.9 - String.prototype.localeCompare"
  (let ((this (js-to-string s))
        (that (js-to-string (or (first args) ""))))
    (if (string< this that)
        -1
      (if (string< that this)
          1
        0))))

(defun js-String-match (s args)
  "15.5.4.10 - String.prototype.match"
  (let ((target (js-to-string s))
        (re (if (js-RegExp-p (car args))
                (car args)
              (js-RegExp--construct-- nil args))))
    (if (not (js-regex-global-p re))
        (js-RegExp-exec re (list target))
      (let ((lastIndex 0)
            (result (js-Array--construct--))
            (args (list target))
            last)
        (loop initially (js-put re "lastIndex" 0)
              for n from 0
              for match = (js-RegExp-exec re args)
              while match
              do (setq last (js-get re "lastIndex"))
              if (eql lastIndex last)
                do (js-put re "lastIndex" (incf last))
              end
              do
                (setq lastIndex last)
                (js-put result n (js-get match "0"))
              finally do (js-put result "length" n)
              finally return result)))))

(defun js-String-replace (this args)
  "15.5.4.11 - String.prototype.replace"
  (let ((target (js-to-string this))
        (sval (if (js-RegExp-p (car args))
                  (car args)
                (if (car args)
                    (js-to-string (car args))
                  "")))
        (rval (if (js-Function-p (second args))
                  (second args)
                (if (second args)
                    (js-to-string (second args))
                  "")))
        result match index m tmp)
    (cond
     ((stringp sval)
      (if (setq index (search sval target))
          (concat (substring target 0 index)
                  (js-string-perform-replace this target sval rval t)
                  (substring target (+ index (length sval))))
        target))
     ((not (js-regex-global-p sval))
      (if (setq match (js-RegExp-exec sval (list target)))
          (progn
            (setq index (js-get match "index")
                  m (js-get match "0"))
            (concat (substring target 0 index)
                    (js-string-perform-replace this target m rval)
                    (substring target (+ index (length m)))))
        target))
     (t
      (loop initially (js-put sval "lastIndex" 0)
            with pos = 0
            while (setq match (js-RegExp-exec sval (list target)))
            do
            (setq index (js-get match "index")
                  m     (js-get match "0"))
            if (eql (setq tmp (js-get sval "lastIndex")) pos)
              do (js-put sval "lastIndex" (incf pos))
            else
            do
              (push (substring target pos index) result)
              (push (js-string-perform-replace this target m rval)
                    result)
              (setq pos tmp)
            finally
            do (if (< pos (length target))
                   (push (substring target pos) result))
            finally return
            (apply #'concat (nreverse result)))))))

(defun js-string-perform-replace (this s match rep &optional literal)
  "Implement replacement rules for 15.5.4.11.
S is the original string.  MATCH is the matched substring.  REP
is either a user function or a string.  LITERAL means the search
value was a string, not a regexp.  THIS is the this-object
originally passed to String.replace.  The `match-data' is set if
it was a regexp search.  Returns the value of calling REP, if REP
was a function, or the string derived by converting REP to a
string by replacing certain $-escapes with portions of the match
data."
  (let (args c2 c3 num)
    (cond
     ((js-Function-p rep)
      (push match args)
      (loop for i from 2 below (length (match-data)) by 2
            do
            (push (match-beginning i) args)
            (push (match-string i s) args))
      (push (match-beginning 0) args)
      (push s args)
      (setq args (nreverse args))
      (js-Function-call rep this args))
     (literal
      rep)
     (t
      (loop with numgroups = (1- (/ (length (match-data)) 2))
            for i from 0 below (length rep)
            for c = (aref rep i)
            if (/= c ?$)
              do (push (string c) args)
            else do
            (setq c2 (aref-safe rep (incf i)))
            (cond
             ((eq c2 ?$)  ; $$ => skip
              (push "$$" args))
             ((eq c2 ?&)
              (push match args))
             ((eq c2 ?`)
              (push (substring s 0 (match-beginning 0)) args))
             ((eq c2 ?')
              (push (substring s (match-end 0)) args))
             ((and (>= c2 ?0) (<= c2 ?9))
              (if (and (setq c3 (aref-safe rep (1+ i)))
                       (>= c3 ?0) (<= c3 ?9))
                  (progn
                    (setq num (+ (* 10 (- c2 ?0)) (- c3 ?0)))
                    (if (>= num numgroups)
                        (push (format "$%d" num) args)
                      (push (match-string num s) args))
                    (incf i))
                (setq num (- c2 ?0))
                (if (> num numgroups)  ; 1-indexed groups
                    (push (format "$%d" num) args)
                  (push (match-string num s) args))))
             (t
              (push "$" args)
              (push (string c2) args)))
            finally return (apply #'concat (nreverse args)))))))

(defun js-String-search (s args)
  "15.5.4.12 - String.prototype.search"
  (let* ((target (js-to-string s))
         (re (if (js-RegExp-p (car args))
                 (car args)
               (js-RegExp--construct-- nil args)))
         (pattern (js-RegExp-elisp re)))
    (or (string-match pattern target)
        -1)))

(defun js-String-slice (s args)
  "15.5.4.13 - String.prototype.slice"
  (let* ((target (js-to-string s))
         (len (length target))
         (beg (js-to-integer (first args)))
         end)
    (if (null args)
        target
      (cond
       ((minusp beg)
        (incf beg len)
        (if (minusp beg)
            (setq beg 0)))
       ((> beg len)
        (setq beg len)))
      (if (= 1 (length args))
          (setq end len)
        (setq end (js-to-integer (second args)))
        (cond
         ((minusp end)
          (incf end len)
          (if (minusp end)
              (setq end 0)))
         ((> end len)
          (setq end len)))
        (if (< end beg)
            (setq end beg)))
      (substring target (truncate beg) (truncate end)))))

(defun js-String-split (str args)
  "15.5.4.14 - String.prototype.split"
  (let* ((s (js-to-string str))
         (sep (when (car args)
                (if (js-RegExp-p (car args))
                    (car args)
                  (js-to-string (car args)))))
         (limit (if (second args)
                    (js-to-uint32 (second args))
                  (1- (expt 2.0 32))))
         (slen (length s))
         (retval (js-Array--construct--))
         (result '())
         (case-fold-search nil)
         (count 0)          ; current result list length
         (pos 0)            ; current position
         (q 0)              ; lookahead position
         z                  ; lookahead match result
         end                ; end of match result
         prefix)            ; substring from pos to next separator
    (cond
     ((zerop limit) nil)    ; step 7
     ((null (car args))     ; step 8, 33
      (push s result))
     ((zerop slen)          ; step 9, 31
      (unless (string-match sep s)
        (push "" result)))
     (t
      (catch 'break
        (while t
          (when (= q slen)
            (push (substring s pos) result)
            (throw 'break nil))
          (setq z (js-string-split-looking-at s q sep)
                end (car z))
          (if (or (not z) (= end pos))
              (incf q)
            (setq prefix (substring s pos q)
                  pos end)
            (dolist (m (cons prefix (cdr z)))
              (push m result)
              (if (>= (incf count) limit)
                  (throw 'break nil))))))))
    (loop for m in (nreverse result)   ; convert to Array
          for i from 0
          do (js-put retval i (or m 'undefined))
          finally return retval)))

(defun js-string-split-looking-at (s q r)
  "15.5.4.14 - SplitMatch helper function.
S is a string, Q is start position, R is a regexp or string.
Returns a MatchResult object, which for now is represented as:

  (END-INDEX . CAPTURES)

where CAPTURES is a list of values, where each Nth value a string
matching or Nth capturing group, or nil if that group didn't match.
The special Ecma-262 15.10.2.1 token `failure' is represented as nil.

Returns a non-nil MatchResult if R matched S exactly at position Q."
  (let (rlen slen index result)
    (if (js-RegExp-p r)
        (when (and (setq index (string-match (js-RegExp-elisp r) s q))
                   (= index q))
          (push (match-end 0) result)
          (loop for i from 1 below (/ (length (match-data)) 2) do
                (push (substring s (match-beginning i) (match-end i))
                      result)
                finally return (nreverse result)))
      (setq rlen (length r)
            slen (length s))
      (cond
       ((> (+ q rlen) slen)
        nil)
       ((not (string= r (substring s q (+ q rlen))))
        nil)
       (t
        (list (+ q rlen)))))))

(defun js-String-substring (s args)
  "15.5.4.15 - String.prototype.substring(start, end)"
  (let* ((target (js-to-string s))
         (beg (js-to-integer (first args)))
         (len (length target))
         end)
    (if (minusp beg)
        (setq beg 0)
      (if (> beg len)
          (setq beg len)))
    (if (js-null-p (second args))
        (setq end len)
      (setq end (js-to-integer (second args)))
      (if (minusp end)
          (setq end 0)
        (if (> end len)
            (setq end len)))
      (if (< end beg)
          (rotatef end beg)))
    (substring target (truncate beg) (truncate end))))

(defun js-String-substr (s args)
  "ECMA B.2.3 -- String.prototype.substr(start, length)"
  (let* ((target (js-to-string s))
         (beg (truncate (js-to-integer (first args))))
         (len (if (second args)
                  (truncate (js-to-integer (second args)))
                1.0e+INF))
         (slen (length target))
         numchars)
    (unless (wholenump beg)
      (setq beg (max (+ slen beg) 0)))
    (if (plusp (setq numchars (min (max len 0) (- slen beg))))
        (substring target beg (+ beg numchars))
      "")))
        
(defun js-String-toLowerCase (s args)
  "15.5.4.16 - String.prototype.toLowerCase"
  (downcase (js-to-string s)))

(defun js-String-toLocaleLowerCase (s args)
  "15.5.4.17 - String.prototype.toLocaleLowerCase"
  (downcase (js-to-string s)))

(defun js-String-toUpperCase (s args)
  "15.5.4.18 - String.prototype.toUpperCase"
  (upcase (js-to-string s)))

(defun js-String-toLocaleUpperCase (s args)
  "15.5.4.19 - String.prototype.toLocaleUpperCase"
  (upcase (js-to-string s)))

(provide 'js-native-string)

;;; js-native-string.el ends here
