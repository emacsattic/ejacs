;;; js-native-date:  implementation of JavaScript Date type

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

(defun js-init-native-Date (obj-proto func-proto)
  (let ((Date (make-js-Function :proto func-proto))
        (date-proto (make-js-Date :proto obj-proto)))

    (setf (js-Function-call-slot Date) 'js-Date--call--)
    (setf (js-Function-construct-slot Date) 'js-Date--construct--)

    ;; 15.9.4  properties of the Date Constructor
    (js-define Date "length" 7 t t t)
    (js-define Date "prototype" date-proto t t t)
    (js-define-builtin Date "parse" 'js-Date-parse t nil nil t)
    (js-define-builtin Date "UTC" 'js-Date-UTC t nil nil t)
    (js-define (js-get Date "UTC") "length" 7 t t t)

    ;; 15.9.5 properties of the Date Prototype object
    (setf (js-Date-value date-proto) 0.0e+NaN)
    (js-define date-proto "constructor" Date nil nil t)

    (dolist (name '("toString" "toDateString" "toTimeString"
                    "toLocaleString" "toLocaleDateString"
                    "toLocaleTimeString" "valueOf" "getTime"
                    "getFullYear" "getUTCFullYear" "getMonth"
                    "getUTCMonth" "getDate" "getUTCDate"
                    "getDay" "getUTCDay" "getHours" "getUTCHours"
                    "getMinutes" "getUTCMinutes" "getSeconds"
                    "getUTCSeconds" "getMilliseconds"
                    "getUTCMilliseconds" "getTimezoneOffset"
                    "setTime" "setMilliseconds" "setSeconds"
                    "setUTCMilliseconds" "setUTCSeconds"
                    "setMinutes" "setUTCMinutes" "setHours"
                    "setUTCHours" "setDate" "setUTCDate"
                    "setMonth" "setUTCMonth" "setFullYear"
                    "setUTCFullYear" "toUTCString" "toSource"))
      (js-define-builtin date-proto name
                         (intern (concat "js-Date-" name))
                         t nil nil t))

    (dolist (name '("setSeconds" "setUTCSeconds"
                    "setMonth" "setUTCMonth"))
      (js-define (js-get date-proto name) "length" 2 t t t))

    (dolist (name '("setMinutes" "setUTCMinutes"
                    "setFullYear" "setUTCFullYear"))
      (js-define (js-get date-proto name) "length" 3 t t t))

    (dolist (name '("setHours" "setUTCHours"))
      (js-define (js-get date-proto name) "length" 4 t t t))

    Date))

;; 15.9.1.3  Year Number

(defconst js-date-ms-per-day (* 86400 1000))

(defsubst js-date-day (time)
  "Return integral float day number given TIME in ms since Epoch."
  (ffloor (/ time js-date-ms-per-day)))

(defsubst js-date-time-within-day (time)
  "Return float time within day given float TIME in ms since Epoch."
  (mod time js-date-ms-per-day))

(defsubst js-date-leap-year-p (year)
  "Return t if YEAR is a Gregorian leap year."
  (if (< year 0) (setq year (1- (abs year))))
  (and (zerop (% year 4))
       (or (not (zerop (% year 100)))
           (zerop (% year 400)))))

(defsubst js-date-days-in-year (year)
  (if (js-date-leap-year-p year) 366 365))

(defsubst js-date-day-from-year (year)
  "Return day number of first day of YEAR."
  (+ (* 365 (- year 1970))
     (floor (/ (- year 1969) 4))
     (- (floor (/ (- year 1901) 100)))
     (floor (/ (- year 1601) 400))))

(defsubst js-date-time-from-year (year)
  "Return float time value at start of YEAR."
  (* js-date-ms-per-day (js-date-day-from-year year)))

(defun js-date-year-from-time (time)
  "Given a float TIME of millis since epoch, return year."
  ;; get fast estimates to within a few days
  (let ((lo (+ 1970
               (truncate (ffloor (/ (/ time js-date-ms-per-day)
                                 366)))))
        (hi (+ 1970
               (truncate (ffloor (/ (/ time js-date-ms-per-day)
                                 365)))))
        (continue t)
        mid)
    (if (< hi lo)
        (rotatef hi lo))  ; handle negative dates

    ;; binary search to find the right year
    (while (and continue (> hi lo))
      (setq mid (/ (+ hi lo) 2))
      (if (> (js-date-time-from-year mid) time)
          (setq hi (1- mid))
        (setq lo (1+ mid))
        (if (> (js-date-time-from-year lo) time)
            (setq continue nil
                  lo mid))))  ; return mid
    lo))

(defun js-date-in-leap-year (time)
  "Return 1 if YEAR is a leap year, else 0."
  (if (js-date-leap-year-p (js-date-year-from-time time))
      1
    0))

;; 15.9.1.4  Month Number

(defsubst js-date-day-within-year (time)
  (- (js-date-day time) (js-date-day-from-year
                         (js-date-year-from-time time))))

(defun js-date-month-from-time (time)
  "Return month number 0-11 from float TIME."
  (let ((leap (js-date-in-leap-year time))
        (day (js-date-day-within-year time)))
    (cond
     ((< day 31) 0)
     ((< day (+ 59 leap)) 1)
     ((< day (+ 90 leap)) 2)
     ((< day (+ 120 leap)) 3)
     ((< day (+ 151 leap)) 4)
     ((< day (+ 181 leap)) 5)
     ((< day (+ 212 leap)) 6)
     ((< day (+ 243 leap)) 7)
     ((< day (+ 273 leap)) 8)
     ((< day (+ 304 leap)) 9)
     ((< day (+ 334 leap)) 10)
     (t 11))))

(defsubst js-date-month-name (month)
  "Return non-localized month long name given MONTH 0-11."
  (aref ["January" "February" "March" "April" "May" "June" "July"
         "August" "September" "October" "November" "December"] month))

(defun js-date-day-from-month (m year)
  (let ((day (* m 30)))
    (cond
     ((>= m 7)
      (incf day (1- (/ m 2))))
     ((>= m 2)
      (incf day (1- (/ (1- m) 2))))
     (t
      (incf day m)))
    (if (and (>= m 2)
             (js-date-leap-year-p
              (js-date-year-from-time year)))
        (incf day))
    day))

;; 15.9.1.5  Date Number

(defun js-date-date-from-time (time)
  "Return date number 1-31 given float TIME as ms since Epoch."
  (let ((day (js-date-day-within-year time))
        (leap (js-date-in-leap-year time))
        (month (js-date-month-from-time time)))
    (case month
      (0 (1+ day))
      (1 (- day 30))
      (2 (- day 58 leap))
      (3 (- day 89 leap))
      (4 (- day 119 leap))
      (5 (- day 150 leap))
      (6 (- day 180 leap))
      (7 (- day 211 leap))
      (8 (- day 242 leap))
      (9 (- day 272 leap))
      (10 (- day 303 leap))
      (11 (- day 333 leap))
      (t (error "PANIC:  computed invalid month")))))

(defsubst js-date-week-day (time)
  (mod (+ (js-date-day time) 4) 7))

(defsubst js-date-week-day-name (weekday)
  "Return non-localized weekday name for WEEKDAY 0 through 6."
  (aref ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday"
         "Friday" "Saturday"] weekday))

(defconst js-date-ms-per-min (* 60 1000))
(defconst js-date-ms-per-hour (* js-date-ms-per-min 60))
(defconst js-date-ms-per-day (* js-date-ms-per-hour 24))

;; `current-time-zone' returns (OFFSET NAME) where offset is secs
(defconst js-date-local-tza
  (* 1000 (car (current-time-zone)))  ; convert to millis
  "ECMA 15.9.1.8 -- Millis to add to UTC to get local time.")

(defun js-date-dst-adjust (time)
  "ECMA 15.9.1.9 -- return Daylight Saving Time adjust for TIME.
TIME is float TIME as ms since Epoch.  Adjustment returned in millis."
  (let* ((decoded-time
          ;; returns (SEC MIN HOUR DAY MONTH YEAR DOW DST ZONE)
          (decode-time (js-date-js-to-elisp time)))
         (dst (nth 7 decoded-time)))  ; t if DST is in effect
    ;; I'll just assume it's 1 hour if DST is in effect.
    (if dst
        js-date-ms-per-hour
      0)))

(defun js-date-local-time (time)
  "ECMA 15.9.1.9 -- convert from TIME (UTC) to local time."
  (+ time js-date-local-tza (js-date-dst-adjust time)))

(defun js-date-utc (time)
  "ECMA 15.9.1.9 -- convert from local TIME to UTC."
  (- time
     js-date-local-tza
     (js-date-dst-adjust (- time js-date-local-tza))))

;; 15.9.1.10  Hours, Minutes, Second, Millis

(defsubst js-date-hour-from-time (time)
  (mod (ffloor (/ time js-date-ms-per-hour)) 24))

(defsubst js-date-min-from-time (time)
  (mod (ffloor (/ time js-date-ms-per-min)) 60))

(defsubst js-date-sec-from-time (time)
  (mod (ffloor (/ time 1000)) 60))

(defsubst js-date-ms-from-time (time)
  (mod time 1000))

;;  -- MakeTime (hour, min, sec, ms)

(defun js-date-make-time (hour min sec ms)
  "ECMA 15.9.1.11 - calc num millis from HOUR, MIN, SEC, MS."
  (if (not (and (js-finite-p hour)
                (js-finite-p min)
                (js-finite-p sec)
                (js-finite-p ms)))
      0.0e+NaN
    (setq hour (js-to-integer hour)
          min  (js-to-integer min)
          sec  (js-to-integer sec)
          ms   (js-to-integer ms))
    (js-add-numbers
     (js-add-numbers
      (js-add-numbers
       (* hour js-date-ms-per-hour)
       (* min js-date-ms-per-min))
      (* sec 1000))
     ms)))

(defun js-date-make-day (year month date)
  "ECMA 15.9.1.12 -- a number of days from YEAR, MONTH, DATE."
  (if (not (and (js-finite-p year)
                (js-finite-p month)
                (js-finite-p date)))
      0.0e+NaN
    (setq year (js-to-integer year)
          month (js-to-integer month)
          date (js-to-integer date))
    (incf year (ffloor (/ month 12)))
    (setq month (mod month 12))
    (if (minusp month)
        (incf month 12))
    (+ (ffloor (/ (js-date-time-from-year year)
                  js-date-ms-per-day))  ; year day
       (js-date-day-from-month month year)
       date
       -1)))

(defun js-date-make-date (day time)
  "ECMA 15.9.1.13 -- MakeDate(day, time)."
  (if (not (and (js-finite-p day)
                (js-finite-p time)))
      0.0e+NaN
    (+ (* day js-date-ms-per-day) time)))

(defun js-date-time-clip (time)
  "ECMA 15.9.1.14 -- TimeClip(time).
Clips TIME value to standard ECMA range."
  (cond
   ((not (js-finite-p time))
    0.0e+NaN)
   ((> (abs time) 8.64e15)
    0.0e+NaN)
   ((plusp time)
    (ffloor time))
   (t
    (fceiling time))))

(defun js-date-elisp-to-js (time)
  "Turn an elisp TIME value into a JS time value.
Elisp represents times as (high low usec), with `high' as
the most significant 16 bits, 'low' as the least sigificant
16 bits, and `usec' as the microsecond value, or zero if not
supported by the machine.  Returns a float time value in ms."
  (let ((msb (first time))     ; high 16 bits of seconds
        (lsb (second time))    ; low 16 bits of seconds
        (usec (third time)))   ; microseconds
    (+ (* 1000.0 msb 65536.0)
       (* 1000.0 lsb)
       (if (plusp usec)
           (/ usec 1000.0)
         0))))

(defun js-date-js-to-elisp (time)
  "Turn a JavaScript ms TIME value into (high low . ignored)
`high' is the most signifcant 16 bits; `low' is the 16 lsb."
  (let ((sec (/ time 1000.0)))
    (list (truncate (/ sec 65536))     ; high 16 bits of sec
          (truncate (mod sec 65536))   ; low 16 bits of sec
          (* 1000 (round (* 1000 (mod sec 1)))))))

(defsubst js-make-null-date ()
  (make-js-Date :proto (js-get
                        (js-global-get "Date")
                        "prototype")))

(defun js-make-date-args (args)
  "ECMA 15.9.3.1 -- new Date(y, m, [d, h, m, s, ms])"
  (let ((result (js-make-null-date)))
    (setf (js-Date-value result) (js-Date-UTC nil args))
    result))

(defun js-make-date-value (value)
  "ECMA 15.9.3.2 -- new Date(value)"
  (let* ((date (js-make-null-date))
         (time (js-to-primitive value))
         (value
          (if (js-string-p time)
              (js-parse-date time)
            (js-to-number time))))
    (setf (js-Date-value date) value)
    date))

(defun js-make-date-now ()
  "ECMA 15.9.3.3 -- new Date()"
  (let ((date (js-make-null-date))
        (time (current-time)))
    (setf (js-Date-value date)
          (js-date-elisp-to-js time))
    date))

(defun js-Date--call-- (ctor args)
  "ECMA 15.9.2 -- Date Constructor called as a function."
  (current-time-string))

(defun js-Date--construct-- (&optional funobj args)
  "ECMA 15.9.3 -- create a new Date object"
  (case (length args)
    (0 (js-make-date-now))
    (1 (js-make-date-value (car args)))
    (t (js-make-date-args args))))

(defun js-Date-UTC (ctor args)
  "ECMA 15.9.4.3 -- Date.UTC(y,m[,d[,h[,m[,s[,ms]]]]])"
  (let ((year (js-to-number (first args)))
        (month (js-to-number (second args)))
        (date (js-to-number (or (third args) 1)))
        (hrs (js-to-number (or (fourth args) 0)))
        (min (js-to-number (or (fifth args) 0)))
        (sec (js-to-number (or (sixth args) 0)))
        (ms (js-to-number (or (seventh args) 0))))
    (if (and (not (js-NaN-p year))
             (plusp year)
             (<= (js-to-integer year) 99))
        (setq year (+ 1900 (js-to-integer year))))
    (js-date-time-clip
     (js-date-make-date
      (js-date-make-day year month date)
      (js-date-make-time hrs min sec ms)))))

(defun js-Date-parse (ctor args)
  (js-parse-date (js-to-primitive (car args))))

(defun js-parse-date (string)
  "ECMA 15.9.4.2 -- Date.parse(string).
Returns UTC time value from STRING as an integral float, or NaN."
  (require 'parse-time)
  ;; (SEC MIN HOUR DAY MON YEAR DOW DST TZ)
  (let ((etime (parse-time-string string)))
    (js-Date-UTC nil (reverse (butlast etime 3)))))

(defsubst js--valid-date (date)
  (unless (js-Date-p date)
    (js-type-error "Invalid date")))

(defun js-format-date (format-string dateobj &optional utc)
  "Invokes `format-time-string' with FORMAT-STRING on DATEOBJ.
UTC is the UTC argument to `format-time-string'.

NOTE:  `format-time-string' has a bug, at least on Emacs 22/WinXP.
If %z (e.g. -0700) appears before %Z (the timezone name), then
it computes the DST wrong, e.g. claiming Pacific Standard Time
when it's really Pacific Daylight Time.  If %Z appears first, or
alone, it gets it correct.  So callers should format time strings
containing %z and %Z separately and concatenate them."
  (js--valid-date dateobj)
  (let ((etime (js-date-js-to-elisp
                (js-Date-value dateobj)))) ; (hi lo micros)
    (format-time-string format-string etime utc)))

(defun js-Date-toSource (dateobj args)
  "Return JavaScript source for cloning this DATEOBJ."
  (format "(new Date(%0.f))" (js-Date-value dateobj)))

(defun js-Date-toString (dateobj args)
  "ECMA 15.9.5.2 -- Date.prototype.toString()"
  ;; Thu Apr 26 2007 13:52:10 GMT-0700 (Pacific Standard Time)
  (concat 
   ;; See comments in `js-format-date' about %z and %Z.
   (js-format-date "%a %b %d %Y %H:%M:%S GMT%z " dateobj)
   (js-format-date "(%Z)" dateobj)))
    
(defun js-Date-toDateString (dateobj args)
  "ECMA 15.9.5.3 -- Date.prototype.toDateString()"
  ;; Thu Apr 26 2007
  (js-format-date "%a %b %d %Y" dateobj))

(defun js-Date-toTimeString (dateobj args)
  "ECMA 15.9.5.4 -- Date.prototype.toTimeString()"
  ;; 14:46:24 GMT-0700 (Pacific Daylight Time)
  (concat
   ;; See comments in `js-format-date' about %z and %Z.
   (js-format-date "%H:%M:%S GMT%z " dateobj)
   (js-format-date "(%Z)" dateobj)))

(defun js-Date-toLocaleString (dateobj args)
  "ECMA 15.9.5.5 -- Date.prototype.toLocaleString()"
  (js-format-date "%c" dateobj))

(defun js-Date-toLocaleDateString (dateobj args)
  "ECMA 15.9.5.6 -- Date.prototype.toLocaleDateString()"
  (js-format-date "%x" dateobj))

(defun js-Date-toLocaleTimeString (dateobj args)
  "ECMA 15.9.5.7 -- Date.prototype.toLocaleTimeString()"
  (js-format-date "%X" dateobj))

(defun js-Date-valueOf (dateobj args)
  "ECMA 15.9.5.8 -- Date.prototype.valueOf()"
  (js-Date-getTime dateobj args))

(defun js-Date-getTime (dateobj args)
  "ECMA 15.9.5.9 -- Date.prototype.getTime()"
  (js--valid-date dateobj)
  (js-Date-value dateobj))

(defmacro js-Date-compute-field (dateobj func &optional utc)
  (let ((value (gensym)))
    `(progn
       (js--valid-date ,dateobj)
       (let ((,value (js-Date-value ,dateobj)))
         (if (js-NaN-p ,value)
             0.0e+NaN
           (funcall ,func (if ,utc
                              ,value
                            (js-date-local-time ,value))))))))

(defun js-Date-getFullYear (dateobj args)
  "ECMA 15.9.5.10 -- Date.prototype.getFullYear()"
  (js-Date-compute-field dateobj #'js-date-year-from-time))

(defun js-Date-getUTCFullYear (dateobj args)
  "ECMA 15.9.5.11 -- Date.prototype.getUTCFullYear()"
  (js-Date-compute-field dateobj #'js-date-year-from-time t))

(defun js-Date-getMonth (dateobj args)
  "ECMA 15.9.5.12 -- Date.prototype.getMonth()"
  (js-Date-compute-field dateobj #'js-date-month-from-time))

(defun js-Date-getUTCMonth (dateobj args)
  "ECMA 15.9.5.13 -- Date.prototype.getUTCMonth()"
  (js-Date-compute-field dateobj #'js-date-month-from-time t))

(defun js-Date-getDate (dateobj args)
  "ECMA 15.9.5.14 -- Date.prototype.getDate()"
  (js-Date-compute-field dateobj #'js-date-date-from-time))

(defun js-Date-getUTCDate (dateobj args)
  "ECMA 15.9.5.15 -- Date.prototype.getUTCDate()"
  (js-Date-compute-field dateobj #'js-date-date-from-time t))

(defun js-Date-getDay (dateobj args)
  "ECMA 15.9.5.16 -- Date.prototype.getDay()"
  (js-Date-compute-field dateobj #'js-date-week-day))

(defun js-Date-getUTCDay (dateobj args)
  "ECMA 15.9.5.17 -- Date.prototype.getUTCDay()"
  (js-Date-compute-field dateobj #'js-date-week-day t))

(defun js-Date-getHours (dateobj args)
  "ECMA 15.9.5.18 -- Date.prototype.getHours()"
  (js-Date-compute-field dateobj #'js-date-hour-from-time))

(defun js-Date-getUTCHours (dateobj args)
  "ECMA 15.9.5.19 -- Date.prototype.getUTCHours()"
  (js-Date-compute-field dateobj #'js-date-hour-from-time t))

(defun js-Date-getMinutes (dateobj args)
  "ECMA 15.9.5.20-- Date.prototype.getMinutes()"
  (js-Date-compute-field dateobj #'js-date-min-from-time))

(defun js-Date-getUTCMinutes (dateobj args)
  "ECMA 15.9.5.21 -- Date.prototype.getUTCMinutes()"
  (js-Date-compute-field dateobj #'js-date-min-from-time t))

(defun js-Date-getSeconds (dateobj args)
  "ECMA 15.9.5.22 -- Date.prototype.getSeconds()"
  (js-Date-compute-field dateobj #'js-date-sec-from-time))

(defun js-Date-getUTCSeconds (dateobj args)
  "ECMA 15.9.5.23 -- Date.prototype.getUTCSeconds()"
  (js-Date-compute-field dateobj #'js-date-sec-from-time t))

(defun js-Date-getMilliseconds (dateobj args)
  "ECMA 15.9.5.24 -- Date.prototype.getMilliseconds()"
  (js-Date-compute-field dateobj #'js-date-ms-from-time))

(defun js-Date-getUTCMilliseconds (dateobj args)
  "ECMA 15.9.5.25 -- Date.prototype.getUTCMilliseconds()"
  (js-Date-compute-field dateobj #'js-date-ms-from-time t))

(defun js-Date-getTimezoneOffset (dateobj args)
  "ECMA 15.9.5.26 -- Date.prototype.getTimezoneOffset()"
  (js--valid-date dateobj)
  (let ((time (js-Date-value dateobj)))
    (if (js-NaN-p time)
        0.0e+NaN
      (/ (- time (js-date-local-time time))
         js-date-ms-per-min))))

(defun js-Date-setTime (dateobj args)
  "ECMA 15.9.5.27 -- Date.prototype.setTime()"
  (js--valid-date dateobj)
  (setf (js-Date-value dateobj)
        (js-date-time-clip (js-to-number (car args)))))

(defun js-Date-setMilliseconds (dateobj args)
  "ECMA 15.9.5.28 -- Date.prototype.setMilliseconds()"
  (js--valid-date dateobj)
  (let ((value (js-date-local-time (js-Date-value dateobj)))
        (ms (js-to-number (car args))))
    (setf (js-Date-value dateobj)
          (js-date-time-clip
           (js-date-utc
            (js-date-make-date
             (js-date-day value)
             (js-date-make-time
              (js-date-hour-from-time value)
              (js-date-min-from-time value)
              (js-date-sec-from-time value)
              ms)))))))

(defun js-Date-setUTCMilliseconds (dateobj args)
  "ECMA 15.9.5.29 -- Date.prototype.setUTCMilliseconds()"
  (js--valid-date dateobj)
  (let ((time (js-Date-value dateobj))
        (ms (js-to-number (car args))))
    (setf (js-Date-value dateobj)
          (js-date-time-clip
           (js-date-make-date
            (js-date-day time)
            (js-date-make-time
             (js-date-hour-from-time time)
             (js-date-min-from-time time)
             (js-date-sec-from-time time)
             ms))))))

(defun js-Date-setSeconds (dateobj args)
  "ECMA 15.9.5.30 -- Date.prototype.setSeconds(sec [, ms])"
  (js--valid-date dateobj)
  (let* ((time (js-date-local-time (js-Date-value dateobj)))
         (sec (js-to-number (car args)))
         (ms (if (cadr args)
                 (js-to-number (cadr args))
               (js-date-ms-from-time time))))
    (setf (js-Date-value dateobj)
          (js-date-time-clip
           (js-date-utc
            (js-date-make-date
             (js-date-day time)
             (js-date-make-time
              (js-date-hour-from-time time)
              (js-date-min-from-time time)
              sec ms)))))))

(defun js-Date-setUTCSeconds (dateobj args)
  "ECMA 15.9.5.31 -- Date.prototype.setUTCSeconds()"
  (js--valid-date dateobj)
  (let* ((time (js-Date-value dateobj))
         (sec (js-to-number (car args)))
         (ms (if (cadr args)
                 (js-to-number (cadr args))
               (js-date-ms-from-time time))))
    (setf (js-Date-value dateobj)
          (js-date-time-clip
           (js-date-make-date
            (js-date-day time)
            (js-date-make-time
             (js-date-hour-from-time time)
             (js-date-min-from-time time)
             sec ms))))))

(defun js-Date-setMinutes (dateobj args)
  "ECMA 15.9.5.32 -- Date.prototype.setMinutes()"
  (js--valid-date dateobj)
  (let* ((time (js-date-local-time (js-Date-value dateobj)))
         (min (js-to-number (car args)))
         (sec (if (second args)
                  (js-to-number (second args))
                (js-date-sec-from-time time)))
         (ms (if (third args)
                 (js-to-number (third args))
               (js-date-ms-from-time time))))
    (setf (js-Date-value dateobj)
          (js-date-time-clip
           (js-date-utc
            (js-date-make-date
             (js-date-day time)
             (js-date-make-time
              (js-date-hour-from-time time)
              min sec ms)))))))

(defun js-Date-setUTCMinutes (dateobj args)
  "ECMA 15.9.5.33 -- Date.prototype.setUTCMinutes()"
  (let* ((time (js-Date-value dateobj))
         (min (js-to-number (car args)))
         (sec (if (second args)
                  (js-to-number (second args))
                (js-date-sec-from-time time)))
         (ms (if (third args)
                 (js-to-number (third args))
               (js-date-ms-from-time time))))
    (setf (js-Date-value dateobj)
          (js-date-time-clip
           (js-date-make-date
            (js-date-day time)
            (js-date-make-time
             (js-date-hour-from-time time)
             min sec ms))))))

(defun js-Date-setHours (dateobj args)
  "ECMA 15.9.5.34 -- Date.prototype.setHours()"
  (let* ((time (js-date-local-time (js-Date-value dateobj)))
         (hrs (js-to-number (car args)))
         (min (if (second args)
                  (js-to-number (second args))
                (js-date-min-from-time time)))
         (sec (if (third args)
                  (js-to-number (third args))
                (js-date-sec-from-time time)))
         (ms (if (fourth args)
                 (js-to-number (fourth args))
               (js-date-ms-from-time time))))
    (setf (js-Date-value dateobj)
          (js-date-time-clip
           (js-date-utc
            (js-date-make-date
             (js-date-day time)
             (js-date-make-time hrs min sec ms)))))))

(defun js-Date-setUTCHours (dateobj args)
  "ECMA 15.9.5.35 -- Date.prototype.setUTCHours()"
  (let* ((time (js-Date-value dateobj))
         (hrs (js-to-number (car args)))
         (min (if (second args)
                  (js-to-number (second args))
                (js-date-min-from-time time)))
         (sec (if (third args)
                  (js-to-number (third args))
                (js-date-sec-from-time time)))
         (ms (if (fourth args)
                 (js-to-number (fourth args))
               (js-date-ms-from-time time))))
    (setf (js-Date-value dateobj)
          (js-date-time-clip
           (js-date-make-date
            (js-date-day time)
            (js-date-make-time hrs min sec ms))))))

(defun js-Date-setDate (dateobj args)
  "ECMA 15.9.5.36 -- Date.prototype.setDate()"
  (js--valid-date dateobj)
  (let ((time (js-date-local-time (js-Date-value dateobj)))
        (date (js-to-number (car args))))
    (setf (js-Date-value dateobj)
          (js-date-time-clip
           (js-date-utc
            (js-date-make-date
             (js-date-make-day
              (js-date-year-from-time time)
              (js-date-month-from-time time)
              date)
             (js-date-time-within-day time)))))))

(defun js-Date-setUTCDate (dateobj args)
  "ECMA 15.9.5.37 -- Date.prototype.setUTCDate()"
  (js--valid-date dateobj)
  (let ((time (js-Date-value dateobj))
        (date (js-to-number (car args))))
    (setf (js-Date-value dateobj)
          (js-date-time-clip
           (js-date-make-date
            (js-date-make-day
             (js-date-year-from-time time)
             (js-date-month-from-time time)
             date)
            (js-date-time-within-day time))))))

(defun js-Date-setMonth (dateobj args)
  "ECMA 15.9.5.38 -- Date.prototype.setMonth()"
  (js--valid-date dateobj)
  (let* ((time (js-date-local-time (js-Date-value dateobj)))
         (month (js-to-number (car args)))
         (date (if (second args)
                   (js-to-number (second args))
                 (js-date-date-from-time time))))
    (setf (js-Date-value dateobj)
          (js-date-time-clip
           (js-date-utc
            (js-date-make-date
             (js-date-make-day
              (js-date-year-from-time time) month date)
             (js-date-time-within-day time)))))))

(defun js-Date-setUTCMonth (dateobj args)
  "ECMA 15.9.5.39 -- Date.prototype.setUTCMonth()"
  (js--valid-date dateobj)
  (let* ((time (js-Date-value dateobj))
         (month (js-to-number (car args)))
         (date (if (second args)
                   (js-to-number (second args))
                 (js-date-date-from-time time))))
    (setf (js-Date-value dateobj)
          (js-date-time-clip
           (js-date-make-date
            (js-date-make-day
             (js-date-year-from-time time)
             month date)
            (js-date-time-within-day time))))))

(defun js-Date-setFullYear (dateobj args)
  "ECMA 15.9.5.40 -- Date.prototype.setFullYear()"
  (js--valid-date dateobj)
  (let* ((time (js-date-local-time (js-Date-value dateobj)))
         (year (js-to-number (car args)))
         (month (if (second args)
                    (js-to-number (second args))
                  (js-date-month-from-time time)))
         (date (if (third args)
                   (js-to-number (third args))
                 (js-date-date-from-time time))))
    (setf (js-Date-value dateobj)
          (js-date-time-clip
           (js-date-utc
            (js-date-make-date
             (js-date-make-day year month date)
             (js-date-time-within-day time)))))))

(defun js-Date-setUTCFullYear (dateobj args)
  "ECMA 15.9.5.41 -- Date.prototype.setUTCFullYear()"
  (let* ((time (js-Date-value dateobj))
         (year (js-to-number (car args)))
         (month (if (second args)
                    (js-to-number (second args))
                  (js-date-month-from-time time)))
         (date (if (third args)
                   (js-to-number (third args))
                 (js-date-date-from-time time))))
    (setf (js-Date-value dateobj)
          (js-date-time-clip
           (js-date-make-date
            (js-date-make-day year month date)
            (js-date-time-within-day time))))))

(defun js-Date-toUTCString (dateobj args)
  "ECMA 15.9.5.42 -- Date.prototype.toUTCString()"
  (js--valid-date dateobj)
  (js-format-date
   "%a %b %d %Y %H:%M:%S GMT" dateobj t))

(provide 'js-native-date)

;;; js-native-date.el ends here
