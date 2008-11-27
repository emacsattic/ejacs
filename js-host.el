;;; js-host.el -- JavaScript host objects for Emacs data types

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
;; I don't yet have an "EmacsConnect" spec for dealing with Lisp entities.
;; It'll likely include hosted objects for 50+ Emacs entities, which will
;; hopefully provide a nice object-oriented interface to the Emacs APIs.
;;
;; Not sure about cons, list, alist - kinda lisp-specific, so we might
;; just automatically map back and forth to JavaScript data structures.
;; Most of these classes will have a combination of static functions and
;; prototype instance functions.  Some will have lots of utilities that
;; provide cleaner APIs, e.g. for font-locking, which is currently a pain.
;;
;; Symbol, Vector, Hashtable, Obarray, PList
;; Buffer, Marker, Mark, Region, Window, Frame,
;; Minibuffer, WindowConfiguration, FrameConfiguration, Display, Desktop,
;; Process, Stream, File, Archive
;; Menu
;; Dialog, DragAndDrop, Terminal, Resource, Color
;; Keymap, Abbrev, Charset, InputMethod, Locale, CodingSystem
;; Syntax, SyntaxTable, Category, CategoryTable,
;; Motion (cursor), Overlay, TextProperty?, FontLock, Image, Cursor
;; Mode, MajorMode, MinorMode, Hook, Help, Customize
;; System, Command, Autoload, Macro?, CommandLoop

(defstruct (js-Host (:include js-Object))
  "JavaScript Host Object instance."
  elisp-obj)  ; slot containing the associated emacs-lisp object

(put 'cl-struct-js-Host 'js-class 'Host)
(put 'cl-struct-js-Host 'js-vtable (make-js-Internal))

;; TODO:  defstruct js-Emacs for the Global object, with own vtable.
;; The internal get/put/has functions should translate to Emacs symbols.

(defun js-add-host-objects (scope)
  "Add objects representing Emacs built-ins to SCOPE."
  (let ((obj-proto (js-get-Object-prototype scope))
        (func-proto (js-get-Function-prototype scope)))
    (js-define scope "Buffer" 
               (js-init-native-Buffer scope obj-proto func-proto)
               nil nil t))) ; {DontEnum}

;; The goal of the Buffer class is to organize the most common buffer
;; functions as instance methods.  You should always be able to call
;; any elisp function directly (how?) if you know its name, but having
;; some of them defined on Buffer should help with discovery.

(defvar js-Buffer-prototype nil "Buffer.prototype")

(defun js-camelize (name)
  "Convert a string NAME like `foo-bar-baz' to `fooBarBaz'.
A name like `foo-p' is converted to `isFoo'."
  (let ((split (split-string name "-")))
    (if (and (= 2 (length split))
             (string= "p" (cadr split)))
        (concat "is" (capitalize (car split)))
      (concat (car split)
              (mapconcat 'capitalize (cdr split) "")))))
                       
;; This almost works.  It would work if any of the buffer-* functions
;; took a buffer name, rather than a buffer object.  To make this
;; work, I'd have to make a defstruct js-Buffer with a slot holding
;; the elisp buffer.  But probably better to make a generic js-Host
;; struct with a slot holding the wrapped elisp object (done).  And
;; then it's just a matter of handling a whole bunch of special cases
;; around names and argument conventions.
;;
;; However, even if I got it working, I'm not convinced that it's "right".
;; I need to think more carefully about static vs. instance methods.
;; I think I should be using more static methods -- e.g., a Buffer object
;; should always correspond to an elisp buffer, and you'd use
;; Buffer.getOrCreate("name") or Buffer.get("name") to get one.  Once I'm
;; comfortable that I like the JavaScript-side interface, I can worry
;; about auto-generating methods.  Meantime I should make 'em by hand.
;;
(defun js-auto-method (elisp proto)
  "Create a method that invokes an elisp function.
The method's name is derived from the elisp function name.
A function like `buffer-disable-undo' is wrapped with a new callable
function called `js-Buffer-disable-undo' that calls the elisp function
with its arguments, or if there are no args, looks for a `name'
property on the target object and uses that as the argument."
  (let* ((elisp-name (symbol-name elisp))   ; buffer-enable-undo
         (split (split-string elisp-name "-")) ; 'buffer', 'enable', 'undo
         (head (car split))  ; "buffer"
         (tail (mapconcat 'identity (cdr split) "-"))  ; "enable-undo"
         (new-lisp-sym (intern (concat "js-"
                                       (capitalize head)
                                       "-" tail)))  ; 'js-Buffer-disable-undo
         (body `(lambda (thisobj args)
                  (apply #',elisp (if (null args)
                                      (list (js-get thisobj "name"))
                                    args)))))
    (fset new-lisp-sym body)
    (js-define-builtin proto (js-camelize tail) new-lisp-sym t)))

(defun js-init-native-Buffer (scope obj-proto func-proto)
  "Create the built-in Buffer constructor function."
  (let ((Buffer (make-js-Function :proto func-proto))
        (buffer-proto (make-js-Object :proto obj-proto)))
    (setq js-Buffer-prototype buffer-proto)

    (js-define Buffer "prototype" buffer-proto t t t)

    ;; make Buffer() same as new Buffer()
    (setf (js-Function-call-slot Buffer) 'js-Buffer--construct--)
    (setf (js-Function-construct-slot Buffer) 'js-Buffer--construct--)

    ;; Buffer static methods - make them all enumerable by default.

    (js-define-builtin Buffer "listBuffers" 'js-Buffer-list-buffers t)
    (js-define-builtin Buffer "get" 'js-Buffer-get t)

    ;; Buffer instance methods

    (js-define-builtin buffer-proto "toString" 'js-Buffer-toString
                       t ; callable signature
                       nil nil t) ; {DontEnum}

    ;; Make get/set access to "name" property read/write actual buffer name.
    (js-define-getter buffer-proto "name"
                      (js-elisp-lambda
                       (lambda (thisobj args)
                         (buffer-name (js-Host-elisp-obj thisobj))))
                      t t nil)         ; perm readonly dontenum
    
    (js-define-setter buffer-proto "name"
                      (js-elisp-lambda
                       (lambda (thisobj args)
                         (setf (buffer-name (js-Host-elisp-obj thisobj))
                               (car args))))
                      t t nil)  ; perm readonly dontenum
    
    (js-define-getter buffer-proto "isReadOnly"
                      (js-elisp-lambda
                       (lambda (thisobj args)
                         (js-to-boolean
                          (save-excursion
                            (set-buffer
                             (js-Host-elisp-obj thisobj))
                            buffer-read-only))))
                      t t nil)         ; perm readonly dontenum

    (js-define-setter buffer-proto "isReadOnly"
                      (js-elisp-lambda
                       (lambda (thisobj args)
                          (save-excursion
                            (set-buffer
                             (js-Host-elisp-obj thisobj))
                            (toggle-read-only (js-test (car args))))))
                      t t nil)         ; perm readonly dontenum

    ;; I can get this working later; see comment above js-auto-method.
    ;(js-auto-method 'buffer-disable-undo buffer-proto)

    (js-define-builtin buffer-proto "exists" 'js-Buffer-exists t)
    (js-put (js-get buffer-proto "exists") "length" 1)

    (js-define-builtin buffer-proto "kill" 'js-Buffer-kill t)
    (js-put (js-get buffer-proto "kill") "length" 1)

    (js-define-builtin buffer-proto "popTo" 'js-Buffer-popTo t)
    (js-put (js-get buffer-proto "popTo") "length" 1)

    Buffer))

(defun make-js-Buffer (&optional buf-or-name)
  "Make a Buffer host object initialized with BUF-OR-NAME.
If BUF is an emacs-lisp buffer object, we wrap it.  Otherwise we
call the normal Buffer constructor, which finds or creates the
buffer named NAME."
  (let* ((ctor (js-global-get "Buffer"))
         (js-buffer (make-js-Host :proto (js-get ctor "prototype"))))
    (setf (js-Host-elisp-obj js-buffer)
          (if (bufferp buf-or-name)
              buf-or-name
            (get-buffer-create (js-to-string buf-or-name))))
    js-buffer))
    
(defun js-Buffer-p (obj)
  "Return t if OBJ is a Buffer."
  (and (js-Host-p obj)
       (eq (js-Object-proto obj) js-Buffer-prototype)))

(defun js-Buffer--construct-- (&optional thisobj args)
  "Create a new Buffer that wraps an Emacs buffer.
ARGS list must have at least one element that is converted to a
string and becomes the buffer name, or a TypeError is thrown.
If (car ARGS) is a Buffer, then we create a new Buffer object
using its name.  If the Emacs buffer with the specified name does
not exist, it is created."
  (let (js-buffer
        buf-or-name)
    (cond
     ((null args)
      (js-type-error "Wrong number of arguments: 0"))
     ((js-Buffer-p thisobj)
      (setq buf-or-name (js-Host-elisp-obj thisobj)))
     (t
      (setq buf-or-name (js-to-string (car args)))
      (if (string-match "^\\s-*$" buf-or-name)
          (js-type-error "Buffer name cannot be empty"))))

    (make-js-Buffer buf-or-name)))

;; static methods

(defun js-Buffer-list-buffers (ctor args)
  "Returns the list of all buffers as a JavaScript array."
  (let* ((frame (and (js-Host-p (car args))
                     (js-Host-elisp-obj (car args))))
         (buflist (mapcar 'make-js-Buffer
                          (buffer-list frame))))
    (apply #'js-make-Array buflist)))
           
(defun js-Buffer-get (ctor args)
  "Returns the specified named buffer, or null."
  (if (null args)
      'undefined
    (let ((buf (get-buffer (js-to-string (car args)))))
      (if buf
          (make-js-Buffer buf)
        'undefined))))
           
;; instance methods

(defun js-Buffer-toString (buffer args)
  (unless (js-Buffer-p buffer)
    (js-type-error (js-format "%s is not a Buffer" buffer)))
  (concat "[buffer " (js-get buffer "name") "]"))

(defun js-to-buffer (buffer args)
  (if (null args)
      (js-get buffer "name")
    (js-to-string (car args))))
  
(defun js-Buffer-exists (buffer args)
  "Returns true if the specified buffer exists."
  (js-to-boolean
   (get-buffer
    (js-to-buffer buffer args))))

(defun js-Buffer-kill (buffer args)
  "Kills the associated Emacs buffer, or buffer named in ARGS."
  (kill-buffer
   (js-to-buffer buffer args)))

(defun js-Buffer-popTo (buffer args)
  "Pop to this buffer."
  (pop-to-buffer
   (js-to-buffer buffer args)))


(provide 'js-host)
