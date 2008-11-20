;;; splay-tree.el - implements a top-down Sleator/Tarjan Splay Tree
;;
;; Author: Steve Yegge (steve.yegge@gmail.com)
;; License:  This code is in the public domain.

;;; Commentary:
;;
;; Implements a top-down Sleator/Tarjan Splay Tree.
;;
;; Adapted from Danny Sleator's public-domain Java code at:
;; http://www.link.cs.cmu.edu/link/ftp-site/splaying/SplayTree.java
;;
;; Supported API:  (see function docs for arguments)
;;
;;  - `make-splay-tree' creates a new, empty tree
;;  * `splay-tree-insert' inserts a key and value
;;  * `splay-tree-find' looks for a key
;;  * `splay-tree-delete' deletes a key and its value
;;  * `splay-tree-find-min' returns the least key in the tree
;;  * `splay-tree-find-max' returns the greatest key in the tree
;;  - `splay-tree-empty-p' tests if tree is empty
;;  - `splay-tree-size' returns number of keys in tree
;;  - `splay-tree-inorder-walk' visits the tree nodes in order
;;  - `splay-tree-preorder-walk' visits the tree nodes in preorder
;;  - `splay-tree-postorder-walk' visits the tree nodes in postorder
;;  - `splay-tree-filter' returns a list of items from the tree
;;  * `splay-tree-delete-if' deletes items satisfying a predicate
;;  - `splay-tree-mapcar' maps a function to each item in the tree
;;  - `splay-tree-build' creates a splay tree from an alist
;;
;; where '*' denotes a potentially tree-modifying operation.
;;
;; Additionally there are some diagnostic functions:
;;
;;  - `splay-tree-depth' returns length of longest path to a leaf
;;  - `splay-tree-prettyprint' prints a debugging view of the tree
;;
;; Performance:
;;  - insert/find/delete are amortized O(log n)
;;    - but if a subset M of the items is being accessed => O(log m)
;;  - the tree's structure changes on insert/find/delete
;;  - consing:  only when inserting a nonexistent key
;;
;; All nodes have a cons for the left/right child links, even leaves,
;; because any node can be splayed to an interior position.


;;; History:
;; 4/27/2007 - initial version

;;; Code:

(eval-when-compile
  (require 'cl))

(defconst splay-tree-version "1.0")

(defstruct splaytree
  root        ; root node, nil when empty
  test        ; comparator
  header      ; private node for splay op
  (size 0))

(defun splay-tree-strcmp (s1 s2)
  "Default comparator function for a new Splay Tree.
Assumes S1 and S2 are strings and/or symbols.
Compares S1 and S2, returning -1, 0 or 1.
Returns -1 if s1 < s2, 0 if s1 == s2, and 1 if s1 > s2."
  (if (string< s1 s2)
      -1
    (if (string< s2 s1)
        1
      0)))

(defsubst splay-tree-node (key &optional value)
  "Internal:  make a new splay tree node with KEY and VALUE."
  (list key value (cons nil nil)))

(defsubst splay-node-key (node)
  "Internal:  return key for NODE."
  (car node))

(defsubst splay-node-value (node)
  "Internal:  return the value for NODE."
  (cadr node))

(defsubst splay-node-key-value (node)
  "Internal:  return (key . value) for NODE."
  (cons (car node) (cadr node)))

(defsubst splay-set-value (node value)
  "Internal:  set value for NODE to VALUE."
  (setcar (cdr node) value))

(defsubst splay-node-left (node)
  "Internal:  return left child link for NODE."
  (caaddr node))

(defsubst splay-node-right (node)
  "Internal:  return right child link for NODE."
  (cdaddr node))

(defsubst splay-set-left (node left)
  "Internal:  set left child of NODE to LEFT, and return LEFT."
  (setcar (caddr node) left))

(defsubst splay-set-right (node right)
  "Internal:  set right child of NODE to RIGHT, and return RIGHT."
  (setcdr (caddr node) right))
     
(defsubst splay-tree-compare-to-root (tree key)
  "Internal:  call the comparator for TREE on KEY and the root key."
  (funcall (splaytree-test tree)
           key
           (splay-node-key (splaytree-root tree))))

(defsubst splay-tree-compare-to-node (tree key node)
  "Internal:  call the comparator for TREE on KEY and NODE's key."
  (funcall (splaytree-test tree)
           key
           (splay-node-key node)))

(defun make-splay-tree (&optional test)
  "Create and return a new splay tree.

TEST is the function to compare keys.  The default test function
is `splay-tree-strcmp', which allows symbols and strings as keys.
The TEST function, if supplied, must take two keys as arguments and
return a negative number, 0, or a positive number if key1 is less
than, equal to, or greater than key2, respectively."
  (make-splaytree :root nil
                  :test (or test #'splay-tree-strcmp)
                  :header (splay-tree-node nil)))
                            
(defun splay-tree-insert (tree key &optional value)
  "Update TREE to include KEY and optional VALUE.
KEY must be non-nil.  After insertion, the node for KEY will be
the root node.  If KEY is already in the tree, its node value is
replaced with VALUE."
  (let (n c root)
    (if (null key)
        (error "Cannot store null keys"))
    (if (null (splaytree-root tree))
        ;; empty tree:  set root node and return
        (progn
          (setf (splaytree-root tree)
                (splay-tree-node key value))
          (incf (splaytree-size tree)))
      (splay-tree-splay tree key)
      (setq root (splaytree-root tree))
      (if (zerop (setq c (splay-tree-compare-to-root tree key)))
          ;; key exists
          (splay-set-value root value)
        ;; key does not exist => make new node
        (incf (splaytree-size tree))
        (setq n (splay-tree-node key value))
        (if (minusp c)
            (progn
              (splay-set-left n (splay-node-left root))
              (splay-set-right n root)
              (splay-set-left root nil))
          (splay-set-right n (splay-node-right root))
          (splay-set-left n root)
          (splay-set-right root nil))
        (setf (splaytree-root tree) n)))))

(defun splay-tree-delete (tree key)
  "Remove KEY from TREE after splaying it to the root.
Returns (KEY . VALUE) for the deleted key, or nil if not found."
  (let (n root)
    (splay-tree-splay tree key)
    (if (not (zerop (splay-tree-compare-to-root tree key)))
        ;; key not found
        nil
      ;; delete the root
      (prog1 (splay-node-key-value (setq root (splaytree-root tree)))
        (decf (splaytree-size tree))
        (if (null (splay-node-left root))
            (setf (splaytree-root tree) (splay-node-right root))
          (setq n (splay-node-right root))
          (setf (splaytree-root tree) (splay-node-left root))
          (splay-tree-splay tree key)
          (splay-set-right (splaytree-root tree) n))))))

(defun splay-tree-find (tree key)
  "Find KEY in TREE and return (KEY . VALUE).
Returns (KEY . VALUE) for KEY, or nil if not found."
  (when (splaytree-root tree)
    (splay-tree-splay tree key)
    (when (zerop (splay-tree-compare-to-root tree key))
      (splay-node-key-value (splaytree-root tree)))))

(defun splay-tree-find-min (tree)
  "Find and return the smallest item in TREE, splaying to root.
Returns (KEY . VALUE) for the smallest key, or nil if TREE is empty."
  (let ((n (splaytree-root tree))
        left)
    (when n
      (while (setq left (splay-node-left n))
        (setq n left))
      (splay-tree-splay tree (splay-node-key n))
      (splay-node-key-value n))))

(defun splay-tree-find-max (tree)
  "Find and return the largest item in TREE, splaying to root.
Returns (KEY . VALUE) for the largest key, or nil if TREE is empty."
  (let ((n (splaytree-root tree))
        right)
    (when n
      (while (setq right (splay-node-right n))
        (setq n right))
      (splay-tree-splay tree (splay-node-key n))
      (splay-node-key-value n))))

(defun splay-tree-splay (tree key)
  "Internal method to perform a top-down splay of KEY in TREE.
If KEY is in the tree, its node becomes the root.
Otherwise, the root because either the greatest key < key
in the tree, or the least key > key in the tree."
  (let* ((header (splaytree-header tree))
         (l header)
         (r header)
         (f (splaytree-root tree))
         y c)
    (splay-set-left header nil)
    (splay-set-right header nil)
    (catch 'break
      (while t
        (setq c (splay-tree-compare-to-node tree key f))
        (cond
         ((minusp c)
          (unless (setq y (splay-node-left f))
            (throw 'break nil))
          (when (minusp (splay-tree-compare-to-node tree key y))
            (splay-set-left f (splay-node-right y))    ; rotate right
            (splay-set-right y f)
            (setq f y)
            (unless (splay-node-left f)
              (throw 'break nil)))
          (splay-set-left r f)                         ; link right
          (setq r f
                f (splay-node-left f)))
         ((plusp c)
          (unless (setq y (splay-node-right f))
            (throw 'break nil))
          (when (plusp (splay-tree-compare-to-node tree key y))
            (splay-set-right f (splay-node-left y))    ; rotate left
            (splay-set-left y f)
            (setq f y)
            (unless (splay-node-right f)
              (throw 'break nil)))
          (splay-set-right l f)                        ; link left
          (setq l f
                f (splay-node-right f)))
         (t
          (throw 'break nil)))))
    (splay-set-right l (splay-node-left f))            ; assemble
    (splay-set-left  r (splay-node-right f))
    (splay-set-left  f (splay-node-right header))
    (splay-set-right f (splay-node-left header))
    (setf (splaytree-root tree) f)))

(defun splay-tree-empty-p (tree)
  "Return t if TREE is empty."
  (null (splaytree-root tree)))

(defun splay-tree-size (tree)
  "Return the number of nodes in TREE.
This constant-time operation does not modify the tree."
  (splaytree-size tree))

(eval-when-compile
  (defun splay-tree-depth-helper (n d) nil)
  (defun splay-tree-inorder-helper (n d) nil)
  (defun splay-tree-preorder-helper (n d) nil)
  (defun splay-tree-postorder-helper (n d) nil))

(defun splay-tree-depth (tree)
  "Return the maximum depth of TREE.
This operation does not modify the tree."
  (let ((max-depth 0))
    (defun splay-tree-depth-helper (node depth)
      (if (> depth max-depth)
          (setq max-depth depth))
      (when (splay-node-left node)
        (splay-tree-depth-helper
         (splay-node-left node) (1+ depth)))
      (when (splay-node-right node)
        (splay-tree-depth-helper
         (splay-node-right node) (1+ depth))))
    (splay-tree-depth-helper (splaytree-root tree) 0)
    max-depth))

(defsubst splay-traverse-left (helper node visitor)
  "Internal:  follow left link"
  (when (splay-node-left node)
    (funcall helper (splay-node-left node) visitor)))

(defsubst splay-traverse-right (helper node visitor)
  "Internal:  follow right link"
  (when (splay-node-right node)
    (funcall helper (splay-node-right node) visitor)))

(defsubst splay-visit-node (node visitor)
  "Internal:  call VISITOR on key/value in NODE"
  (funcall visitor
           (splay-node-key node)
           (splay-node-value node)))

(defun splay-tree-inorder-walk (tree visitor)
  "Visit the nodes of TREE in an inorder traversal.

That is, first a node's left subtree is visited, then the node, then
the node's right subtree.  VISITOR is a function that takes a key and
optional value.  For example, calling (splay-tree-inorder-walk 'identity)
will print the keys in sorted order.

TREE is not modified during the traversal.  Modifying TREE during
the traversal will yield undefined, but probably bad, results."

  (defun splay-tree-inorder-helper (node visitor)
    (splay-traverse-left #'splay-tree-inorder-helper node visitor)
    (splay-visit-node node visitor)
    (splay-traverse-right #'splay-tree-inorder-helper node visitor))

  (splay-tree-inorder-helper (splaytree-root tree) visitor))

(defun splay-tree-preorder-walk (tree visitor)
  "Visit the nodes of TREE in a preorder traversal.

Each node is visited, then its left subtree, then its right subtree.
VISITOR is a function that takes a key and optional value.

TREE is not modified during the traversal.  Modifying TREE during
the traversal will yield undefined, but probably bad, results."

  (defun splay-tree-preorder-helper (node visitor)
    (splay-visit-node node visitor)
    (splay-traverse-left #'splay-tree-preorder-helper node visitor)
    (splay-traverse-right #'splay-tree-preorder-helper node visitor))

  (splay-tree-preorder-helper (splaytree-root tree) visitor))

(defun splay-tree-postorder-walk (tree visitor)
  "Visit the nodes of TREE in an postorder traversal.

That is, first a node's left subtree is visited, then the right
subtree, and finally the node.  VISITOR is a function that takes a key
and optional value.

TREE is not modified during the traversal.  Modifying TREE during
the traversal will yield undefined, but probably bad, results."

  (defun splay-tree-postorder-helper (node visitor)
    (splay-traverse-left #'splay-tree-postorder-helper node visitor)
    (splay-traverse-right #'splay-tree-postorder-helper node visitor)
    (splay-visit-node node visitor))

  (splay-tree-postorder-helper (splaytree-root tree) visitor))

(defun splay-tree-filter (tree predicate)
  "Return a list of all items from TREE that pass PREDICATE.

PREDICATE is a function that takes a key and value, and returns t
to add the item to the result tree.  The return value of the function
is an alist of (key . value) pairs.

This function does not modify TREE."
  (let ((result '()))
    (splay-tree-preorder-walk tree
                              (lambda (k v)
                                (if (funcall predicate k v)
                                    (push (cons k v) result))))
    result))
  
(defun splay-tree-delete-if (tree predicate &optional count)
  "Remove COUNT (default: all) items from TREE that pass PREDICATE.

PREDICATE is a function that takes a key and optional value.
Returns a list of the deleted (key . value) pairs.

This is a destructive function - the tree is modified in place."
  (let ((max (or count (splaytree-size tree)))
        (items (splay-tree-filter tree predicate)))
    (loop for (k . v) in items do
          (splay-tree-delete tree k)
          finally return items)))

(defun splay-tree-mapcar (tree function &optional order)
  "Apply FUNCTION to each item in TREE and make a list of the results.

The list will have as many elements as TREE.  If ORDER is 'preorder
or 'postorder, it will use a preorder or postorder walk, respectively.
If ORDER is any other value it will do an inorder traversal.

FUNCTION should take two arguments:  key and value.
The results of calling FUNCTION are added to the result list.

For example, assuming the keys in TREE are symbols, this code

  (splay-tree-mapcar tree
                     (lambda (k v)
                       (upcase (symbol-name k))))

will assemble a list of the sorted, uppercased keys of TREE."
  (let ((walker (case order
                  (preorder #'splay-tree-preorder-walk)
                  (postorder #'splay-tree-postorder-walk)
                  (t #'splay-tree-inorder-walk)))
        result)
    (funcall walker tree (lambda (k v)
                           (push (funcall function k v) result)))
    (nreverse result)))

(defun splay-tree-prettyprint (tree)
  "Insert a quasi-pretty printed rendition of TREE in current buffer.
Tree nodes are in the form (KEY VALUE (LEFT . RIGHT)."
  (cl-prettyprint (splaytree-root tree)))

(defun splay-tree-build (alist &optional test)
  "Make a splay tree and populate it from ALIST.
ALIST is an alist of key/value pairs that are inserted into
the tree.  Returns the new tree.  TEST, if specified, is a
splay-tree comparator function that takes two keys and returns
negative, zero, or positive if key1 is less, equal to, or
greater than key2."
  (loop with tree = (make-splay-tree test)
        for (k . v) in alist do
        (splay-tree-insert tree k v)
        finally return tree))

(provide 'splay-tree)

;;; splay-tree.el ends here
