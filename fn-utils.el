;;; fn-utils.el --- Functional utilities for Emacs Lisp   -*- lexical-binding: t -*-

;; Copyright (C) 2016 Troy Pracy

;; Author: Troy Pracy
;; Keywords: functional
;; Version: 0.1.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (dash "2.12.1") (dash-functional "1.2.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; While cl and dash provide a good foundation for functional programming in
;; Emacs Lisp, some things remain unavailable, inconvenient or verbose.

;; fn.el provides essential facilities for concise and readable functional
;; programming, including a concise anonymous function syntax for inline use, a
;; function alias binding construct, convenient range-function/list-comprehension
;; syntax and a predicate-based case-expression.

;;; Code:

(require 'cl-lib)
(require 'dash-functional)



;; ,-------------------,
;; | Function Creation |
;; '-------------------'

(defmacro fn (&rest body)
  "Return a function defined by BODY.

Intended for inline use where concision is desired.  If creating a function to
bind as a function value, use `lambda' or `-lambda'.

The definition BODY may use anaphoric parameters to refer to the arguments. For
a single-argument function, use <> or it. For a multiple-argument function, use
<1> to refer to the first argument, <2> to refer to the second, and so on up to
<9>.

If applied to a literal, creates a constant function, or equivalently, a thunk (
since it can be called with any number of arguments).

Examples:

  (-map (fn (* <> <>)) (number-sequence 0 10))
  ;; (0 1 4 9 16 25 36 49 64 81 100)

  (-map (fn (/ (-sum <>)
               (length <>)))
        '((3.0 4.0 5.0 5.0 10.0)
          (1.0 2.0 2.0 2.0)
          (1 5)))
  ;; (5.4 1.75 3)
    ;; find average of each list

  (-filter (fn (zerop (mod <> 3)))
           (number-sequence 1 10))
  ;; (3 6 9)

  (funcall (fn 7))
  ;; 7"
  (declare (debug 'body))
  (let* ((argsym (make-symbol "ARGS"))
         (symbol-vars '(<> it))
         (digit-vars '(<1> <2> <3> <4> <5> <6> <7> <8> <9>))
         (symbols (eval (backquote (-flatten ',body))))
         (digit-vars-used (-intersection digit-vars symbols))
         (symbol-vars-used (-intersection symbol-vars symbols))
         bindings)
    (--map (!cons (list  it
                         `(nth 0 ,argsym))
                  bindings)
           symbol-vars-used)
    (--map (!cons (list  it
                         `(nth ,(-elem-index it digit-vars) ,argsym))
                  bindings)
           digit-vars-used)
    `(lambda (&rest ,argsym)
       (let (,@bindings)
         ,@body))))



(defmacro fn: (&rest body)
  "Return a function defined by (BODY).

Intended for inline use where concision is desired.  If creating a function to
bind as a function value, use `lambda' or `-lambda'.

The definition BODY may use the anaphoric parameters <>, it or <1> to refer to the
first argument, <2> to refer to the second, and so on up to <9>.

Examples:

  (-map (fn: * <> <>) (number-sequence 0 10))
  ;; (0 1 4 9 16 25 36 49 64 81 100)

  (-filter (fn: > it 0)
           '(-5 2 0 0 3 -1 0 4))
  ;; (2 3 4)"
  (declare (debug 'body))
  (let* ((argsym (make-symbol "ARGS"))
         (symbol-vars '(<> it))
         (digit-vars '(<1> <2> <3> <4> <5> <6> <7> <8> <9>))
         (symbols (eval (backquote (-flatten ',body))))
         (digit-vars-used (-intersection digit-vars symbols))
         (symbol-vars-used (-intersection symbol-vars symbols))

         bindings)
    (--map-indexed (!cons (list  it
                                 `(nth 0 ,argsym))
                          bindings)
                   symbol-vars-used)
    (--map-indexed (!cons (list  it
                                 `(nth ,(-elem-index it digit-vars) ,argsym))
                          bindings)
                   digit-vars-used)
    `(lambda (&rest ,argsym)
       (let (,@bindings)
         (,@body)))))



(defun fn-bindl (function &rest largs)
  "Partial application of FUNCTION to LARGS."
  `(closure (t) (&rest args)
            (apply ',function
                   ,@(mapcar (lambda (x) `',x)
                             largs)
                   args)))

(defun fn-bindr (function &rest rargs)
  "Right-hand partial application of FUNCTION to RARGS."
  `(closure (t) (&rest args)
            (apply ',function
                   (append args ',rargs))))



(defun fn-fork (merge-function &rest functions)
  "Apply MERGE-FUNCTION to FUNCTIONS to produce a composed function.

Examples:

  (fn-call (fn-fork 'list '+ '*) 2 3 5)
  ;; (10 30)
  ;; compute the sum and product
  ;; equivalent to: (fn-call (-juxt '+ '*) 2 3 5)

  (fn-call (fn-fork '/ '-sum 'length)
           '(2.0 0 2.0 3.0 5.0 1.0 8.0))
  ;; 3.0
  ;; compute the mean of a list

  (-map (fn-fork '+
          (fn (expt <> 5))
          (fn <>)
          (fn 1))
        '(1 2 10))
  ;; (3 35 100011)
  ;; compute the polynomial x^5+x+1 at the points 1, 2, 10

Notes:

This is variadic analogue of J's \"fork\" phrasal form.

It is also a generalization of dash.el's `-juxt' function, which is equivalent
to `fn-fork'with the `list' function passed as the first argument."
  (lambda (&rest args)
    (apply merge-function
           (-map (lambda (f) (apply f args))
                 functions))))



(defun fn-pos? (transform)
  "Return a predicate that returns t if TRANSFORM of its argument is positive.

Example:
  (-filter (fn-pos? 'car)
          '(( 3 .  0)
            (-2 .  5)
            ( 0 .  1)
            ( 9 . -3)))
  ;; ((3 . 0) (9 . -3))"
  (lambda (x)
    (> (funcall transform x) 0)))

(defun fn-neg? (transform)
  "Return a predicate that returns t if TRANSFORM of its argument is negative.

Example:
  (-filter (fn-neg? 'car)
          '(( 2 . -2)
            (-2 .  3)
            ( 0 .  1)
            (-9 .  0)))
  ;; ((-2 . 3) (-9 . 0))"
  (lambda (x)
    (< (funcall transform x) 0)))

(defun fn-zero? (transform)
  "Return a predicate that returns t if TRANSFORM of its argument is zero.

Example:
  (-filter (fn-zero? 'car)
          '(( 0 .  0)
            (-1 .  1)
            ( 0 .  1)
            ( 7 . -5)))
  ;; ((0 . 0) (0 . 1))"
  (lambda (x)
    (= (funcall transform x) 0)))



;; ,-----------------------------------,
;; | Higher-Order Functions and Macros |
;; '-----------------------------------'

(defalias 'fn-call 'funcall)


(defmacro fn-case (expr &rest cases)
  "Eval EXPR and choose value by sequentially testing predicates.

CASES is a list of (PREDICATE VALUE) pairs.

Example:
  (fn-case 77
    ((fn (> it 100))  'too-big)
    (oddp             'odd-number)
    (evenp            'even-number))
  odd-number

Ideal for fairly simple case expressions involving predicates.  If you only
need to test equality against values, use regular `case'.  If you need more
complex predicates, consider using the more powerful but verbose `pcase'.

\(fn EXPR (PREDICATE VALUE)...)"
  (declare (indent 1)
           (debug (form &rest (sexp body))))
  (let ((transformed-cases
         (-map (-lambda ((function definition))
                 (if (symbolp function)
                     `((funcall ',function ,expr)
                       ,definition)
                   `((funcall ,function ,expr)
                     ,definition)))
               cases)))
    `(cond ,@transformed-cases)))



(defmacro fn-alias (bindings &rest body)
  "Execute with temporary function definitions.

Each DEFINITION in BINDINGS may be a either a symbol, in which case FUNC is
bound to its `symbol-function', or an expression evaluating to a function value.

Example:
  (fn-alias ((seq    'number-sequence)
            (double  (fn (* 2 <>))))
    (--map (double it) (seq 1 10)))
  ;; (2 4 6 8 10 12 14 16 18 20)

\(fn ((FUNC DEFINITION) ...) BODY...)"
  (declare (indent 1)
           (debug let))
  (let ((set-forms
         (cl-loop
          for (name defn) in bindings
          collect
          `(fset ',name
                 ,(if (symbolp defn)
                      (symbol-function defn)
                    defn)))))
    `(unwind-protect
         (progn
           ,@set-forms
           ,@body))))



(defmacro fn-setq (&rest bindings)
  "Set the FUNCTION definition of each SYMBOL in a set of BINDINGS.

Example:
  (fn-setq my-increment (fn-bindl '+ 1)
           my-double    (fn-bindl '* 2))

\(fn [SYMBOL FUNCTION] ...)"
  (declare (debug t))
  (let ((set-forms
         (cl-loop for (name defn) on bindings by 'cddr
                  collect `(fset ',name ,defn))))
    `(progn ,@set-forms)))



;; ,---------------,
;; | Miscellaneous |
;; '---------------'



(provide 'fn-utils)

;;; fn-utils.el ends here
