;;; ipe-test-list.el --- Insert Pair Edit - List Movement Tests -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Brian Kavanagh

;; Author: Brian Kavanagh <brians.emacs@gmail.com>
;; Maintainer: Brian Kavanagh <brians.emacs@gmail.com>
;; Created: 28 June, 2020
;; Version: 1.1
;; Package: ipe
;; Keywords: internal local
;; Homepage: https://github.com/BriansEmacs/insert-pair-edit.el

;; -------------------------------------------------------------------
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this programe.  If not, see
;; <https://www.gnu.org/licenses/>.

;; -------------------------------------------------------------------
;;; Commentary:
;;
;; This file defines a set of `ert' (Emacs Regression Test)s for the
;; `ipe' (Insert Pair Edit) package.
;;
;; These tests are all defined using the `ipe-test-def-kbd' macro
;; (See `ipe-test.el'), which is used to test the interactive
;; functions within `ipe-edit-mode' by executing a set of keystrokes
;; against a buffer containing text, and comparing the result (both
;; output text, and cursor positions) with an 'expected' output.
;;
;; The tests within this file are used to test the movement of the
;; OPEN and CLOSE strings of an `ipe' PAIR with a :movement of `list'
;; This is done via the 'Basic Movement' 'Insert Pair Edit'
;; `ipe-edit-mode' commands:
;;
;;    `ipe-edit--open-beg'
;;    `ipe-edit--open-up'
;;    `ipe-edit--open-backward'
;;    `ipe-edit--open-forward'
;;    `ipe-edit--open-down'
;;    `ipe-edit--open-end'
;;    `ipe-edit--close-beg'
;;    `ipe-edit--close-up'
;;    `ipe-edit--close-backward'
;;    `ipe-edit--close-forward'
;;    `ipe-edit--close-down'
;;    `ipe-edit--close-end'

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'ipe-test)

(defvar ipe-test-list-options
  '((ipe-move-point-on-insert   nil)
    (ipe-prefix-moves-close-p   t)
    (ipe-edit--movement-keysets '(modifiers))
    (ipe-update-forward-first-p nil)
    (ipe-pairs
     '(("{" "{" "}" (:movement list))
       ("[" "[" "]" (:movement list))))
    (ipe-mode-pairs             nil))
  "Options used by `ipe-test-def-kbd' for `ipe-test-list'.")

(ipe-test-def-kbd list-basic-insert-1 ()
  "Test `ipe-insert-pair-edit' in an empty buffer.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  ""
  "{|}"
  "M-( {")

(ipe-test-def-kbd list-basic-insert-2 ()
  "Test `ipe-insert-pair-edit' at the start of buffer.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "|(((a)(b)(c)(d)(f)(g)(h)(i)))"
  "{|}(((a)(b)(c)(d)(f)(g)(h)(i)))"
  "M-( { RET")

(ipe-test-def-kbd list-basic-insert-3 ()
  "Test `ipe-insert-pair-edit' in the middle of a buffer.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b)(c)(d){|}(f)(g)(h)(i)))"
  "M-( { RET")

(ipe-test-def-kbd list-basic-insert-4 ()
  "Test `ipe-insert-pair-edit' at the end of buffer.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)(f)(g)(h)(i)))|"
  "(((a)(b)(c)(d)(f)(g)(h)(i))){|}"
  "M-( { RET")

(ipe-test-def-kbd list-basic-insert-5 ()
  "Test `ipe-insert-pair-edit' at 'offset' beginning of buffer.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "|  (((a)(b)(c)(d)(f)(g)(h)(i)))"
  "{|}  (((a)(b)(c)(d)(f)(g)(h)(i)))"
  "M-( { RET")

(ipe-test-def-kbd list-basic-insert-6 ()
  "Test `ipe-insert-pair-edit' at 'offset' end of buffer.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)(f)(g)(h)(i)))  |"
  "(((a)(b)(c)(d)(f)(g)(h)(i)))  {|}"
  "M-( { RET")

(ipe-test-def-kbd list-basic-prefix-insert-1 ()
  "Test `ipe-insert-pair-edit' at with a numeric prefix.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b)(c)(d){|(f)}(g)(h)(i)))"
  "C-1 M-( { RET")

(ipe-test-def-kbd list-basic-prefix-insert-2 ()
  "Test `ipe-insert-pair-edit' with a '2' numeric prefix.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b)(c)(d){|(f)(g)}(h)(i)))"
  "C-2 M-( { RET")

(ipe-test-def-kbd list-basic-prefix-insert-3 ()
  "Test `ipe-insert-pair-edit' with a '3' numeric prefix.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b)(c)(d){|(f)(g)(h)}(i)))"
  "C-3 M-( { RET")

(ipe-test-def-kbd list-basic-prefix-insert-4 ()
  "Test `ipe-insert-pair-edit' with a '4' numeric prefix.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b)(c)(d){|(f)(g)(h)(i)}))"
  "C-4 M-( { RET")

(ipe-test-def-kbd list-open-start-1 ()
  "Test `ipe-insert-pair-edit' OPEN start.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "{}(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "M-( { C-a RET")

(ipe-test-def-kbd list-open-start-2 ()
  "Test `ipe-insert-pair-edit' OPEN start.

Using a 'list PAIR at start of buffer."
  ipe-test-list-options
  nil
  "|(((a)(b)(c)(d)(f)(g)(h)(i)))"
  "{|}(((a)(b)(c)(d)(f)(g)(h)(i)))"
  "M-( { C-a RET")

(ipe-test-def-kbd list-open-start-3 ()
  "Test `ipe-insert-pair-edit' OPEN start.

Using a 'list PAIR at end of buffer."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)(f)(g)(h)(i)))|"
  "{}(((a)(b)(c)(d)(f)(g)(h)(i)))|"
  "M-( { C-a RET")

(ipe-test-def-kbd list-open-start-4 ()
  "Test `ipe-insert-pair-edit' OPEN start.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  '("(((a)(b)(c)(d)(f)(g)(h)(i)))"
    "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
    "(((a)(b)(c)(d)(f)(g)(h)(i)))")
  '("{}(((a)(b)(c)(d)(f)(g)(h)(i)))"
    "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
    "(((a)(b)(c)(d)(f)(g)(h)(i)))")
  "M-( { C-a C-a RET")

(ipe-test-def-kbd list-open-up-1 ()
  "Test `ipe-insert-pair-edit' OPEN up.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "({((a)(b)(c)(d)|(f)(g)(h)(i))})"
  "M-( { C-p RET")

(ipe-test-def-kbd list-open-up-2 ()
  "Test `ipe-insert-pair-edit' OPEN up x2.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "{(((a)(b)(c)(d)|(f)(g)(h)(i)))}"
  "M-( { C-p C-p RET")

(ipe-test-def-kbd list-open-up-3 ()
  "Test `ipe-insert-pair-edit' OPEN up x3.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((((a)(b)(c)(d)|(f)(g)(h)(i)))))"
  "({((((a)(b)(c)(d)|(f)(g)(h)(i))))})"
  "M-( { 3 C-p RET")

(ipe-test-def-kbd list-open-up-4 ()
  "Test `ipe-insert-pair-edit' OPEN up x4.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((((a)(b)(c)(d)|(f)(g)(h)(i)))))"
  "{(((((a)(b)(c)(d)|(f)(g)(h)(i)))))}"
  "M-( { C-u C-p RET")

(ipe-test-def-kbd list-open-up-blank ()
  "Test `ipe-insert-pair-edit' OPEN up with a blank list.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "( ( (a)(b)(c)(d)|(f)(g)(h)(i) ) )"
  "( {( (a)(b)(c)(d)|(f)(g)(h)(i) )} )"
  "M-( { C-p RET")

(ipe-test-def-kbd list-open-up-blank-2 ()
  "Test `ipe-insert-pair-edit' OPEN up x2 with a blank list.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "( ( (a)(b)(c)(d)|(f)(g)(h)(i) ) )"
  "{( ( (a)(b)(c)(d)|(f)(g)(h)(i) ) )}"
  "M-( { C-p C-p RET")

(ipe-test-def-kbd list-open-backward-1 ()
  "Test `ipe-insert-pair-edit' OPEN backward.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b)(c){(d)|}(f)(g)(h)(i)))"
  "M-( { C-b RET")

(ipe-test-def-kbd list-open-backward-2 ()
  "Test `ipe-insert-pair-edit' OPEN backward x2.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b){(c)(d)|}(f)(g)(h)(i)))"
  "M-( { C-b C-b RET")

(ipe-test-def-kbd list-open-backward-3 ()
  "Test `ipe-insert-pair-edit' OPEN backward x3.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a){(b)(c)(d)|}(f)(g)(h)(i)))"
  "M-( { 3 C-b RET")

(ipe-test-def-kbd list-open-backward-4 ()
  "Test `ipe-insert-pair-edit' OPEN backward x4.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(({(a)(b)(c)(d)|}(f)(g)(h)(i)))"
  "M-( { C-u C-b RET")

(ipe-test-def-kbd list-open-backward-16 ()
  "Test `ipe-insert-pair-edit' OPEN backward x16.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(({(a)(b)(c)(d)|}(f)(g)(h)(i)))"
  "M-( { C-u C-u C-b RET")

(ipe-test-def-kbd list-open-backward-beginning ()
  "Test `ipe-insert-pair-edit' OPEN backward at beginning of buffer.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "|(((a)(b)(c)(d)(f)(g)(h)(i)))"
  "{|}(((a)(b)(c)(d)(f)(g)(h)(i)))"
  "M-( { C-b RET")

(ipe-test-def-kbd list-open-backward-blank ()
  "Test `ipe-insert-pair-edit' OPEN backward with a blank list.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c) () | (f)(g)(h)(i)))"
  "(((a)(b)(c) {() |} (f)(g)(h)(i)))"
  "M-( { C-b RET")

(ipe-test-def-kbd list-open-backward-blank-2 ()
  "Test `ipe-insert-pair-edit' OPEN backward x2 with a blank list.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c) () | (f)(g)(h)(i)))"
  "(((a)(b){(c) () |} (f)(g)(h)(i)))"
  "M-( { C-b C-b RET")

(ipe-test-def-kbd list-open-forward-1 ()
  "Test `ipe-insert-pair-edit' OPEN forward.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b)(c)(d)|(f){}(g)(h)(i)))"
  "M-( { M-f RET")

(ipe-test-def-kbd list-open-forward-2 ()
  "Test `ipe-insert-pair-edit' OPEN forward x2.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b)(c)(d)|(f)(g){}(h)(i)))"
  "M-( { M-f M-f RET")

(ipe-test-def-kbd list-open-forward-3 ()
  "Test `ipe-insert-pair-edit' OPEN forward x3.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b)(c)(d)|(f)(g)(h){}(i)))"
  "M-( { 3 M-f RET")

(ipe-test-def-kbd list-open-forward-4 ()
  "Test `ipe-insert-pair-edit' OPEN forward x4.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b)(c)(d)|(f)(g)(h)(i){}))"
  "M-( { C-u M-f RET")

(ipe-test-def-kbd list-open-forward-16 ()
  "Test `ipe-insert-pair-edit' OPEN forward x16.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b)(c)(d)|(f)(g)(h)(i){}))"
  "M-( { C-u C-u M-f RET")

(ipe-test-def-kbd list-open-forward-end ()
  "Test `ipe-insert-pair-edit' OPEN forward at the end of buffer.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)(f)(g)(h)(i)))|"
  "(((a)(b)(c)(d)(f)(g)(h)(i))){|}"
  "M-( { M-f RET")

(ipe-test-def-kbd list-open-forward-blank ()
  "Test `ipe-insert-pair-edit' OPEN forward with a blank list.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)| () (g)(h)(i)))"
  "(((a)(b)(c)(d)| (){} (g)(h)(i)))"
  "M-( { M-f RET")

(ipe-test-def-kbd list-open-forward-blank-2 ()
  "Test `ipe-insert-pair-edit' OPEN forward x2 with a blank list.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)| () (g)(h)(i)))"
  "(((a)(b)(c)(d)| () (g){}(h)(i)))"
  "M-( { M-f M-f RET")

(ipe-test-def-kbd list-open-down-1 ()
  "Test `ipe-insert-pair-edit' OPEN down.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "|(((a)(b)(c)(d)(f)(g)(h)(i)))"
  "|({((a)(b)(c)(d)(f)(g)(h)(i))})"
  "M-( { M-n RET")

(ipe-test-def-kbd list-open-down-2 ()
  "Test `ipe-insert-pair-edit' OPEN down x2.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "|(((a)(b)(c)(d)(f)(g)(h)(i)))"
  "|(({(a)(b)(c)(d)(f)(g)(h)(i)}))"
  "M-( { M-n M-n RET")

(ipe-test-def-kbd list-open-down-3 ()
  "Test `ipe-insert-pair-edit' OPEN down x3.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "|(((a)(b)(c)(d)(f)(g)(h)(i)))"
  "|((({a})(b)(c)(d)(f)(g)(h)(i)))"
  "M-( { 3 M-n RET")

(ipe-test-def-kbd list-open-down-4 ()
  "Test `ipe-insert-pair-edit' OPEN down x4.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "|(((a)(b)(c)(d)(f)(g)(h)(i)))"
  "|((({a})(b)(c)(d)(f)(g)(h)(i)))"
  "M-( { C-u M-n RET")

(ipe-test-def-kbd list-open-down-blank ()
  "Test `ipe-insert-pair-edit' OPEN down with a blank list.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "|( ( (a)(b)(c)(d)(f)(g)(h)(i) ) )"
  "|( {( (a)(b)(c)(d)(f)(g)(h)(i) )} )"
  "M-( { M-n RET")

(ipe-test-def-kbd list-open-down-blank-2 ()
  "Test `ipe-insert-pair-edit' OPEN down x2 with a blank list.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "|( ( (a)(b)(c)(d)(f)(g)(h)(i) ) )"
  "|( ( {(a)(b)(c)(d)(f)(g)(h)(i)} ) )"
  "M-( { M-n M-n RET")

(ipe-test-def-kbd list-open-down-blank-3 ()
  "Test `ipe-insert-pair-edit' OPEN down x3 with a blank list.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "|( ( (a)(b)(c)(d)(f)(g)(h)(i) ) )"
  "|( ( ({a})(b)(c)(d)(f)(g)(h)(i) ) )"
  "M-( { 3 M-n RET")

(ipe-test-def-kbd list-open-down-blank-4 ()
  "Test `ipe-insert-pair-edit' OPEN down x4 with a blank list.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "|( ( (a)(b)(c)(d)(f)(g)(h)(i) ) )"
  "|( ( ({a})(b)(c)(d)(f)(g)(h)(i) ) )"
  "M-( { C-u M-n RET")

(ipe-test-def-kbd list-open-end-1 ()
  "Test `ipe-insert-pair-edit' OPEN end.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b)(c)(d)|(f)(g)(h)(i))){}"
  "M-( { M-e RET")

(ipe-test-def-kbd list-open-end-2 ()
  "Test `ipe-insert-pair-edit' OPEN end.

Using a 'list PAIR at end of buffer."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b)(c)(d)|(f)(g)(h)(i))){}"
  "M-( { M-e M-e RET")

(ipe-test-def-kbd list-open-end-3 ()
  "Test `ipe-insert-pair-edit' OPEN end.

Using a 'list PAIR at end of line."
  ipe-test-list-options
  nil
  '("(((a)(b)(c)(d)(f)(g)(h)(i)))"
    "|(((a)(b)(c)(d)(f)(g)(h)(i)))"
    "(((a)(b)(c)(d)(f)(g)(h)(i)))")
  '("(((a)(b)(c)(d)(f)(g)(h)(i)))"
    "|(((a)(b)(c)(d)(f)(g)(h)(i)))"
    "(((a)(b)(c)(d)(f)(g)(h)(i))){}")
  "M-( { M-e M-e RET")

(ipe-test-def-kbd list-open-end-4 ()
  "Test `ipe-insert-pair-edit' OPEN end.

Using a 'list PAIR with a numeric prefix."
  ipe-test-list-options
  nil
  "|(((a)(b)(c)(d)(f)(g)(h)(i)))"
  "|(((a)(b)(c)(d)(f)(g)(h)(i))){}"
  "M-( { 2 M-e RET")

(ipe-test-def-kbd list-close-start-1 ()
  "Test `ipe-insert-pair-edit' CLOSE start.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "{}(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "M-( { M-a RET")

(ipe-test-def-kbd list-close-start-2 ()
  "Test `ipe-insert-pair-edit' CLOSE start.

Using a 'list PAIR at the start of buffer."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)(f)(g)(h)(i)))|"
  "{}(((a)(b)(c)(d)(f)(g)(h)(i)))|"
  "M-( { M-a M-a RET")

(ipe-test-def-kbd list-close-start-3 ()
  "Test `ipe-insert-pair-edit' CLOSE start.

Using a 'list PAIR at the beginning of line."
  ipe-test-list-options
  nil
  '("(((a)(b)(c)(d)(f)(g)(h)(i)))"
    "(((a)(b)(c)(d)(f)(g)(h)(i)))|"
    "(((a)(b)(c)(d)(f)(g)(h)(i)))")
  '("{}(((a)(b)(c)(d)(f)(g)(h)(i)))"
    "(((a)(b)(c)(d)(f)(g)(h)(i)))|"
    "(((a)(b)(c)(d)(f)(g)(h)(i)))")
  "M-( { M-a M-a RET")

(ipe-test-def-kbd list-close-start-4 ()
  "Test `ipe-insert-pair-edit' CLOSE start.

Using a 'list PAIR with a numeric prefix."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)(f)(g)(h)(i))|)"
  "{}(((a)(b)(c)(d)(f)(g)(h)(i))|)"
  "M-( { 2 M-a RET")

(ipe-test-def-kbd list-close-up-1 ()
  "Test `ipe-insert-pair-edit' CLOSE up.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "({((a)(b)(c)(d)|(f)(g)(h)(i))})"
  "M-( { M-p RET")

(ipe-test-def-kbd list-close-up-2 ()
  "Test `ipe-insert-pair-edit' CLOSE up x2.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "{(((a)(b)(c)(d)|(f)(g)(h)(i)))}"
  "M-( { M-p M-p RET")

(ipe-test-def-kbd list-close-up-3 ()
  "Test `ipe-insert-pair-edit' CLOSE up x3.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "((((a)(b)(c)(d)|(f)(g)(h)(i))))"
  "{((((a)(b)(c)(d)|(f)(g)(h)(i))))}"
  "M-( { 3 M-p RET")

(ipe-test-def-kbd list-close-up-4 ()
  "Test `ipe-insert-pair-edit' CLOSE up x4.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "((((a)(b)(c)(d)|(f)(g)(h)(i))))"
  "{((((a)(b)(c)(d)|(f)(g)(h)(i))))}"
  "M-( { C-u M-p RET")

(ipe-test-def-kbd list-close-up-blank ()
  "Test `ipe-insert-pair-edit' CLOSE up with a blank list.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "( ( (a)(b)(c)(d)|(f)(g)(h)(i) ) )"
  "( {( (a)(b)(c)(d)|(f)(g)(h)(i) )} )"
  "M-( { M-p RET")

(ipe-test-def-kbd list-close-up-blank-2 ()
  "Test `ipe-insert-pair-edit' CLOSE up x2 with a blank list.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "( ( (a)(b)(c)(d)|(f)(g)(h)(i) ) )"
  "{( ( (a)(b)(c)(d)|(f)(g)(h)(i) ) )}"
  "M-( { M-p M-p RET")

(ipe-test-def-kbd list-close-backward-1 ()
  "Test `ipe-insert-pair-edit' CLOSE backward.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b)(c){}(d)|(f)(g)(h)(i)))"
  "M-( { M-b RET")

(ipe-test-def-kbd list-close-backward-2 ()
  "Test `ipe-insert-pair-edit' CLOSE backward x2.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b){}(c)(d)|(f)(g)(h)(i)))"
  "M-( { M-b M-b RET")

(ipe-test-def-kbd list-close-backward-3 ()
  "Test `ipe-insert-pair-edit' CLOSE backward x3.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a){}(b)(c)(d)|(f)(g)(h)(i)))"
  "M-( { 3 M-b RET")

(ipe-test-def-kbd list-close-backward-4 ()
  "Test `ipe-insert-pair-edit' CLOSE backward x4.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(({}(a)(b)(c)(d)|(f)(g)(h)(i)))"
  "M-( { C-u M-b RET")

(ipe-test-def-kbd list-close-backward-16 ()
  "Test `ipe-insert-pair-edit' CLOSE backward x16.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(({}(a)(b)(c)(d)|(f)(g)(h)(i)))"
  "M-( { C-u C-u M-b RET")

(ipe-test-def-kbd list-close-backward-beginning ()
  "Test `ipe-insert-pair-edit' CLOSE backward at the beginning of buffer.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "|(((a)(b)(c)(d)(f)(g)(h)(i)))"
  "{|}(((a)(b)(c)(d)(f)(g)(h)(i)))"
  "M-( { M-b RET")

(ipe-test-def-kbd list-close-backward-blank ()
  "Test `ipe-insert-pair-edit' CLOSE backward with a blank list.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c) () |(f)(g)(h)(i)))"
  "(((a)(b)(c) {}() |(f)(g)(h)(i)))"
  "M-( { M-b RET")

(ipe-test-def-kbd list-close-backward-blank-2 ()
  "Test `ipe-insert-pair-edit' CLOSE backward x2 with a blank list.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c) () |(f)(g)(h)(i)))"
  "(((a)(b){}(c) () |(f)(g)(h)(i)))"
  "M-( { M-b M-b RET")

(ipe-test-def-kbd list-close-forward-1 ()
  "Test `ipe-insert-pair-edit' CLOSE forward.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b)(c)(d){|(f)}(g)(h)(i)))"
  "M-( { C-f RET")

(ipe-test-def-kbd list-close-forward-2 ()
  "Test `ipe-insert-pair-edit' CLOSE forward x2.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b)(c)(d){|(f)(g)}(h)(i)))"
  "M-( { C-f C-f RET")

(ipe-test-def-kbd list-close-forward-3 ()
  "Test `ipe-insert-pair-edit' CLOSE forward x3.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b)(c)(d){|(f)(g)(h)}(i)))"
  "M-( { 3 C-f RET")

(ipe-test-def-kbd list-close-forward-4 ()
  "Test `ipe-insert-pair-edit' CLOSE forward x4.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b)(c)(d){|(f)(g)(h)(i)}))"
  "M-( { C-u C-f RET")

(ipe-test-def-kbd list-close-forward-16 ()
  "Test `ipe-insert-pair-edit' CLOSE forward x16.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b)(c)(d){|(f)(g)(h)(i)}))"
  "M-( { C-u C-u C-f RET")

(ipe-test-def-kbd list-close-forward-end ()
  "Test `ipe-insert-pair-edit' CLOSE forward at end of buffer.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)(f)(g)(h)(i)))|"
  "(((a)(b)(c)(d)(f)(g)(h)(i))){|}"
  "M-( { C-f RET")

(ipe-test-def-kbd list-close-forward-blank ()
  "Test `ipe-insert-pair-edit' CLOSE forward with a blank list.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)| () (g)(h)(i)))"
  "(((a)(b)(c)(d){| ()} (g)(h)(i)))"
  "M-( { C-f RET")

(ipe-test-def-kbd list-close-forward-blank-2 ()
  "Test `ipe-insert-pair-edit' CLOSE forward x2 with a blank list.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)| () (g)(h)(i)))"
  "(((a)(b)(c)(d){| () (g)}(h)(i)))"
  "M-( { C-f C-f RET")

(ipe-test-def-kbd list-close-down-1 ()
  "Test `ipe-insert-pair-edit' CLOSE down.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)(f)(g)(h)(i)))|"
  "({((a)(b)(c)(d)(f)(g)(h)(i))})|"
  "M-( { C-n RET")

(ipe-test-def-kbd list-close-down-2 ()
  "Test `ipe-insert-pair-edit' CLOSE down x2.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)(f)(g)(h)(i)))|"
  "(({(a)(b)(c)(d)(f)(g)(h)(i)}))|"
  "M-( { C-n C-n RET")

(ipe-test-def-kbd list-close-down-3 ()
  "Test `ipe-insert-pair-edit' CLOSE down x3.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)(f)(g)(h)(i)))|"
  "(((a)(b)(c)(d)(f)(g)(h)({i})))|"
  "M-( { 3 C-n RET")

(ipe-test-def-kbd list-close-down-4 ()
  "Test `ipe-insert-pair-edit' CLOSE down x4.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)(f)(g)(h)(i)))|"
  "(((a)(b)(c)(d)(f)(g)(h)({i})))|"
  "M-( { C-u C-n RET")

(ipe-test-def-kbd list-close-down-blank ()
  "Test `ipe-insert-pair-edit' CLOSE down with a blank list.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "( ( (a)(b)(c)(d)(f)(g)(h)(i) ) )|"
  "( {( (a)(b)(c)(d)(f)(g)(h)(i) )} )|"
  "M-( { C-n RET")

(ipe-test-def-kbd list-close-down-blank-2 ()
  "Test `ipe-insert-pair-edit' CLOSE down x2 with a blank list.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "( ( (a)(b)(c)(d)(f)(g)(h)(i) ) )|"
  "( ( {(a)(b)(c)(d)(f)(g)(h)(i)} ) )|"
  "M-( { C-n C-n RET")

(ipe-test-def-kbd list-close-down-blank-3 ()
  "Test `ipe-insert-pair-edit' CLOSE down x3 with a blank list.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "( ( (a)(b)(c)(d)(f)(g)(h)(i) ) )|"
  "( ( (a)(b)(c)(d)(f)(g)(h)({i}) ) )|"
  "M-( { 3 C-n RET")

(ipe-test-def-kbd list-close-down-blank-4 ()
  "Test `ipe-insert-pair-edit' CLOSE down x4 with a blank list.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "( ( (a)(b)(c)(d)(f)(g)(h)(i) ) )|"
  "( ( (a)(b)(c)(d)(f)(g)(h)({i}) ) )|"
  "M-( { C-u C-n RET")

(ipe-test-def-kbd list-close-end-1 ()
  "Test `ipe-insert-pair-edit' CLOSE end.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
  "(((a)(b)(c)(d)|(f)(g)(h)(i))){}"
  "M-( { C-e RET")

(ipe-test-def-kbd list-close-end-2 ()
  "Test `ipe-insert-pair-edit' CLOSE end.

Using a 'list PAIR at the start of buffer."
  ipe-test-list-options
  nil
  "|(((a)(b)(c)(d)(f)(g)(h)(i)))"
  "|(((a)(b)(c)(d)(f)(g)(h)(i))){}"
  "M-( { C-e RET")

(ipe-test-def-kbd list-close-end-3 ()
  "Test `ipe-insert-pair-edit' CLOSE end.

Using a 'list PAIR at the end of buffer."
  ipe-test-list-options
  nil
  "(((a)(b)(c)(d)(f)(g)(h)(i)))|"
  "(((a)(b)(c)(d)(f)(g)(h)(i))){|}"
  "M-( { C-e RET")

(ipe-test-def-kbd list-close-end-4 ()
  "Test `ipe-insert-pair-edit' CLOSE end x2.

Using a 'list PAIR."
  ipe-test-list-options
  nil
  '("(((a)(b)(c)(d)(f)(g)(h)(i)))"
    "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
    "(((a)(b)(c)(d)(f)(g)(h)(i)))")
  '("(((a)(b)(c)(d)(f)(g)(h)(i)))"
    "(((a)(b)(c)(d)|(f)(g)(h)(i)))"
    "(((a)(b)(c)(d)(f)(g)(h)(i))){}")
  "M-( { C-e C-e RET")

(ipe-test-def-kbd list-close-end-5 ()
  "Test `ipe-insert-pair-edit' CLOSE end.

Using a 'list PAIR with a numeric prefix."
  ipe-test-list-options
  nil
  '("(((a)(b)(c)(d)|(f)(g)(h)(i)))"
    "(((a)(b)(c)(d)(f)(g)(h)(i)))"
    "(((a)(b)(c)(d)(f)(g)(h)(i)))")
  '("(((a)(b)(c)(d)|(f)(g)(h)(i)))"
    "(((a)(b)(c)(d)(f)(g)(h)(i)))"
    "{}(((a)(b)(c)(d)(f)(g)(h)(i)))")
  "M-( { 2 C-e RET")

(provide 'ipe-test-list)

;;; ipe-test-list.el ends here
