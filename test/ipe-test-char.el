;;; ipe-test-char.el --- Insert Pair Edit - Character Movement Tests
;; Copyright (C) 2023 Brian Kavanagh

;; Author: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Maintainer: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Created: 28 June, 2020
;; Version: 2023.12.30
;; Package-Requires: ((emacs "24.3"))
;; Keywords: internal local
;; Homepage: http://github.com/brians.emacs/insert-pair-edit

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
;; OPEN and CLOSE strings of an `ipe' PAIR with a :movement of `char'
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

(setq ipe-test-char-options
      '((ipe-move-point-on-insert   nil)
        (ipe-prefix-moves-close-p   t)
        (ipe-edit--movement-keysets '(modifiers))
        (ipe-pairs                  '(("(" "(" ")" (:movement char))
                                      ("[" "[" "]")))
        (ipe-mode-pairs             nil)))

(ipe-test-def-kbd char-basic-insert-1 ()
  "Test `insert-pair-edit' in an empty buffer.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  ""
  "(|)"
  "M-( (")

(ipe-test-def-kbd char-basic-insert-2 ()
  "Test `insert-pair-edit' at the start of buffer.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "|The quick brown fox jumps over the lazy dog."
  "(|)The quick brown fox jumps over the lazy dog."
  "M-( ( RET")

(ipe-test-def-kbd char-basic-insert-3 ()
  "Test `insert-pair-edit' in the middle of buffer.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|)jumps over the lazy dog."
  "M-( ( RET")

(ipe-test-def-kbd char-basic-insert-4 ()
  "Test `insert-pair-edit' at the end of buffer.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox jumps over the lazy dog.|"
  "The quick brown fox jumps over the lazy dog.(|)"
  "M-( ( RET")

(ipe-test-def-kbd char-basic-insert-5 ()
  "Test `insert-pair-edit' at an 'offset' beginning of buffer.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "|  The quick brown fox jumps over the lazy dog."
  "(|)  The quick brown fox jumps over the lazy dog."
  "M-( ( RET")

(ipe-test-def-kbd char-basic-insert-6 ()
  "Test `insert-pair-edit' at an 'offset' end of buffer.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox jumps over the lazy dog.  |"
  "The quick brown fox jumps over the lazy dog.  (|)"
  "M-( ( RET")

(ipe-test-def-kbd char-basic-prefix-insert-1 ()
  "Test `insert-pair-edit' with a numeric prefix.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|j)umps over the lazy dog."
  "C-1 M-( ( RET")

(ipe-test-def-kbd char-basic-prefix-insert-2 ()
  "Test `insert-pair-edit' with a '2' numeric prefix.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|ju)mps over the lazy dog."
  "C-2 M-( ( RET")

(ipe-test-def-kbd char-basic-prefix-insert-3 ()
  "Test `insert-pair-edit' with a '3' numeric prefix.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jum)ps over the lazy dog."
  "C-3 M-( ( RET")

(ipe-test-def-kbd char-basic-prefix-insert-4 ()
  "Test `insert-pair-edit' with a '4' numeric prefix.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jump)s over the lazy dog."
  "C-4 M-( ( RET")

(ipe-test-def-kbd char-basic-prefix-insert-5 ()
  "Test `insert-pair-edit' with a negative numeric prefix.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown (fox |)jumps over the lazy dog."
  "C-- C-4 M-( ( RET")

(ipe-test-def-kbd char-open-start-1 ()
  "Test `insert-pair-edit' OPEN start.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "(The quick brown fox |)jumps over the lazy dog."
  "M-( ( C-a RET")

(ipe-test-def-kbd char-open-start-2 ()
  "Test `insert-pair-edit' OPEN start.

Using a 'char PAIR at the beginning of buffer."
  ipe-test-char-options
  nil
  "|The quick brown fox jumps over the lazy dog."
  "(|)The quick brown fox jumps over the lazy dog."
  "M-( ( C-a C-a RET")

(ipe-test-def-kbd char-open-start-3 ()
  "Test `insert-pair-edit' OPEN start.

Using a 'char PAIR at the beginning of line."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "|The quick brown fox jumps over the lazy dog.")
  '("(The quick brown fox jumps over the lazy dog."
    "|)The quick brown fox jumps over the lazy dog.")
    "M-( ( C-a RET")

(ipe-test-def-kbd char-open-start-4 ()
  "Test `insert-pair-edit' OPEN start.

Using a 'char PAIR at the end of buffer."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.|")
  '("(The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.|)")
    "M-( ( C-a C-a RET")

(ipe-test-def-kbd char-open-start-5 ()
  "Test `insert-pair-edit' OPEN start x2.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("(The quick brown fox jumps over the lazy dog."
    "The quick brown fox |)jumps over the lazy dog.")
  "M-( ( C-a C-a RET")

(ipe-test-def-kbd char-open-start-6 ()
  "Test `insert-pair-edit' OPEN start x3.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "(The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |)jumps over the lazy dog.")
  "M-( ( 3 C-a RET")

(ipe-test-def-kbd char-open-start-7 ()
  "Test `insert-pair-edit' OPEN start x4.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "(The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |)jumps over the lazy dog.")
  "M-( ( C-u C-a RET")

(ipe-test-def-kbd char-open-start-8 ()
  "Test `insert-pair-edit' OPEN start x-1.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "(The quick brown fox |)jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( - 1 C-a RET")

(ipe-test-def-kbd char-open-start-9 ()
  "Test `insert-pair-edit' OPEN start x-2.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "()The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( - 2 C-a RET")

(ipe-test-def-kbd char-open-up-1 ()
  "Test `insert-pair-edit' OPEN up.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (jumps over the lazy dog."
    "The quick brown fox |)jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( C-p RET")

(ipe-test-def-kbd char-open-up-2 ()
  "Test `insert-pair-edit' OPEN up x2.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (jumps over the lazy dog."
    "The quick brown fox |)jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( C-p C-p RET")

(ipe-test-def-kbd char-open-up-3 ()
  "Test `insert-pair-edit' OPEN up x3.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox (jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |)jumps over the lazy dog.")
  "M-( ( 3 C-p RET")

(ipe-test-def-kbd char-open-up-4 ()
  "Test `insert-pair-edit' OPEN up x4.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox (jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |)jumps over the lazy dog.")
  "M-( ( C-u C-p RET")

(ipe-test-def-kbd char-open-up-blank ()
  "Test `insert-pair-edit' OPEN up with a blank line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "("
    "The quick brown fox |)jumps over the lazy dog.")
  "M-( ( C-p RET")

(ipe-test-def-kbd char-open-up-blank-2 ()
  "Test `insert-pair-edit' OPEN up x2 with a blank line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog.")
  '("(The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox |)jumps over the lazy dog.")
  "M-( ( C-p C-p RET")

(ipe-test-def-kbd char-open-up-offset ()
  "Test `insert-pair-edit' OPEN up with an offset.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown foxes jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown foxe(s jumps over the lazy dog."
    "The quick brown fox |)jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( C-p RET")

(ipe-test-def-kbd char-open-up-offset-beginning ()
  "Test `insert-pair-edit' OPEN up offset to beginning of line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "  |The quick brown fox jumps over the lazy dog.")
  '("Th(e quick brown fox jumps over the lazy dog."
    "  |)The quick brown fox jumps over the lazy dog.")
  "M-( ( C-p RET")

(ipe-test-def-kbd char-open-up-offset-beginning-2 ()
  "Test `insert-pair-edit' OPEN up to offset beginning of line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("  The quick brown fox jumps over the lazy dog."
    "|The quick brown fox jumps over the lazy dog.")
  '("(  The quick brown fox jumps over the lazy dog."
    "|)The quick brown fox jumps over the lazy dog.")
  "M-( ( C-p RET")

(ipe-test-def-kbd char-open-backward-1 ()
  "Test `insert-pair-edit' OPEN backward.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox( |)jumps over the lazy dog."
  "M-( ( C-b RET")

(ipe-test-def-kbd char-open-backward-2 ()
  "Test `insert-pair-edit' OPEN backward x2.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fo(x |)jumps over the lazy dog."
  "M-( ( C-b C-b RET")

(ipe-test-def-kbd char-open-backward-3 ()
  "Test `insert-pair-edit' OPEN backward x3.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown f(ox |)jumps over the lazy dog."
  "M-( ( 3 C-b RET")

(ipe-test-def-kbd char-open-backward-4 ()
  "Test `insert-pair-edit' OPEN backward x4.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown (fox |)jumps over the lazy dog."
  "M-( ( C-u C-b RET")

(ipe-test-def-kbd char-open-backward-16 ()
  "Test `insert-pair-edit' OPEN backward x16.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The (quick brown fox |)jumps over the lazy dog."
  "M-( ( C-u C-u C-b RET")

(ipe-test-def-kbd char-open-backward-beginning ()
  "Test `insert-pair-edit' OPEN backward at beginning of buffer.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "|The quick brown fox jumps over the lazy dog."
  "(|)The quick brown fox jumps over the lazy dog."
  "M-( ( C-b RET")

(ipe-test-def-kbd char-open-backward-blank ()
  "Test `insert-pair-edit' OPEN backward with a blank line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "|The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "("
    "|)The quick brown fox jumps over the lazy dog.")
  "M-( ( C-b RET")

(ipe-test-def-kbd char-open-backward-blank-2 ()
  "Test `insert-pair-edit' OPEN backward x2 with a blank line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "|The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog.("
    ""
    "|)The quick brown fox jumps over the lazy dog.")
  "M-( ( C-b C-b RET")

(ipe-test-def-kbd char-open-forward-1 ()
  "Test `insert-pair-edit' OPEN forward.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |j()umps over the lazy dog."
  "M-( ( M-f RET")

(ipe-test-def-kbd char-open-forward-2 ()
  "Test `insert-pair-edit' OPEN forward x2.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |ju()mps over the lazy dog."
  "M-( ( M-f M-f RET")

(ipe-test-def-kbd char-open-forward-3 ()
  "Test `insert-pair-edit' OPEN forward x3.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |jum()ps over the lazy dog."
  "M-( ( 3 M-f RET")

(ipe-test-def-kbd char-open-forward-4 ()
  "Test `insert-pair-edit' OPEN forward x4.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |jump()s over the lazy dog."
  "M-( ( C-u M-f RET")

(ipe-test-def-kbd char-open-forward-16 ()
  "Test `insert-pair-edit' OPEN forward x16.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |jumps over the l()azy dog."
  "M-( ( C-u C-u M-f RET")

(ipe-test-def-kbd char-open-forward-end ()
  "Test `insert-pair-edit' OPEN forward at the end of buffer.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox jumps over the lazy dog.|"
  "The quick brown fox jumps over the lazy dog.(|)"
  "M-( ( M-f RET")

(ipe-test-def-kbd char-open-forward-blank ()
  "Test `insert-pair-edit' OPEN forward with a blank line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog.|"
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog.|"
    "()"
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( M-f RET")

(ipe-test-def-kbd char-open-forward-blank-2 ()
  "Test `insert-pair-edit' OPEN forward x2 with a blank line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog.|"
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog.|"
    ""
    "()The quick brown fox jumps over the lazy dog.")
  "M-( ( M-f M-f RET")

(ipe-test-def-kbd char-open-down-1 ()
  "Test `insert-pair-edit' OPEN down.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox ()jumps over the lazy dog.")
  "M-( ( M-n RET")

(ipe-test-def-kbd char-open-down-2 ()
  "Test `insert-pair-edit' OPEN down x2.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox ()jumps over the lazy dog.")
  "M-( ( M-n M-n RET")

(ipe-test-def-kbd char-open-down-3 ()
  "Test `insert-pair-edit' OPEN down x3.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox ()jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( 3 M-n RET")

(ipe-test-def-kbd char-open-down-4 ()
  "Test `insert-pair-edit' OPEN down x4.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox ()jumps over the lazy dog.")
  "M-( ( C-u M-n RET")

(ipe-test-def-kbd char-open-down-blank ()
  "Test `insert-pair-edit' OPEN down with a blank line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "()"
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( M-n RET")

(ipe-test-def-kbd char-open-down-blank-2 ()
  "Test `insert-pair-edit' OPEN down x2 with a blank line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    ""
    "()The quick brown fox jumps over the lazy dog.")
  "M-( ( M-n M-n RET")

(ipe-test-def-kbd char-open-down-offset ()
  "Test `insert-pair-edit' OPEN down with an offset.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown foxes jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown foxe()s jumps over the lazy dog.")
  "M-( ( M-n RET")

(ipe-test-def-kbd char-open-down-offset-beginning ()
  "Test `insert-pair-edit' OPEN down to offset beginning of line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("|The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog.")
  '("|The quick brown fox jumps over the lazy dog."
    "()  The quick brown fox jumps over the lazy dog.")
  "M-( ( M-n RET")

(ipe-test-def-kbd char-open-down-offset-beginning-2 ()
  "Test `insert-pair-edit' OPEN down offset to beginning of line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("|  The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("|  The quick brown fox jumps over the lazy dog."
    "()The quick brown fox jumps over the lazy dog.")
  "M-( ( M-n RET")

(ipe-test-def-kbd char-open-end-1 ()
  "Test `insert-pair-edit' OPEN end.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |jumps over the lazy dog.()"
  "M-( ( M-e RET")

(ipe-test-def-kbd char-open-end-2 ()
  "Test `insert-pair-edit' OPEN end.

Using a 'char PAIR at beginning of buffer."
  ipe-test-char-options
  nil
  "|The quick brown fox jumps over the lazy dog."
  "|The quick brown fox jumps over the lazy dog.()"
  "M-( ( M-e RET")

(ipe-test-def-kbd char-open-end-3 ()
  "Test `insert-pair-edit' OPEN end.

Using a 'char PAIR at the end of buffer."
  ipe-test-char-options
  nil
  "The quick brown fox jumps over the lazy dog.|"
  "The quick brown fox jumps over the lazy dog.(|)"
  "M-( ( M-e RET")

(ipe-test-def-kbd char-open-end-4 ()
  "Test `insert-pair-edit' OPEN end.

Using a 'char PAIR at the end of line."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog.|"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog.|"
    "The quick brown fox jumps over the lazy dog.()")
  "M-( ( M-e RET")

(ipe-test-def-kbd char-open-end-5 ()
  "Test `insert-pair-edit' OPEN end x2.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.()"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( M-e M-e RET")

(ipe-test-def-kbd char-open-end-6 ()
  "Test `insert-pair-edit' OPEN end x3.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.()"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( 3 M-e RET")

(ipe-test-def-kbd char-open-end-7 ()
  "Test `insert-pair-edit' OPEN end x4.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.()"
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( C-u M-e RET")

(ipe-test-def-kbd char-open-end-8 ()
  "Test `insert-pair-edit' OPEN end x-1.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.("
    "The quick brown fox |)jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( - 1 M-e RET")

(ipe-test-def-kbd char-open-end-9 ()
  "Test `insert-pair-edit' OPEN end x-2.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog.("
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |)jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( - 2 M-e RET")

(ipe-test-def-kbd char-close-start-1 ()
  "Test `insert-pair-edit' CLOSE start.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "()The quick brown fox |jumps over the lazy dog."
  "M-( ( M-a RET")

(ipe-test-def-kbd char-close-start-2 ()
  "Test `insert-pair-edit' CLOSE start.

Using a 'char PAIR at the beginning of buffer."
  ipe-test-char-options
  nil
  "|The quick brown fox jumps over the lazy dog."
  "(|)The quick brown fox jumps over the lazy dog."
  "M-( ( M-a RET")

(ipe-test-def-kbd char-close-start-3 ()
  "Test `insert-pair-edit' CLOSE start.

Using a 'char PAIR at the end of buffer."
  ipe-test-char-options
  nil
  "The quick brown fox jumps over the lazy dog.|"
  "()The quick brown fox jumps over the lazy dog.|"
  "M-( ( M-a RET")

(ipe-test-def-kbd char-close-start-4 ()
  "Test `insert-pair-edit' CLOSE start x2.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("()The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  "M-( ( M-a M-a RET")

(ipe-test-def-kbd char-close-start-5 ()
  "Test `insert-pair-edit' CLOSE start x3.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "()The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  "M-( ( 3 M-a RET")

(ipe-test-def-kbd char-close-start-6 ()
  "Test `insert-pair-edit' CLOSE start x4.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "()The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  "M-( ( C-u M-a RET")

(ipe-test-def-kbd char-close-start-7 ()
  "Test `insert-pair-edit' CLOSE start x-1.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (|jumps over the lazy dog."
    ")The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( - 1 M-a RET")

(ipe-test-def-kbd char-close-start-8 ()
  "Test `insert-pair-edit' CLOSE start x-2.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (|jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    ")The quick brown fox jumps over the lazy dog.")
  "M-( ( - 2 M-a RET")

(ipe-test-def-kbd char-close-up-1 ()
  "Test `insert-pair-edit' CLOSE up.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox ()jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( M-p RET")

(ipe-test-def-kbd char-close-up-2 ()
  "Test `insert-pair-edit' CLOSE up x2.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox ()jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( M-p M-p RET")

(ipe-test-def-kbd char-close-up-3 ()
  "Test `insert-pair-edit' CLOSE up x3.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox ()jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  "M-( ( 3 M-p RET")

(ipe-test-def-kbd char-close-up-4 ()
  "Test `insert-pair-edit' CLOSE up x4.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox ()jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  "M-( ( C-u M-p RET")

(ipe-test-def-kbd char-close-up-blank ()
  "Test `insert-pair-edit' CLOSE up with a blank line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "()"
    "The quick brown fox |jumps over the lazy dog.")
  "M-( ( M-p RET")

(ipe-test-def-kbd char-close-up-blank-2 ()
  "Test `insert-pair-edit' CLOSE up x2 with a blank line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog.")
  '("()The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog.")
  "M-( ( M-p M-p RET")

(ipe-test-def-kbd char-close-up-offset ()
  "Test `insert-pair-edit' CLOSE up with an offset.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown foxes jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown foxe()s jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( M-p RET")

(ipe-test-def-kbd char-close-up-offset-beginning ()
  "Test `insert-pair-edit' CLOSE up offset to beginning of line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "|  The quick brown fox jumps over the lazy dog.")
  '("()The quick brown fox jumps over the lazy dog."
    "|  The quick brown fox jumps over the lazy dog.")
  "M-( ( M-p RET")

(ipe-test-def-kbd char-close-up-offset-beginning-2 ()
  "Test `insert-pair-edit' CLOSE up to offset beginning of line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("  The quick brown fox jumps over the lazy dog."
    "|The quick brown fox jumps over the lazy dog.")
  '("()  The quick brown fox jumps over the lazy dog."
    "|The quick brown fox jumps over the lazy dog.")
  "M-( ( M-p RET")

(ipe-test-def-kbd char-close-backward-1 ()
  "Test `insert-pair-edit' CLOSE backward.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox() |jumps over the lazy dog."
  "M-( ( M-b RET")

(ipe-test-def-kbd char-close-backward-2 ()
  "Test `insert-pair-edit' CLOSE backward x2.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fo()x |jumps over the lazy dog."
  "M-( ( M-b M-b RET")

(ipe-test-def-kbd char-close-backward-3 ()
  "Test `insert-pair-edit' CLOSE backward x3.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown f()ox |jumps over the lazy dog."
  "M-( ( 3 M-b RET")

(ipe-test-def-kbd char-close-backward-4 ()
  "Test `insert-pair-edit' CLOSE backward x4.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown ()fox |jumps over the lazy dog."
  "M-( ( C-u M-b RET")

(ipe-test-def-kbd char-close-backward-16 ()
  "Test `insert-pair-edit' CLOSE backward x16.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The ()quick brown fox |jumps over the lazy dog."
  "M-( ( C-u C-u M-b RET")

(ipe-test-def-kbd char-close-backward-beginning ()
  "Test `insert-pair-edit' CLOSE backward at the beginning of buffer.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "|The quick brown fox jumps over the lazy dog."
  "(|)The quick brown fox jumps over the lazy dog."
  "M-( ( M-b RET")

(ipe-test-def-kbd char-close-backward-blank ()
  "Test `insert-pair-edit' CLOSE backward with a blank line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "|The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "()"
    "|The quick brown fox jumps over the lazy dog.")
  "M-( ( M-b RET")

(ipe-test-def-kbd char-close-backward-blank-2 ()
  "Test `insert-pair-edit' CLOSE backward x2 with a blank line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "|The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog.()"
    ""
    "|The quick brown fox jumps over the lazy dog.")
  "M-( ( M-b M-b RET")

(ipe-test-def-kbd char-close-forward-1 ()
  "Test `insert-pair-edit' CLOSE forward.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|j)umps over the lazy dog."
  "M-( ( C-f RET")

(ipe-test-def-kbd char-close-forward-2 ()
  "Test `insert-pair-edit' CLOSE forward x2.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|ju)mps over the lazy dog."
  "M-( ( C-f C-f RET")

(ipe-test-def-kbd char-close-forward-3 ()
  "Test `insert-pair-edit' CLOSE forward x3.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jum)ps over the lazy dog."
  "M-( ( 3 C-f RET")

(ipe-test-def-kbd char-close-forward-4 ()
  "Test `insert-pair-edit' CLOSE forward x4.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jump)s over the lazy dog."
  "M-( ( C-u C-f RET")

(ipe-test-def-kbd char-close-forward-16 ()
  "Test `insert-pair-edit' CLOSE forward x16.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jumps over the l)azy dog."
  "M-( ( C-u C-u C-f RET")

(ipe-test-def-kbd char-close-forward-end ()
  "Test `insert-pair-edit' CLOSE forward at end of buffer.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox jumps over the lazy dog.|"
  "The quick brown fox jumps over the lazy dog.(|)"
  "M-( ( C-f RET")

(ipe-test-def-kbd char-close-forward-blank ()
  "Test `insert-pair-edit' CLOSE forward with a blank line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog.|"
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog.(|"
    ")"
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( C-f RET")

(ipe-test-def-kbd char-close-forward-blank-2 ()
  "Test `insert-pair-edit' CLOSE forward x2 with a blank line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog.|"
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog.(|"
    ""
    ")The quick brown fox jumps over the lazy dog.")
  "M-( ( C-f C-f RET")

(ipe-test-def-kbd char-close-down-1 ()
  "Test `insert-pair-edit' CLOSE down.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox (|jumps over the lazy dog."
    "The quick brown fox )jumps over the lazy dog.")
  "M-( ( C-n RET")

(ipe-test-def-kbd char-close-down-2 ()
  "Test `insert-pair-edit' CLOSE down x2.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox (|jumps over the lazy dog."
    "The quick brown fox )jumps over the lazy dog.")
  "M-( ( C-n C-n RET")

(ipe-test-def-kbd char-close-down-3 ()
  "Test `insert-pair-edit' CLOSE down x3.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (|jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox )jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( 3 C-n RET")

(ipe-test-def-kbd char-close-down-4 ()
  "Test `insert-pair-edit' CLOSE down x4.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (|jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox )jumps over the lazy dog.")
  "M-( ( C-u C-n RET")

(ipe-test-def-kbd char-close-down-blank ()
  "Test `insert-pair-edit' CLOSE down with a blank line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (|jumps over the lazy dog."
    ")"
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( C-n RET")

(ipe-test-def-kbd char-close-down-blank-2 ()
  "Test `insert-pair-edit' CLOSE down x2 with a blank line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (|jumps over the lazy dog."
    ""
    ")The quick brown fox jumps over the lazy dog.")
  "M-( ( C-n C-n RET")

(ipe-test-def-kbd char-close-down-offset ()
  "Test `insert-pair-edit' CLOSE down with an offset.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown foxes jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox (|jumps over the lazy dog."
    "The quick brown foxe)s jumps over the lazy dog.")
  "M-( ( C-n RET")

(ipe-test-def-kbd char-close-down-offset-beginning ()
  "Test `insert-pair-edit' CLOSE down to offset beginning of line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("|The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog.")
  '("(|The quick brown fox jumps over the lazy dog."
    ")  The quick brown fox jumps over the lazy dog.")
  "M-( ( C-n RET")

(ipe-test-def-kbd char-close-down-offset-beginning-2 ()
  "Test `insert-pair-edit' CLOSE down offset to beginning of line.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("|  The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("(|  The quick brown fox jumps over the lazy dog."
    ")The quick brown fox jumps over the lazy dog.")
  "M-( ( C-n RET")

(ipe-test-def-kbd char-close-end-1 ()
  "Test `insert-pair-edit' CLOSE end.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jumps over the lazy dog.)"
  "M-( ( C-e RET")

(ipe-test-def-kbd char-close-end-2 ()
  "Test `insert-pair-edit' CLOSE end.

Using a 'char PAIR at the beginning of buffer."
  ipe-test-char-options
  nil
  "|The quick brown fox jumps over the lazy dog."
  "(|The quick brown fox jumps over the lazy dog.)"
  "M-( ( C-e RET")

(ipe-test-def-kbd char-close-end-3 ()
  "Test `insert-pair-edit' CLOSE end.

Using a 'char PAIR at the end of buffer."
  ipe-test-char-options
  nil
  "The quick brown fox jumps over the lazy dog.|"
  "The quick brown fox jumps over the lazy dog.(|)"
  "M-( ( C-e RET")

(ipe-test-def-kbd char-close-end-4 ()
  "Test `insert-pair-edit' CLOSE end x2.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (|jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.)")
  "M-( ( C-e C-e RET")

(ipe-test-def-kbd char-close-end-5 ()
  "Test `insert-pair-edit' CLOSE end x3.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (|jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.)"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( 3 C-e RET")

(ipe-test-def-kbd char-close-end-6 ()
  "Test `insert-pair-edit' CLOSE end x4.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (|jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.)"
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( C-u C-e RET")

(ipe-test-def-kbd char-close-end-7 ()
  "Test `insert-pair-edit' CLOSE end x-1.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.()"
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( - 1 C-e RET")

(ipe-test-def-kbd char-close-end-8 ()
  "Test `insert-pair-edit' CLOSE end x-2.

Using a 'char PAIR."
  ipe-test-char-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog.()"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( - 2 C-e RET")

(provide 'ipe-test-char)

;; ipe-test-char.el ends here
