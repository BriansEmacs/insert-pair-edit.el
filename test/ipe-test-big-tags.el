;;; ipe-test-big-tags.el --- Insert Pair Edit - 'Long OPEN / CLOSE' Tests
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
;; The tests within this file are used to test the use of "big" tags
;; (i.e. 'Insert Pair Edit' (ipe) PAIRS with "long" OPEN and CLOSE
;; strings.)  These tests are run separately from the other `ipe'
;; tests, so as to focus upon the:
;;
;; * Positioning of the cursor upon insert, when inserting lots of
;;   text, and;
;; * Processing at the start and end of the buffer, when the buffer
;;   size changes significantly when the 'Insert Pair Edit' (ipe) PAIR
;;   is inserted.

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'ipe-test)

(setq ipe-test-big-tag-options
      '((ipe-move-point-on-insert   nil)
        (ipe-prefix-moves-close-p   t)
        (ipe-edit--movement-keysets '(modifiers))
        (ipe-pairs                  '(("(" "<start-of-tag>" "<end-of-tag>")
                                      ("[" "[" "]")))
        (ipe-mode-pairs             nil)))

(ipe-test-def-kbd big-tag-basic-insert-1 ()
  "Test `insert-pair-edit' in an empty buffer.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  ""
  "<start-of-tag>|<end-of-tag>"
  "M-( (")

(ipe-test-def-kbd big-tag-basic-insert-2 ()
  "Test `insert-pair-edit' at the start of buffer.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "|The quick brown fox jumps over the lazy dog."
  "<start-of-tag>|The<end-of-tag> quick brown fox jumps over the lazy dog."
  "M-( ( RET")

(ipe-test-def-kbd big-tag-basic-insert-3 ()
  "Test `insert-pair-edit' in the middle of a buffer.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox <start-of-tag>|jumps<end-of-tag> over the lazy dog."
  "M-( ( RET")

(ipe-test-def-kbd big-tag-basic-insert-4 ()
  "Test `insert-pair-edit' at the end of buffer.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox jumps over the lazy dog.|"
  "The quick brown fox jumps over the lazy <start-of-tag>dog<end-of-tag>.|"
  "M-( ( RET")

(ipe-test-def-kbd big-tag-basic-insert-5 ()
  "Test `insert-pair-edit' at 'offset' beginning of buffer.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "|  The quick brown fox jumps over the lazy dog."
  "|  <start-of-tag>The<end-of-tag> quick brown fox jumps over the lazy dog."
  "M-( ( RET")

(ipe-test-def-kbd big-tag-basic-insert-6 ()
  "Test `insert-pair-edit' at 'offset' end of buffer.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox jumps over the lazy dog.  |"
  "The quick brown fox jumps over the lazy <start-of-tag>dog<end-of-tag>.  |"
  "M-( ( RET")

(ipe-test-def-kbd big-tag-basic-prefix-insert-1 ()
  "Test `insert-pair-edit' with a numeric prefix.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox <start-of-tag>|jumps<end-of-tag> over the lazy dog."
  "C-1 M-( ( RET")

(ipe-test-def-kbd big-tag-basic-prefix-insert-2 ()
  "Test `insert-pair-edit' with a '2' numeric prefix.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox <start-of-tag>|jumps over<end-of-tag> the lazy dog."
  "C-2 M-( ( RET")

(ipe-test-def-kbd big-tag-basic-prefix-insert-3 ()
  "Test `insert-pair-edit' with a '3' numeric prefix.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox <start-of-tag>|jumps over the<end-of-tag> lazy dog."
  "C-3 M-( ( RET")

(ipe-test-def-kbd big-tag-basic-prefix-insert-4 ()
  "Test `insert-pair-edit' with a '4' numeric prefix.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox <start-of-tag>|jumps over the lazy<end-of-tag> dog."
  "C-4 M-( ( RET")

(ipe-test-def-kbd big-tag-basic-prefix-insert-5 ()
  "Test `insert-pair-edit' with a negative numeric prefix.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The <start-of-tag>quick brown fox |jumps<end-of-tag> over the lazy dog."
  "C-- C-4 M-( ( RET")

(ipe-test-def-kbd big-tag-open-backward-1 ()
  "Test `insert-pair-edit' OPEN backward.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown <start-of-tag>fox |jumps<end-of-tag> over the lazy dog."
  "M-( ( C-b RET")

(ipe-test-def-kbd big-tag-open-backward-2 ()
  "Test `insert-pair-edit' OPEN backward x2.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick <start-of-tag>brown fox |jumps<end-of-tag> over the lazy dog."
  "M-( ( C-b C-b RET")

(ipe-test-def-kbd big-tag-open-backward-3 ()
  "Test `insert-pair-edit' OPEN backward x3.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The <start-of-tag>quick brown fox |jumps<end-of-tag> over the lazy dog."
  "M-( ( 3 C-b RET")

(ipe-test-def-kbd big-tag-open-backward-4 ()
  "Test `insert-pair-edit' OPEN backward x4.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "<start-of-tag>The quick brown fox |jumps<end-of-tag> over the lazy dog."
  "M-( ( C-u C-b RET")

(ipe-test-def-kbd big-tag-open-backward-16 ()
  "Test `insert-pair-edit' OPEN backward x16.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "<start-of-tag>The quick brown fox |jumps<end-of-tag> over the lazy dog."
  "M-( ( C-u C-u C-b RET")

(ipe-test-def-kbd big-tag-open-backward-beginning ()
  "Test `insert-pair-edit' OPEN backward at beginning of buffer.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "|The quick brown fox jumps over the lazy dog."
  "<start-of-tag>|The<end-of-tag> quick brown fox jumps over the lazy dog."
  "M-( ( C-b RET")

(ipe-test-def-kbd big-tag-open-backward-blank ()
  "Test `insert-pair-edit' OPEN backward with a blank line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "|The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy <start-of-tag>dog."
    ""
    "|The<end-of-tag> quick brown fox jumps over the lazy dog.")
  "M-( ( C-b RET")

(ipe-test-def-kbd big-tag-open-backward-blank-2 ()
  "Test `insert-pair-edit' OPEN backward x2 with a blank line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "|The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the <start-of-tag>lazy dog."
    ""
    "|The<end-of-tag> quick brown fox jumps over the lazy dog.")
  "M-( ( C-b C-b RET")

(ipe-test-def-kbd big-tag-close-forward-1 ()
  "Test `insert-pair-edit' CLOSE forward.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox <start-of-tag>|jumps over<end-of-tag> the lazy dog."
  "M-( ( C-f RET")

(ipe-test-def-kbd big-tag-close-forward-2 ()
  "Test `insert-pair-edit' CLOSE forward x2.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox <start-of-tag>|jumps over the<end-of-tag> lazy dog."
  "M-( ( C-f C-f RET")

(ipe-test-def-kbd big-tag-close-forward-3 ()
  "Test `insert-pair-edit' CLOSE forward x3.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox <start-of-tag>|jumps over the lazy<end-of-tag> dog."
  "M-( ( 3 C-f RET")

(ipe-test-def-kbd big-tag-close-forward-4 ()
  "Test `insert-pair-edit' CLOSE forward x4.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox <start-of-tag>|jumps over the lazy dog<end-of-tag>."
  "M-( ( C-u C-f RET")

(ipe-test-def-kbd big-tag-close-forward-16 ()
  "Test `insert-pair-edit' CLOSE forward x16.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox <start-of-tag>|jumps over the lazy dog<end-of-tag>."
  "M-( ( C-u C-u C-f RET")

(ipe-test-def-kbd big-tag-close-forward-end ()
  "Test `insert-pair-edit' CLOSE forward at end of buffer.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox jumps over the lazy dog.|"
  "The quick brown fox jumps over the lazy <start-of-tag>dog<end-of-tag>.|"
  "M-( ( C-f RET")

(ipe-test-def-kbd big-tag-close-forward-blank ()
  "Test `insert-pair-edit' CLOSE forward with a blank line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy |dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy <start-of-tag>|dog."
    ""
    "The<end-of-tag> quick brown fox jumps over the lazy dog.")
  "M-( ( C-f RET")

(ipe-test-def-kbd big-tag-close-forward-blank-2 ()
  "Test `insert-pair-edit' CLOSE forward x2 with a blank line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy |dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy <start-of-tag>|dog."
    ""
    "The quick<end-of-tag> brown fox jumps over the lazy dog.")
  "M-( ( C-f C-f RET")

(ipe-test-def-kbd big-tag-open-forward-1 ()
  "Test `insert-pair-edit' OPEN forward.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |jumps <start-of-tag>over<end-of-tag> the lazy dog."
  "M-( ( M-f RET")

(ipe-test-def-kbd big-tag-open-forward-2 ()
  "Test `insert-pair-edit' OPEN forward x2.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |jumps over <start-of-tag>the<end-of-tag> lazy dog."
  "M-( ( M-f M-f RET")

(ipe-test-def-kbd big-tag-open-forward-3 ()
  "Test `insert-pair-edit' OPEN forward x3.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |jumps over the <start-of-tag>lazy<end-of-tag> dog."
  "M-( ( 3 M-f RET")

(ipe-test-def-kbd big-tag-open-forward-4 ()
  "Test `insert-pair-edit' OPEN forward x4.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |jumps over the lazy <start-of-tag>dog<end-of-tag>."
  "M-( ( C-u M-f RET")

(ipe-test-def-kbd big-tag-open-forward-16 ()
  "Test `insert-pair-edit' OPEN forward x16.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |jumps over the lazy dog.<start-of-tag><end-of-tag>"
  "M-( ( C-u C-u M-f RET")

(ipe-test-def-kbd big-tag-open-forward-end ()
  "Test `insert-pair-edit' OPEN forward at the end of buffer.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox jumps over the lazy dog.|"
  "The quick brown fox jumps over the lazy dog.<start-of-tag>|<end-of-tag>"
  "M-( ( M-f RET")

(ipe-test-def-kbd big-tag-open-forward-blank ()
  "Test `insert-pair-edit' OPEN forward with a blank line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy |dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy |dog."
    ""
    "<start-of-tag>The<end-of-tag> quick brown fox jumps over the lazy dog.")
  "M-( ( M-f RET")

(ipe-test-def-kbd big-tag-open-forward-blank-2 ()
  "Test `insert-pair-edit' OPEN forward x2 with a blank line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy |dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy |dog."
    ""
    "The <start-of-tag>quick<end-of-tag> brown fox jumps over the lazy dog.")
  "M-( ( M-f M-f RET")

(ipe-test-def-kbd big-tag-close-backward-1 ()
  "Test `insert-pair-edit' CLOSE backward.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown <start-of-tag>fox<end-of-tag> |jumps over the lazy dog."
  "M-( ( M-b RET")

(ipe-test-def-kbd big-tag-close-backward-2 ()
  "Test `insert-pair-edit' CLOSE backward x2.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick <start-of-tag>brown<end-of-tag> fox |jumps over the lazy dog."
  "M-( ( M-b M-b RET")

(ipe-test-def-kbd big-tag-close-backward-3 ()
  "Test `insert-pair-edit' CLOSE backward x3.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The <start-of-tag>quick<end-of-tag> brown fox |jumps over the lazy dog."
  "M-( ( 3 M-b RET")

(ipe-test-def-kbd big-tag-close-backward-4 ()
  "Test `insert-pair-edit' CLOSE backward x4.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "<start-of-tag>The<end-of-tag> quick brown fox |jumps over the lazy dog."
  "M-( ( C-u M-b RET")

(ipe-test-def-kbd big-tag-close-backward-16 ()
  "Test `insert-pair-edit' CLOSE backward x16.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "<start-of-tag><end-of-tag>The quick brown fox |jumps over the lazy dog."
  "M-( ( C-u C-u M-b RET")

(ipe-test-def-kbd big-tag-close-backward-beginning ()
  "Test `insert-pair-edit' CLOSE backward at the beginning of buffer.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  "|The quick brown fox jumps over the lazy dog."
  "<start-of-tag>|<end-of-tag>The quick brown fox jumps over the lazy dog."
  "M-( ( M-b RET")

(ipe-test-def-kbd big-tag-close-backward-blank ()
  "Test `insert-pair-edit' CLOSE backward with a blank line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "|The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy <start-of-tag>dog<end-of-tag>."
    ""
    "|The quick brown fox jumps over the lazy dog.")
  "M-( ( M-b RET")

(ipe-test-def-kbd big-tag-close-backward-blank-2 ()
  "Test `insert-pair-edit' CLOSE backward x2 with a blank line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "|The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the <start-of-tag>lazy<end-of-tag> dog."
    ""
    "|The quick brown fox jumps over the lazy dog.")
  "M-( ( M-b M-b RET")

(ipe-test-def-kbd big-tag-open-up-1 ()
  "Test `insert-pair-edit' OPEN up.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox <start-of-tag>jumps over the lazy dog."
    "The quick brown fox |jumps<end-of-tag> over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( C-p RET")

(ipe-test-def-kbd big-tag-open-up-2 ()
  "Test `insert-pair-edit' OPEN up x2.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox <start-of-tag>jumps over the lazy dog."
    "The quick brown fox |jumps<end-of-tag> over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( C-p C-p RET")

(ipe-test-def-kbd big-tag-open-up-3 ()
  "Test `insert-pair-edit' OPEN up x3.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox <start-of-tag>jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps<end-of-tag> over the lazy dog.")
  "M-( ( 3 C-p RET")

(ipe-test-def-kbd big-tag-open-up-4 ()
  "Test `insert-pair-edit' OPEN up x4.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox <start-of-tag>jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps<end-of-tag> over the lazy dog.")
  "M-( ( C-u C-p RET")

(ipe-test-def-kbd big-tag-open-up-blank ()
  "Test `insert-pair-edit' OPEN up with a blank line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<start-of-tag>"
    "The quick brown fox |jumps<end-of-tag> over the lazy dog.")
  "M-( ( C-p RET")

(ipe-test-def-kbd big-tag-open-up-blank-2 ()
  "Test `insert-pair-edit' OPEN up x2 with a blank line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog.")
  '("<start-of-tag>The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox |jumps<end-of-tag> over the lazy dog.")
  "M-( ( C-p C-p RET")

(ipe-test-def-kbd big-tag-open-up-offset ()
  "Test `insert-pair-edit' OPEN up with an offset.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown foxes jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown <start-of-tag>foxes jumps over the lazy dog."
    "The quick brown fox |jumps<end-of-tag> over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( C-p RET")

(ipe-test-def-kbd big-tag-open-up-offset-beginning ()
  "Test `insert-pair-edit' OPEN up at offset beginning of line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "  |The quick brown fox jumps over the lazy dog.")
  '("<start-of-tag>The quick brown fox jumps over the lazy dog."
    "  |The<end-of-tag> quick brown fox jumps over the lazy dog.")
  "M-( ( C-p RET")

(ipe-test-def-kbd big-tag-open-up-offset-beginning-2 ()
  "Test `insert-pair-edit' OPEN up offset at beginning of line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("  The quick brown fox jumps over the lazy dog."
    "|The quick brown fox jumps over the lazy dog.")
  '("  <start-of-tag>The quick brown fox jumps over the lazy dog."
    "|The<end-of-tag> quick brown fox jumps over the lazy dog.")
  "M-( ( C-p RET")

(ipe-test-def-kbd big-tag-close-down-1 ()
  "Test `insert-pair-edit' CLOSE down.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox <start-of-tag>|jumps over the lazy dog."
    "The quick brown fox jumps<end-of-tag> over the lazy dog.")
  "M-( ( C-n RET")

(ipe-test-def-kbd big-tag-close-down-2 ()
  "Test `insert-pair-edit' CLOSE down x2.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox <start-of-tag>|jumps over the lazy dog."
    "The quick brown fox jumps<end-of-tag> over the lazy dog.")
  "M-( ( C-n C-n RET")

(ipe-test-def-kbd big-tag-close-down-3 ()
  "Test `insert-pair-edit' CLOSE down x3.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox <start-of-tag>|jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps<end-of-tag> over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( 3 C-n RET")

(ipe-test-def-kbd big-tag-close-down-4 ()
  "Test `insert-pair-edit' CLOSE down x4.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox <start-of-tag>|jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps<end-of-tag> over the lazy dog.")
  "M-( ( C-u C-n RET")

(ipe-test-def-kbd big-tag-close-down-blank ()
  "Test `insert-pair-edit' CLOSE down with a blank line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox <start-of-tag>|jumps over the lazy dog."
    "<end-of-tag>"
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( C-n RET")

(ipe-test-def-kbd big-tag-close-down-blank-2 ()
  "Test `insert-pair-edit' CLOSE down x2 with a blank line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox <start-of-tag>|jumps over the lazy dog."
    ""
    "The<end-of-tag> quick brown fox jumps over the lazy dog.")
  "M-( ( C-n C-n RET")

(ipe-test-def-kbd big-tag-close-down-offset ()
  "Test `insert-pair-edit' CLOSE down with an offset.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown foxes jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox <start-of-tag>|jumps over the lazy dog."
    "The quick brown foxes jumps<end-of-tag> over the lazy dog.")
  "M-( ( C-n RET")

(ipe-test-def-kbd big-tag-close-down-offset-beginning ()
  "Test `insert-pair-edit' CLOSE down at offset beginning of line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("|The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog.")
  '("<start-of-tag>|The quick brown fox jumps over the lazy dog."
    "  The<end-of-tag> quick brown fox jumps over the lazy dog.")
  "M-( ( C-n RET")

(ipe-test-def-kbd big-tag-close-down-offset-beginning-2 ()
  "Test `insert-pair-edit' CLOSE down offset at beginning of line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("|  The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("|  <start-of-tag>The quick brown fox jumps over the lazy dog."
    "The quick<end-of-tag> brown fox jumps over the lazy dog.")
  "M-( ( C-n RET")

(ipe-test-def-kbd big-tag-open-down-1 ()
  "Test `insert-pair-edit' OPEN down.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox <start-of-tag>jumps<end-of-tag> over the lazy dog.")
  "M-( ( M-n RET")

(ipe-test-def-kbd big-tag-open-down-2 ()
  "Test `insert-pair-edit' OPEN down x2.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox <start-of-tag>jumps<end-of-tag> over the lazy dog.")
  "M-( ( M-n M-n RET")

(ipe-test-def-kbd big-tag-open-down-3 ()
  "Test `insert-pair-edit' OPEN down x3.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox <start-of-tag>jumps<end-of-tag> over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( 3 M-n RET")

(ipe-test-def-kbd big-tag-open-down-4 ()
  "Test `insert-pair-edit' OPEN down x4.

Using a 'word PAIR."
  ipe-test-big-tag-options
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
    "The quick brown fox <start-of-tag>jumps<end-of-tag> over the lazy dog.")
  "M-( ( C-u M-n RET")

(ipe-test-def-kbd big-tag-open-down-blank ()
  "Test `insert-pair-edit' OPEN down with a blank line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "<start-of-tag><end-of-tag>"
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( M-n RET")

(ipe-test-def-kbd big-tag-open-down-blank-2 ()
  "Test `insert-pair-edit' OPEN down x2 with a blank line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    ""
    "<start-of-tag>The<end-of-tag> quick brown fox jumps over the lazy dog.")
  "M-( ( M-n M-n RET")

(ipe-test-def-kbd big-tag-open-down-offset ()
  "Test `insert-pair-edit' OPEN down with an offset.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown foxes jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown foxes <start-of-tag>jumps<end-of-tag> over the lazy dog.")
  "M-( ( M-n RET")

(ipe-test-def-kbd big-tag-open-down-offset-beginning ()
  "Test `insert-pair-edit' OPEN down offset beginning of to line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("|The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog.")
  '("|The quick brown fox jumps over the lazy dog."
    "  <start-of-tag>The<end-of-tag> quick brown fox jumps over the lazy dog.")
  "M-( ( M-n RET")

(ipe-test-def-kbd big-tag-open-down-offset-beginning-2 ()
  "Test `insert-pair-edit' OPEN down offset beginning of from line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("|  The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("|  The quick brown fox jumps over the lazy dog."
    "The <start-of-tag>quick<end-of-tag> brown fox jumps over the lazy dog.")
  "M-( ( M-n RET")

(ipe-test-def-kbd big-tag-close-up-1 ()
  "Test `insert-pair-edit' CLOSE up.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox <start-of-tag>jumps<end-of-tag> over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( M-p RET")

(ipe-test-def-kbd big-tag-close-up-2 ()
  "Test `insert-pair-edit' CLOSE up x2.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox <start-of-tag>jumps<end-of-tag> over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( M-p M-p RET")

(ipe-test-def-kbd big-tag-close-up-3 ()
  "Test `insert-pair-edit' CLOSE up x3.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox <start-of-tag>jumps<end-of-tag> over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  "M-( ( 3 M-p RET")

(ipe-test-def-kbd big-tag-close-up-4 ()
  "Test `insert-pair-edit' CLOSE up x4.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox <start-of-tag>jumps<end-of-tag> over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  "M-( ( C-u M-p RET")

(ipe-test-def-kbd big-tag-close-up-blank ()
  "Test `insert-pair-edit' CLOSE up with a blank line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<start-of-tag><end-of-tag>"
    "The quick brown fox |jumps over the lazy dog.")
  "M-( ( M-p RET")

(ipe-test-def-kbd big-tag-close-up-blank-2 ()
  "Test `insert-pair-edit' CLOSE up x2 with a blank line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog.")
  '("<start-of-tag>The<end-of-tag> quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog.")
  "M-( ( M-p M-p RET")

(ipe-test-def-kbd big-tag-close-up-offset ()
  "Test `insert-pair-edit' CLOSE up with an offset.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown foxes jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown <start-of-tag>foxes<end-of-tag> jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( M-p RET")

(ipe-test-def-kbd big-tag-close-up-offset-beginning ()
  "Test `insert-pair-edit' CLOSE up at offset beginning of line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "|  The quick brown fox jumps over the lazy dog.")
  '("<start-of-tag>The<end-of-tag> quick brown fox jumps over the lazy dog."
    "|  The quick brown fox jumps over the lazy dog.")
  "M-( ( M-p RET")

(ipe-test-def-kbd big-tag-close-up-offset-beginning-2 ()
  "Test `insert-pair-edit' CLOSE up offset at beginning of line.

Using a 'word PAIR."
  ipe-test-big-tag-options
  nil
  '("  The quick brown fox jumps over the lazy dog."
    "|The quick brown fox jumps over the lazy dog.")
  '("  <start-of-tag>The<end-of-tag> quick brown fox jumps over the lazy dog."
    "|The quick brown fox jumps over the lazy dog.")
  "M-( ( M-p RET")

(provide 'ipe-test-big-tags)

;; ipe-test-big-tags.el ends here
