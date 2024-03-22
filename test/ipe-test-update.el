;;; ipe-test-update.el --- Insert Pair Edit - Update Existing PAIRs
;; Copyright (C) 2023 Brian Kavanagh

;; Author: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Maintainer: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Created: 28 June, 2020
;; Version: 2023.12.30
;; Package: ipe
;; Package-Requires: ((emacs "24.3"))
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
;; The tests within this file are used to test the processing
;; performed by the:
;;
;;    `insert-pair-edit-update'
;;
;; function.

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'ipe-test)

(defvar ipe-test-update-options
  '((ipe-move-point-on-insert   nil)
    (ipe-prefix-moves-close-p   t)
    (ipe-edit--movement-keysets '(modifiers))
    (ipe-pairs
     '(("("  "((((("  ")))))")
       ("{"  "{{{{{"  "}}}}}")
       ("<"  "<"      ">")
       ("'"  "'"      "'"    (:escapes (("'" "\\'"))))
       ("\"" "\""     "\""   (:escapes (("\"" "\\\""))))
       (";"  "<-- "   " -->" (:movement line :infix " -- "))
       ("*"  "/*"     "*/"   (:movement line :infix "**"))
       ("1"  "/*"     "*/"   (:movement line))))
    (ipe-mode-pairs        nil))
  "Options used by `ipe-test-def-kbd' for `ipe-test-update'.")

(ipe-test-def-kbd update-cursor-1 ()
  "Test `insert-pair-edit-update' cursor position.

Cursor before PAIR."
  ipe-test-update-options
  nil
  "The quick brown| (((((fox))))) jumps over the lazy dog."
  "The quick brown| {{{{{fox}}}}} jumps over the lazy dog."
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-2 ()
  "Test `insert-pair-edit-update' cursor position.

Cursor before OPEN."
  ipe-test-update-options
  nil
  "The quick brown |(((((fox))))) jumps over the lazy dog."
  "The quick brown |{{{{{fox}}}}} jumps over the lazy dog."
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-3 ()
  "Test `insert-pair-edit-update' cursor position.

Cursor inside OPEN (1)."
  ipe-test-update-options
  nil
  "The quick brown (|((((fox))))) jumps over the lazy dog."
  "The quick brown {|{{{{fox}}}}} jumps over the lazy dog."
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-4 ()
  "Test `insert-pair-edit-update' cursor position.

Cursor inside OPEN (2)."
  ipe-test-update-options
  nil
  "The quick brown ((|(((fox))))) jumps over the lazy dog."
  "The quick brown {{|{{{fox}}}}} jumps over the lazy dog."
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-5 ()
  "Test `insert-pair-edit-update' cursor position.

Cursor inside OPEN (3)."
  ipe-test-update-options
  nil
  "The quick brown (((|((fox))))) jumps over the lazy dog."
  "The quick brown {{{|{{fox}}}}} jumps over the lazy dog."
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-6 ()
  "Test `insert-pair-edit-update' cursor position.

Cursor inside OPEN (4)."
  ipe-test-update-options
  nil
  "The quick brown ((((|(fox))))) jumps over the lazy dog."
  "The quick brown {{{{|{fox}}}}} jumps over the lazy dog."
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-7 ()
  "Test `insert-pair-edit-update' cursor position.

Cursor after OPEN."
  ipe-test-update-options
  nil
  "The quick brown (((((|fox))))) jumps over the lazy dog."
  "The quick brown {{{{{|fox}}}}} jumps over the lazy dog."
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-8 ()
  "Test `insert-pair-edit-update' cursor position.

Cursor inside PAIR (1)."
  ipe-test-update-options
  nil
  "The quick brown (((((f|ox))))) jumps over the lazy dog."
  "The quick brown {{{{{f|ox}}}}} jumps over the lazy dog."
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-9 ()
  "Test `insert-pair-edit-update' cursor position.

Cursor inside PAIR (2)."
  ipe-test-update-options
  nil
  "The quick brown (((((fo|x))))) jumps over the lazy dog."
  "The quick brown {{{{{fo|x}}}}} jumps over the lazy dog."
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-10 ()
  "Test `insert-pair-edit-update' cursor position.

Cursor before CLOSE."
  ipe-test-update-options
  nil
  "The quick brown (((((fox|))))) jumps over the lazy dog."
  "The quick brown {{{{{fox|}}}}} jumps over the lazy dog."
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-11 ()
  "Test `insert-pair-edit-update' cursor position.

Cursor inside CLOSE (1)."
  ipe-test-update-options
  nil
  "The quick brown (((((fox)|)))) jumps over the lazy dog."
  "The quick brown {{{{{fox}|}}}} jumps over the lazy dog."
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-12 ()
  "Test `insert-pair-edit-update' cursor position.

Cursor inside CLOSE (1)."
  ipe-test-update-options
  nil
  "The quick brown (((((fox))|))) jumps over the lazy dog."
  "The quick brown {{{{{fox}}|}}} jumps over the lazy dog."
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-13 ()
  "Test `insert-pair-edit-update' cursor position.

Cursor inside CLOSE (1)."
  ipe-test-update-options
  nil
  "The quick brown (((((fox)))|)) jumps over the lazy dog."
  "The quick brown {{{{{fox}}}|}} jumps over the lazy dog."
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-14 ()
  "Test `insert-pair-edit-update' cursor position.

Cursor inside CLOSE (1)."
  ipe-test-update-options
  nil
  "The quick brown (((((fox))))|) jumps over the lazy dog."
  "The quick brown {{{{{fox}}}}|} jumps over the lazy dog."
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-15 ()
  "Test `insert-pair-edit-update' cursor position.

Cursor after CLOSE."
  ipe-test-update-options
  nil
  "The quick brown (((((fox)))))| jumps over the lazy dog."
  "The quick brown {{{{{fox}}}}}| jumps over the lazy dog."
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-16 ()
  "Test `insert-pair-edit-update' cursor position.

Cursor after PAIR."
  ipe-test-update-options
  nil
  "The quick brown (((((fox))))) |jumps over the lazy dog."
  "The quick brown {{{{{fox}}}}} |jumps over the lazy dog."
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-17 ()
  "Test `insert-pair-edit-update' cursor position.

Cursor after at start of PAIR, multiple matches."
  ipe-test-update-options
  nil
  "The (((((quick))))) brown |(((((fox))))) jumps (((((over))))) the lazy dog."
  "The (((((quick))))) brown |{{{{{fox}}}}} jumps (((((over))))) the lazy dog."
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-18 ()
  "Test `insert-pair-edit-update' cursor position.

OPEN = CLOSE, cursor before PAIR."
  ipe-test-update-options
  nil
  "The quick |brown 'fox' jumps over the lazy dog."
  "The quick |brown \"fox\" jumps over the lazy dog."
  "C-u M-( ' ( \" RET")

(ipe-test-def-kbd update-cursor-19 ()
  "Test `insert-pair-edit-update' cursor position.

OPEN = CLOSE, cursor inside PAIR."
  ipe-test-update-options
  nil
  "The quick brown 'f|ox' jumps over the lazy dog."
  "The quick brown \"f|ox\" jumps over the lazy dog."
  "C-u M-( ' ( \" RET")

(ipe-test-def-kbd update-cursor-20 ()
  "Test `insert-pair-edit-update' cursor position.

OPEN = CLOSE, cursor after PAIR."
  ipe-test-update-options
  nil
  "The quick brown 'fox' jumps| over the lazy dog."
  "The quick brown \"fox\" jumps| over the lazy dog."
  "C-u M-( ' ( \" RET")

(ipe-test-def-kbd update-cursor-21 ()
  "Test `insert-pair-edit-update' cursor position.

OPEN = CLOSE, cursor at start of PAIR, multiple PAIRS."
  ipe-test-update-options
  nil
  "The 'quick' brown |'fox' jumps 'over' the lazy dog."
  "The 'quick' brown |\"fox\" jumps 'over' the lazy dog."
  "C-u M-( ' ( \" RET")

(ipe-test-def-kbd update-cursor-22 ()
  "Test `insert-pair-edit-update' cursor position.

OPEN = CLOSE, cursor inside PAIR, multiple PAIRS."
  ipe-test-update-options
  nil
  "The 'quick' brown 'f|ox' jumps 'over' the lazy dog."
  "The 'quick' brown \"f|ox\" jumps 'over' the lazy dog."
  "C-u M-( ' ( \" RET")

(ipe-test-def-kbd update-cursor-23 ()
  "Test `insert-pair-edit-update' cursor position.

OPEN = CLOSE, cursor at end of PAIR, multiple PAIRS."
  ipe-test-update-options
  nil
  "The 'quick' brown 'fox|' jumps 'over' the lazy dog."
  "The 'quick' brown '\"fox| jumps over\"' the lazy dog."
  "C-u M-( ' ( \" RET")

(ipe-test-def-kbd update-cursor-24 ()
  "Test `insert-pair-edit-update' cursor position.

Minimal buffer. Cursor before PAIR."
  ipe-test-update-options
  nil
  "|''''"
  "|''''"
  "C-u M-( ' RET")

(ipe-test-def-kbd update-cursor-25 ()
  "Test `insert-pair-edit-update' cursor position.

Minimal buffer. Cursor inside PAIR."
  ipe-test-update-options
  nil
  "'|'''"
  "'|'''"
  "C-u M-( ' RET")

(ipe-test-def-kbd update-cursor-26 ()
  "Test `insert-pair-edit-update' cursor position.

Minimal buffer. Cursor after PAIR."
  ipe-test-update-options
  nil
  "''|''"
  "''|''"
  "C-u M-( ' RET")

(ipe-test-def-kbd update-cursor-27 ()
  "Test `insert-pair-edit-update' cursor position.

Minimal buffer. Cursor between PAIRs."
  ipe-test-update-options
  nil
  "'''|'"
  "'''|'"
  "C-u M-( ' RET")

(ipe-test-def-kbd update-cursor-28 ()
  "Test `insert-pair-edit-update' cursor position.

Minimal buffer. Cursor afters PAIRs."
  ipe-test-update-options
  nil
  "''''|"
  "''''|"
  "C-u M-( ' RET")

(ipe-test-def-kbd update-cursor-29 ()
  "Test `insert-pair-edit-update' cursor position.

Indents around PAIR.  Cursor at start of buffer."
  ipe-test-update-options
  nil
  "|     (((((     The     )))))     quick brown fox jumps over the lazy dog.   "
  "|          {{{{{The          quick}}}}} brown fox jumps over the lazy dog.   "
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-30 ()
  "Test `insert-pair-edit-update' cursor position.

Indents around PAIR.  Cursor inside OPEN leading indent."
  ipe-test-update-options
  nil
  "  |   (((((     The     )))))     quick brown fox jumps over the lazy dog.   "
  "  |        {{{{{The          quick}}}}} brown fox jumps over the lazy dog.   "
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-31 ()
  "Test `insert-pair-edit-update' cursor position.

Indents around PAIR.  Cursor inside OPEN trailing indent."
  ipe-test-update-options
  nil
  "     (((((  |   The     )))))     quick brown fox jumps over the lazy dog.   "
  "       |   {{{{{The          quick}}}}} brown fox jumps over the lazy dog.   "
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-32 ()
  "Test `insert-pair-edit-update' cursor position.

Indents around PAIR.  Cursor inside CLOSE leading indent."
  ipe-test-update-options
  nil
  "     (((((     The  |   )))))     quick brown fox jumps over the lazy dog.   "
  "          {{{{{The  |        quick}}}}} brown fox jumps over the lazy dog.   "
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-cursor-33 ()
  "Test `insert-pair-edit-update' cursor position.

Indents around PAIR.  Cursor inside CLOSE trailing indent."
  ipe-test-update-options
  nil
  "     (((((     The     )))))  |   quick brown fox jumps over the lazy dog.   "
  "          {{{{{The       |   quick}}}}} brown fox jumps over the lazy dog.   "
  "C-u M-( ( ( { RET")

(ipe-test-def-kbd update-nested-1 ()
  "Test `insert-pair-edit-update' cursor position.

Large tag nested PAIRs."
  ipe-test-update-options
  nil
  "|(((((((((())))))))))"
  "|<((((()))))>"
  "C-u M-( ( ( < RET")

(ipe-test-def-kbd update-nested-2 ()
  "Test `insert-pair-edit-update' cursor position.

Large tag nested PAIRs."
  ipe-test-update-options
  nil
  "(((((|((((())))))))))"
  "(((((|<>)))))"
  "C-u M-( ( ( < RET")

(ipe-test-def-kbd update-infix-1 ()
  "Test `insert-pair-edit-update' with infixed line.

Simple update."
  ipe-test-update-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "<-- The quick brown fox |jumps over the lazy dog. -->"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; C-d")

(ipe-test-def-kbd update-infix-2 ()
  "Test `insert-pair-edit-update' with infixed line.

Minimal buffer."
  ipe-test-update-options
  nil
  "<-- |. -->"
  "|."
  "C-u M-( ; C-d")

(ipe-test-def-kbd update-infix-3 ()
  "Test `insert-pair-edit-update' with infixed line.

Minimal buffer + indent."
  ipe-test-update-options
  nil
  '("    <-- |. -->")
  '("|.")
  "C-u M-( ; C-d")

(ipe-test-def-kbd update-infix-4 ()
  "Test `insert-pair-edit-update' with infixed line.

Minimal buffer + post indent."
  ipe-test-update-options
  nil
  '("<--     |. -->")
  '("|.")
  "C-u M-( ; C-d")

(ipe-test-def-kbd update-infix-5 ()
  "Test `insert-pair-edit-update' with infixed line.

Minimal buffer + toggled OPEN + CLOSE."
  ipe-test-update-options
  nil
  '("<-- "
    " -- |."
    " -->")
  '("|.")
  "C-u M-( ; C-d")

(ipe-test-def-kbd update-infix-6 ()
  "Test `insert-pair-edit-update' with infixed line.

Minimal buffer + toggled OPEN + CLOSE + indents."
  ipe-test-update-options
  nil
  '("    <-- "
    "     -- |    ."
    "     -->")
  '("|.")
  "C-u M-( ; C-d")

(ipe-test-def-kbd update-infix-7 ()
  "Test `insert-pair-edit-update' with infixed line.

Minimal buffer + toggled OPEN + CLOSE + indents, no infix."
  ipe-test-update-options
  nil
  '("    /*"
    "    |   ."
    "    */")
  '("|.")
  "C-u M-( 1 C-d")

(ipe-test-def-kbd update-infix-8 ()
  "Test `insert-pair-edit-update' with infixed line.

Update before cursor."
  ipe-test-update-options
  nil
  '("<-- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  "C-u M-( ; C-d")

(ipe-test-def-kbd update-infix-9 ()
  "Test `insert-pair-edit-update' with infixed line.

Update after cursor."
  ipe-test-update-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "<-- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; C-d")

(ipe-test-def-kbd update-infix-10 ()
  "Test `insert-pair-edit-update' with infixed line.

Update with leading whitespace."
  ipe-test-update-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "    <-- The quick brown fox jumps over the lazy dog."
    "     -- The quick brown fox |jumps over the lazy dog."
    "     -- The quick brown fox jumps over the lazy dog. -->"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; C-d")

(ipe-test-def-kbd update-infix-11 ()
  "Test `insert-pair-edit-update' with infixed line.

Update with trailing whitespace."
  ipe-test-update-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "<--     The quick brown fox jumps over the lazy dog."
    " --     The quick brown fox |jumps over the lazy dog."
    " --     The quick brown fox jumps over the lazy dog. -->"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; C-d")

(ipe-test-def-kbd update-infix-12 ()
  "Test `insert-pair-edit-update' with infixed line.

Update with toggled OPEN."
  ipe-test-update-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "<-- "
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox |jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; C-d")

(ipe-test-def-kbd update-infix-13 ()
  "Test `insert-pair-edit-update' with infixed line.

Update with toggled CLOSE."
  ipe-test-update-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "<--     The quick brown fox jumps over the lazy dog."
    " --     The quick brown fox |jumps over the lazy dog."
    " --     The quick brown fox jumps over the lazy dog."
    " -->"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; C-d")

(ipe-test-def-kbd update-infix-14 ()
  "Test `insert-pair-edit-update' with infixed line.

Update with leading / trailing whitespace + toggled OPEN & CLOSE."
  ipe-test-update-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "    <-- "
    "     --     The quick brown fox jumps over the lazy dog."
    "     --     The quick brown fox |jumps over the lazy dog."
    "     --     The quick brown fox jumps over the lazy dog."
    "     -->"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; C-d")

(ipe-test-def-kbd update-infix-15 ()
  "Test `insert-pair-edit-update' with infixed line.

Update with leading / trailing whitespace + toggled OPEN & CLOSE,
no-infix."
  ipe-test-update-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "    /*"
    "        The quick brown fox jumps over the lazy dog."
    "        The quick brown fox |jumps over the lazy dog."
    "        The quick brown fox jumps over the lazy dog."
    "    */"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( 1 C-d")

(ipe-test-def-kbd update-infix-16 ()
  "Test `insert-pair-edit-update' with infixed line.

Update with leading / trailing whitespace + toggled OPEN & CLOSE from
infixed line to a different infixed line."
  ipe-test-update-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "    <-- "
    "     --     The quick brown fox jumps over the lazy dog."
    "     --     The quick brown fox |jumps over the lazy dog."
    "     --     The quick brown fox jumps over the lazy dog."
    "     -->"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "    /*"
    "    **    The quick brown fox jumps over the lazy dog."
    "    **    The quick brown fox |jumps over the lazy dog."
    "    **    The quick brown fox jumps over the lazy dog."
    "    */"
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; ( * RET")

(ipe-test-def-kbd update-infix-17 ()
  "Test `insert-pair-edit-update' with infixed line.

Update with leading / trailing whitespace + toggled OPEN & CLOSE from
non-infixed line to infixed line."
  ipe-test-update-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "    /*"
    "        The quick brown fox jumps over the lazy dog."
    "        The quick brown fox |jumps over the lazy dog."
    "        The quick brown fox jumps over the lazy dog."
    "    */"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "    /*"
    "    **    The quick brown fox jumps over the lazy dog."
    "    **    The quick brown fox |jumps over the lazy dog."
    "    **    The quick brown fox jumps over the lazy dog."
    "    */"
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( 1 ( * RET")

(ipe-test-def-kbd update-infix-18 ()
  "Test `insert-pair-edit-update' with infixed line.

Update with leading / trailing whitespace + toggled OPEN & CLOSE from
infixed line to non-infixed line."
  ipe-test-update-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "    /*"
    "    **    The quick brown fox jumps over the lazy dog."
    "    **    The quick brown fox |jumps over the lazy dog."
    "    **    The quick brown fox jumps over the lazy dog."
    "    */"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "    /*"
    "        The quick brown fox jumps over the lazy dog."
    "        The quick brown fox |jumps over the lazy dog."
    "        The quick brown fox jumps over the lazy dog."
    "    */"
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( * ( 1 RET")

(ipe-test-def-kbd update-infix-19 ()
  "Test `insert-pair-edit-update' with infixed line.

Update with cursor inside OPEN (1)."
  ipe-test-update-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "<|-- "
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -->"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "/|*"
    "**The quick brown fox jumps over the lazy dog."
    "**The quick brown fox jumps over the lazy dog."
    "**The quick brown fox jumps over the lazy dog."
    "*/"
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; ( * RET")

(ipe-test-def-kbd update-infix-20 ()
  "Test `insert-pair-edit-update' with infixed line.

Update with cursor inside OPEN (2)."
  ipe-test-update-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "<-|- "
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -->"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "/*|"
    "**The quick brown fox jumps over the lazy dog."
    "**The quick brown fox jumps over the lazy dog."
    "**The quick brown fox jumps over the lazy dog."
    "*/"
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; ( * RET")

(ipe-test-def-kbd update-infix-21 ()
  "Test `insert-pair-edit-update' with infixed line.

Update with cursor inside OPEN (3)."
  ipe-test-update-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "    <-|- "
    "     -- The quick brown fox jumps over the lazy dog."
    "     -- The quick brown fox jumps over the lazy dog."
    "     -- The quick brown fox jumps over the lazy dog."
    "     -->"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "    /*|"
    "    **The quick brown fox jumps over the lazy dog."
    "    **The quick brown fox jumps over the lazy dog."
    "    **The quick brown fox jumps over the lazy dog."
    "    */"
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; ( * RET")

(ipe-test-def-kbd update-infix-22 ()
  "Test `insert-pair-edit-update' with infixed line.

Update with cursor inside OPEN (4)."
  ipe-test-update-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "<--  |   The quick brown fox jumps over the lazy dog."
    " --     The quick brown fox jumps over the lazy dog."
    " --     The quick brown fox jumps over the lazy dog. -->"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "/*   | The quick brown fox jumps over the lazy dog."
    "**    The quick brown fox jumps over the lazy dog."
    "**    The quick brown fox jumps over the lazy dog.*/"
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; ( * RET")

(ipe-test-def-kbd update-infix-23 ()
  "Test `insert-pair-edit-update' with infixed line.

Update with cursor inside CLOSE (1)."
  ipe-test-update-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "<-- "
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " |-->"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "/*"
    "**The quick brown fox jumps over the lazy dog."
    "**The quick brown fox jumps over the lazy dog."
    "**The quick brown fox jumps over the lazy dog."
    "*|/"
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; ( * RET")

(ipe-test-def-kbd update-infix-24 ()
  "Test `insert-pair-edit-update' with infixed line.

Update with cursor inside CLOSE (2)."
  ipe-test-update-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "<-- "
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " |-->"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "/*"
    "**The quick brown fox jumps over the lazy dog."
    "**The quick brown fox jumps over the lazy dog."
    "**The quick brown fox jumps over the lazy dog."
    "*|/"
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; ( * RET")

(ipe-test-def-kbd update-infix-25 ()
  "Test `insert-pair-edit-update' with infixed line.

Update with cursor inside CLOSE (3)."
  ipe-test-update-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "    <-- "
    "     -- The quick brown fox jumps over the lazy dog."
    "     -- The quick brown fox jumps over the lazy dog."
    "     -- The quick brown fox jumps over the lazy dog."
    " |    -->"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "    /*"
    "    **The quick brown fox jumps over the lazy dog."
    "    **The quick brown fox jumps over the lazy dog."
    "    **The quick brown fox jumps over the lazy dog."
    " |   */"
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; ( * RET")

(ipe-test-def-kbd update-infix-26 ()
  "Test `insert-pair-edit-update' with infixed line.

Update with cursor inside CLOSE (4)."
  ipe-test-update-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "    <-- The quick brown fox jumps over the lazy dog."
    "     -- The quick brown fox jumps over the lazy dog."
    "     -- The quick brown fox jumps over the lazy dog. |-->"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "    /*The quick brown fox jumps over the lazy dog."
    "    **The quick brown fox jumps over the lazy dog."
    "    **The quick brown fox jumps over the lazy dog.*|/"
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; ( * RET")

(ipe-test-def-kbd update-infix-27 ()
  "Test `insert-pair-edit-update' with infixed line.

Update with single empty line."
  ipe-test-update-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "<-- | -->"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "/*|*/"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; ( * RET")

(ipe-test-def-kbd update-infix-28 ()
  "Test `insert-pair-edit-update' with infixed line.

Update with single empty line at start of buffer."
  ipe-test-update-options
  nil
  '("<-- | -->"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("/*|*/"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; ( * RET")

(ipe-test-def-kbd update-infix-29 ()
  "Test `insert-pair-edit-update' with infixed line.

Update with single empty line at end of buffer."
  ipe-test-update-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "<-- | -->")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "/*|*/")
  "C-u M-( ; ( * RET")

(provide 'ipe-test-update)

;;; ipe-test-update.el ends here
