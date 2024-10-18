;;; ipe-test-missing.el --- Insert Pair Edit - Empty OPEN CLOSE Tests -*- lexical-binding: t; -*-
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
;; The tests within this file are used to test the processing
;; performed by 'Insert Pair Edit' when using PAIR Definitions which
;; include an empty ("") OPEN or CLOSE definition.

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'ipe-test)

(defvar ipe-test-missing-options
  '((ipe-move-point-on-insert   nil)
    (ipe-prefix-moves-close-p   t)
    (ipe-edit--movement-keysets '(modifiers))
    (ipe-update-forward-first-p nil)
    (ipe-delete-action          'delete)
    (ipe-pairs
     '(("1" "<"   ""    (:movement char))
       ("2" ""    ">"   (:movement char))
       ("3" ""    ""    (:movement char))
       ("4" "{{{" ""    (:movement word))
       ("5" ""    "}}}" (:movement word))
       ("6" ""    ""    (:movement word))
       ("7" "// " ""    (:movement line :infix "// "))
       ("8" ""    "// " (:movement line :infix "// "))
       ("9" ""    ""    (:movement line :infix "// "))))
    (ipe-mode-pairs nil))
  "Options used by `ipe-test-def-kbd' for `ipe-test-missing'.")

;; *********************************************************************
;; Missing CLOSE
;; *********************************************************************

(ipe-test-def-kbd missing-close-empty-buffer-insert-1 ()
  "Test `ipe-insert-pair-edit' in an empty buffer.

PAIR has only a single character OPEN string (:movement 'char)."
  ipe-test-missing-options
  nil
  "|"
  "<|"
  "M-( 1")

(ipe-test-def-kbd missing-close-empty-buffer-insert-2 ()
  "Test `ipe-insert-pair-edit' in an empty buffer.

PAIR has only a multi-character OPEN string (:movement 'word)."
  ipe-test-missing-options
  nil
  "|"
  "{{{|"
  "M-( 4")

(ipe-test-def-kbd missing-close-empty-buffer-insert-3 ()
  "Test `ipe-insert-pair-edit' in an empty buffer.

PAIR has only a multi-character OPEN string (:movement 'line)."
  ipe-test-missing-options
  nil
  "|"
  "// |"
  "M-( 7")

(ipe-test-def-kbd missing-close-insert-1 ()
  "Test `ipe-insert-pair-edit' with only an OPEN string.

PAIR has only a single character OPEN string (:movement 'char)."
  ipe-test-missing-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox <|jumps over the lazy dog."
  "M-( 1 RET")

(ipe-test-def-kbd missing-close-insert-2 ()
  "Test `ipe-insert-pair-edit' with only an OPEN string.

PAIR has only a multi-character OPEN string (:movement 'word)."
  ipe-test-missing-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox {{{|jumps over the lazy dog."
  "M-( 4 RET")

(ipe-test-def-kbd missing-close-insert-3 ()
  "Test `ipe-insert-pair-edit' with only an OPEN string.

PAIR has only a multi-character OPEN string (:movement 'line)."
  ipe-test-missing-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "// The quick brown fox |jumps over the lazy dog."
  "M-( 7 RET")

(ipe-test-def-kbd missing-close-to-open-replace-1 ()
  "Test `ipe-insert-pair-edit-replace' with a missing CLOSE -> missing OPEN.

Cursor after CLOSE; CLOSE at start of buffer."
  ipe-test-missing-options
  nil
  "}}}The quick brown fox |jumps over the lazy dog."
  "{{{The quick brown fox |jumps over the lazy dog."
  "C-u C-u C-u M-( 5 4 RET")

(ipe-test-def-kbd missing-close-to-open-replace-2 ()
  "Test `ipe-insert-pair-edit-replace' with a missing CLOSE -> missing OPEN.

Cursor after CLOSE."
  ipe-test-missing-options
  nil
  "The quick brown}}} fox |jumps over the lazy dog."
  "The quick {{{brown fox |jumps over the lazy dog."
  "C-u C-u C-u M-( 5 4 RET")

(ipe-test-def-kbd missing-close-to-open-replace-3 ()
  "Test `ipe-insert-pair-edit-replace' with a missing CLOSE -> missing OPEN.

Cursor within CLOSE."
  ipe-test-missing-options
  nil
  "The quick brown fox }|}}jumps over the lazy dog."
  "The quick brown {{{fox |jumps over the lazy dog."
  "C-u C-u C-u M-( 5 4 RET")

;;; FIXME - OPEN is placed @cursor.
;;(ipe-test-def-kbd missing-close-to-open-replace-4 ()
;;  "Test `ipe-insert-pair-edit-replace' with a missing CLOSE -> missing OPEN.
;;
;;Cursor before CLOSE."
;;  ipe-test-missing-options
;;  nil
;;  "The quick brown fox |jumps over}}} the lazy dog."
;;  "The quick brown fox |jumps {{{over the lazy dog."
;;  "C-u C-u C-u M-( 5 4 RET")

;;; FIXME - OPEN is placed @cursor.
;;(ipe-test-def-kbd missing-close-to-open-replace-5 ()
;;  "Test `ipe-insert-pair-edit-replace' with a missing CLOSE -> missing OPEN.
;;
;;Cursor before CLOSE; CLOSE at end of buffer."
;;  ipe-test-missing-options
;;  nil
;;  "The quick brown fox |jumps over the lazy dog.}}}"
;;  "The quick brown fox |jumps over the lazy {{{dog."
;;  "C-u C-u C-u M-( 5 4 RET")

(ipe-test-def-kbd missing-close-delete-1 ()
  "Test `ipe-insert-pair-edit-delete' with only an OPEN string.

Cursor at start of buffer."
  ipe-test-missing-options
  nil
  "|<The quick brown fox jumps over the lazy dog."
  "|The quick brown fox jumps over the lazy dog."
  "C-u C-u M-( 1")

(ipe-test-def-kbd missing-close-delete-2 ()
  "Test `ipe-insert-pair-edit-delete' with only an OPEN string.

Cursor in middle of buffer."
  ipe-test-missing-options
  nil
  "The quick brown fox <|jumps over the lazy dog."
  "The quick brown fox |jumps over the lazy dog."
  "C-u C-u M-( 1")

(ipe-test-def-kbd missing-close-delete-3 ()
  "Test `ipe-insert-pair-edit-delete' with only an OPEN string.

Cursor at end of buffer."
  ipe-test-missing-options
  nil
  "The quick brown fox jumps over the lazy dog.<|"
  "The quick brown fox jumps over the lazy dog.|"
  "C-u C-u M-( 1")

(ipe-test-def-kbd missing-close-delete-4 ()
  "Test `ipe-insert-pair-edit-delete' with only an OPEN string.

OPEN is a multi-character string."
  ipe-test-missing-options
  nil
  "The quick brown fox {{{|jumps over the lazy dog."
  "The quick brown fox |jumps over the lazy dog."
  "C-u C-u M-( 4")

(ipe-test-def-kbd missing-close-delete-5 ()
  "Test `ipe-insert-pair-edit-delete' with only an OPEN string.

OPEN is a multi-character string; Multiple OPENs."
  ipe-test-missing-options
  nil
  '("The quick brown fox {{{jumps over the lazy dog."
    "The quick brown fox {|{{jumps over the lazy dog."
    "The quick brown fox {{{jumps over the lazy dog.")
  '("The quick brown fox {{{jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox {{{jumps over the lazy dog.")
  "C-u C-u M-( 4")

(ipe-test-def-kbd missing-close-delete-6 ()
  "Test `ipe-insert-pair-edit-delete' with only an OPEN string.

(:movement 'line within :infix); Multiple matches; Cursor in first match."
  ipe-test-missing-options
  nil
  '("// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog.")
  "C-u C-u M-( 7")

(ipe-test-def-kbd missing-close-delete-7 ()
  "Test `ipe-insert-pair-edit-delete' with only an OPEN string.

(:movement 'line within :infix); Multiple matches; Cursor between matches."
  ipe-test-missing-options
  nil
  '("// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog.")
  "C-u C-u M-( 7")

(ipe-test-def-kbd missing-close-delete-8 ()
  "Test `ipe-insert-pair-edit-delete' with only an OPEN string.

(:movement 'line within :infix); Multiple matches; Cursor in second match."
  ipe-test-missing-options
  nil
  '("// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "// The quick brown fox |jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog.")
  '("// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u C-u M-( 7")

(ipe-test-def-kbd missing-close-delete-9 ()
  "Test `ipe-insert-pair-edit-delete' with only an OPEN string.

(:movement 'line within :infix); Cursor at end-of-buffer."
  ipe-test-missing-options
  nil
  '("// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog.|")
  '("// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.|")
  "C-u C-u M-( 7")

(ipe-test-def-kbd missing-close-delete-region ()
  "Test `ipe-insert-pair-edit-delete' with only and OPEN string.

Region active; multiple matches (x3)."
  ipe-test-missing-options
  nil
  '("@The quick brown fox {{{jumps over the lazy dog."
    "The quick brown fox {{{jumps over the lazy dog."
    "The quick brown fox {{{jumps over the lazy dog.|")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.|")
  "C-u C-u M-( 4")

;; *********************************************************************
;; Missing OPEN
;; *********************************************************************

(ipe-test-def-kbd missing-open-empty-buffer-insert-1 ()
  "Test `ipe-insert-pair-edit' in an empty buffer.

PAIR has only a single character CLOSE string (:movement 'char)."
  ipe-test-missing-options
  nil
  "|"
  "|>"
  "M-( 2")

(ipe-test-def-kbd missing-open-empty-buffer-insert-2 ()
  "Test `ipe-insert-pair-edit' in an empty buffer.

PAIR has only a multi-character CLOSE string (:movement 'word)."
  ipe-test-missing-options
  nil
  "|"
  "|}}}"
  "M-( 5")

(ipe-test-def-kbd missing-open-empty-buffer-insert-3 ()
  "Test `ipe-insert-pair-edit' in an empty buffer.

PAIR has only a multi-character CLOSE string (:movement 'line)."
  ipe-test-missing-options
  nil
  "|"
  "|// "
  "M-( 8")

(ipe-test-def-kbd missing-open-insert-1 ()
  "Test `ipe-insert-pair-edit' with only a CLOSE string.

PAIR has only a single character CLOSE string (:movement 'char)."
  ipe-test-missing-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |>jumps over the lazy dog."
  "M-( 2 RET")

(ipe-test-def-kbd missing-open-insert-2 ()
  "Test `ipe-insert-pair-edit' with only a CLOSE string.

PAIR has only a multi-character CLOSE string (:movement 'word)."
  ipe-test-missing-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |jumps}}} over the lazy dog."
  "M-( 5 RET")

(ipe-test-def-kbd missing-open-insert-3 ()
  "Test `ipe-insert-pair-edit' with only a CLOSE string.

PAIR has only a multi-character CLOSE string (:movement 'line)."
  ipe-test-missing-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "// The quick brown fox |jumps over the lazy dog.// "
  "M-( 8 RET")

(ipe-test-def-kbd missing-open-infix-insert-1 ()
  "Test `ipe-insert-pair-edit' with only a CLOSE string.

PAIR has only a multi-character CLOSE string (:movement 'line);
Multiple lines."
  ipe-test-missing-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("// The quick brown fox |jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog.// ")
  "M-( 8 C-n C-n RET")

(ipe-test-def-kbd missing-open-to-close-replace-1 ()
  "Test `ipe-insert-pair-edit-replace' with a missing OPEN -> missing CLOSE.

Cursor after OPEN; OPEN at start of buffer."
  ipe-test-missing-options
  nil
  "{{{The quick brown fox |jumps over the lazy dog."
  "The}}} quick brown fox |jumps over the lazy dog."
  "C-u C-u C-u M-( 4 5 RET")

(ipe-test-def-kbd missing-open-to-close-replace-2 ()
  "Test `ipe-insert-pair-edit-replace' with a missing OPEN -> missing CLOSE.

Cursor after OPEN."
  ipe-test-missing-options
  nil
  "The quick {{{brown fox |jumps over the lazy dog."
  "The quick brown}}} fox |jumps over the lazy dog."
  "C-u C-u C-u M-( 4 5 RET")

(ipe-test-def-kbd missing-open-to-close-replace-3 ()
  "Test `ipe-insert-pair-edit-replace' with a missing OPEN -> missing CLOSE.

Cursor within OPEN."
  ipe-test-missing-options
  nil
  "The quick brown fox {|{{jumps over the lazy dog."
  "The quick brown fox |jumps}}} over the lazy dog."
  "C-u C-u C-u M-( 4 5 RET")

(ipe-test-def-kbd missing-open-to-close-replace-4 ()
  "Test `ipe-insert-pair-edit-replace' with a missing OPEN -> missing CLOSE.

Cursor before OPEN."
  ipe-test-missing-options
  nil
  "The quick brown fox |jumps over {{{the lazy dog."
  "The quick brown fox |jumps over the}}} lazy dog."
  "C-u C-u C-u M-( 4 5 RET")

(ipe-test-def-kbd missing-open-to-close-replace-5 ()
  "Test `ipe-insert-pair-edit-replace' with a missing OPEN -> missing CLOSE.

Cursor before OPEN."
  ipe-test-missing-options
  nil
  "The quick brown fox |jumps over the lazy dog.{{{"
  "The quick brown fox |jumps over the lazy dog}}}."
  "C-u C-u C-u M-( 4 5 RET")

(ipe-test-def-kbd missing-open-delete-1 ()
  "Test `ipe-insert-pair-edit-delete' with only a CLOSE string.

Cursor at start of buffer."
  ipe-test-missing-options
  nil
  ">|The quick brown fox jumps over the lazy dog."
  "|The quick brown fox jumps over the lazy dog."
  "C-u C-u M-( 2")

(ipe-test-def-kbd missing-open-delete-2 ()
  "Test `ipe-insert-pair-edit-delete' with only a CLOSE string.

Cursor in middle of buffer."
  ipe-test-missing-options
  nil
  "The quick brown fox |>jumps over the lazy dog."
  "The quick brown fox |jumps over the lazy dog."
  "C-u C-u M-( 2")

(ipe-test-def-kbd missing-open-delete-3 ()
  "Test `ipe-insert-pair-edit-delete' with only a CLOSE string.

Cursor at end of buffer."
  ipe-test-missing-options
  nil
  "The quick brown fox jumps over the lazy dog.>|"
  "The quick brown fox jumps over the lazy dog.|"
  "C-u C-u M-( 2")

(ipe-test-def-kbd missing-open-delete-4 ()
  "Test `ipe-insert-pair-edit-delete' with only a CLOSE string.

CLOSE is a multi-character string."
  ipe-test-missing-options
  nil
  "The quick brown fox jumps}|}} over the lazy dog."
  "The quick brown fox jumps| over the lazy dog."
  "C-u C-u M-( 5")

(ipe-test-def-kbd missing-open-delete-5 ()
  "Test `ipe-insert-pair-edit-delete' with only a CLOSE string.

CLOSE is a multi-character string; Multiple CLOSEs."
  ipe-test-missing-options
  nil
  '("The quick brown fox jumps}}} over the lazy dog."
    "The quick brown fox jumps}|}} over the lazy dog."
    "The quick brown fox jumps}}} over the lazy dog.")
  '("The quick brown fox jumps}}} over the lazy dog."
    "The quick brown fox jumps| over the lazy dog."
    "The quick brown fox jumps}}} over the lazy dog.")
  "C-u C-u M-( 5")

(ipe-test-def-kbd missing-open-delete-6 ()
  "Test `ipe-insert-pair-edit-delete' with only a CLOSE string.

(:movement 'line :infix); Multiple matches; Cursor in first match."
  ipe-test-missing-options
  nil
  '("// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox |jumps over the lazy dog.// "
    "The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog.// ")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog.// ")
  "C-u C-u M-( 8")

(ipe-test-def-kbd missing-open-delete-7 ()
  "Test `ipe-insert-pair-edit-delete' with only a CLOSE string.

(:movement 'line :infix); Multiple matches; Cursor between matches."
  ipe-test-missing-options
  nil
  '("// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog.// "
    "The quick brown fox |jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog.// ")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog.// ")
  "C-u C-u M-( 8")

(ipe-test-def-kbd missing-open-delete-8 ()
  "Test `ipe-insert-pair-edit-delete' with only a CLOSE string.

(:movement 'line :infix); Multiple matches; Cursor in second match."
  ipe-test-missing-options
  nil
  '("// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog.// "
    "The quick brown fox jumps over the lazy dog."
    "// The quick brown fox |jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog.// ")
  '("// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog.// "
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u C-u M-( 8")

(ipe-test-def-kbd missing-open-delete-9 ()
  "Test `ipe-insert-pair-edit-delete' with only a CLOSE string.

(:movement 'line :infix); Cursor at end-of-buffer."
  ipe-test-missing-options
  nil
  '("// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog.// "
    "The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog.// |")
  '("// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog.// "
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.|")
  "C-u C-u M-( 8")

(ipe-test-def-kbd missing-open-delete-10 ()
  "Test `ipe-insert-pair-edit-delete' with only a CLOSE string.

(:movement 'line :infix); Mix of infix + EOL CLOSE."
  ipe-test-missing-options
  nil
  '("// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox |jumps over the lazy dog.// "
    "// The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog.")
  "C-u C-u M-( 8")

(ipe-test-def-kbd missing-open-delete-region ()
  "Test `ipe-insert-pair-edit-delete' with only a CLOSE string.

Region active; multiple matches (x3)."
  ipe-test-missing-options
  nil
  '("@The quick brown fox jumps}}} over the lazy dog."
    "The quick brown fox jumps}}} over the lazy dog."
    "The quick brown fox jumps}}} over the lazy dog.|")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.|")
  "C-u C-u M-( 5")

;; *********************************************************************
;; Missing BOTH
;; *********************************************************************

(ipe-test-def-kbd missing-both-empty-buffer-insert-1 ()
  "Test `ipe-insert-pair-edit' in an empty buffer.

PAIR has neither OPEN nor CLOSE string (:movement 'char)."
  ipe-test-missing-options
  nil
  "|"
  "|"
  "M-( 3")

(ipe-test-def-kbd missing-both-empty-buffer-insert-2 ()
  "Test `ipe-insert-pair-edit' in an empty buffer.

PAIR has neither OPEN nor CLOSE string (:movement 'word)."
  ipe-test-missing-options
  nil
  "|"
  "|"
  "M-( 6")

(ipe-test-def-kbd missing-both-empty-buffer-insert-3 ()
  "Test `ipe-insert-pair-edit' in an empty buffer.

PAIR has neither OPEN nor CLOSE string (:movement 'line)."
  ipe-test-missing-options
  nil
  "|"
  "|"
  "M-( 9")

(ipe-test-def-kbd missing-both-insert-1 ()
  "Test `ipe-insert-pair-edit' with a missing OPEN and CLOSE string.

PAIR has neither OPEN nor CLOSE string (:movement 'char)."
  ipe-test-missing-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |jumps over the lazy dog."
  "M-( 3 RET")

(ipe-test-def-kbd missing-both-insert-2 ()
  "Test `ipe-insert-pair-edit' with a missing OPEN and CLOSE string.

PAIR has neither OPEN nor CLOSE string (:movement 'word)."
  ipe-test-missing-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |jumps over the lazy dog."
  "M-( 6 RET")

(ipe-test-def-kbd missing-both-insert-3 ()
  "Test `ipe-insert-pair-edit' with a missing OPEN and CLOSE string.

PAIR has neither OPEN nor CLOSE string (:movement 'line)."
  ipe-test-missing-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "// The quick brown fox |jumps over the lazy dog."
  "M-( 9 RET")

(ipe-test-def-kbd missing-both-delete-1 ()
  "Test `ipe-insert-pair-edit-delete' with a missing OPEN and CLOSE string.

(:movement 'char)."
  ipe-test-missing-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |jumps over the lazy dog."
  "C-u C-u M-( 3")

(ipe-test-def-kbd missing-both-delete-2 ()
  "Test `ipe-insert-pair-edit-delete' with a missing OPEN and CLOSE string.

(:movement 'word)."
  ipe-test-missing-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |jumps over the lazy dog."
  "C-u C-u M-( 6")

(ipe-test-def-kbd missing-both-delete-3 ()
  "Test `ipe-insert-pair-edit-delete' with a missing OPEN and CLOSE string.

(:movement 'line :infix)."
  ipe-test-missing-options
  nil
  "// The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |jumps over the lazy dog."
  "C-u C-u M-( 9")

(ipe-test-def-kbd missing-both-delete-4 ()
  "Test `ipe-insert-pair-edit-delete' with a missing OPEN and CLOSE string.

(:movement 'line :infix);  Region active."
  ipe-test-missing-options
  nil
  '("|// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog."
    "// The quick brown fox jumps over the lazy dog.@")
  '("|The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u C-u M-( 9")

(ipe-test-def-kbd missing-both-delete-region ()
  "Test `ipe-insert-pair-edit-delete' with a missing OPEN and CLOSE string.

Region active."
  ipe-test-missing-options
  nil
  '("@The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.|")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.|")
  "C-u C-u M-( 6")

(ipe-test-def-kbd missing-both-replace-region ()
  "Test `ipe-insert-pair-edit-replace' with a missing OPEN and CLOSE string.

Region active."
  ipe-test-missing-options
  nil
  '("@The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.|")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.|")
  "C-u C-u C-u M-( 6 3")

(provide 'ipe-test-missing)

;;; ipe-test-missing.el ends here
