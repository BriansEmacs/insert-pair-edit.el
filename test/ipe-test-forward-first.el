;;; ipe-test-forward-first.el --- Insert Pair Edit - ipe-update-forward-first-p Tests -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Brian Kavanagh

;; Author: Brian Kavanagh <brians.emacs@gmail.com>
;; Maintainer: Brian Kavanagh <brians.emacs@gmail.com>
;; Created: 09 April, 2024
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
;; performed by 'Insert Pair Edit' when the
;; `ipe-update-forward-first-p' option is set to `t'.

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'ipe-test)

(defvar ipe-test-forward-first-options
  '((ipe-move-point-on-insert   nil)
    (ipe-prefix-moves-close-p   t)
    (ipe-edit--movement-keysets '(modifiers))
    (ipe-update-forward-first-p t)
    (ipe-pairs
     '(("(" "("   ")")
       ("[" "["   "]")
       ("'" "'"   "'")
       ("/" "/**" " */"
	(
	 :movement        line
	 :infix           " * "
	 :indent-function previous))))
    (ipe-mode-pairs             nil))
  "Options used by `ipe-test-def-kbd' for `ipe-test-forward-first'.")

(ipe-test-def-kbd search-forward-1 ()
  "Test `ipe-insert-pair-edit' with `ipe-test-forward-first' = t.

Simple search forward."
  ipe-test-forward-first-options
  nil
  "The quick brown (fox) ju|mps (over) the lazy dog."
  "The quick brown (fox) ju|mps [over] the lazy dog."
  "C-u M-( ( ( [ RET")

(ipe-test-def-kbd search-forward-2 ()
  "Test `ipe-insert-pair-edit' with `ipe-test-forward-first' = t.

Search forward where OPEN = CLOSE."
  ipe-test-forward-first-options
  nil
  "The quick brown 'fox' ju|mps 'over' the lazy dog."
  "The quick brown 'fox' ju|mps [over] the lazy dog."
  "C-u M-( ' ( [ RET")

(ipe-test-def-kbd search-forward-3 ()
  "Test `ipe-insert-pair-edit' with `ipe-test-forward-first' = t.

Search forward where movement-by = 'line."
  ipe-test-forward-first-options
  nil
  '("/** The quick brown fox jumps over the lazy dog. */"
    "The quick brown fox ju|mps over the lazy dog."
    "/** The quick brown fox jumps over the lazy dog. */")
  '("/** The quick brown fox jumps over the lazy dog. */"
    "The quick brown fox ju|mps over the lazy dog."
    "[The quick brown fox jumps over the lazy dog].")
  "C-u M-( / ( [ RET")

(ipe-test-def-kbd search-forward-4 ()
  "Test `ipe-insert-pair-edit' with `ipe-test-forward-first' = t.

Search forward where movement-by = 'line and OPEN / CLOSE toggled."
  ipe-test-forward-first-options
  nil
  '("/**"
    " * The quick brown fox jumps over the lazy dog."
    " */"
    "The quick brown fox ju|mps over the lazy dog."
    "/**"
    " * The quick brown fox jumps over the lazy dog."
    " */")
  '("/**"
    " * The quick brown fox jumps over the lazy dog."
    " */"
    "The quick brown fox ju|mps over the lazy dog."
    "[The quick brown fox jumps over the lazy dog].")
  "C-u M-( / ( [ RET")

(ipe-test-def-kbd search-forward-5 ()
  "Test `ipe-insert-pair-edit' with `ipe-test-forward-first' = t.

Search forward where cursor is within PAIR."
  ipe-test-forward-first-options
  nil
  "The quick (brown) fox (ju|mps) over (the) lazy dog."
  "The quick (brown) fox [ju|mps] over (the) lazy dog."
  "C-u M-( ( ( [ RET")

(provide 'ipe-test-forward-first)

;;; ipe-test-forward-first.el ends here
