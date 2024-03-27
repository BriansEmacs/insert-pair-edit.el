;;; ipe-test-indent.el --- Insert Pair Edit - Indent Function Tests -*- lexical-binding: t; -*-
;; Copyright (C) 2023 Brian Kavanagh

;; Author: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Maintainer: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Created: 28 June, 2020
;; Version: 1.1
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
;; performed by 'Insert Pair Edit' when using PAIR Definitions which
;; include an :indent-function.

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'ipe-test)

(defvar ipe-test-indent-options
  '((ipe-move-point-on-insert   nil)
    (ipe-prefix-moves-close-p   t)
    (ipe-edit--movement-keysets '(modifiers))
    (ipe-pairs
     '(("(" "("   ")")
       ("/" "/**" " */"
	(
	 :movement        line
	 :infix           " * "
	 :indent-function previous))))
    (ipe-mode-pairs             nil))
  "Options used by `ipe-test-def-kbd' for `ipe-test-indent'.")

(ipe-test-def-kbd indent-basic-insert-1 ()
  "Test `ipe-insert-pair-edit' in with indents.

Using a 'line (+ :indent-function) PAIR in an empty buffer."
  ipe-test-indent-options
  nil
  '("|")
  '("/**| */")
  "M-( /")

(ipe-test-def-kbd indent-basic-insert-2 ()
  "Test `ipe-insert-pair-edit' in with indents.

Using a 'line (+ :indent-function) PAIR on a single line."
  ipe-test-indent-options
  nil
  '("|The quick brown fox jumps over the lazy dog.")
  '("/**|The quick brown fox jumps over the lazy dog. */")
  "M-( / RET")

(ipe-test-def-kbd indent-basic-insert-3 ()
  "Test `ipe-insert-pair-edit' in with indents.

Using a 'line (+ :indent-function) PAIR with multiple line."
  ipe-test-indent-options
  nil
  '("|The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("/**|The quick brown fox jumps over the lazy dog."
    " * The quick brown fox jumps over the lazy dog."
    " * The quick brown fox jumps over the lazy dog. */")
  "M-( / C-n C-n RET")

;; TODO: Fix 'extra' whitespace insertion
(ipe-test-def-kbd indent-insert-1 ()
  "Test `ipe-insert-pair-edit' in with indents.

Using a 'line (+ :indent-function) PAIR with indented line."
  ipe-test-indent-options
  nil
  '("  The quick brown fox jumps over the lazy dog."
    "    The quick brown fox jumps over the lazy dog."
    "    |The quick brown fox jumps over the lazy dog.")
  '("  The quick brown fox jumps over the lazy dog."
    "    The quick brown fox jumps over the lazy dog."
    "    /**    |The quick brown fox jumps over the lazy dog. */")
  "M-( / RET")

(ipe-test-def-kbd indent-insert-2 ()
  "Test `ipe-insert-pair-edit' in with indents.

Using a 'line (+ :indent-function) PAIR with indented previous line."
  ipe-test-indent-options
  nil
  '("  The quick brown fox jumps over the lazy dog."
    "    The quick brown fox jumps over the lazy dog."
    "       The quick brown fox jumps over the lazy dog."
    "    |The quick brown fox jumps over the lazy dog.")
  '("  The quick brown fox jumps over the lazy dog."
    "    The quick brown fox jumps over the lazy dog."
    "       The quick brown fox jumps over the lazy dog."
    "       /**    |The quick brown fox jumps over the lazy dog. */")
  "M-( / RET")

(ipe-test-def-kbd indent-insert-3 ()
  "Test `ipe-insert-pair-edit' in with indents.

Using a 'line (+ :indent-function) PAIR with indented line and movement."
  ipe-test-indent-options
  nil
  '("  The quick brown fox jumps over the lazy dog."
    "    The quick brown fox jumps over the lazy dog."
    "    |The quick brown fox jumps over the lazy dog."
    "    The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog.")
  '("  The quick brown fox jumps over the lazy dog."
    "  /**    The quick brown fox jumps over the lazy dog."
    "   *     |The quick brown fox jumps over the lazy dog."
    "   *     The quick brown fox jumps over the lazy dog. */"
    "  The quick brown fox jumps over the lazy dog.")
  "M-( / C-p C-n RET")

(ipe-test-def-kbd indent-insert-4 ()
  "Test `ipe-insert-pair-edit' in with indents.

Using a 'line (+ :indent-function) PAIR with indented line and movement."
  ipe-test-indent-options
  nil
  '("  The quick brown fox jumps over the lazy dog."
    "    The quick brown fox jumps over the lazy dog."
    "    The quick brown fox jumps over the lazy dog."
    "    |The quick brown fox jumps over the lazy dog."
    "    The quick brown fox jumps over the lazy dog."
    "    The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog.")
  '("  The quick brown fox jumps over the lazy dog."
    "    The quick brown fox jumps over the lazy dog."
    "    /**    The quick brown fox jumps over the lazy dog."
    "     *     |The quick brown fox jumps over the lazy dog."
    "     *     The quick brown fox jumps over the lazy dog. */"
    "    The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog.")
  "M-( / C-p C-n RET")

(ipe-test-def-kbd indent-insert-5 ()
  "Test `ipe-insert-pair-edit' in with indents.

Using a 'line (+ :indent-function) PAIR with multiple indented lines
and movement."
  ipe-test-indent-options
  nil
  '("  The quick brown fox jumps over the lazy dog."
    "    The quick brown fox jumps over the lazy dog."
    "    The quick brown fox jumps over the lazy dog."
    "      |The quick brown fox jumps over the lazy dog."
    "    The quick brown fox jumps over the lazy dog."
    "    The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog.")
  '("  The quick brown fox jumps over the lazy dog."
    "    The quick brown fox jumps over the lazy dog."
    "    /**    The quick brown fox jumps over the lazy dog."
    "     *       |The quick brown fox jumps over the lazy dog."
    "     *     The quick brown fox jumps over the lazy dog. */"
    "    The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog.")
  "M-( / C-p C-n RET")

(provide 'ipe-test-indent)

;;; ipe-test-indent.el ends here
