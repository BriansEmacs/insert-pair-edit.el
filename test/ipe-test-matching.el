;;; ipe-test-matching.el --- Insert Pair Edit - OPEN CLOSE Matching
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
;; The tests within this file are used to test that the
;; `insert-pair-edit-update' function correctly 'matches' nested
;; 'Insert Pair Edit' (ipe) PAIRs when entering `ipe-edit-mode'.

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'ipe-test)

(setq ipe-test-matching-options
      '((ipe-move-point-on-insert   nil)
        (ipe--escapes-show-p        t)
        (ipe-prefix-moves-close-p   t)
        (ipe-edit--movement-keysets '(modifiers))
        (ipe-pairs '(("("  "(" ")")))
        (ipe-mode-pairs nil)))

(ipe-test-def-kbd matching-update-1 ()
  "Test `insert-pair-edit-update' function with nested PAIRs.

Test single nested PAIR."
  ipe-test-matching-options
  nil
  "|The quick brown (fox (jumps) over) the lazy dog."
  "|The quick brown fox (jumps) over the lazy dog."
  "C-u M-( ( C-d")

(ipe-test-def-kbd matching-update-2 ()
  "Test `insert-pair-edit-update' function with nested PAIRs.

Test multiple nested PAIRs."
  ipe-test-matching-options
  nil
  "|The quick (brown (fox) (jumps) (over) the) lazy dog."
  "|The quick brown (fox) (jumps) (over) the lazy dog."
  "C-u M-( ( C-d")

(ipe-test-def-kbd matching-update-3 ()
  "Test `insert-pair-edit-update' function with nested PAIRs.

Test multiple doubly nested PAIRs."
  ipe-test-matching-options
  nil
  "|The quick (brown ((fox) (jumps) (over)) the) lazy dog."
  "|The quick brown ((fox) (jumps) (over)) the lazy dog."
  "C-u M-( ( C-d")

(ipe-test-def-kbd matching-update-4 ()
  "Test `insert-pair-edit-update' function with nested PAIRs.

Test inner PAIR update."
  ipe-test-matching-options
  nil
  "The quick brown (fox (|jumps) over) the lazy dog."
  "The quick brown (fox |jumps over) the lazy dog."
  "C-u M-( ( C-d")

(ipe-test-def-kbd matching-update-5 ()
  "Test `insert-pair-edit-update' function with nested PAIRs.

Test single and doubly nested PAIRs."
  ipe-test-matching-options
  nil
  "|The (quick (brown) ((fox) ((jumps)) (over)) (the) lazy) dog."
  "|The quick (brown) ((fox) ((jumps)) (over)) (the) lazy dog."
  "C-u M-( ( C-d")

(ipe-test-def-kbd matching-update-6 ()
  "Test `insert-pair-edit-update' function with nested PAIRs.

Test unmatched PAIR."
  ipe-test-matching-options
  nil
  "|The quick brown (fox (jumps) over the lazy dog."
  "|The quick brown (fox jumps over the lazy dog."
  "C-u M-( ( C-d")

(ipe-test-def-kbd matching-update-7 ()
  "Test `insert-pair-edit-update' function with nested PAIRs.

Test multiple unmatched PAIRs."
  ipe-test-matching-options
  nil
  "|The quick brown (fox (jumps((() over the lazy dog."
  "|The quick brown (fox (jumps(( over the lazy dog."
  "C-u M-( ( C-d")

(provide 'ipe-test-matching)

;; ipe-test-matching.el ends here
