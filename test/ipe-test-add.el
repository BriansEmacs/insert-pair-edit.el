;;; ipe-test-add.el --- Insert Pair Edit - 'Multiple' ERT Tests
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
;; The tests within this file are used to test the 'addition' of extra
;; 'Insert Pair Edit' (ipe) PAIRS during `ipe-edit-mode'.  This is
;; done via the 'Add ...' 'Insert Pair Edit' `ipe-edit-mode' 'Add ...'
;; commands:
;;
;;   `ipe-edit--add-next-pair'
;;   `ipe-edit--add-previous-pair'
;;   `ipe-edit--add-next-contents'
;;   `ipe-edit--add-previous-contents'

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'ipe-test)

(setq ipe-test-add-options
      '((ipe-move-point-on-insert   nil)
        (ipe-prefix-moves-close-p   t)
        (ipe-edit--movement-keysets '(modifiers))
        (ipe-pairs
         '(
           ("("  "(((((" ")))))")
           ("<"  "<"     ">")
           ("'"  "'"     "'"     (:escapes (("'" "\\'"))))
           (";"  "<-- "  " -->"  (:movement line :infix    " -- "))))
        (ipe-mode-pairs        nil)))

(ipe-test-def-kbd add-next-pair-1 ()
  "Test `insert-pair-edit' `ipe-edit--add-next-pair'.

Using a 'word PAIR."
  ipe-test-add-options
  nil
  '("The quick brown fox (((((|jumps))))) over the lazy dog."
    "The quick brown fox (((((jumps))))) over the lazy dog."
    "The quick brown fox (((((jumps))))) over the lazy dog.")
  '("The quick brown fox <|jumps> over the lazy dog."
    "The quick brown fox <!jumps> over the lazy dog."
    "The quick brown fox (((((jumps))))) over the lazy dog.")
  "C-u M-( ( s ( < RET")

(ipe-test-def-kbd add-next-pair-2 ()
  "Test `insert-pair-edit' `ipe-edit--add-next-pair'.

Using a 'word PAIR.  Search (x2)"
  ipe-test-add-options
  nil
  '("The quick brown fox (((((|jumps))))) over the lazy dog."
    "The quick brown fox (((((jumps))))) over the lazy dog."
    "The quick brown fox (((((jumps))))) over the lazy dog.")
  '("The quick brown fox <|jumps> over the lazy dog."
    "The quick brown fox <!jumps> over the lazy dog."
    "The quick brown fox <!jumps> over the lazy dog.")
  "C-u M-( ( s s ( < RET")

(ipe-test-def-kbd add-next-pair-3 ()
  "Test `insert-pair-edit' `ipe-edit--add-next-pair'.

Using a 'word PAIR.  With prefix arg."
  ipe-test-add-options
  nil
  '("The quick brown fox (((((|jumps))))) over the lazy dog."
    "The quick brown fox (((((jumps))))) over the lazy dog."
    "The quick brown fox (((((jumps))))) over the lazy dog.")
  '("The quick brown fox <|jumps> over the lazy dog."
    "The quick brown fox (((((jumps))))) over the lazy dog."
    "The quick brown fox <!jumps> over the lazy dog.")
  "C-u M-( ( 2 s ( < RET")

(ipe-test-def-kbd add-previous-pair-1 ()
  "Test `insert-pair-edit' `ipe-edit--add-previous-pair'.

Using a 'word PAIR."
  ipe-test-add-options
  nil
  '("The quick brown fox (((((jumps))))) over the lazy dog."
    "The quick brown fox (((((jumps))))) over the lazy dog."
    "The quick brown fox (((((|jumps))))) over the lazy dog.")
  '("The quick brown fox (((((jumps))))) over the lazy dog."
    "The quick brown fox <!jumps> over the lazy dog."
    "The quick brown fox <|jumps> over the lazy dog.")
  "C-u M-( ( r ( < RET")

(ipe-test-def-kbd add-previous-pair-2 ()
  "Test `insert-pair-edit' `ipe-edit--add-previous-pair'.

Using a 'word PAIR.  Search (x2)"
  ipe-test-add-options
  nil
  '("The quick brown fox (((((jumps))))) over the lazy dog."
    "The quick brown fox (((((jumps))))) over the lazy dog."
    "The quick brown fox (((((|jumps))))) over the lazy dog.")
  '("The quick brown fox <!jumps> over the lazy dog."
    "The quick brown fox <!jumps> over the lazy dog."
    "The quick brown fox <|jumps> over the lazy dog.")
  "C-u M-( ( r r ( < RET")

(ipe-test-def-kbd add-previous-pair-3 ()
  "Test `insert-pair-edit' `ipe-edit--add-previous-pair'.

Using a 'word PAIR.  With prefix arg."
  ipe-test-add-options
  nil
  '("The quick brown fox (((((jumps))))) over the lazy dog."
    "The quick brown fox (((((jumps))))) over the lazy dog."
    "The quick brown fox (((((|jumps))))) over the lazy dog.")
  '("The quick brown fox <!jumps> over the lazy dog."
    "The quick brown fox (((((jumps))))) over the lazy dog."
    "The quick brown fox <|jumps> over the lazy dog.")
  "C-u M-( ( 2 r ( < RET")

(ipe-test-def-kbd add-next-contents-1 ()
  "Test `insert-pair-edit' `ipe-edit--add-next-contents'.

Using a 'word PAIR."
  ipe-test-add-options
  nil
  '("The quick brown fox (((|((jumps))))) over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (((|((jumps))))) over the lazy dog."
    "The quick brown fox (((!((jumps))))) over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ( S RET")

(ipe-test-def-kbd add-next-contents-2 ()
  "Test `insert-pair-edit' `ipe-edit--add-next-contents'.

Using a 'word PAIR.  Search (x2)"
  ipe-test-add-options
  nil
  '("The quick brown fox (((|((jumps))))) over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (((|((jumps))))) over the lazy dog."
    "The quick brown fox (((!((jumps))))) over the lazy dog."
    "The quick brown fox (((!((jumps))))) over the lazy dog.")
  "C-u M-( ( S S RET")

(ipe-test-def-kbd add-next-contents-3 ()
  "Test `insert-pair-edit' `ipe-edit--add-next-contents'.

Using a 'word PAIR.  With prefix arg."
  ipe-test-add-options
  nil
  '("The quick brown fox (((|((jumps))))) over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (((|((jumps))))) over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (((!((jumps))))) over the lazy dog.")
  "C-u M-( ( 2 S RET")

(ipe-test-def-kbd add-previous-contents-1 ()
  "Test `insert-pair-edit' `ipe-edit--add-previous-contents'.

Using a 'word PAIR."
  ipe-test-add-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (((|((jumps))))) over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox (((!((jumps))))) over the lazy dog."
    "The quick brown fox (((|((jumps))))) over the lazy dog.")
  "C-u M-( ( R RET")

(ipe-test-def-kbd add-previous-contents-2 ()
  "Test `insert-pair-edit' `ipe-edit--add-previous-contents'.

Using a 'word PAIR.  Search (x2)"
  ipe-test-add-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (((|((jumps))))) over the lazy dog.")
  '("The quick brown fox (((!((jumps))))) over the lazy dog."
    "The quick brown fox (((!((jumps))))) over the lazy dog."
    "The quick brown fox (((|((jumps))))) over the lazy dog.")
  "C-u M-( ( R R RET")

(ipe-test-def-kbd add-previous-contents-3 ()
  "Test `insert-pair-edit' `ipe-edit--add-previous-contents'.

Using a 'word PAIR.  With prefix arg."
  ipe-test-add-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (((|((jumps))))) over the lazy dog.")
  '("The quick brown fox (((!((jumps))))) over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (((|((jumps))))) over the lazy dog.")
  "C-u M-( ( 2 R RET")

(provide 'ipe-test-add)

;; ipe-test-add.el ends here
