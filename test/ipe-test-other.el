;;; ipe-test-other.el --- Insert Pair Edit - Other Tests
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
;; The tests within this file are used to test miscellaneous 'Insert
;; Pair Edit' processing not performed specifically in the other
;; 'categorised' ERT test files.

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'ipe-test)

(defvar ipe-test-other-options
  '((ipe-move-point-on-insert   nil)
    (ipe-prefix-moves-close-p   t)
    (ipe-edit--movement-keysets '(modifiers))
    (ipe-pairs
     '(("(" "(" ")")
       ("<" "<" ">")
       ("`" "`" "'"     (:movement char))
       ("{" "{" "}"     (:movement word))
       ("[" "[" "]"     (:movement line))
       ("/" "/**" " */" (:movement line :infix " * "))))
    (ipe-mode-pairs nil))
  "Options used by `ipe-test-def-kbd' for `ipe-test-other'.")

(ipe-test-def-kbd other-replace-1 ()
  "Test `insert-pair-edit-replace'.

Using a 'word PAIR -> 'word PAIR."
  ipe-test-other-options
  nil
  "The quick brown |fox jumps over the lazy dog."
  "The quick brown <|fox> jumps over the lazy dog."
  "M-( ( ( < RET")

(ipe-test-def-kbd other-replace-2 ()
  "Test `insert-pair-edit-replace' changing movement unit.

Using a 'char PAIR -> 'word PAIR."
  ipe-test-other-options
  nil
  "The quick brown |fox jumps over the lazy dog."
  "The quick brown {|fox} jumps over the lazy dog."
  "M-( ` ( { RET")

(ipe-test-def-kbd other-replace-3 ()
  "Test `insert-pair-edit-replace' changing movement unit.

Using a 'char PAIR -> 'line PAIR."
  ipe-test-other-options
  nil
  "The quick brown |fox jumps over the lazy dog."
  "[The quick brown |fox jumps over the lazy dog.]"
  "M-( { ( [ RET")

(ipe-test-def-kbd other-replace-4 ()
  "Test `insert-pair-edit-replace' changing movement unit.

Using a 'word PAIR -> 'char PAIR."
  ipe-test-other-options
  nil
  "The quick brown |fox jumps over the lazy dog."
  "The quick brown `|fox' jumps over the lazy dog."
  "M-( { ( ` RET")

(ipe-test-def-kbd other-replace-5 ()
  "Test `insert-pair-edit-replace' changing movement unit.

Using a 'line PAIR -> 'word PAIR."
  ipe-test-other-options
  nil
  "The quick brown |fox jumps over the lazy dog."
  "{The quick brown |fox jumps over the lazy dog}."
  "M-( [ ( { RET")

(ipe-test-def-kbd other-replace-6 ()
  "Test `insert-pair-edit-replace' changing movement unit.

Using a 'line PAIR -> 'char PAIR."
  ipe-test-other-options
  nil
  "The quick brown |fox jumps over the lazy dog."
  "`The quick brown |fox jumps over the lazy dog.'"
  "M-( [ ( ` RET")

(ipe-test-def-kbd other-replace-7 ()
  "Test `insert-pair-edit-replace'."
  ipe-test-other-options
  nil
  "The quick brown |fox jumps over the lazy dog."
  "The quick brown [|fox] jumps over the lazy dog."
  "M-( ( RET C-u C-u C-u M-( ( [ RET")

(ipe-test-def-kbd other-replace-8 ()
  "Test `insert-pair-edit-replace'."
  ipe-test-other-options
  nil
  "The quick brown |fox jumps over the lazy dog."
  "The quick brown `|fox' jumps over the lazy dog."
  "M-( ( RET C-u C-u C-u M-( ( ` RET")

(ipe-test-def-kbd other-double-insert-1 ()
  "Test `insert-pair-edit' inserting two pairs.

Using a 'word PAIR."
  ipe-test-other-options
  nil
  "The quick brown |fox jumps over the lazy dog."
  "The quick brown ((|fox)) jumps over the lazy dog."
  "M-( ( RET M-( ( RET")

(ipe-test-def-kbd other-double-insert-2 ()
  "Test `insert-pair-edit' inserting two different pairs.

Using a 'word PAIR."
  ipe-test-other-options
  nil
  "The quick brown |fox jumps over the lazy dog."
  "The quick brown (<|fox>) jumps over the lazy dog."
  "M-( ( RET M-( < RET")

(ipe-test-def-kbd other-triple-insert ()
  "Test `insert-pair-edit' inserting three pairs.

Using a 'word PAIR."
  ipe-test-other-options
  nil
  "The quick brown |fox jumps over the lazy dog."
  "The quick brown (<(|fox)>) jumps over the lazy dog."
  "M-( ( RET M-( < RET M-( ( RET")

(ipe-test-def-kbd other-infix-update ()
  "Test `insert-pair-edit' updating an :infix PAIR.

Using a 'line PAIR."
  ipe-test-other-options
  nil
  '("|The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("|The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( / C-n C-n RET C-u M-( / C-d")

(ipe-test-def-kbd other-infix-replace ()
  "Test `insert-pair-edit' replacing an :infix PAIR.

Using a 'line PAIR and a 'word PAIR."
  ipe-test-other-options
  nil
  '("|The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("(|The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.)")
  "M-( / C-n C-n RET C-u C-u C-u M-( / ( RET")

(when (>= emacs-major-version 26)
  (ipe-test-def-kbd other-undo-1 ()
    "Test `insert-pair-edit' undo.

Using multiple:

- C-s (`ipe-edit--next-pair')
- C-r (`ipe-edit--previous-pair')

calls."
    ipe-test-other-options
    nil
    '("|The quick brown fox (jumps) over the lazy dog."
      "The quick brown fox (jumps) over the lazy dog."
      "The quick brown fox (jumps) over the lazy dog.")
    '("The quick brown fox |(jumps) over the lazy dog."
      "The quick brown fox (jumps) over the lazy dog."
      "The quick brown fox (jumps) over the lazy dog.")
    "C-u M-( ( C-s C-s C-r C-r RET C-/")

  (ipe-test-def-kbd other-undo-2 ()
    "Test `insert-pair-edit' undo.

Using:

- M-u (`ipe-edit--contents-upcase')
- M-c (`ipe-edit--contents-capitalize')
- %   (`ipe-edit--contents-replace')

calls."
    ipe-test-other-options
    nil
    '("|The quick brown fox (jumps) over the lazy dog."
      "The quick brown fox (jumps) over the lazy dog."
      "The quick brown fox (jumps) over the lazy dog.")
    '("The quick brown fox |(jumps) over the lazy dog."
      "The quick brown fox (jumps) over the lazy dog."
      "The quick brown fox (jumps) over the lazy dog.")
    "C-u M-( ( M-u C-s M-c C-s % x C-a C-k test RET RET C-/"))

(provide 'ipe-test-other)

;;; ipe-test-other.el ends here
