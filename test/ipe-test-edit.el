;;; ipe-test-edit.el --- Insert Pair Edit - 'Other Command' Tests -*- lexical-binding: t; -*-
;; Copyright (C) 2023 Brian Kavanagh

;; Author: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Maintainer: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Created: 28 June, 2020
;; Version: 1.0
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
;; The tests within this file are used to test the update / delete /
;; replace 'Insert Pair Edit' (ipe) functions.  This is done via the
;; top-level 'Insert Pair Edit' (ipe) commands:
;;
;;   `insert-pair-edit-update'
;;   `insert-pair-edit-delete'
;;   `insert-pair-edit-replace'

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'ipe-test)

(defvar ipe-test-edit-options
  '((ipe-move-point-on-insert   nil)
    (ipe-prefix-moves-close-p   t)
    (ipe-edit--movement-keysets '(modifiers))
    (ipe-pairs
     '(("(" "(" ")")
       ("<" "<" ">")
       (";" "<-- " " -->" (:movement line :infix " -- "))))
    (ipe-mode-pairs        nil))
  "Options used by `ipe-test-def-kbd' for `ipe-test-edit'.")

(ipe-test-def-kbd edit-update-1 ()
  "Test `insert-pair-edit-update' function.

Using a ('word pair) -> ('word pair)"
  ipe-test-edit-options
  nil
  "The quick brown (|fox) jumps over the (lazy) dog."
  "The quick brown (|fox jumps) over the (lazy) dog."
  "C-u M-( ( C-f RET")

(ipe-test-def-kbd edit-update-2 ()
  "Test `insert-pair-edit-update' function.

Using a ('word pair) -> ('word pair) with cursor before pair."
  ipe-test-edit-options
  nil
  "The quick |brown (fox) jumps over the (lazy) dog."
  "The quick |brown (fox jumps) over the (lazy) dog."
  "C-u M-( ( C-f RET")

(ipe-test-def-kbd edit-update-3 ()
  "Test `insert-pair-edit-update' function.

Using a ('word pair) -> ('word pair) with cursor after pair."
  ipe-test-edit-options
  nil
  "The quick brown (fox) |jumps over the (lazy) dog."
  "The quick brown (fox |jumps) over the (lazy) dog."
  "C-u M-( ( C-f RET")

(ipe-test-def-kbd edit-update-4 ()
  "Test `insert-pair-edit-update' function.

Using a ('word pair) -> ('word pair) with cursor after pair."
  ipe-test-edit-options
  nil
  "The quick brown (fox) |jumps over the (lazy) dog."
  "The quick brown (fox |jumps) over the (lazy) dog."
  "C-u M-( ( C-f RET")

(ipe-test-def-kbd edit-update-5 ()
  "Test `insert-pair-edit-update' function.

Using a :infix"
  ipe-test-edit-options
  nil
  '("<-- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->")
  '("(|The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog).")
  "C-u M-( ; ( ( RET")

(ipe-test-def-kbd edit-delete-2 ()
  "Test `insert-pair-edit-delete' function.

Using a ('word pair) -> ('word pair)"
  ipe-test-edit-options
  nil
  "The quick brown (|fox) jumps over the (lazy) dog."
  "The quick brown |fox jumps over the (lazy) dog."
  "C-u C-u M-( (")

(ipe-test-def-kbd edit-delete-3 ()
  "Test `insert-pair-edit-delete' function.

Using a ('word pair) -> ('word pair) with cursor before pair."
  ipe-test-edit-options
  nil
  "The quick |brown (fox) jumps over the (lazy) dog."
  "The quick |brown fox jumps over the (lazy) dog."
  "C-u C-u M-( (")

(ipe-test-def-kbd edit-delete-4 ()
  "Test `insert-pair-edit-delete' function.

Using a ('word pair) -> ('word pair) with cursor after pair."
  ipe-test-edit-options
  nil
  "The quick brown (fox) |jumps over the (lazy) dog."
  "The quick brown fox |jumps over the (lazy) dog."
  "C-u C-u M-( (")

(ipe-test-def-kbd edit-delete-5 ()
  "Test `insert-pair-edit-delete' function.

Using a :infix"
  ipe-test-edit-options
  nil
  '("<-- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->")
  '("|The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u C-u M-( ;")

(ipe-test-def-kbd edit-delete-region ()
  "Test `insert-pair-edit-delete' function with a region."
  ipe-test-edit-options
  nil
  "@The (quick) brown (fox) jumps (over) the (lazy) dog.|"
  "The quick brown fox jumps over the lazy dog.|"
  "C-u C-u M-( (")

(ipe-test-def-kbd edit-replace-empty-1 ()
  "Test `insert-pair-edit-replace' function.

Using a ('word pair) -> ('word pair) in empty buffer."
  ipe-test-edit-options
  nil
  "|()"
  "|<>"
  "C-u C-u C-u M-( ( <")

(ipe-test-def-kbd edit-replace-empty-2 ()
  "Test `insert-pair-edit-replace' function.

Using a ('word pair) -> ('word pair) in empty buffer."
  ipe-test-edit-options
  nil
  "(|)"
  "<|>"
  "C-u C-u C-u M-( ( <")

(ipe-test-def-kbd edit-replace-empty-3 ()
  "Test `insert-pair-edit-replace' function.

Using a ('word pair) -> ('word pair) in empty buffer."
  ipe-test-edit-options
  nil
  "()|"
  "<>|"
  "C-u C-u C-u M-( ( <")

(ipe-test-def-kbd edit-replace-1 ()
  "Test `insert-pair-edit-replace' function.

Using a ('word pair) -> ('word pair)"
  ipe-test-edit-options
  nil
  "(|fox)"
  "<|fox>"
  "C-u C-u C-u M-( ( < RET")

(ipe-test-def-kbd edit-replace-2 ()
  "Test `insert-pair-edit-replace' function.

Using a ('word pair) -> ('word pair)"
  ipe-test-edit-options
  nil
  "The quick brown (|fox) jumps over the (lazy) dog."
  "The quick brown <|fox> jumps over the (lazy) dog."
  "C-u C-u C-u M-( ( < RET")

(ipe-test-def-kbd edit-replace-3 ()
  "Test `insert-pair-edit-replace' function.

Using a ('word pair) -> ('word pair) with cursor before pair."
  ipe-test-edit-options
  nil
  "The quick |brown (fox) jumps over the (lazy) dog."
  "The quick |brown <fox> jumps over the (lazy) dog."
  "C-u C-u C-u M-( ( < RET")

(ipe-test-def-kbd edit-replace-4 ()
  "Test `insert-pair-edit-replace' function.

Using a ('word pair) -> ('word pair) with cursor after pair."
  ipe-test-edit-options
  nil
  "The quick brown (fox) |jumps over the (lazy) dog."
  "The quick brown <fox> |jumps over the (lazy) dog."
  "C-u C-u C-u M-( ( < RET")

(ipe-test-def-kbd edit-replace-5 ()
  "Test `insert-pair-edit-replace' function.

Using a :infix"
  ipe-test-edit-options
  nil
  '("<-- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->")
  '("(|The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.)")
  "C-u C-u C-u M-( ; ( RET")

(provide 'ipe-test-edit)

;;; ipe-test-edit.el ends here
