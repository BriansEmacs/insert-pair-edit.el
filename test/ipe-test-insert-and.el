;;; ipe-test-insert-and.el --- Insert Pair Edit - 'Insert And...' Tests -*- lexical-binding: t; -*-
;; Copyright (C) 2023 Brian Kavanagh

;; Author: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Maintainer: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
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
;; The tests within this file are used to test the function which
;; perform 'extra' operations when inserting an 'Insert Pair Edit'
;; PAIR into the buffer on exit of `ipe-edit-mode'.  This is done via
;; the 'Insert And...' 'Insert Pair Edit' (ipe) commands:
;;
;;   `ipe-edit--ia-goto-open'
;;   `ipe-edit--ia-goto-close'
;;   `ipe-edit--ia-resume'
;;   `ipe-edit--ia-copy-text'
;;   `ipe-edit--ia-kill-text'

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'ipe-test)

(defvar ipe-test-insert-and-options
  '((ipe-move-point-on-insert   nil)
    (ipe-prefix-moves-close-p   t)
    (ipe-edit--movement-keysets '(modifiers))
    (ipe-update-forward-first-p nil)
    (ipe-pairs                  '(("(" "<start>" "<end>")))
    (ipe-mode-pairs             nil))
  "Options used by `ipe-test-def-kbd' for `ipe-test-insert-and'.")

(ipe-test-def-kbd insert-and-goto-open-1 ()
  "Test `ipe-insert-pair-edit' \"Insert And... -> Goto Open\".

Using a 'word PAIR."
  ipe-test-insert-and-options
  nil
  "The quick brown fox ju|mps over the lazy dog."
  "The quick brown fox <start>|jumps<end> over the lazy dog."
  "M-( ( O")

(ipe-test-def-kbd insert-and-goto-open-2 ()
  "Test `ipe-insert-pair-edit' \"Insert And... -> Goto Open\".

Using a 'word PAIR and a C-u argument."
  ipe-test-insert-and-options
  nil
  "The quick brown fox ju|mps over the lazy dog."
  "The quick brown fox |<start>jumps<end> over the lazy dog."
  "M-( ( C-u O")

(ipe-test-def-kbd insert-and-goto-open-3 ()
  "Test `ipe-insert-pair-edit' \"Insert And... -> Goto Open\".

Using a 'word PAIR and a numeric argument."
  ipe-test-insert-and-options
  nil
  "The quick brown fox ju|mps over the lazy dog."
  "The quick brown fox <st|art>jumps<end> over the lazy dog."
  "M-( ( C-3 O")

(ipe-test-def-kbd insert-and-goto-close-1 ()
  "Test `ipe-insert-pair-edit' \"Insert And... -> Goto Close\".

Using a 'word PAIR."
  ipe-test-insert-and-options
  nil
  "The quick brown fox ju|mps over the lazy dog."
  "The quick brown fox <start>jumps<end>| over the lazy dog."
  "M-( ( C")

(ipe-test-def-kbd insert-and-goto-close-2 ()
  "Test `ipe-insert-pair-edit' \"Insert And... -> Goto Close\".

Using a 'word PAIR and a universal-argument."
  ipe-test-insert-and-options
  nil
  "The quick brown fox ju|mps over the lazy dog."
  "The quick brown fox <start>jumps|<end> over the lazy dog."
  "M-( ( C-u C")

(ipe-test-def-kbd insert-and-goto-close-3 ()
  "Test `ipe-insert-pair-edit' \"Insert And... -> Goto Close\".

Using a 'word PAIR and a numeric argument."
  ipe-test-insert-and-options
  nil
  "The quick brown fox ju|mps over the lazy dog."
  "The quick brown fox <start>jumps<en|d> over the lazy dog."
  "M-( ( C-3 C")

(ipe-test-def-kbd insert-and-resume ()
  "Test `ipe-insert-pair-edit' \"Insert And... -> Resume\".

Using a 'word PAIR."
  ipe-test-insert-and-options
  nil
  "The quick brown fox ju|mps over the lazy dog."
  "The quick brown fox <start>ju|mps<end> over the lazy dog."
  "M-( ( U")

;; Headless ERT has problems with Kill Ring <= 24.
(when (> emacs-major-version 24)
  (ipe-test-def-kbd insert-and-copy-text ()
    "Test `ipe-insert-pair-edit' \"Insert And... -> Copy Text\".

Using a 'word PAIR."
    ipe-test-insert-and-options
    nil
    "The quick brown fox ju|mps over the lazy dog."
    "The quick brown fox <start>jumps<end> over the lazy dog.jumps|"
    "M-( ( Y M-e C-y"))

(ipe-test-def-kbd insert-and-kill-text-1 ()
  "Test `ipe-insert-pair-edit' \"Insert And... -> Kill Text\".

Using a 'word PAIR."
  ipe-test-insert-and-options
  nil
  "The quick brown fox ju|mps over the lazy dog."
  "The quick brown fox <start>|<end> over the lazy dog."
  "M-( ( K")

(ipe-test-def-kbd insert-and-kill-text-2 ()
  "Test `ipe-insert-pair-edit' \"Insert And... -> Kill Text\".

Using a 'word PAIR at start-of-line."
  ipe-test-insert-and-options
  nil
  "|The quick brown fox jumps over the lazy dog."
  "<start>|<end> quick brown fox jumps over the lazy dog."
  "M-( ( K")

(ipe-test-def-kbd insert-and-kill-text-3 ()
  "Test `ipe-insert-pair-edit' \"Insert And... -> Kill Text\".

Using a 'word PAIR at end-of-line."
  ipe-test-insert-and-options
  nil
  "The quick brown fox jumps over the lazy dog|"
  "The quick brown fox jumps over the lazy <start><end>|"
  "M-( ( K")

(ipe-test-def-kbd insert-and-kill-text-4 ()
  "Test `ipe-insert-pair-edit' \"Insert And... -> Kill Text\".

Using a 'line PAIR."
  ipe-test-insert-and-options
  nil
  "The quick brown fox ju|mps over the lazy dog."
  "<start>|<end>"
  "M-( ( m l K")

(provide 'ipe-test-insert-and)

;;; ipe-test-insert-and.el ends here
