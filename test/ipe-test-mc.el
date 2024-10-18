;;; ipe-test-mc.el --- Insert Pair Edit - Multiple Cursor Tests -*- lexical-binding: t; -*-
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
;; The tests within this file are used to test that the 'Insert Pair
;; Edit' commands work with the `multiple-cursors' package.

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'ipe-test)

(defvar ipe-test-mc-options
  '((ipe-move-point-on-insert   nil)
    (ipe-prefix-moves-close-p   t)
    (ipe-edit--movement-keysets '(modifiers))
    (ipe-update-forward-first-p nil)
    (ipe-delete-action          'delete)
    (ipe-pairs
     '(("(" "(" ")")
       ("S" "<start>" "<end>")
       ("L" "<really-large-open-tag>" "<really-large-close-tag>")
       ("i" "(" ")" (:auto-insert t))))
    (ipe-mode-pairs             nil))
  "Options used by `ipe-test-def-kbd' for `ipe-test-mc'.")

(ipe-test-def-kbd mc-basic-insert-1 ()
  "Test `ipe-insert-pair-edit' at start of a buffer.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("!!|a")
  '("!!(|a)()()")
  "M-( ( RET")

(ipe-test-def-kbd mc-basic-insert-2 ()
  "Test `ipe-insert-pair-edit' at start of a line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("!The quick brown fox jumps over the lazy dog."
    "|The quick brown fox jumps over the lazy dog."
    "!The quick brown fox jumps over the lazy dog.")
  '("(!The) quick brown fox jumps over the lazy dog."
    "(|The) quick brown fox jumps over the lazy dog."
    "(!The) quick brown fox jumps over the lazy dog.")
  "M-( ( RET")

(ipe-test-def-kbd mc-basic-insert-3 ()
  "Test `ipe-insert-pair-edit' in middle of a line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox (|jumps) over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog.")
  "M-( ( RET")

(ipe-test-def-kbd mc-basic-insert-4 ()
  "Test `ipe-insert-pair-edit' at end of the line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog.!"
    "The quick brown fox jumps over the lazy dog.|"
    "The quick brown fox jumps over the lazy dog.!")
  '("The quick brown fox jumps over the lazy (dog).!"
    "The quick brown fox jumps over the lazy (dog).|"
    "The quick brown fox jumps over the lazy (dog).!")
  "M-( ( RET")
(ipe-test-def-kbd mc-basic-insert-5 ()
  "Test `ipe-insert-pair-edit' at 'offset' beginning of a line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("!  The quick brown fox jumps over the lazy dog."
    "|  The quick brown fox jumps over the lazy dog."
    "!  The quick brown fox jumps over the lazy dog.")
  '("!  (The) quick brown fox jumps over the lazy dog."
    "|  (The) quick brown fox jumps over the lazy dog."
    "!  (The) quick brown fox jumps over the lazy dog.")
  "M-( ( RET")

(ipe-test-def-kbd mc-basic-insert-6 ()
  "Test `ipe-insert-pair-edit' at 'offset' end of a line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog.  !"
    "The quick brown fox jumps over the lazy dog.  |"
    "The quick brown fox jumps over the lazy dog.  !")
  '("The quick brown fox jumps over the lazy (dog).  !"
    "The quick brown fox jumps over the lazy (dog).  |"
    "The quick brown fox jumps over the lazy (dog).  !")
  "M-( ( RET")

(ipe-test-def-kbd mc-basic-prefix-insert-1 ()
  "Test `ipe-insert-pair-edit' with a numeric prefix.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox (|jumps) over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog.")
  "C-1 M-( ( RET")

(ipe-test-def-kbd mc-basic-prefix-insert-2 ()
  "Test `ipe-insert-pair-edit' with a '2' numeric prefix.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox (!jumps over) the lazy dog."
    "The quick brown fox (|jumps over) the lazy dog."
    "The quick brown fox (!jumps over) the lazy dog.")
  "C-2 M-( ( RET")

(ipe-test-def-kbd mc-basic-prefix-insert-3 ()
  "Test `ipe-insert-pair-edit' with a '3' numeric prefix.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox (!jumps over the) lazy dog."
    "The quick brown fox (|jumps over the) lazy dog."
    "The quick brown fox (!jumps over the) lazy dog.")
  "C-3 M-( ( RET")

(ipe-test-def-kbd mc-basic-prefix-insert-4 ()
  "Test `ipe-insert-pair-edit' with a '4' numeric prefix.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox (!jumps over the lazy) dog."
    "The quick brown fox (|jumps over the lazy) dog."
    "The quick brown fox (!jumps over the lazy) dog.")
  "C-4 M-( ( RET")

(ipe-test-def-kbd mc-open-backward-1 ()
  "Test `ipe-insert-pair-edit' OPEN backward.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown (fox !jumps) over the lazy dog."
    "The quick brown (fox |jumps) over the lazy dog."
    "The quick brown (fox !jumps) over the lazy dog.")
  "M-( ( C-b RET")

(ipe-test-def-kbd mc-open-backward-2 ()
  "Test `ipe-insert-pair-edit' OPEN backward x2.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick (brown fox !jumps) over the lazy dog."
    "The quick (brown fox |jumps) over the lazy dog."
    "The quick (brown fox !jumps) over the lazy dog.")
  "M-( ( C-b C-b RET")

(ipe-test-def-kbd mc-open-backward-3 ()
  "Test `ipe-insert-pair-edit' OPEN backward x3.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The (quick brown fox !jumps) over the lazy dog."
    "The (quick brown fox |jumps) over the lazy dog."
    "The (quick brown fox !jumps) over the lazy dog.")
  "M-( ( 3 C-b RET")

(ipe-test-def-kbd mc-open-backward-4 ()
  "Test `ipe-insert-pair-edit' OPEN backward x4.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("(The quick brown fox !jumps) over the lazy dog."
    "(The quick brown fox |jumps) over the lazy dog."
    "(The quick brown fox !jumps) over the lazy dog.")
  "M-( ( C-u C-b RET")

(ipe-test-def-kbd mc-open-backward-16 ()
  "Test `ipe-insert-pair-edit' OPEN backward x16.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("(The quick brown fox !jumps)( over the lazy dog."
    "The quick brown fox |jumps)( over the lazy dog."
    "The quick brown fox !jumps) over the lazy dog.")
  "M-( ( C-u C-u C-b RET")

(ipe-test-def-kbd mc-open-backward-beginning-1 ()
  "Test `ipe-insert-pair-edit' OPEN backward at beginning of buffer.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("!The quick brown fox jumps over the lazy dog."
    "|The quick brown fox jumps over the lazy dog."
    "!The quick brown fox jumps over the lazy dog.")
  '("(!The) quick brown fox jumps over the lazy (dog."
    "|The) quick brown fox jumps over the lazy (dog."
    "!The) quick brown fox jumps over the lazy dog.")
  "M-( ( C-b RET")

(ipe-test-def-kbd mc-open-backward-beginning-2 ()
  "Test `ipe-insert-pair-edit' OPEN backward at beginning of buffer.

Using a 'word PAIR with multiple-cursors, single-line (x1)."
  ipe-test-mc-options
  nil
  "|The !quick !brown fox jumps over the lazy dog."
  "(|The)( !quick)( !brown) fox jumps over the lazy dog."
  "M-( ( C-b RET")

(ipe-test-def-kbd mc-open-backward-beginning-3 ()
  "Test `ipe-insert-pair-edit' OPEN backward at beginning of buffer.

Using a 'word PAIR with multiple-cursors, single-line (x2)."
  ipe-test-mc-options
  nil
  "|The !quick !brown fox jumps over the lazy dog."
  "(|The)( !quick)( !brown) fox jumps over the lazy dog."
  "M-( ( C-b C-b RET")

(ipe-test-def-kbd mc-open-backward-beginning-4 ()
  "Test `ipe-insert-pair-edit' OPEN backward at beginning of buffer.

Using a 'word PAIR with multiple-cursors, single-line (x3)."
  ipe-test-mc-options
  nil
  "|The !quick !brown fox jumps over the lazy dog."
  "(|The)( !quick)( !brown) fox jumps over the lazy dog."
  "M-( ( C-b C-b C-b RET")

(ipe-test-def-kbd mc-open-backward-blank ()
  "Test `ipe-insert-pair-edit' OPEN backward with a blank line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "!The quick brown fox jumps over the lazy dog."
    ""
    "|The quick brown fox jumps over the lazy dog."
    ""
    "!The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy (dog."
    ""
    "!The) quick brown fox jumps over the lazy (dog."
    ""
    "|The) quick brown fox jumps over the lazy (dog."
    ""
    "!The) quick brown fox jumps over the lazy dog.")
  "M-( ( C-b RET")

(ipe-test-def-kbd mc-open-backward-blank-2 ()
  "Test `ipe-insert-pair-edit' OPEN backward x2 with a blank line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "!The quick brown fox jumps over the lazy dog."
    ""
    "|The quick brown fox jumps over the lazy dog."
    ""
    "!The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the (lazy dog."
    ""
    "!The) quick brown fox jumps over the (lazy dog."
    ""
    "|The) quick brown fox jumps over the (lazy dog."
    ""
    "!The) quick brown fox jumps over the lazy dog.")
  "M-( ( C-b C-b RET")

(ipe-test-def-kbd mc-close-forward-1 ()
  "Test `ipe-insert-pair-edit' CLOSE forward.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox (!jumps over) the lazy dog."
    "The quick brown fox (|jumps over) the lazy dog."
    "The quick brown fox (!jumps over) the lazy dog.")
  "M-( ( C-f RET")

(ipe-test-def-kbd mc-close-forward-2 ()
  "Test `ipe-insert-pair-edit' CLOSE forward x2.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox (!jumps over the) lazy dog."
    "The quick brown fox (|jumps over the) lazy dog."
    "The quick brown fox (!jumps over the) lazy dog.")
  "M-( ( C-f C-f RET")

(ipe-test-def-kbd mc-close-forward-3 ()
  "Test `ipe-insert-pair-edit' CLOSE forward x3.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox (!jumps over the lazy) dog."
    "The quick brown fox (|jumps over the lazy) dog."
    "The quick brown fox (!jumps over the lazy) dog.")
  "M-( ( 3 C-f RET")

(ipe-test-def-kbd mc-close-forward-4 ()
  "Test `ipe-insert-pair-edit' CLOSE forward x4.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox (!jumps over the lazy dog)."
    "The quick brown fox (|jumps over the lazy dog)."
    "The quick brown fox (!jumps over the lazy dog).")
  "M-( ( C-u C-f RET")

(ipe-test-def-kbd mc-close-forward-16 ()
  "Test `ipe-insert-pair-edit' CLOSE forward x16.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox (!jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown)( fox !jumps over the lazy dog)().")
  "M-( ( C-u C-u C-f RET")

(ipe-test-def-kbd mc-close-forward-end-1 ()
  "Test `ipe-insert-pair-edit' CLOSE forward at end of buffer.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog.!"
    "The quick brown fox jumps over the lazy dog.|"
    "The quick brown fox jumps over the lazy dog.!")
  '("The quick brown fox jumps over the lazy (dog.!"
    "The) quick brown fox jumps over the lazy (dog.|"
    "The) quick brown fox jumps over the lazy (dog).!")
  "M-( ( C-f RET")

(ipe-test-def-kbd mc-close-forward-end-2 ()
  "Test `ipe-insert-pair-edit' CLOSE forward at end of buffer.

Using a 'word PAIR with multiple-cursors, single-line (x1)."
  ipe-test-mc-options
  nil
  "The quick brown fox jumps over !the !lazy |dog."
  "The quick brown fox jumps over (!the !lazy)( |dog)()."
  "M-( ( C-f RET")

(ipe-test-def-kbd mc-close-forward-end-3 ()
  "Test `ipe-insert-pair-edit' CLOSE forward at end of buffer.

Using a 'word PAIR with multiple-cursors, single-line (x2)."
  ipe-test-mc-options
  nil
  "The quick brown fox jumps over !the !lazy |dog."
  "The quick brown fox jumps over (!the !lazy |dog)()()."
  "M-( ( C-f C-f RET")

(ipe-test-def-kbd mc-close-forward-end-4 ()
  "Test `ipe-insert-pair-edit' CLOSE forward at end of buffer.

Using a 'word PAIR with multiple-cursors, single-line (x3)."
  ipe-test-mc-options
  nil
  "The quick brown fox jumps over !the !lazy |dog."
  "The quick brown fox jumps over (!the !lazy |dog)()()."
  "M-( ( C-f C-f C-f RET")

(ipe-test-def-kbd mc-close-forward-blank ()
  "Test `ipe-insert-pair-edit' CLOSE forward with a blank line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy !dog."
    ""
    "The quick brown fox jumps over the lazy |dog."
    ""
    "The quick brown fox jumps over the lazy !dog."
    "");
  '("The quick brown fox jumps over the lazy (!dog."
    ""
    "The) quick brown fox jumps over the lazy (|dog."
    ""
    "The) quick brown fox jumps over the lazy (!dog."
    ")")
  "M-( ( C-f RET")

(ipe-test-def-kbd mc-close-forward-blank-2 ()
  "Test `ipe-insert-pair-edit' CLOSE forward x2 with a blank line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy !dog."
    ""
    "The quick brown fox jumps over the lazy |dog."
    ""
    "The quick brown fox jumps over the lazy !dog."
    "");
  '("The quick brown fox jumps over the lazy (!dog."
    ""
    "The quick) brown fox jumps over the lazy (|dog."
    ""
    "The quick) brown fox jumps over the lazy (!dog."
    ")")
  "M-( ( C-f C-f RET")

(ipe-test-def-kbd mc-open-forward-1 ()
  "Test `ipe-insert-pair-edit' OPEN forward.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox !jumps (over) the lazy dog."
    "The quick brown fox |jumps (over) the lazy dog."
    "The quick brown fox !jumps (over) the lazy dog.")
  "M-( ( M-f RET")

(ipe-test-def-kbd mc-open-forward-2 ()
  "Test `ipe-insert-pair-edit' OPEN forward x2.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox !jumps over (the) lazy dog."
    "The quick brown fox |jumps over (the) lazy dog."
    "The quick brown fox !jumps over (the) lazy dog.")
  "M-( ( M-f M-f RET")

(ipe-test-def-kbd mc-open-forward-3 ()
  "Test `ipe-insert-pair-edit' OPEN forward x3.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox !jumps over the (lazy) dog."
    "The quick brown fox |jumps over the (lazy) dog."
    "The quick brown fox !jumps over the (lazy) dog.")
  "M-( ( 3 M-f RET")

(ipe-test-def-kbd mc-open-forward-4 ()
  "Test `ipe-insert-pair-edit' OPEN forward x4.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox !jumps over the lazy (dog)."
    "The quick brown fox |jumps over the lazy (dog)."
    "The quick brown fox !jumps over the lazy (dog).")
  "M-( ( C-u M-f RET")

(ipe-test-def-kbd mc-open-forward-16 ()
  "Test `ipe-insert-pair-edit' OPEN forward x16.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick (brown) fox !jumps over the lazy dog.()()")
  "M-( ( C-u C-u M-f RET")

(ipe-test-def-kbd mc-open-forward-end-1 ()
  "Test `ipe-insert-pair-edit' OPEN forward at the end of buffer.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog.!"
    "The quick brown fox jumps over the lazy dog.|"
    "The quick brown fox jumps over the lazy dog.!")
  '("The quick brown fox jumps over the lazy dog.!"
    "(The) quick brown fox jumps over the lazy dog.|"
    "(The) quick brown fox jumps over the lazy dog.(!)")
  "M-( ( M-f RET")

(ipe-test-def-kbd mc-open-forward-end-2 ()
  "Test `ipe-insert-pair-edit' OPEN forward at the end of buffer.

Using a 'word PAIR with multiple-cursors, single-line (x1)."
  ipe-test-mc-options
  nil
  "The quick brown fox jumps over !the !lazy |dog."
  "The quick brown fox jumps over !the !(lazy) |(dog).()"
  "M-( ( M-f RET")

(ipe-test-def-kbd mc-open-forward-end-3 ()
  "Test `ipe-insert-pair-edit' OPEN forward at the end of buffer.

Using a 'word PAIR with multiple-cursors, single-line (x2)."
  ipe-test-mc-options
  nil
  "The quick brown fox jumps over !the !lazy |dog."
  "The quick brown fox jumps over !the !lazy |(dog).()()"
  "M-( ( M-f M-f RET")

(ipe-test-def-kbd mc-open-forward-end-4 ()
  "Test `ipe-insert-pair-edit' OPEN forward at the end of buffer.

Using a 'word PAIR with multiple-cursors, single-line (x3)."
  ipe-test-mc-options
  nil
  "The quick brown fox jumps over !the !lazy |dog."
  "The quick brown fox jumps over !the !lazy |dog.()()()"
  "M-( ( M-f M-f M-f RET")

(ipe-test-def-kbd mc-open-forward-blank ()
  "Test `ipe-insert-pair-edit' OPEN forward with a blank line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy !dog."
    ""
    "The quick brown fox jumps over the lazy |dog."
    ""
    "The quick brown fox jumps over the lazy !dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy !dog."
    ""
    "(The) quick brown fox jumps over the lazy |dog."
    ""
    "(The) quick brown fox jumps over the lazy !dog."
    ""
    "(The) quick brown fox jumps over the lazy dog.")
  "M-( ( M-f RET")

(ipe-test-def-kbd mc-open-forward-blank-2 ()
  "Test `ipe-insert-pair-edit' OPEN forward x2 with a blank line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy !dog."
    ""
    "The quick brown fox jumps over the lazy |dog."
    ""
    "The quick brown fox jumps over the lazy !dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy !dog."
    ""
    "The (quick) brown fox jumps over the lazy |dog."
    ""
    "The (quick) brown fox jumps over the lazy !dog."
    ""
    "The (quick) brown fox jumps over the lazy dog.")
  "M-( ( M-f M-f RET")

(ipe-test-def-kbd mc-close-backward-1 ()
  "Test `ipe-insert-pair-edit' CLOSE backward.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown (fox) !jumps over the lazy dog."
    "The quick brown (fox) |jumps over the lazy dog."
    "The quick brown (fox) !jumps over the lazy dog.")
  "M-( ( M-b RET")

(ipe-test-def-kbd mc-close-backward-2 ()
  "Test `ipe-insert-pair-edit' CLOSE backward x2.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick (brown) fox !jumps over the lazy dog."
    "The quick (brown) fox |jumps over the lazy dog."
    "The quick (brown) fox !jumps over the lazy dog.")
  "M-( ( M-b M-b RET")

(ipe-test-def-kbd mc-close-backward-3 ()
  "Test `ipe-insert-pair-edit' CLOSE backward x3.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The (quick) brown fox !jumps over the lazy dog."
    "The (quick) brown fox |jumps over the lazy dog."
    "The (quick) brown fox !jumps over the lazy dog.")
  "M-( ( 3 M-b RET")

(ipe-test-def-kbd mc-close-backward-4 ()
  "Test `ipe-insert-pair-edit' CLOSE backward x4.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("(The) quick brown fox !jumps over the lazy dog."
    "(The) quick brown fox |jumps over the lazy dog."
    "(The) quick brown fox !jumps over the lazy dog.")
  "M-( ( C-u M-b RET")

(ipe-test-def-kbd mc-close-backward-16 ()
  "Test `ipe-insert-pair-edit' CLOSE backward x16.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("()()The quick brown fox !jumps over (the) lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  "M-( ( C-u C-u M-b RET")

(ipe-test-def-kbd mc-close-backward-beginning-1 ()
  "Test `ipe-insert-pair-edit' CLOSE backward at the beginning of buffer.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("!The quick brown fox jumps over the lazy dog."
    "|The quick brown fox jumps over the lazy dog."
    "!The quick brown fox jumps over the lazy dog.")
  '("(!)The quick brown fox jumps over the lazy (dog)."
    "|The quick brown fox jumps over the lazy (dog)."
    "!The quick brown fox jumps over the lazy dog.")
  "M-( ( M-b RET")

(ipe-test-def-kbd mc-close-backward-beginning-2 ()
  "Test `ipe-insert-pair-edit' CLOSE backward at the beginning of buffer.

Using a 'word PAIR with multiple-cursors, single-line (x1)."
  ipe-test-mc-options
  nil
  "|The !quick !brown fox jumps over the lazy dog."
  "(|)(The) !(quick) !brown fox jumps over the lazy dog."
  "M-( ( M-b RET")

(ipe-test-def-kbd mc-close-backward-beginning-3 ()
  "Test `ipe-insert-pair-edit' CLOSE backward at the beginning of buffer.

Using a 'word PAIR with multiple-cursors, single-line (x2)."
  ipe-test-mc-options
  nil
  "|The !quick !brown fox jumps over the lazy dog."
  "(|)()(The) !quick !brown fox jumps over the lazy dog."
  "M-( ( M-b M-b RET")

(ipe-test-def-kbd mc-close-backward-beginning-4 ()
  "Test `ipe-insert-pair-edit' CLOSE backward at the beginning of buffer.

Using a 'word PAIR with multiple-cursors, single-line (x3)."
  ipe-test-mc-options
  nil
  "|The !quick !brown fox jumps over the lazy dog."
  "(|)()()The !quick !brown fox jumps over the lazy dog."
  "M-( ( M-b M-b M-b RET")

(ipe-test-def-kbd mc-close-backward-blank ()
  "Test `ipe-insert-pair-edit' CLOSE backward with a blank line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "!The quick brown fox jumps over the lazy dog."
    ""
    "|The quick brown fox jumps over the lazy dog."
    ""
    "!The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy (dog)."
    ""
    "!The quick brown fox jumps over the lazy (dog)."
    ""
    "|The quick brown fox jumps over the lazy (dog)."
    ""
    "!The quick brown fox jumps over the lazy dog.")
  "M-( ( M-b RET")

(ipe-test-def-kbd mc-close-backward-blank-2 ()
  "Test `ipe-insert-pair-edit' CLOSE backward x2 with a blank line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "!The quick brown fox jumps over the lazy dog."
    ""
    "|The quick brown fox jumps over the lazy dog."
    ""
    "!The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the (lazy) dog."
    ""
    "!The quick brown fox jumps over the (lazy) dog."
    ""
    "|The quick brown fox jumps over the (lazy) dog."
    ""
    "!The quick brown fox jumps over the lazy dog.")
  "M-( ( M-b M-b RET")

(ipe-test-def-kbd mc-open-up-1 ()
  "Test `ipe-insert-pair-edit' OPEN up.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (jumps over the lazy dog."
    "The quick brown fox !jumps)( over the lazy dog."
    "The quick brown fox |jumps)( over the lazy dog."
    "The quick brown fox !jumps) over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( C-p RET")

(ipe-test-def-kbd mc-open-up-2 ()
  "Test `ipe-insert-pair-edit' OPEN up x2.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (jumps over the lazy dog."
    "The quick brown fox !jumps)( over the lazy dog."
    "The quick brown fox |jumps)( over the lazy dog."
    "The quick brown fox !jumps) over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( C-p C-p RET")

(ipe-test-def-kbd mc-open-up-3 ()
  "Test `ipe-insert-pair-edit' OPEN up x3.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox (jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps)( over the lazy dog."
    "The quick brown fox |jumps)( over the lazy dog."
    "The quick brown fox !jumps) over the lazy dog.")
  "M-( ( 3 C-p RET")

(ipe-test-def-kbd mc-open-up-4 ()
  "Test `ipe-insert-pair-edit' OPEN up x4.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox (jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps)( over the lazy dog."
    "The quick brown fox |jumps)( over the lazy dog."
    "The quick brown fox !jumps) over the lazy dog.")
  "M-( ( C-u C-p RET")

(ipe-test-def-kbd mc-open-up-blank ()
  "Test `ipe-insert-pair-edit' OPEN up with a blank line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox !jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "("
    "The quick brown fox !jumps) over the lazy dog."
    "("
    "The quick brown fox |jumps) over the lazy dog."
    "("
    "The quick brown fox !jumps) over the lazy dog.")
  "M-( ( C-p RET")

(ipe-test-def-kbd mc-open-up-blank-2 ()
  "Test `ipe-insert-pair-edit' OPEN up x2 with a blank line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox !jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox !jumps over the lazy dog.")
  '("(The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox !jumps)( over the lazy dog."
    ""
    "The quick brown fox |jumps)( over the lazy dog."
    ""
    "The quick brown fox !jumps) over the lazy dog.")
  "M-( ( C-p C-p RET")

(ipe-test-def-kbd mc-open-up-offset ()
  "Test `ipe-insert-pair-edit' OPEN up with an offset.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown foxes jumps over the lazy dog."
    "The quicker brown fox !jumps over the lazy dog."
    "The quickest brown fox| jumps over the lazier dog."
    "The fast brown fox jum!ps over the lazy dog.")
  '("The quick brown foxes (jumps over the lazy dog."
    "The quicker brown fox !jumps)( over the lazy dog."
    "The quickest brown fox|)( jumps over the lazier dog."
    "The fast brown fox jum!ps) over the lazy dog.")
  "M-( ( C-p RET")

(ipe-test-def-kbd mc-open-up-offset-beginning ()
  "Test `ipe-insert-pair-edit' OPEN up offset beginning of from line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "  !The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "  |The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "  !The quick brown fox jumps over the lazy dog.")
  '("(The quick brown fox jumps over the lazy dog."
    "  !The) quick brown fox jumps over the lazy dog."
    "(The quick brown fox jumps over the lazy dog."
    "  |The) quick brown fox jumps over the lazy dog."
    "(The quick brown fox jumps over the lazy dog."
    "  !The) quick brown fox jumps over the lazy dog.")
  "M-( ( C-p RET")

(ipe-test-def-kbd mc-open-up-offset-beginning-2 ()
  "Test `ipe-insert-pair-edit' OPEN up offset beginning of to line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("  The quick brown fox jumps over the lazy dog."
    "!The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog."
    "|The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog."
    "!The quick brown fox jumps over the lazy dog.")
  '("  (The quick brown fox jumps over the lazy dog."
    "!The) quick brown fox jumps over the lazy dog."
    "  (The quick brown fox jumps over the lazy dog."
    "|The) quick brown fox jumps over the lazy dog."
    "  (The quick brown fox jumps over the lazy dog."
    "!The) quick brown fox jumps over the lazy dog.")
  "M-( ( C-p RET")

(ipe-test-def-kbd mc-close-down-1 ()
  "Test `ipe-insert-pair-edit' CLOSE down.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox (!jumps over the lazy dog."
    "The quick brown fox |jumps)( over the lazy dog."
    "The quick brown fox !jumps)( over the lazy dog."
    "The quick brown fox jumps) over the lazy dog.")
  "M-( ( C-n RET")

(ipe-test-def-kbd mc-close-down-2 ()
  "Test `ipe-insert-pair-edit' CLOSE down x2.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox (!jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps)( over the lazy dog."
    "The quick brown fox jumps)() over the lazy dog.")
  "M-( ( C-n C-n RET")

(ipe-test-def-kbd mc-close-down-3 ()
  "Test `ipe-insert-pair-edit' CLOSE down x3.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox (!jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox jumps)()() over the lazy dog.")
  "M-( ( 3 C-n RET")

(ipe-test-def-kbd mc-close-down-4 ()
  "Test `ipe-insert-pair-edit' CLOSE down x4.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox (!jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox jumps)()() over the lazy dog.")
  "M-( ( C-u C-n RET")

(ipe-test-def-kbd mc-close-down-blank ()
  "Test `ipe-insert-pair-edit' CLOSE down with a blank line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox !jumps over the lazy dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (!jumps over the lazy dog."
    ")"
    "The quick brown fox (|jumps over the lazy dog."
    ")"
    "The quick brown fox (!jumps over the lazy dog."
    ")"
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( C-n RET")

(ipe-test-def-kbd mc-close-down-blank-2 ()
  "Test `ipe-insert-pair-edit' CLOSE down x2 with a blank line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox !jumps over the lazy dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (!jumps over the lazy dog."
    ""
    "The) quick brown fox (|jumps over the lazy dog."
    ""
    "The) quick brown fox (!jumps over the lazy dog."
    ""
    "The) quick brown fox jumps over the lazy dog.")
  "M-( ( C-n C-n RET")

(ipe-test-def-kbd mc-close-down-offset ()
  "Test `ipe-insert-pair-edit' CLOSE down with an offset.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown foxes jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown foxes |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown foxes jumps over the lazy dog.")
  '("The quick brown foxes jumps over the lazy dog."
    "The quick brown fox (!jumps over the lazy dog."
    "The quick brown foxes |jumps)( over the lazy dog."
    "The quick brown fox !jumps over)( the lazy dog."
    "The quick brown foxes jumps) over the lazy dog.")
  "M-( ( C-n RET")

(ipe-test-def-kbd mc-close-down-offset-beginning ()
  "Test `ipe-insert-pair-edit' CLOSE down offset beginning of to line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("!The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog."
    "|The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog."
    "!The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog.")
  '("(!The quick brown fox jumps over the lazy dog."
    "  The) quick brown fox jumps over the lazy dog."
    "(|The quick brown fox jumps over the lazy dog."
    "  The) quick brown fox jumps over the lazy dog."
    "(!The quick brown fox jumps over the lazy dog."
    "  The) quick brown fox jumps over the lazy dog.")
  "M-( ( C-n RET")

(ipe-test-def-kbd mc-close-down-offset-beginning-2 ()
  "Test `ipe-insert-pair-edit' CLOSE down offset beginning of from line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("!  The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "|  The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "!  The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("!  (The quick brown fox jumps over the lazy dog."
    "The quick) brown fox jumps over the lazy dog."
    "|  (The quick brown fox jumps over the lazy dog."
    "The quick) brown fox jumps over the lazy dog."
    "!  (The quick brown fox jumps over the lazy dog."
    "The quick) brown fox jumps over the lazy dog.")
  "M-( ( C-n RET")

(ipe-test-def-kbd mc-open-down-1 ()
  "Test `ipe-insert-pair-edit' OPEN down.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |(jumps) over the lazy dog."
    "The quick brown fox !(jumps) over the lazy dog."
    "The quick brown fox (jumps) over the lazy dog.")
  "M-( ( M-n RET")

(ipe-test-def-kbd mc-open-down-2 ()
  "Test `ipe-insert-pair-edit' OPEN down x2.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !(jumps) over the lazy dog."
    "The quick brown fox (jumps)() over the lazy dog.")
  "M-( ( M-n M-n RET")

(ipe-test-def-kbd mc-open-down-3 ()
  "Test `ipe-insert-pair-edit' OPEN down x3.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox (jumps)()() over the lazy dog.")
  "M-( ( 3 M-n RET")

(ipe-test-def-kbd mc-open-down-4 ()
  "Test `ipe-insert-pair-edit' OPEN down x4.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox (jumps)()() over the lazy dog.")
  "M-( ( C-u M-n RET")

(ipe-test-def-kbd mc-open-down-blank ()
  "Test `ipe-insert-pair-edit' OPEN down with a blank line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox !jumps over the lazy dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox !jumps over the lazy dog."
    "()"
    "The quick brown fox |jumps over the lazy dog."
    "()"
    "The quick brown fox !jumps over the lazy dog."
    "()"
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( M-n RET")

(ipe-test-def-kbd mc-open-down-blank-2 ()
  "Test `ipe-insert-pair-edit' OPEN down x2 with a blank line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox !jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox !jumps over the lazy dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox !jumps over the lazy dog."
    ""
    "(The) quick brown fox |jumps over the lazy dog."
    ""
    "(The) quick brown fox !jumps over the lazy dog."
    ""
    "(The) quick brown fox jumps over the lazy dog.")
  "M-( ( M-n M-n RET")

(ipe-test-def-kbd mc-open-down-offset ()
  "Test `ipe-insert-pair-edit' OPEN down with an offset.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown foxes jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown foxes |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown foxes jumps over the lazy dog.")
  '("The quick brown foxes jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown foxes |(jumps) over the lazy dog."
    "The quick brown fox !jumps (over) the lazy dog."
    "The quick brown foxes (jumps) over the lazy dog.")
  "M-( ( M-n RET")

(ipe-test-def-kbd mc-open-down-offset-beginning ()
  "Test `ipe-insert-pair-edit' OPEN down offset beginning of to line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("!The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog."
    "|The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog."
    "!The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog.")
  '("!The quick brown fox jumps over the lazy dog."
    "  (The) quick brown fox jumps over the lazy dog."
    "|The quick brown fox jumps over the lazy dog."
    "  (The) quick brown fox jumps over the lazy dog."
    "!The quick brown fox jumps over the lazy dog."
    "  (The) quick brown fox jumps over the lazy dog.")
  "M-( ( M-n RET")

(ipe-test-def-kbd mc-open-down-offset-beginning-2 ()
  "Test `ipe-insert-pair-edit' OPEN down offset beginning of from line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("!  The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "|  The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "!  The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("!  The quick brown fox jumps over the lazy dog."
    "The (quick) brown fox jumps over the lazy dog."
    "|  The quick brown fox jumps over the lazy dog."
    "The (quick) brown fox jumps over the lazy dog."
    "!  The quick brown fox jumps over the lazy dog."
    "The (quick) brown fox jumps over the lazy dog.")
  "M-( ( M-n RET")

(ipe-test-def-kbd mc-close-up-1 ()
  "Test `ipe-insert-pair-edit' CLOSE up.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (jumps) over the lazy dog."
    "The quick brown fox !(jumps) over the lazy dog."
    "The quick brown fox |(jumps) over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( M-p RET")

(ipe-test-def-kbd mc-close-up-2 ()
  "Test `ipe-insert-pair-edit' CLOSE up x2.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (jumps)() over the lazy dog."
    "The quick brown fox !(jumps) over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( M-p M-p RET")

(ipe-test-def-kbd mc-close-up-3 ()
  "Test `ipe-insert-pair-edit' CLOSE up x3.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (jumps)()() over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( 3 M-p RET")

(ipe-test-def-kbd mc-close-up-4 ()
  "Test `ipe-insert-pair-edit' CLOSE up x4.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (jumps)()() over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( C-u M-p RET")

(ipe-test-def-kbd mc-close-up-blank ()
  "Test `ipe-insert-pair-edit' CLOSE up with a blank line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox !jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "()"
    "The quick brown fox !jumps over the lazy dog."
    "()"
    "The quick brown fox |jumps over the lazy dog."
    "()"
    "The quick brown fox !jumps over the lazy dog.")
  "M-( ( M-p RET")

(ipe-test-def-kbd mc-close-up-blank-2 ()
  "Test `ipe-insert-pair-edit' CLOSE up x2 with a blank line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox !jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox !jumps over the lazy dog.")
  '("(The) quick brown fox jumps over the lazy dog."
    ""
    "(The) quick brown fox !jumps over the lazy dog."
    ""
    "(The) quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox !jumps over the lazy dog.")
  "M-( ( M-p M-p RET")

(ipe-test-def-kbd mc-close-up-offset ()
  "Test `ipe-insert-pair-edit' CLOSE up with an offset.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown foxes jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown foxes |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown foxes jumps over the lazy dog.")
  '("The quick brown (foxes) jumps over the lazy dog."
    "The quick brown fox !(jumps) over the lazy dog."
    "The quick brown (foxes) |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown foxes jumps over the lazy dog.")
  "M-( ( M-p RET")

(ipe-test-def-kbd mc-close-up-offset-beginning ()
  "Test `ipe-insert-pair-edit' CLOSE up offset beginning of from line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "!   The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "|   The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "!  The quick brown fox jumps over the lazy dog.")
  '("(The) quick brown fox jumps over the lazy dog."
    "!   The quick brown fox jumps over the lazy dog."
    "(The) quick brown fox jumps over the lazy dog."
    "|   The quick brown fox jumps over the lazy dog."
    "(The) quick brown fox jumps over the lazy dog."
    "!  The quick brown fox jumps over the lazy dog.")
  "M-( ( M-p RET")

(ipe-test-def-kbd mc-close-up-offset-beginning-2 ()
  "Test `ipe-insert-pair-edit' CLOSE up offset beginning of to line.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("  The quick brown fox jumps over the lazy dog."
    "!The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog."
    "|The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog."
    "!The quick brown fox jumps over the lazy dog.")
  '("  (The) quick brown fox jumps over the lazy dog."
    "!The quick brown fox jumps over the lazy dog."
    "  (The) quick brown fox jumps over the lazy dog."
    "|The quick brown fox jumps over the lazy dog."
    "  (The) quick brown fox jumps over the lazy dog."
    "!The quick brown fox jumps over the lazy dog.")
  "M-( ( M-p RET")

;; TODO: mc + ert bug?
;; (ipe-test-def-kbd mc-edit-replace-1 ()
;;   "Test `ipe-edit--change-pair' replace using a small tag to large tag.
;;
;; Using a 'word PAIR with multiple-cursors.
;;
;; Short -> Long."
;;   ipe-test-mc-options
;;   nil
;;   '("The quick brown fox jumps over the lazy dog."
;;     "The quick brown fox (!jumps) over the lazy dog."
;;     "The quick brown fox (|jumps) over the lazy dog."
;;     "The quick brown fox (!jumps) over the lazy dog."
;;     "The quick brown fox jumps over the lazy dog.")
;;   '("The quick brown fox jumps over the lazy dog."
;;     "The quick brown fox <really-large-open-tag>!jumps<really-large-close-tag> over the lazy dog."
;;     "The quick brown fox <really-large-open-tag>|jumps<really-large-close-tag> over the lazy dog."
;;     "The quick brown fox <really-large-open-tag>!jumps<really-large-close-tag> over the lazy dog."
;;     "The quick brown fox jumps over the lazy dog.")
;;   "C-u M-( ( ( L RET")

(ipe-test-def-kbd mc-edit-update-region ()
  "Test `ipe-insert-pair-edit-update' function with a region.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  "@The (quick) brown (fox) jumps (over) the (lazy) dog.|"
  "The |<start>quick<end> brown !<start>fox<end> jumps !<start>over<end> the !<start>lazy<end> dog."
  "C-u M-( ( ( S RET")

(ipe-test-def-kbd mc-edit-replace-region ()
  "Test `ipe-insert-pair-edit-replace' function with a region.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  "@The (quick) brown (fox) jumps (over) the (lazy) dog.|"
  "The |<start>quick<end> brown !<start>fox<end> jumps !<start>over<end> the !<start>lazy<end> dog."
  "C-u C-u C-u M-( ( S RET")

(ipe-test-def-kbd mc-auto-insert ()
  "Test `ipe-insert-pair-edit-delete' with multiple-cursors.

Auto-insert PAIR."
  ipe-test-mc-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox (|jumps) over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog.")
  "M-( i i i")

(ipe-test-def-kbd mc-delete-large-tags ()
  "Test `ipe-insert-pair-edit-delete' with multiple-cursors.

With 'large' OPEN and CLOSE strings."
  ipe-test-mc-options
  nil
  "@<really-large-open-tag>1<really-large-close-tag><really-large-open-tag>2<really-large-close-tag><really-large-open-tag>3<really-large-close-tag>|"
  "123|"
  "C-u C-u M-( L")

;; Missing
(setq ipe-test-mc-missing-options
      '((ipe-move-point-on-insert   nil)
	(ipe-prefix-moves-close-p   t)
	(ipe-edit--movement-keysets '(modifiers))
	(ipe-pairs
	 '(
	   ("1" "<"   ""    (:movement char))
	   ("2" ""   ">"    (:movement char))
	   ("3" ""    ""    (:movement char))
	   ("4" "{{{" ""    (:movement word))
	   ("5" ""    "}}}" (:movement word))
	   ("6" ""    ""    (:movement word))
	   ("7" "// " ""    (:movement line :infix "// "))))
	(ipe-mode-pairs nil)))

;; TODO: mc bug?
;; (ipe-test-def-kbd mc-missing-open ()
;;   "Test `ipe-insert-pair-edit-delete' with a missing OPEN.
;;
;; Using a 'word PAIR and multiple-cursors."
;;   ipe-test-missing-options
;;   nil
;;   "The quick brown fox !!!!|jumps over the lazy dog."
;;   "The quick brown fox <!<!<!<|<jumps over the lazy dog."
;;   "M-( 1 RET")

;; TODO: mc bug?
;; (ipe-test-def-kbd mc-missing-close ()
;;   "Test `ipe-insert-pair-edit-delete' with a missing CLOSE.
;;
;; Using a 'word PAIR and multiple-cursors."
;;   ipe-test-missing-options
;;   nil
;;   "The quick brown fox !!!!|jumps over the lazy dog."
;;   "The quick brown fox !>!>!>!>|>jumps over the lazy dog."
;;   "M-( 2 RET")

;; TODO: mc bug?
;; (ipe-test-def-kbd mc-edit-replace-2 ()
;;   "Test `ipe-edit--change-pair' replace using a large to to small tag.
;;
;; Using a 'word PAIR with multiple-cursors.
;;
;; Long -> Short."
;;   ipe-test-mc-missing-options
;;   nil
;;   '("The quick brown fox jumps over the lazy dog."
;;     "The quick brown fox <really-large-open-tag>!jumps<really-large-close-tag> over the lazy dog."
;;     "The quick brown fox <really-large-open-tag>|jumps<really-large-close-tag> over the lazy dog."
;;     "The quick brown fox <really-large-open-tag>!jumps<really-large-close-tag> over the lazy dog."
;;     "The quick brown fox jumps over the lazy dog.")
;;   '("The quick brown fox jumps over the lazy dog."
;;     "The quick brown fox (!jumps) over the lazy dog."
;;     "The quick brown fox (|jumps) over the lazy dog."
;;     "The quick brown fox (!jumps) over the lazy dog."
;;     "The quick brown fox jumps over the lazy dog.")
;;   "C-u M-( L ( ( RET")

;; TODO: mc + ert bug?
;; (ipe-test-def-kbd mc-replace-1 ()
;;   "Test `ipe-insert-pair-edit' replace using a small tag to large tag.
;;
;; Using a 'word PAIR with multiple-cursors.
;;
;; Short -> Long."
;;   ipe-test-mc-options
;;   nil
;;   '("The quick brown fox jumps over the lazy dog."
;;     "The quick brown fox (!jumps) over the lazy dog."
;;     "The quick brown fox (|jumps) over the lazy dog."
;;     "The quick brown fox (!jumps) over the lazy dog."
;;     "The quick brown fox jumps over the lazy dog.")
;;   '("The quick brown fox jumps over the lazy dog."
;;     "The quick brown fox <really-large-open-tag>!jumps<really-large-close-tag> over the lazy dog."
;;     "The quick brown fox <really-large-open-tag>|jumps<really-large-close-tag> over the lazy dog."
;;     "The quick brown fox <really-large-open-tag>!jumps<really-large-close-tag> over the lazy dog."
;;     "The quick brown fox jumps over the lazy dog.")
;;   "C-u C-u C-u M-( ( L RET")

;; TODO: mc bug?
;; (ipe-test-def-kbd mc-replace-2 ()
;;   "Test `ipe-insert-pair-edit' replace using a large to to small tag.
;;
;; Using a 'word PAIR with multiple-cursors.
;;
;; Long -> Short."
;;   ipe-test-mc-missing-options
;;   nil
;;   '("The quick brown fox jumps over the lazy dog."
;;     "The quick brown fox <really-large-open-tag>!jumps<really-large-close-tag> over the lazy dog."
;;     "The quick brown fox <really-large-open-tag>|jumps<really-large-close-tag> over the lazy dog."
;;     "The quick brown fox <really-large-open-tag>!jumps<really-large-close-tag> over the lazy dog."
;;     "The quick brown fox jumps over the lazy dog.")
;;   '("The quick brown fox jumps over the lazy dog."
;;     "The quick brown fox (!jumps) over the lazy dog."
;;     "The quick brown fox (|jumps) over the lazy dog."
;;     "The quick brown fox (!jumps) over the lazy dog."
;;     "The quick brown fox jumps over the lazy dog.")
;;   "C-u C-u C-u M-( L ( RET")

(ipe-test-def-kbd mc-missing-open-insert ()
  "Test `ipe-insert-pair-edit' with only an OPEN string.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-missing-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox {{{|jumps over the lazy dog."
    "The quick brown fox {{{!jumps over the lazy dog."
    "The quick brown fox {{{!jumps over the lazy dog.")
  "M-( 4 RET")

(ipe-test-def-kbd mc-missing-close-insert ()
  "Test `ipe-insert-pair-edit' with only a CLOSE string.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-missing-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox |jumps}}} over the lazy dog."
    "The quick brown fox !jumps}}} over the lazy dog."
    "The quick brown fox !jumps}}} over the lazy dog.")
  "M-( 5 RET")

(ipe-test-def-kbd mc-missing-open-to-close-replace ()
  "Test `ipe-insert-pair-edit-replace' with a missing OPEN with active region.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-missing-options
  nil
  '("@The quick brown fox {{{jumps over the lazy dog."
    "The quick brown fox {{{jumps over the lazy dog."
    "The quick brown fox {{{jumps over the lazy dog.|")
  '("The quick brown fox |jumps}}} over the lazy dog."
    "The quick brown fox !jumps}}} over the lazy dog."
    "The quick brown fox !jumps}}} over the lazy dog.")
  "C-u C-u C-u M-( 4 5 RET")

;;; FIXME - Cursors should be after new OPEN.
;;(ipe-test-def-kbd mc-missing-close-to-open-replace ()
;;  "Test `ipe-insert-pair-edit-replace' with a missing CLOSE with active region.
;;
;;Using a 'word PAIR with multiple-cursors."
;;  ipe-test-mc-missing-options
;;  nil
;;  '("@The quick brown fox jumps}}} over the lazy dog."
;;    "The quick brown fox jumps}}} over the lazy dog."
;;    "The quick brown fox jumps}}} over the lazy dog.|")
;;  '("{{{|The quick brown fox jumps{{{! over the lazy dog."
;;    "The quick brown fox jumps{{{! over the lazy dog."
;;    "The quick brown fox jumps over the lazy dog.")
;;  "C-u C-u C-u M-( 5 4 RET")

(ipe-test-def-kbd mc-missing-both-multiple ()
  "Test `ipe-insert-pair-edit-delete' with multiple-cursors.

Using a 'word PAIR with no OPEN nor CLOSE string."
  ipe-test-mc-missing-options
  nil
  "The quick brown fox !!|jumps over the lazy dog."
  "The quick brown fox !!|jumps over the lazy dog."
  "M-( 3 RET")

;; *********************************************************************
;; ipe-edit--* functions.
;; *********************************************************************

(ipe-test-def-kbd mc-contents-forward-1 ()
  "Test `ipe-edit--add-contents-forward' with multiple-cursors.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (|jumps) over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( S S S RET")

(ipe-test-def-kbd mc-contents-backward-1 ()
  "Test `ipe-edit--add-contents-backward' with multiple-cursors.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox (|jumps) over the lazy dog.")
  "M-( ( R R R RET")

(ipe-test-def-kbd mc-contents-upcase ()
  "Test `ipe-edit--contents-upcase' with multiple-cursors."
  ipe-test-mc-options
  nil
  "The (!quick) brown (!fox) jumps (!over) the (|lazy) dog."
  "The (!QUICK) brown (!FOX) jumps (!OVER) the (|LAZY) dog."
  "C-u M-( ( M-u RET")

(ipe-test-def-kbd mc-contents-capitalize ()
  "Test `ipe-edit--contents-capitalize' with multiple-cursors."
  ipe-test-mc-options
  nil
  "The (!quick) brown (!fox) jumps (!over) the (|lazy) dog."
  "The (!Quick) brown (!Fox) jumps (!Over) the (|Lazy) dog."
  "C-u M-( ( M-c RET")

(ipe-test-def-kbd mc-contents-downcase ()
  "Test `ipe-edit--contents-downcase' with multiple-cursors."
  ipe-test-mc-options
  nil
  "THE (!QUICK) BROWN (!FOX) JUMPS (!OVER) THE (|LAZY) DOG."
  "THE (!quick) BROWN (!fox) JUMPS (!over) THE (|lazy) DOG."
  "C-u M-( ( M-l RET")

(ipe-test-def-kbd mc-contents-replace-1 ()
  "Test `ipe-edit--contents-replace' with multiple-cursors."
  ipe-test-mc-options
  nil
  "The (!quick) brown (!fox) jumps (!over) the (|lazy) dog."
  "The (!xxx) brown (!xxx) jumps (!xxx) the (|xxx) dog."
  "C-u M-( ( % x C-a C-k xxx RET RET")

(ipe-test-def-kbd mc-contents-replace-2 ()
  "Test `ipe-edit--contents-replace' with multiple-cursors.

With Prefix ARG."
  ipe-test-mc-options
  nil
  "The (!quick) brown (!fox) jumps (!over) the (|lazy) dog."
  "The (!www) brown (!xxx) jumps (!yyy) the (|zzz) dog."
  "C-u M-( ( C-u % x C-a C-k www RET C-a C-k xxx RET C-a C-k yyy RET C-a C-k zzz RET RET")

(ipe-test-def-kbd mc-contents-kill ()
  "Test `ipe-edit--contents-kill' with multiple-cursors."
  ipe-test-mc-options
  nil
  "The (!quick) brown (!fox) jumps (!over) the (|lazy) dog."
  "The (!) brown (!) jumps (!) the (|) dog."
  "C-u M-( ( C-k RET")

(ipe-test-def-kbd mc-contents-kill-to-empty ()
  "Test `ipe-edit--contents-kill' with multiple-cursors.

Reduce to empty buffer."
  ipe-test-mc-options
  nil
  "@(The)(quick)(brown)|"
  "|()()()"
  "C-u M-( ( C-k")

(ipe-test-def-kbd mc-insert-first-1 ()
  "Test `ipe-edit--insert-first-pair' with multiple-cursors.

Using a 'word PAIR with multiple-cursors. Delete first two."
  ipe-test-mc-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox |(jumps) over the lazy dog."
    "The quick brown fox !(jumps) over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  "M-( ( M-j M-j DEL")

(ipe-test-def-kbd mc-insert-first-2 ()
  "Test `ipe-edit--insert-first-pair' with multiple-cursors.

Using a 'word PAIR with multiple-cursors. Delete 2nd and 4th"
  ipe-test-mc-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !(jumps) over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !(jumps) over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  "M-( ( 4 M-j 2 M-j DEL")

(ipe-test-def-kbd mc-insert-first-3 ()
  "Test `ipe-edit--insert-first-pair' with multiple-cursors.

Using a 'word PAIR with multiple-cursors. Delete 3rd and 1st."
  ipe-test-mc-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox |(jumps) over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !(jumps) over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  "M-( ( 3 M-j 1 M-j DEL")

(ipe-test-def-kbd mc-insert-all-1 ()
  "Test `ipe-edit--insert-all-pairs' with multiple-cursors.

Using a 'word PAIR with multiple-cursors. Primary cursor is first."
  ipe-test-mc-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox (|jumps) over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog.")
  "M-( ( RET")

(ipe-test-def-kbd mc-insert-last-1 ()
  "Test `ipe-edit--insert-last-pair' with multiple-cursors.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !(jumps) over the lazy dog."
    "The quick brown fox !(jumps) over the lazy dog.")
  "M-( ( C-j C-j DEL")

(ipe-test-def-kbd mc-insert-last-2 ()
  "Test `ipe-edit--insert-last-pair' with multiple-cursors.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !(jumps) over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !(jumps) over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  "M-( ( 4 C-j 2 C-j DEL")

(ipe-test-def-kbd mc-insert-last-3 ()
  "Test `ipe-edit--insert-last-pair' with multiple-cursors.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !(jumps) over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !(jumps) over the lazy dog.")
  "M-( ( 3 C-j 1 C-j DEL")

(ipe-test-def-kbd mc-delete-first-1 ()
  "Test `ipe-edit--delete-first-pair' with multiple-cursors.

Using a 'word PAIR with multiple-cursors. Delete first two."
  ipe-test-mc-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog.")
  "M-( ( M-d M-d RET")

(ipe-test-def-kbd mc-delete-first-2 ()
  "Test `ipe-edit--delete-first-pair' with multiple-cursors.

Using a 'word PAIR with multiple-cursors. Delete 2nd and 4th"
  ipe-test-mc-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox (|jumps) over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog.")
  "M-( ( 4 M-d 2 M-d RET")

(ipe-test-def-kbd mc-delete-first-3 ()
  "Test `ipe-edit--delete-first-pair' with multiple-cursors.

Using a 'word PAIR with multiple-cursors. Delete 3rd and 1st."
  ipe-test-mc-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog.")
  "M-( ( 3 M-d 1 M-d RET")

(ipe-test-def-kbd mc-delete-all-1 ()
  "Test `ipe-edit--delete-all-pairs' with multiple-cursors.

Using a 'word PAIR with multiple-cursors. Primary cursor is first."
  ipe-test-mc-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  "M-( ( C-d")

(ipe-test-def-kbd mc-delete-all-2 ()
  "Test `ipe-edit--delete-all-pairs' with region.

Using a 'word PAIR.  Region is middle three lines."
  ipe-test-mc-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "|The quick brown fox (jumps) over the lazy dog."
    "The quick brown fox (jumps) over the lazy dog."
    "The quick brown fox (jumps) over the lazy dog.@"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ( DEL")

(ipe-test-def-kbd mc-delete-last-1 ()
  "Test `ipe-edit--delete-last-pair' with multiple-cursors.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox (|jumps) over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( C-d C-d RET")

(ipe-test-def-kbd mc-delete-last-2 ()
  "Test `ipe-edit--delete-last-pair' with multiple-cursors.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox (|jumps) over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog.")
  "M-( ( 4 C-d 2 C-d RET")

(ipe-test-def-kbd mc-delete-last-3 ()
  "Test `ipe-edit--delete-last-pair' with multiple-cursors.

Using a 'word PAIR with multiple-cursors."
  ipe-test-mc-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog."
    "The quick brown fox !jumps over the lazy dog.")
  '("The quick brown fox (|jumps) over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( 3 C-d 1 C-d RET")

(provide 'ipe-test-mc)

;;; ipe-test-mc.el ends here
