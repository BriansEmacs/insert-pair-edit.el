;;; ipe-test-adjust.el --- Insert Pair Edit - 'Next/Previous' Tests -*- lexical-binding: t; -*-
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
;; The tests within this file are used to test 'searching' for matches
;; to the current 'Insert Pair Edit' (ipe) PAIR within the current
;; buffer during `ipe-edit-mode'.  This is done via the
;; 'Next/Previous' 'Insert Pair Edit' `ipe-edit-mode' commands:
;;
;;   `ipe-edit--update-next-pair'
;;   `ipe-edit--update-previous-pair'
;;   `ipe-edit--update-next-open'
;;   `ipe-edit--update-previous-open'
;;   `ipe-edit--update-next-close'
;;   `ipe-edit--update-previous-close'
;;   `ipe-edit--update-next-contents'
;;   `ipe-edit--update-previous-contents'

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'ipe-test)

(defvar ipe-test-adjust-options
  '((ipe-move-point-on-insert   nil)
    (ipe-prefix-moves-close-p   t)
    (ipe-edit--movement-keysets '(modifiers))
    (ipe-update-forward-first-p nil)
    (ipe-pairs
     '(("("  "(((((" ")))))")
       ("<"  "<"     ">")
       ("'"  "'"     "'"     (:escapes (("'" "\\'"))))
       ("{"  "{"     "}"     (:movement line))
       ("-"  "<-- "  " -->"  (:movement line :infix " -- "))
       (";"  ";"     ";"     (:movement line :infix ";"))
       ("1"  ";"     ""      (:movement line :infix ";"))
       ("2"  ""      ";"     (:movement line :infix ";"))
       ("3"  ""      ""      (:movement line :infix ";"))))
    (ipe-mode-pairs        nil))
  "Options used by `ipe-test-def-kbd' for `ipe-test--adjust'.")

(ipe-test-def-kbd adjust-next-pair-1 ()
  "Test `ipe-edit--update-next-pair'."
  ipe-test-adjust-options
  nil
  "The (((((quick))))) brown (((((|fox))))) jumps (((((over))))) the (((((lazy))))) dog."
  "The (((((quick))))) brown (((((|fox))))) jumps <over> the (((((lazy))))) dog."
  "C-u M-( ( C-s ( < RET")

(ipe-test-def-kbd adjust-next-pair-2 ()
  "Test `ipe-edit--update-next-pair'.

With no next PAIR."
  ipe-test-adjust-options
  nil
  "The (((((quick))))) brown (((((fox))))) jumps (((((over))))) the (((((|lazy))))) dog."
  "The (((((quick))))) brown (((((fox))))) jumps (((((over))))) the <|lazy> dog."
  "C-u M-( ( C-s ( < RET")

(ipe-test-def-kbd adjust-next-pair-3 ()
  "Test `ipe-edit--update-next-pair'.

With minimal PAIRs."
  ipe-test-adjust-options
  nil
  "|''''"
  "|''((((()))))"
  "C-u M-( ' C-s ( ( RET")

(ipe-test-def-kbd adjust-next-pair-4 ()
  "Test `ipe-edit--update-next-pair'.

With nested PAIRs."
  ipe-test-adjust-options
  nil
  "The (((((|quick (((((brown))))) fox))))) jumps (((((over (((((the))))) lazy))))) dog."
  "The (((((|quick <brown> fox))))) jumps (((((over (((((the))))) lazy))))) dog."
  "C-u M-( ( C-s ( < RET")

(ipe-test-def-kbd adjust-next-pair-5 ()
  "Test `ipe-edit--update-next-pair'.

With PAIR at start of buffer."
  ipe-test-adjust-options
  nil
  "(((((|The))))) (((((quick))))) brown (((((fox))))) jumps (((((over))))) the (((((lazy))))) dog."
  "(((((|The))))) <quick> brown (((((fox))))) jumps (((((over))))) the (((((lazy))))) dog."
  "C-u M-( ( C-s ( < RET")

(ipe-test-def-kbd adjust-next-pair-6 ()
  "Test `ipe-edit--update-next-pair'.

With PAIR at end of buffer."
  ipe-test-adjust-options
  nil
  "The (((((quick))))) brown (((((fox))))) jumps (((((over))))) the (((((lazy))))) dog.(((((|)))))"
  "The (((((quick))))) brown (((((fox))))) jumps (((((over))))) the (((((lazy))))) <dog>.|"
  "C-u M-( ( C-s ( < RET")

(ipe-test-def-kbd adjust-next-pair-7 ()
  "Test `ipe-edit--update-next-pair'.

With :escapes."
  ipe-test-adjust-options
  nil
  "The 'quick' brown '|fox' jumps 'over \\'the\\' lazy' dog."
  "The 'quick' brown '|fox' jumps <over 'the' lazy> dog."
  "C-u M-( ' C-s ( < RET")

(ipe-test-def-kbd adjust-next-pair-8 ()
  "Test `ipe-edit--update-next-pair'.

With numeric prefix."
  ipe-test-adjust-options
  nil
  "(((((|The))))) quick (((((brown))))) fox (((((jumps))))) over (((((the))))) lazy (((((dog)))))."
  "(((((|The))))) quick (((((brown))))) fox (((((jumps))))) over <the> lazy (((((dog)))))."
  "C-u M-( ( M-3 C-s ( < RET")

(ipe-test-def-kbd adjust-next-pair-9 ()
  "Test `ipe-edit--update-next-pair'.

With universal arg prefix."
  ipe-test-adjust-options
  nil
  "(((((|The))))) quick (((((brown))))) fox (((((jumps))))) over (((((the))))) lazy (((((dog)))))."
  "(((((|The))))) quick (((((brown))))) fox (((((jumps))))) over (((((the))))) lazy <dog>."
  "C-u M-( ( C-u C-s ( < RET")

(ipe-test-def-kbd adjust-next-pair-10 ()
  "Test `ipe-edit--update-next-pair'.

With nested PAIRS (inner)."
  ipe-test-adjust-options
  nil
  "The quick (((((brown (((((|fox))))) jumps))))) over (((((the))))) lazy dog."
  "The quick (((((brown (((((|fox))))) jumps))))) over <the> lazy dog."
  "C-u M-( ( C-s ( < RET")

(ipe-test-def-kbd adjust-next-pair-11 ()
  "Test `ipe-edit--update-next-pair'.

With nested PAIRS (outer)."
  ipe-test-adjust-options
  nil
  "The quick (((((|brown (((((fox))))) jumps))))) over (((((the))))) lazy dog."
  "The quick (((((|brown <fox> jumps))))) over (((((the))))) lazy dog."
  "C-u M-( ( C-s ( < RET")

(ipe-test-def-kbd adjust-next-pair-12 ()
  "Test `ipe-edit--update-next-pair'.

To nested PAIR."
  ipe-test-adjust-options
  nil
  "The (((((quick (((((|brown))))) fox))))) (((((jumps (((((over))))) the))))) lazy dog."
  "The (((((quick (((((|brown))))) fox))))) <jumps (((((over))))) the> lazy dog."
  "C-u M-( ( C-s ( < RET")

(ipe-test-def-kbd adjust-next-pair-13 ()
  "Test `ipe-edit--update-next-pair'.

To nested PAIR (x2)."
  ipe-test-adjust-options
  nil
  "The (((((quick (((((|brown))))) fox))))) (((((jumps (((((over))))) the))))) lazy dog."
  "The (((((quick (((((|brown))))) fox))))) (((((jumps <over> the))))) lazy dog."
  "C-u M-( ( C-s C-s ( < RET")

(ipe-test-def-kbd adjust-next-pair-14 ()
  "Test `ipe-edit--update-next-pair'.

To nested PAIR (x3) (No third PAIR)."
  ipe-test-adjust-options
  nil
  "The (((((quick (((((|brown))))) fox))))) (((((jumps (((((over))))) the))))) lazy dog."
  "The (((((quick (((((|brown))))) fox))))) (((((jumps <over> the))))) lazy dog."
  "C-u M-( ( C-s C-s C-s ( < RET")

(ipe-test-def-kbd adjust-previous-pair-1 ()
  "Test `ipe-edit--update-previous-pair'."
  ipe-test-adjust-options
  nil
  "The (((((quick))))) brown (((((|fox))))) jumps (((((over))))) the (((((lazy))))) dog."
  "The <quick> brown (((((|fox))))) jumps (((((over))))) the (((((lazy))))) dog."
  "C-u M-( ( C-r ( < RET")

(ipe-test-def-kbd adjust-previous-pair-2 ()
  "Test `ipe-edit--update-previous-pair'.

With no previous PAIR."
  ipe-test-adjust-options
  nil
  "The (((((|quick))))) brown (((((fox))))) jumps (((((over))))) the (((((lazy))))) dog."
  "The <|quick> brown (((((fox))))) jumps (((((over))))) the (((((lazy))))) dog."
  "C-u M-( ( C-r ( < RET")

(ipe-test-def-kbd adjust-previous-pair-3 ()
  "Test `ipe-edit--update-previous-pair'.

With minimal PAIRs."
  ipe-test-adjust-options
  nil
  "'''|'"
  "((((()))))'|'"
  "C-u M-( ' C-r ( ( RET")

(ipe-test-def-kbd adjust-previous-pair-4 ()
  "Test `ipe-edit--update-previous-pair'.

With nested PAIRs."
  ipe-test-adjust-options
  nil
  "The (((((quick (((((brown))))) fox))))) jumps (((((|over (((((the))))) lazy))))) dog."
  "The (((((quick <brown> fox))))) jumps (((((|over (((((the))))) lazy))))) dog."
  "C-u M-( ( C-r ( < RET")

(ipe-test-def-kbd adjust-previous-pair-5 ()
  "Test `ipe-edit--update-previous-pair'.

With PAIR at start of buffer."
  ipe-test-adjust-options
  nil
  "(((((|)))))The (((((quick))))) brown (((((fox))))) jumps (((((over))))) the (((((lazy))))) dog."
  "<|The> (((((quick))))) brown (((((fox))))) jumps (((((over))))) the (((((lazy))))) dog."
  "C-u M-( ( C-r ( < RET")

(ipe-test-def-kbd adjust-previous-pair-6 ()
  "Test `ipe-edit--update-previous-pair'.

With PAIR at end of buffer."
  ipe-test-adjust-options
  nil
  "The (((((quick))))) brown (((((fox))))) jumps (((((over))))) the (((((lazy))))) (((((|dog.)))))"
  "The (((((quick))))) brown (((((fox))))) jumps (((((over))))) the <lazy> (((((|dog.)))))"
  "C-u M-( ( C-r ( < RET")

(ipe-test-def-kbd adjust-previous-pair-7 ()
  "Test `ipe-edit--update-previous-pair'.

With :escapes."
  ipe-test-adjust-options
  nil
  "The 'quick \\'brown\\' fox' jumps '|over' the 'lazy' dog."
  "The <quick 'brown' fox> jumps '|over' the 'lazy' dog."
  "C-u M-( ' C-r ( < RET")

(ipe-test-def-kbd adjust-previous-pair-8 ()
  "Test `ipe-edit--update-previous-pair'.

With numeric prefix."
  ipe-test-adjust-options
  nil
  "(((((The))))) quick (((((brown))))) fox (((((jumps))))) over (((((the))))) lazy (((((|dog)))))."
  "(((((The))))) quick <brown> fox (((((jumps))))) over (((((the))))) lazy (((((|dog)))))."
  "C-u M-( ( C-3 C-r ( < RET")

(ipe-test-def-kbd adjust-previous-pair-9 ()
  "Test `ipe-edit--update-previous-pair'.

With universal arg prefix."
  ipe-test-adjust-options
  nil
  "(((((The))))) quick (((((brown))))) fox (((((jumps))))) over (((((the))))) lazy (((((|dog)))))."
  "<The> quick (((((brown))))) fox (((((jumps))))) over (((((the))))) lazy (((((|dog)))))."
  "C-u M-( ( C-u C-r ( < RET")

(ipe-test-def-kbd adjust-previous-pair-10 ()
  "Test `ipe-edit--update-previous-pair'.

With nested PAIRS (inner)."
  ipe-test-adjust-options
  nil
  "The quick (((((brown))))) fox (((((jumps (((((|over))))) the))))) lazy dog."
  "The quick (((((brown))))) fox <jumps (((((|over))))) the> lazy dog."
  "C-u M-( ( C-r ( < RET")

(ipe-test-def-kbd adjust-previous-pair-11 ()
  "Test `ipe-edit--update-previous-pair'.

With nested PAIRS (outer)."
  ipe-test-adjust-options
  nil
  "The (((((quick (((((brown))))) fox))))) (((((jumps (((((over))))) |the))))) lazy dog."
  "The (((((quick (((((brown))))) fox))))) <jumps (((((over))))) |the> lazy dog."
  "C-u M-( ( C-r ( < RET")

(ipe-test-def-kbd adjust-previous-pair-12 ()
  "Test `ipe-edit--update-previous-pair'.

To nested PAIR."
  ipe-test-adjust-options
  nil
  "The quick (((((brown (((((fox))))) jumps))))) (((((|over (((((the))))) lazy))))) dog."
  "The quick (((((brown <fox> jumps))))) (((((|over (((((the))))) lazy))))) dog."
  "C-u M-( ( C-r ( < RET")

(ipe-test-def-kbd adjust-previous-pair-13 ()
  "Test `ipe-edit--update-previous-pair'.

To nested PAIR. (x2)"
  ipe-test-adjust-options
  nil
  "The quick (((((brown (((((fox))))) jumps))))) (((((|over (((((the))))) lazy))))) dog."
  "The quick <brown (((((fox))))) jumps> (((((|over (((((the))))) lazy))))) dog."
  "C-u M-( ( C-r C-r ( < RET")

(ipe-test-def-kbd adjust-previous-pair-14 ()
  "Test `ipe-edit--update-previous-pair'.

To nested PAIR. (x3) (No third PAIR)"
  ipe-test-adjust-options
  nil
  "The quick (((((brown (((((fox))))) jumps))))) (((((|over (((((the))))) lazy))))) dog."
  "The quick <brown (((((fox))))) jumps> (((((|over (((((the))))) lazy))))) dog."
  "C-u M-( ( C-r C-r ( < RET")

;; TODO: Add extra NEXT-OPEN / NEXT-CLOSE cases as per NEXT-PAIR /
;; PREVIOUS-PAIR.  i.e. Start of buffer / end-of-buffer / :escapes.
(ipe-test-def-kbd adjust-next-open-1 ()
  "Test `ipe-edit--update-next-open'."
  ipe-test-adjust-options
  nil
  "The (((((quick))))) (((((|brown (((((fox))))) jumps))))) (((((over))))) the (((((lazy))))) dog."
  "The (((((quick))))) (((((|brown <fox))))) jumps> (((((over))))) the (((((lazy))))) dog."
  "C-u M-( ( M-> ( < RET")

(ipe-test-def-kbd adjust-next-open-2 ()
  "Test `ipe-edit--update-next-open'.

With minimal PAIRS."
  ipe-test-adjust-options
  nil
  "|<<>>"
  "|<'>'"
  "C-u M-( < M-> ( ' RET")

(ipe-test-def-kbd adjust-next-open-3 ()
  "Test `ipe-edit--update-next-open'.

With numeric prefix."
  ipe-test-adjust-options
  nil
  "(((((|The (((((quick (((((brown (((((fox (((((jumps))))) over))))) the))))) lazy))))) dog)))))."
  "(((((|The (((((quick (((((brown <fox (((((jumps))))) over))))) the))))) lazy))))) dog>."
  "C-u M-( ( M-3 M-> ( < RET")

(ipe-test-def-kbd adjust-next-open-4 ()
  "Test `ipe-edit--update-next-open'.

With universal arg prefix."
  ipe-test-adjust-options
  nil
  "(((((|The (((((quick (((((brown (((((fox (((((jumps))))) over))))) the))))) lazy))))) dog)))))."
  "(((((|The (((((quick (((((brown (((((fox <jumps))))) over))))) the))))) lazy))))) dog>."
  "C-u M-( ( C-u M-> ( < RET")

(ipe-test-def-kbd adjust-next-open-5 ()
  "Test `ipe-edit--update-next-open'.

With no next OPEN."
  ipe-test-adjust-options
  nil
  "The quick brown fox (((((|jumps))))) over the lazy dog."
  "The quick brown fox <|jumps> over the lazy dog."
  "C-u M-( ( C-u M-> ( < RET")

(ipe-test-def-kbd adjust-previous-open-1 ()
  "Test `ipe-edit--update-previous-open'."
  ipe-test-adjust-options
  nil
  "The (((((quick))))) (((((brown (((((|fox))))) jumps))))) (((((over))))) the (((((lazy))))) dog."
  "The (((((quick))))) <brown (((((|fox> jumps))))) (((((over))))) the (((((lazy))))) dog."
  "C-u M-( ( C-< ( < RET")

(ipe-test-def-kbd adjust-previous-open-2 ()
  "Test `ipe-edit--update-previous-open'.

With minimal PAIRS."
  ipe-test-adjust-options
  nil
  "<<|>>"
  "'<|'>"
  "C-u M-( < C-< ( ' RET")

(ipe-test-def-kbd adjust-previous-open-3 ()
  "Test `ipe-edit--update-previous-open'.

With numeric prefix."
  ipe-test-adjust-options
  nil
  "(((((The (((((quick (((((brown (((((fox (((((|jumps))))) over))))) the))))) lazy))))) dog)))))."
  "(((((The <quick (((((brown (((((fox (((((|jumps> over))))) the))))) lazy))))) dog)))))."
  "C-u M-( ( C-3 C-< ( < RET")

(ipe-test-def-kbd adjust-previous-open-4 ()
  "Test `ipe-edit--update-previous-open'.

With universal arg prefix."
  ipe-test-adjust-options
  nil
  "(((((The (((((quick (((((brown (((((fox (((((|jumps))))) over))))) the))))) lazy))))) dog)))))."
  "<The (((((quick (((((brown (((((fox (((((|jumps> over))))) the))))) lazy))))) dog)))))."
  "C-u M-( ( C-u C-< ( < RET")

(ipe-test-def-kbd adjust-previous-open-5 ()
  "Test `ipe-edit--update-next-open'.

With no previous OPEN."
  ipe-test-adjust-options
  nil
  "The quick brown fox (((((|jumps))))) over the lazy dog."
  "The quick brown fox <|jumps> over the lazy dog."
  "C-u M-( ( C-u C-< ( < RET")

(ipe-test-def-kbd adjust-next-close-1 ()
  "Test `ipe-edit--update-next-close'."
  ipe-test-adjust-options
  nil
  "The (((((quick))))) (((((brown (((((|fox))))) jumps))))) (((((over))))) the (((((lazy))))) dog."
  "The (((((quick))))) (((((brown <|fox))))) jumps> (((((over))))) the (((((lazy))))) dog."
  "C-u M-( ( C-> ( < RET")

(ipe-test-def-kbd adjust-next-close-2 ()
  "Test `ipe-edit--update-next-close'.

With minimal PAIRS."
  ipe-test-adjust-options
  nil
  "|<><>"
  "|'><'"
  "C-u M-( < C-> ( ' RET")

(ipe-test-def-kbd adjust-next-close-3 ()
  "Test `ipe-edit--update-next-close'.

With numeric prefix."
  ipe-test-adjust-options
  nil
  "(((((The (((((quick (((((brown (((((fox (((((|jumps))))) over))))) the))))) lazy))))) dog)))))."
  "(((((The (((((quick (((((brown (((((fox <|jumps))))) over))))) the))))) lazy> dog)))))."
  "C-u M-( ( C-3 C-> ( < RET")

(ipe-test-def-kbd adjust-next-close-4 ()
  "Test `ipe-edit--update-next-close'.

With universal arg prefix."
  ipe-test-adjust-options
  nil
  "(((((The (((((quick (((((brown (((((fox (((((|jumps))))) over))))) the))))) lazy))))) dog)))))."
  "(((((The (((((quick (((((brown (((((fox <|jumps))))) over))))) the))))) lazy))))) dog>."
  "C-u M-( ( C-u C-> ( < RET")

(ipe-test-def-kbd adjust-next-close-5 ()
  "Test `ipe-edit--update-next-close'.

With no next CLOSE."
  ipe-test-adjust-options
  nil
  "The quick brown fox (((((|jumps))))) over the lazy dog."
  "The quick brown fox <|jumps> over the lazy dog."
  "C-u M-( ( C-u C-> ( < RET")

(ipe-test-def-kbd adjust-previous-close-1 ()
  "Test `ipe-edit--update-previous-close'."
  ipe-test-adjust-options
  nil
  "The (((((quick))))) (((((|brown (((((fox))))) jumps))))) (((((over))))) the (((((lazy))))) dog."
  "The (((((quick))))) <|brown (((((fox> jumps))))) (((((over))))) the (((((lazy))))) dog."
  "C-u M-( ( M-< ( < RET")

(ipe-test-def-kbd adjust-previous-close-2 ()
  "Test `ipe-edit--update-previous-close'.

With minimal PAIRS."
  ipe-test-adjust-options
  nil
  "|<<>>"
  "|'<'>"
  "C-u M-( < M-< ( ' RET")

(ipe-test-def-kbd adjust-previous-close-3 ()
  "Test `ipe-edit--update-previous-close'.

With numeric prefix."
  ipe-test-adjust-options
  nil
  "(((((|The (((((quick (((((brown (((((fox (((((jumps))))) over))))) the))))) lazy))))) dog)))))."
  "<|The (((((quick (((((brown (((((fox (((((jumps))))) over> the))))) lazy))))) dog)))))."
  "C-u M-( ( C-3 M-< ( < RET")

(ipe-test-def-kbd adjust-previous-close-4 ()
  "Test `ipe-edit--update-previous-close'.

With universal arg prefix."
  ipe-test-adjust-options
  nil
  "(((((|The (((((quick (((((brown (((((fox (((((jumps))))) over))))) the))))) lazy))))) dog)))))."
  "<|The (((((quick (((((brown (((((fox (((((jumps> over))))) the))))) lazy))))) dog)))))."
  "C-u M-( ( C-u M-< ( < RET")

(ipe-test-def-kbd adjust-previous-close-5 ()
  "Test `ipe-edit--update-previous-close'.

With no previous CLOSE."
  ipe-test-adjust-options
  nil
  "The quick brown fox (((((|jumps))))) over the lazy dog."
  "The quick brown fox <|jumps> over the lazy dog."
  "C-u M-( ( C-u M-< ( < RET")

(ipe-test-def-kbd adjust-next-contents-1 ()
  "Test `ipe-edit--update-next-contents'.

Using a 'word PAIR."
  ipe-test-adjust-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (((((|jumps))))) over the lazy dog."
    "The quick brown fox (((((jumps))))) over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( M-s RET")

(ipe-test-def-kbd adjust-next-contents-2 ()
  "Test `ipe-edit--update-next-contents'.

Using a 'word PAIR.  Search (x2)"
  ipe-test-adjust-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (((((|jumps))))) over the lazy dog."
    "The quick brown fox (((((jumps))))) over the lazy dog."
    "The quick brown fox (((((jumps))))) over the lazy dog.")
  "M-( ( M-s M-s RET")

(ipe-test-def-kbd adjust-next-contents-3 ()
  "Test `ipe-edit--update-next-contents'.

Using a 'word PAIR.  No next CONTENTS."
  ipe-test-adjust-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (((((|jumps))))) over the lazy dog.")
  "M-( ( M-s RET")

(ipe-test-def-kbd adjust-previous-contents-1 ()
  "Test `ipe-edit--update-previous-contents'.

Using a 'word PAIR."
  ipe-test-adjust-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox (((((jumps))))) over the lazy dog."
    "The quick brown fox (((((|jumps))))) over the lazy dog.")
  "M-( ( M-r RET")

(ipe-test-def-kbd adjust-previous-contents-2 ()
  "Test `ipe-edit--update-previous-contents'.

Using a 'word PAIR.  Search (x2)"
  ipe-test-adjust-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox (((((jumps))))) over the lazy dog."
    "The quick brown fox (((((jumps))))) over the lazy dog."
    "The quick brown fox (((((|jumps))))) over the lazy dog.")
  "M-( ( M-r M-r RET")

(ipe-test-def-kbd adjust-previous-contents-3 ()
  "Test `ipe-edit--update-previous-contents'.

Using a 'word PAIR.  No previous CONTENTS."
  ipe-test-adjust-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (((((|jumps))))) over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( M-r RET")

;; ----------------------------------------------------------------------
;; Infixes
;; ----------------------------------------------------------------------

;; Next PAIR
(ipe-test-def-kbd adjust-next-infix-1 ()
  "Test `ipe-edit--update-next-pair'.

With :infix."
  ipe-test-adjust-options
  nil
  '("<-- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->"
    "<-- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->")
  '("<-- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( - C-s C-d")

(ipe-test-def-kbd adjust-next-infix-2 ()
  "Test `ipe-edit--update-next-pair'.

With :infix.  Newline at start."
  ipe-test-adjust-options
  nil
  '("<-- "
    " -- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->"
    "<-- "
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->")
  '("<-- "
    " -- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( - C-s C-d")

(ipe-test-def-kbd adjust-next-infix-3 ()
  "Test `ipe-edit--update-next-pair'.

With :infix.  Newline at end"
  ipe-test-adjust-options
  nil
  '("<-- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -->"
    "<-- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -->")
  '("<-- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -->"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( - C-s C-d")

(ipe-test-def-kbd adjust-next-infix-4 ()
  "Test `ipe-edit--update-next-pair'.

With :infix.  Newline at start and end."
  ipe-test-adjust-options
  nil
  '("<-- "
    " -- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -->"
    "<-- "
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -->")
  '("<-- "
    " -- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -->"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( - C-s C-d")

(ipe-test-def-kbd adjust-next-infix-5 ()
  "Test `ipe-edit--update-next-pair'.

With :infix.  No next PAIR."
  ipe-test-adjust-options
  nil
  '("<-- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->"
    "<-- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->")
  '("<-- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->"
    "{|The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.}")
  "C-u M-( - C-s ( { RET")

(ipe-test-def-kbd adjust-next-infix-6 ()
  "Test `ipe-edit--update-next-pair'.

With :infix.  No next PAIR.  Newline at start and end."
  ipe-test-adjust-options
  nil
  '("<-- "
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -->"
    "<-- "
    " -- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -->")
  '("<-- "
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -->"
    "{"
    "|The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "}")
  "C-u M-( - C-s ( { RET")

(ipe-test-def-kbd adjust-next-infix-7 ()
  "Test `ipe-edit--update-next-pair'.

With :infix = :open = :close."
  ipe-test-adjust-options
  nil
  '(";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox |jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog.;"
    "The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog.;")
  '(";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox |jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog.;"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; C-s C-d")

(ipe-test-def-kbd adjust-next-infix-8 ()
  "Test `ipe-edit--update-next-pair'.

With indented :infix = :open = :close."
  ipe-test-adjust-options
  nil
  '("    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox |jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog.;"
    "The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog.;")
  '("    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox |jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog.;"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; C-s C-d")

(ipe-test-def-kbd adjust-next-infix-9 ()
  "Test `ipe-edit--update-next-pair'.

With post indented :infix = :open = :close."
  ipe-test-adjust-options
  nil
  '(";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox |jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog.;"
    "The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog.;")
  '(";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox |jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog.;"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; C-s C-d")

(ipe-test-def-kbd adjust-next-infix-10 ()
  "Test `ipe-edit--update-next-pair'.

With pre + post indented :infix = :open = :close."
  ipe-test-adjust-options
  nil
  '("    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.;"
    "The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.;")
  '("    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.;"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; C-s C-d")

(ipe-test-def-kbd adjust-next-infix-11 ()
  "Test `ipe-edit--update-next-pair'.

With toggled pre + post indented :infix = :open = :close."
  ipe-test-adjust-options
  nil
  '("    ;"
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.;"
    "The quick brown fox jumps over the lazy dog."
    "    ;"
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.;")
  '("    ;"
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.;"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ; C-s C-d")

;;; FIXME - Should not match second indents.
;;(ipe-test-def-kbd adjust-next-infix-12 ()
;;  "Test `ipe-edit--update-next-pair'.
;;
;;With region + :infix = :open = :close."
;;  ipe-test-adjust-options
;;  nil
;;  '("|;The quick brown fox jumps over the lazy dog."
;;    ";The quick brown fox jumps over the lazy dog."
;;    ";The quick brown fox jumps over the lazy dog.;@"
;;    ";The quick brown fox jumps over the lazy dog."
;;    ";The quick brown fox jumps over the lazy dog."
;;    ";The quick brown fox jumps over the lazy dog."
;;    ";The quick brown fox jumps over the lazy dog.")
;;  '("|;The quick brown fox jumps over the lazy dog."
;;    ";The quick brown fox jumps over the lazy dog."
;;    ";The quick brown fox jumps over the lazy dog.;"
;;    "The quick brown fox jumps over the lazy dog."
;;    ";The quick brown fox jumps over the lazy dog."
;;    ";The quick brown fox jumps over the lazy dog."
;;    ";The quick brown fox jumps over the lazy dog.")
;;  "C-u M-( ; C-s C-d")

(ipe-test-def-kbd adjust-next-infix-13 ()
  "Test `ipe-edit--update-next-pair'.

With :infix = :open."
  ipe-test-adjust-options
  nil
  '(";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox |jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog.")
  '(";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox |jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( 1 C-s C-d")

(ipe-test-def-kbd adjust-next-infix-14 ()
  "Test `ipe-edit--update-next-pair'.

With indented :infix = :open."
  ipe-test-adjust-options
  nil
  '("    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox |jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog.")
  '("    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox |jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( 1 C-s C-d")

(ipe-test-def-kbd adjust-next-infix-15 ()
  "Test `ipe-edit--update-next-pair'.

With post indented :infix = :open."
  ipe-test-adjust-options
  nil
  '(";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox |jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog.")
  '(";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox |jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( 1 C-s C-d")

(ipe-test-def-kbd adjust-next-infix-16 ()
  "Test `ipe-edit--update-next-pair'.

With pre + post indented :infix = :open."
  ipe-test-adjust-options
  nil
  '("    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.")
  '("    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( 1 C-s C-d")

(ipe-test-def-kbd adjust-next-infix-17 ()
  "Test `ipe-edit--update-next-pair'.

With toggled pre + post indented :infix = :open."
  ipe-test-adjust-options
  nil
  '("    ;"
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;"
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.")
  '("    ;"
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( 1 C-s C-d")

(ipe-test-def-kbd adjust-next-infix-18 ()
  "Test `ipe-edit--update-next-pair'.

With region + :infix = :open."
  ipe-test-adjust-options
  nil
  '("|;The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog.@"
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog.")
  '("|;The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( 1 C-s C-d")

(ipe-test-def-kbd adjust-next-infix-19 ()
  "Test `ipe-edit--update-next-pair'.

With :infix = :open. No next."
  ipe-test-adjust-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "   ;   The quick brown fox jumps over the lazy dog."
    "   ;   The quick brown fox |jumps over the lazy dog."
    "   ;   The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( 1 C-s C-d")

(ipe-test-def-kbd adjust-next-infix-20 ()
  "Test `ipe-edit--update-next-pair'.

With :infix + empty :open."
  ipe-test-adjust-options
  nil
  '(";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox |jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog.")
  '(";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox |jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( 3 C-s C-d")

(ipe-test-def-kbd adjust-next-infix-21 ()
  "Test `ipe-edit--update-next-pair'.

With indented :infix + empty :open."
  ipe-test-adjust-options
  nil
  '("    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox |jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog.")
  '("    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox |jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( 3 C-s C-d")

(ipe-test-def-kbd adjust-next-infix-22 ()
  "Test `ipe-edit--update-next-pair'.

With post indented :infix + empty :open."
  ipe-test-adjust-options
  nil
  '(";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox |jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog.")
  '(";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox |jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( 3 C-s C-d")

(ipe-test-def-kbd adjust-next-infix-23 ()
  "Test `ipe-edit--update-next-pair'.

With pre + post indented :infix + empty :open."
  ipe-test-adjust-options
  nil
  '("    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.")
  '("    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( 3 C-s C-d")

(ipe-test-def-kbd adjust-next-infix-24 ()
  "Test `ipe-edit--update-next-pair'.

With toggled pre + post indented :infix + empty :open."
  ipe-test-adjust-options
  nil
  '("    ;    "
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;    "
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.")
  '("    ;    "
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( 3 C-s C-d")

(ipe-test-def-kbd adjust-next-infix-25 ()
  "Test `ipe-edit--update-next-pair'.

With region + :infix + empty :open."
  ipe-test-adjust-options
  nil
  '("|;The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog.@"
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog.")
  '("|;The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( 3 C-s C-d")

(ipe-test-def-kbd adjust-next-infix-26 ()
  "Test `ipe-edit--update-next-pair'.

With differing indents."
  ipe-test-adjust-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "  {|"
    "  The quick brown fox jumps over the lazy dog."
    "  }"
    "The quick brown fox jumps over the lazy dog."
    "    {"
    "    The quick brown fox jumps over the lazy dog."
    "    }"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "  {|"
    "  The quick brown fox jumps over the lazy dog."
    "  }"
    "The quick brown fox jumps over the lazy dog."
    "    <-- "
    "     -- The quick brown fox jumps over the lazy dog."
    "     -->"
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( { C-s ( - RET")

(ipe-test-def-kbd adjust-next-infix-27 ()
  "Test `ipe-edit--update-next-pair'.

With differing open-toggle / close-toggle."
  ipe-test-adjust-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "  {|"
    "  The quick brown fox jumps over the lazy dog."
    "  }"
    "The quick brown fox jumps over the lazy dog."
    "  {The quick brown fox jumps over the lazy dog.}"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "  {|"
    "  The quick brown fox jumps over the lazy dog."
    "  }"
    "The quick brown fox jumps over the lazy dog."
    "  <-- The quick brown fox jumps over the lazy dog. -->"
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( { C-s ( - RET")

(ipe-test-def-kbd adjust-next-infix-28 ()
  "Test `ipe-edit--update-next-pair'.

With differing open-toggle / close-toggle."
  ipe-test-adjust-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "  {|The quick brown fox jumps over the lazy dog.}"
    "The quick brown fox jumps over the lazy dog."
    "  {"
    "  The quick brown fox jumps over the lazy dog."
    "  }"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "  {|The quick brown fox jumps over the lazy dog.}"
    "The quick brown fox jumps over the lazy dog."
    "  <-- "
    "   -- The quick brown fox jumps over the lazy dog."
    "   -->"
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( { C-s ( - RET")

;; *******************************************************************
;; Previous PAIR
;; *******************************************************************

(ipe-test-def-kbd adjust-previous-infix-1 ()
  "Test `ipe-edit--update-previous-pair'.

With :infix."
  ipe-test-adjust-options
  nil
  '("<-- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->"
    "<-- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "<-- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->")
  "C-u M-( - C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-2 ()
  "Test `ipe-edit--update-previous-pair'.

With :infix.  Newline at start."
  ipe-test-adjust-options
  nil
  '("<-- "
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->"
    "<-- "
    " -- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "<-- "
    " -- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->")
  "C-u M-( - C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-3 ()
  "Test `ipe-edit--update-previous-pair'.

With :infix.  Newline at end."
  ipe-test-adjust-options
  nil
  '("<-- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -->"
    "<-- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -->")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "<-- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -->")
  "C-u M-( - C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-4 ()
  "Test `ipe-edit--update-previous-pair'.

With :infix.  Newline at start and end."
  ipe-test-adjust-options
  nil
  '("<-- "
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -->"
    "<-- "
    " -- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -->")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "<-- "
    " -- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -->")
  "C-u M-( - C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-5 ()
  "Test `ipe-edit--update-previous-pair'.

With :infix.  No previous PAIR."
  ipe-test-adjust-options
  nil
  '("<-- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->"
    "<-- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->")
  '("{|The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.}"
    "<-- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog. -->")
  "C-u M-( - C-r ( { RET")

(ipe-test-def-kbd adjust-previous-infix-6 ()
  "Test `ipe-edit--update-previous-pair'.

With :infix.  No previous PAIR.  Newline at start and end."
  ipe-test-adjust-options
  nil
  '("<-- "
    " -- |The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -->"
    "<-- "
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -->")
  '("{"
    "|The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "}"
    "<-- "
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -- The quick brown fox jumps over the lazy dog."
    " -->")
  "C-u M-( - C-r ( { RET")

(ipe-test-def-kbd adjust-previous-infix-7 ()
  "Test `ipe-edit--update-previous-pair'.

With :infix = :open = :close."
  ipe-test-adjust-options
  nil
  '(";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog.;"
    "The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox |jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog.;")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox |jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog.;")
  "C-u M-( ; C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-8 ()
  "Test `ipe-edit--update-previous-pair'.

With indented :infix = :open = :close."
  ipe-test-adjust-options
  nil
  '("    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog.;"
    "The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox |jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog.;")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox |jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog.;")
  "C-u M-( ; C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-9 ()
  "Test `ipe-edit--update-previous-pair'.

With post indented :infix = :open = :close."
  ipe-test-adjust-options
  nil
  '(";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog.;"
    "The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox |jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog.;")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox |jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog.;")
  "C-u M-( ; C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-10 ()
  "Test `ipe-edit--update-previous-pair'.

With pre+post indented :infix = :open = :close."
  ipe-test-adjust-options
  nil
  '("    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.;"
    "The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.;")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.;")
  "C-u M-( ; C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-11 ()
  "Test `ipe-edit--update-previous-pair'.

With toggled pre+post indented :infix = :open = :close."
  ipe-test-adjust-options
  nil
  '("    ;"
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.;"
    "The quick brown fox jumps over the lazy dog."
    "    ;"
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.;")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;"
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.;")
  "C-u M-( ; C-r C-d")

;;; FIXME - Delete extra characters before cursor.
;;(ipe-test-def-kbd adjust-previous-infix-12 ()
;;  "Test `ipe-edit--update-previous-pair'.
;;
;;With region + :infix = :open = :close."
;;  ipe-test-adjust-options
;;  nil
;;  '(";The quick brown fox jumps over the lazy dog."
;;    ";The quick brown fox jumps over the lazy dog."
;;    ";The quick brown fox jumps over the lazy dog."
;;    "|;The quick brown fox jumps over the lazy dog."
;;    ";The quick brown fox jumps over the lazy dog."
;;    ";The quick brown fox jumps over the lazy dog."
;;    ";The quick brown fox jumps over the lazy dog.@")
;;  '("The quick brown fox jumps over the lazy dog."
;;    "The quick brown fox jumps over the lazy dog."
;;    "The quick brown fox jumps over the lazy dog."
;;    "|;The quick brown fox jumps over the lazy dog."
;;    ";The quick brown fox jumps over the lazy dog."
;;    ";The quick brown fox jumps over the lazy dog."
;;    ";The quick brown fox jumps over the lazy dog.")
;;  "C-u M-( ; C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-13 ()
  "Test `ipe-edit--update-previous-pair'.

With :infix = :open."
  ipe-test-adjust-options
  nil
  '(";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox |jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox |jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog.")
  "C-u M-( 1 C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-14 ()
  "Test `ipe-edit--update-previous-pair'.

With indented :infix = :open."
  ipe-test-adjust-options
  nil
  '("    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox |jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox |jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog.")
  "C-u M-( 1 C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-15 ()
  "Test `ipe-edit--update-previous-pair'.

With post indented :infix = :open."
  ipe-test-adjust-options
  nil
  '(";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox |jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox |jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog.")
  "C-u M-( 1 C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-16 ()
  "Test `ipe-edit--update-previous-pair'.

With pre+post indented :infix = :open."
  ipe-test-adjust-options
  nil
  '("    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.")
  "C-u M-( 1 C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-17 ()
  "Test `ipe-edit--update-previous-pair'.

With toggled pre+post indented :infix = :open."
  ipe-test-adjust-options
  nil
  '("    ;"
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;"
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;"
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.")
  "C-u M-( 1 C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-18 ()
  "Test `ipe-edit--update-previous-pair'.

With region + :infix = :open."
  ipe-test-adjust-options
  nil
  '(";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    "|;The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog.@")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "|;The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog.")
  "C-u M-( 1 C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-19 ()
  "Test `ipe-edit--update-previous-pair'.

With :infix = :open. No previous."
  ipe-test-adjust-options
  nil
  '("   ;   The quick brown fox jumps over the lazy dog."
    "   ;   The quick brown fox |jumps over the lazy dog."
    "   ;   The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( 1 C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-20 ()
  "Test `ipe-edit--update-previous-pair'.

With :infix + empty :open."
  ipe-test-adjust-options
  nil
  '(";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox |jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox |jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog.")
  "C-u M-( 3 C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-21 ()
  "Test `ipe-edit--update-previous-pair'.

With indented :infix + empty :open."
  ipe-test-adjust-options
  nil
  '("    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox |jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog."
    "    ;The quick brown fox |jumps over the lazy dog."
    "    ;The quick brown fox jumps over the lazy dog.")
  "C-u M-( 3 C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-22 ()
  "Test `ipe-edit--update-previous-pair'.

With post indented :infix + empty :open."
  ipe-test-adjust-options
  nil
  '(";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox |jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog."
    ";    The quick brown fox |jumps over the lazy dog."
    ";    The quick brown fox jumps over the lazy dog.")
  "C-u M-( 3 C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-23 ()
  "Test `ipe-edit--update-previous-pair'.

With pre+post indented :infix + empty :open."
  ipe-test-adjust-options
  nil
  '("    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.")
  "C-u M-( 3 C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-24 ()
  "Test `ipe-edit--update-previous-pair'.

With toggled pre+post indented :infix + empty :open."
  ipe-test-adjust-options
  nil
  '("    ;    "
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;    "
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.")
  '(""
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "    ;    "
    "    ;    The quick brown fox jumps over the lazy dog."
    "    ;    The quick brown fox |jumps over the lazy dog."
    "    ;    The quick brown fox jumps over the lazy dog.")
  "C-u M-( 3 C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-25 ()
  "Test `ipe-edit--update-previous-pair'.

With region + :infix + empty :open."
  ipe-test-adjust-options
  nil
  '(";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    "|;The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog.@")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "|;The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog."
    ";The quick brown fox jumps over the lazy dog.")
  "C-u M-( 3 C-r C-d")

(ipe-test-def-kbd adjust-previous-infix-26 ()
  "Test `ipe-edit--update-previous-pair'.

With differing indents."
  ipe-test-adjust-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "  {"
    "  The quick brown fox jumps over the lazy dog."
    "  }"
    "The quick brown fox jumps over the lazy dog."
    "    {|"
    "    The quick brown fox jumps over the lazy dog."
    "    }"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "  <-- "
    "   -- The quick brown fox jumps over the lazy dog."
    "   -->"
    "The quick brown fox jumps over the lazy dog."
    "    {|"
    "    The quick brown fox jumps over the lazy dog."
    "    }"
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( { C-r ( - RET")

(ipe-test-def-kbd adjust-previous-infix-27 ()
  "Test `ipe-edit--update-previous-pair'.

With differing open-toggle / close-toggle."
  ipe-test-adjust-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "  {The quick brown fox jumps over the lazy dog.}"
    "The quick brown fox jumps over the lazy dog."
    "  {|"
    "  The quick brown fox jumps over the lazy dog."
    "  }"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "  <-- The quick brown fox jumps over the lazy dog. -->"
    "The quick brown fox jumps over the lazy dog."
    "  {|"
    "  The quick brown fox jumps over the lazy dog."
    "  }"
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( { C-r ( - RET")

(ipe-test-def-kbd adjust-previous-infix-28 ()
  "Test `ipe-edit--update-previous-pair'.

With differing open-toggle / close-toggle."
  ipe-test-adjust-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "  {"
    "  The quick brown fox jumps over the lazy dog."
    "  }"
    "The quick brown fox jumps over the lazy dog."
    "  {|The quick brown fox jumps over the lazy dog.}"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "  <-- "
    "   -- The quick brown fox jumps over the lazy dog."
    "   -->"
    "The quick brown fox jumps over the lazy dog."
    "  {|The quick brown fox jumps over the lazy dog.}"
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( { C-r ( - RET")

(provide 'ipe-test-adjust)

;;; ipe-test-adjust.el ends here
