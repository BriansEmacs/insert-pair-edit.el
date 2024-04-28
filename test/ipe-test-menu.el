;;; ipe-test-menu.el --- Insert Pair Edit - Menu Tests -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Brian Kavanagh

;; Author: Brian Kavanagh <brians.emacs@gmail.com>
;; Maintainer: Brian Kavanagh <brians.emacs@gmail.com>
;; Created: 3 March, 2024
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
;; The tests within this file are used to test:
;;
;; * The "Edit" -> "Pairs" global menu,
;; * The "Insert Pair Edit" modal menu, and;
;; * The "Insert Pair Edit" mouse-3 menu.

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'ipe-test)

(defvar ipe-test-menu-options
  '((ipe-move-point-on-insert   nil)
    (ipe-prefix-moves-close-p   t)
    (ipe-edit--movement-keysets '(modifiers))
    (ipe-update-forward-first-p nil)
    (ipe-menu-support-p         t)
    (ipe-pairs
     '(("(" "(" ")")
       ("[" "[" "]")))
    (ipe-mode-pairs             nil))
  "Options used by `ipe-test-def-kbd' for `ipe-test-menu'.")

;; -------------------------------------------------------------------
;;;; "Edit" -> "Pairs" menu tests.
;; -------------------------------------------------------------------

;;;;; "Insert PAIR >" tests.
(ipe-test-def-kbd menu-insert-pair-1 ()
  "Test `ipe-insert-pair-edit' using the Edit -> Pairs menu.

Insert '(' ')' PAIR."
  ipe-test-menu-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jumps) over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-insert-pair-2 ()
  "Test `ipe-insert-pair-edit' using the Edit -> Pairs menu.

Insert '(' ')' PAIR."
  ipe-test-menu-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox [|jumps] over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-[> "
	  "<menu-bar> <ipe> <insert-pair>"))

;;;;; "Update PAIR >" tests.
(ipe-test-def-kbd menu-update-pair-1 ()
  "Test `ipe-insert-pair-edit-update' using the Edit -> Pairs menu.

Update '(' ')' PAIR."
  ipe-test-menu-options
  nil
  "The quick brown fox (|jumps) over the lazy dog."
  "The quick brown fox [|jumps] over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <change-pair> <mnemonic-[> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-update-pair-2 ()
  "Test `ipe-insert-pair-edit-update' using the Edit -> Pairs menu.

Update '[' ']' PAIR."
  ipe-test-menu-options
  nil
  "The quick brown fox [|jumps] over the lazy dog."
  "The quick brown fox (|jumps) over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-[> "
	  "<menu-bar> <ipe> <change-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <insert-pair>"))

;;;;; "Delete PAIR >" tests.
(ipe-test-def-kbd menu-delete-pair-1 ()
  "Test `ipe-insert-pair-edit-delete' using the Edit -> Pairs menu.

Delete PAIR surrounding cursor."
  ipe-test-menu-options
  nil
  "The quick brown fox (|jumps) over the lazy dog."
  "The quick brown fox |jumps over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <delete-pair> <mnemonic-(> "))

(ipe-test-def-kbd menu-delete-pair-2 ()
  "Test `ipe-insert-pair-edit-delete' using the Edit -> Pairs menu.

Delete PAIR before cursor."
  ipe-test-menu-options
  nil
  "The quick (brown) fox |jumps over the lazy dog."
  "The quick brown fox |jumps over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <delete-pair> <mnemonic-(> "))

(ipe-test-def-kbd menu-delete-pair-3 ()
  "Test `ipe-insert-pair-edit-delete' using the Edit -> Pairs menu.

Delete PAIR after cursor."
  ipe-test-menu-options
  nil
  "The quick brown fox |jumps over (the) lazy dog."
  "The quick brown fox |jumps over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <delete-pair> <mnemonic-(> "))

;; -------------------------------------------------------------------
;;;; "Insert Pair Edit" modal menu tests.
;; -------------------------------------------------------------------

;; -------------------------------------------------------------------
;;;;; "Insert And..." tests.
;; -------------------------------------------------------------------

(ipe-test-def-kbd menu-ia-goto-open ()
  "Test `ipe-edit--ia-goto-open' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox jumps| over the lazy dog."
  "The quick brown fox (|jumps) over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <insert-and> <goto-open>"))

(ipe-test-def-kbd menu-ia-goto-close ()
  "Test `ipe-edit--ia-goto-close' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (jumps)| over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <insert-and> <goto-close>"))

(ipe-test-def-kbd menu-ia-resume ()
  "Test `ipe-edit--ia-goto-close' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jumps) over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <insert-and> <resume>"))

;; Headless ERT has problems with Kill Ring <= 24.
(when (> emacs-major-version 24)
  (ipe-test-def-kbd menu-ia-copy-text ()
    "Test `ipe-edit--ia-copy-text' using the Insert Pair Edit menu."
    ipe-test-menu-options
    nil
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox (jumps|jumps) over the lazy dog."
    (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	    "<menu-bar> <ipe> <insert-and> <copy-text> "
	    "C-y")))

(ipe-test-def-kbd menu-ia-kill-text ()
  "Test `ipe-edit--ia-kill-text' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|) over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <insert-and> <kill-text>"))

(ipe-test-def-kbd menu-ia-update-forward ()
  "Test `ipe-edit--ia-update-forward' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The quick [brown] fox (|jumps) over [the] lazy dog."
  "The quick [brown] fox (|jumps) over (the) lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <insert-and> <update-forward> [ "
	  "<menu-bar> <ipe> <change-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-ia-update-backward ()
  "Test `ipe-edit--ia-update-backward' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The quick [brown] fox (|jumps) over [the] lazy dog."
  "The quick (brown) fox (|jumps) over [the] lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <insert-and> <update-backward> [ "
	  "<menu-bar> <ipe> <change-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <insert-pair>"))

;; -------------------------------------------------------------------
;;;;; "OPEN <movement>" tests.
;; -------------------------------------------------------------------

(ipe-test-def-kbd menu-open-beg ()
  "Test `ipe-edit--open-beg' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "(The quick brown fox |jumps) over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <open-beg> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-open-up ()
  "Test `ipe-edit--open-up' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (jumps over the lazy dog."
    "The quick brown fox |jumps) over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <open-up> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-open-backward ()
  "Test `ipe-edit--open-backward' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown (fox |jumps) over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <open-backward> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-open-forward ()
  "Test `ipe-edit--open-forward' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |jumps (over) the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <open-forward> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-open-down ()
  "Test `ipe-edit--open-down' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox (jumps) over the lazy dog.")
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <open-down> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-open-end ()
  "Test `ipe-edit--open-end' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |jumps over the lazy (dog)."
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <open-end> "
	  "<menu-bar> <ipe> <insert-pair>"))

;; -------------------------------------------------------------------
;;;;; "CLOSE <movement>" tests.
;; -------------------------------------------------------------------

(ipe-test-def-kbd menu-close-beg ()
  "Test `ipe-edit--close-beg' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "(The) quick brown fox |jumps over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <close-beg> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-close-up ()
  "Test `ipe-edit--close-up' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox (jumps) over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <close-up> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-close-backward ()
  "Test `ipe-edit--close-backward' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown (fox) |jumps over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <close-backward> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-close-forward ()
  "Test `ipe-edit--close-forward' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jumps over) the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <close-forward> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-close-down ()
  "Test `ipe-edit--close-down' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox (|jumps over the lazy dog."
    "The quick brown fox jumps) over the lazy dog.")
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <close-down> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-close-end ()
  "Test `ipe-edit--close-end' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jumps over the lazy dog.)"
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <close-end> "
	  "<menu-bar> <ipe> <insert-pair>"))

;; -------------------------------------------------------------------
;;;;; "Change PAIR >" tests.
;; -------------------------------------------------------------------

(ipe-test-def-kbd menu-change-pair ()
  "Test `ipe-edit--change-pair' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox (|jumps) over the lazy dog."
  "The quick brown fox [|jumps] over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <change-pair> <mnemonic-[> "
	  "<menu-bar> <ipe> <insert-pair>"))

;; -------------------------------------------------------------------
;;;;; "Change Movement >" tests.
;; -------------------------------------------------------------------

(ipe-test-def-kbd menu-change-movement-char ()
  "Test `ipe-edit--movement-by-char' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox (|jumps) over the lazy dog."
  "The quick brown fox |j(umps) over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <change-movement> <char> "
	  "<menu-bar> <ipe> <open-forward> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-change-movement-word ()
  "Test `ipe-edit--movement-by-word' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox (|jumps) over the lazy dog."
  "The quick brown fox |jumps (over) the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <change-movement> <word> "
	  "<menu-bar> <ipe> <open-forward> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-change-movement-line ()
  "Test `ipe-edit--movement-by-line' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox (|jumps) over the lazy dog."
  "(The quick brown fox |jumps over the lazy dog.)"
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <change-movement> <line> "
	  "<menu-bar> <ipe> <open-forward> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-change-movement-list ()
  "Test `ipe-edit--movement-by-list' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox (|jumps) over the lazy dog."
  "The quick brown fox |jumps over the lazy dog.()"
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <change-movement> <list> "
	  "<menu-bar> <ipe> <open-forward> "
	  "<menu-bar> <ipe> <insert-pair>"))

;; -------------------------------------------------------------------
;;;;; "Edit CONTENTS >" tests.
;; -------------------------------------------------------------------

;; Headless ERT has problems with Kill Ring <= 24.
(when (> emacs-major-version 24)
  (ipe-test-def-kbd menu-edit-contents-kill ()
    "Test `ipe-edit--contents-kill' using the Insert Pair Edit menu."
    ipe-test-menu-options
    nil
    "The quick brown fox (|jumps) over the lazy dog."
    "The quick brown fox (|) over the lazy dog."
    (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	    "<menu-bar> <ipe> <edit-contents> <kill> "
	    "<menu-bar> <ipe> <insert-pair>"))

  (ipe-test-def-kbd menu-edit-contents-copy ()
    "Test `ipe-edit--contents-copy' using the Insert Pair Edit menu."
    ipe-test-menu-options
    nil
    "The quick brown fox (|jumps) over the lazy dog."
    "The quick brown fox (jumps|jumps) over the lazy dog."
    (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	    "<menu-bar> <ipe> <edit-contents> <copy> "
	    "<menu-bar> <ipe> <insert-pair> "
	    "<menu-bar> <edit> <paste>"))

  (ipe-test-def-kbd menu-edit-contents-paste ()
    "Test `ipe-edit--contents-paste' using the Insert Pair Edit menu."
    ipe-test-menu-options
    nil
    "The quick brown fox (|jumps) over the lazy dog."
    "The quick brown fox |jumps (jumps) the lazy dog."
    (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	    "<menu-bar> <ipe> <edit-contents> <copy> "
	    "<menu-bar> <ipe> <open-forward> "
	    "<menu-bar> <ipe> <edit-contents> <paste> "
	    "<menu-bar> <ipe> <insert-pair>")))

(ipe-test-def-kbd menu-edit-contents-replace ()
  "Test `ipe-edit--contents-replace' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox (|jumps) over the lazy dog."
  "The quick brown fox (|walks) over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <edit-contents> <replace> "
	  "walks RET "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-edit-contents-trim ()
  "Test `ipe-edit--contents-trim' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox (|   jumps   ) over the lazy dog."
  "The quick brown fox (|jumps) over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <edit-contents> <trim> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-edit-contents-upcase ()
  "Test `ipe-edit--contents-upcase' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown (fox |jumps over) the lazy dog."
  "The quick brown (FOX |JUMPS OVER) the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <edit-contents> <upcase> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-edit-contents-capitalize ()
  "Test `ipe-edit--contents-capitalize' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown (fox |jumps over) the lazy dog."
  "The quick brown (Fox |Jumps Over) the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <edit-contents> <capitalize> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-edit-contents-downcase ()
  "Test `ipe-edit--contents-downcase' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "THE QUICK BROWN (FOX |JUMPS OVER) THE LAZY DOG."
  "THE QUICK BROWN (fox |jumps over) THE LAZY DOG."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <edit-contents> <downcase> "
	  "<menu-bar> <ipe> <insert-pair>"))

;; -------------------------------------------------------------------
;;;;; "Next / Previous >" tests.
;; -------------------------------------------------------------------

(ipe-test-def-kbd menu-next-pair ()
  "Test `ipe-edit--update-next-pair' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick (brown) fox (|jumps) over (the) lazy dog."
  "The quick (brown) fox (|jumps) over [the] lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <next-prev> <next-pair> "
	  "<menu-bar> <ipe> <change-pair> <mnemonic-[> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-next-contents ()
  "Test `ipe-edit--update-next-contents' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "(the) quick brown fox |jumps over the lazy dog."
  "(the) quick brown fox |jumps over (the) lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <next-prev> <next-contents> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-next-open ()
  "Test `ipe-edit--update-next-open' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick (brown (|fox (jumps) over) the) lazy dog."
  "The quick (brown (|fox [jumps) over] the) lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <next-prev> <next-open> "
	  "<menu-bar> <ipe> <change-pair> <mnemonic-[> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-next-close ()
  "Test `ipe-edit--update-next-close' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick (brown (|fox (jumps) over) the) lazy dog."
  "The quick (brown [|fox (jumps) over) the] lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <next-prev> <next-close> "
	  "<menu-bar> <ipe> <change-pair> <mnemonic-[> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-previous-pair ()
  "Test `ipe-edit--update-previous-pair' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick (brown) fox (|jumps) over (the) lazy dog."
  "The quick [brown] fox (|jumps) over (the) lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <next-prev> <previous-pair> "
	  "<menu-bar> <ipe> <change-pair> <mnemonic-[> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-previous-contents ()
  "Test `ipe-edit--update-previous-contents' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "the quick brown fox |jumps over (the) lazy dog."
  "(the) quick brown fox |jumps over (the) lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <next-prev> <previous-contents> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-previous-open ()
  "Test `ipe-edit--update-previous-open' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick (brown (|fox (jumps) over) the) lazy dog."
  "The quick [brown (|fox (jumps) over] the) lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <next-prev> <previous-open> "
	  "<menu-bar> <ipe> <change-pair> <mnemonic-[> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-previous-close ()
  "Test `ipe-edit--update-previous-close' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick (brown (|fox (jumps) over) the) lazy dog."
  "The quick (brown [|fox (jumps] over) the) lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <next-prev> <previous-close> "
	  "<menu-bar> <ipe> <change-pair> <mnemonic-[> "
	  "<menu-bar> <ipe> <insert-pair>"))

;; -------------------------------------------------------------------
;;;; "Multiple >" menu tests.
;; -------------------------------------------------------------------

(ipe-test-def-kbd menu-add-pair-point ()
  "Test `ipe-edit--insert-pair' (x2) using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox (|jumps) over the lazy dog."
  "The quick brown fox |(!jumps) (over) the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <open-forward> "
	  "<menu-bar> <ipe> <multiple> <add-pair> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-add-pair-forward ()
  "Test `ipe-edit--add-next-pair'using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown (fox) (|jumps) (over) the lazy dog."
  "The quick brown (fox) [|jumps] [!over] the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <multiple> <add-pair-forward> "
	  "<menu-bar> <ipe> <change-pair> <mnemonic-[> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-add-pair-backward ()
  "Test `ipe-edit--add-previous-pair'using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown (fox) (|jumps) (over) the lazy dog."
  "The quick brown [!fox] [|jumps] (over) the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <multiple> <add-pair-backward> "
	  "<menu-bar> <ipe> <change-pair> <mnemonic-[> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-insert-first-pair-1 ()
  "Test `ipe-edit--delete-all-pairs' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |(jumps) over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <insert-first> "
	  "<menu-bar> <ipe> <multiple> <delete-all>"))

(ipe-test-def-kbd menu-insert-first-pair-2 ()
  "Test `ipe-edit--insert-first-pair' using the Insert Pair Edit menu.

x3."
  ipe-test-menu-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |(jumps) over the lazy dog."
    "The quick brown fox (jumps) over the lazy dog."
    "The quick brown fox (jumps) over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <insert-first> "
	  "<menu-bar> <ipe> <multiple> <insert-first> "
	  "<menu-bar> <ipe> <multiple> <insert-first> "
	  "<menu-bar> <ipe> <multiple> <delete-all>"))

(ipe-test-def-kbd menu-insert-last-pair-1 ()
  "Test `ipe-edit--insert-last-pair' using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (jumps) over the lazy dog.")
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <insert-last> "
	  "<menu-bar> <ipe> <multiple> <delete-all>"))

(ipe-test-def-kbd menu-insert-last-pair-2 ()
  "Test `ipe-edit--insert-last-pair' using the Insert Pair Edit menu.

x3."
  ipe-test-menu-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (jumps) over the lazy dog."
    "The quick brown fox (jumps) over the lazy dog."
    "The quick brown fox (jumps) over the lazy dog.")
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <insert-last> "
	  "<menu-bar> <ipe> <multiple> <insert-last> "
	  "<menu-bar> <ipe> <multiple> <insert-last> "
	  "<menu-bar> <ipe> <multiple> <delete-all>"))

(ipe-test-def-kbd menu-delete-pair ()
  "Test `ipe-edit--delete-all-pairs'using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The quick brown fox (|jumps) over the lazy dog."
  "The quick brown fox |jumps over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <multiple> <delete-pair>"))

(ipe-test-def-kbd menu-delete-first-pair ()
  "Test `ipe-edit--delete-first-pair'using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The (quick) |(brown) (fox) (jumps) (over) (the)@ (lazy) dog."
  "The (quick) |brown !(fox) !(jumps) !(over) !(the) (lazy) dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <multiple> <delete-first> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-delete-all-pairs ()
  "Test `ipe-edit--delete-all-pairs'using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The (quick) |(brown) (fox) (jumps) (over) (the)@ (lazy) dog."
  "The (quick) |brown fox jumps over the (lazy) dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <multiple> <delete-all>"))

(ipe-test-def-kbd menu-delete-last-pair ()
  "Test `ipe-edit--delete-last-pair'using the Insert Pair Edit menu."
  ipe-test-menu-options
  nil
  "The (quick) |(brown) (fox) (jumps) (over) (the)@ (lazy) dog."
  "The (quick) |(brown) !(fox) !(jumps) !(over) the (lazy) dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <multiple> <delete-last> "
	  "<menu-bar> <ipe> <insert-pair>"))

;; -------------------------------------------------------------------
;;;; "Insert Pair Edit" mouse-3 menu tests.
;; -------------------------------------------------------------------

(ipe-test-def-kbd menu-mouse-3-insert-pair ()
  "Test `ipe-edit--insert-pair' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jumps) over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-insert-pair>"))

(ipe-test-def-kbd menu-mouse-3-insert-first-pair-1 ()
  "Test `ipe-edit--insert-first-pair' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |(jumps) over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<mouse-3> <ipe-mouse-insert> <first> "
	  "<mouse-3> <ipe-mouse-delete> <all>"))

(ipe-test-def-kbd menu-mouse-3-insert-first-pair-2 ()
  "Test `ipe-edit--insert-first-pair' using the `ipe' mouse-3 menu.

x3."
  ipe-test-menu-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |(jumps) over the lazy dog."
    "The quick brown fox (jumps) over the lazy dog."
    "The quick brown fox (jumps) over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<mouse-3> <ipe-mouse-insert> <first> "
	  "<mouse-3> <ipe-mouse-insert> <first> "
	  "<mouse-3> <ipe-mouse-insert> <first> "
	  "<mouse-3> <ipe-mouse-delete> <all>"))

(ipe-test-def-kbd menu-mouse-3-insert-last-pair-1 ()
  "Test `ipe-edit--insert-last-pair' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (jumps) over the lazy dog.")
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<mouse-3> <ipe-mouse-insert> <last> "
	  "<mouse-3> <ipe-mouse-delete> <all>"))

(ipe-test-def-kbd menu-mouse-3-insert-last-pair-2 ()
  "Test `ipe-edit--insert-last-pair' using the `ipe' mouse-3 menu.

x3."
  ipe-test-menu-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (jumps) over the lazy dog."
    "The quick brown fox (jumps) over the lazy dog."
    "The quick brown fox (jumps) over the lazy dog.")
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<mouse-3> <ipe-mouse-insert> <last> "
	  "<mouse-3> <ipe-mouse-insert> <last> "
	  "<mouse-3> <ipe-mouse-insert> <last> "
	  "<mouse-3> <ipe-mouse-delete> <all>"))

(ipe-test-def-kbd menu-mouse-3-insert-all ()
  "Test `ipe-edit--insert-pair' using the `ipe' mouse-3 menu.

Multiple PAIRs."
  ipe-test-menu-options
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
    "The quick brown fox (!jumps) over the lazy dog.")
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<mouse-3> <ipe-mouse-insert> <all>"))

(ipe-test-def-kbd menu-mouse-3-first-pair-multiple-1 ()
  "Test `ipe-edit--*-first-pair' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |(jumps) over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (jumps) over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (!jumps) over the lazy dog.")
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<mouse-3> <ipe-mouse-insert> <first> "
	  "<mouse-3> <ipe-mouse-delete> <first> "
	  "<mouse-3> <ipe-mouse-insert> <first> "
	  "<mouse-3> <ipe-mouse-delete> <first> "
	  "<mouse-3> <ipe-mouse-insert> <first>"))

(ipe-test-def-kbd menu-mouse-3-last-pair-multiple-1 ()
  "Test `ipe-edit--*-first-pair' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox (jumps) over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox (jumps) over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<menu-bar> <ipe> <multiple> <add-contents-forward> "
	  "<mouse-3> <ipe-mouse-delete> <last> "
	  "<mouse-3> <ipe-mouse-insert> <last> "
	  "<mouse-3> <ipe-mouse-delete> <last> "
	  "<mouse-3> <ipe-mouse-insert> <last> "
	  "<mouse-3> <ipe-mouse-delete> <last>"))

;; -------------------------------------------------------------------
;;;;; "Insert And..." tests.
;; -------------------------------------------------------------------

(ipe-test-def-kbd menu-mouse-3-ia-goto-open ()
  "Test `ipe-edit--ia-goto-open' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The quick brown fox jumps| over the lazy dog."
  "The quick brown fox (|jumps) over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-ia> <goto-open>"))

(ipe-test-def-kbd menu-mouse-3-ia-goto-close ()
  "Test `ipe-edit--ia-goto-close' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (jumps)| over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-ia> <goto-close>"))

(ipe-test-def-kbd menu-mouse-3-ia-resume ()
  "Test `ipe-edit--ia-goto-close' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jumps) over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-ia> <resume>"))

;; Headless ERT has problems with Kill Ring <= 24.
(when (> emacs-major-version 24)
  (ipe-test-def-kbd menu-mouse-3-ia-copy-text ()
    "Test `ipe-edit--ia-copy-text' using the `ipe' mouse-3 menu."
    ipe-test-menu-options
    nil
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox (jumps|jumps) over the lazy dog."
    (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	    "<mouse-3> <ipe-mouse-ia> <copy-text> "
	    "C-y"))

  (ipe-test-def-kbd menu-mouse-3-ia-kill-text ()
    "Test `ipe-edit--ia-kill-text' using the `ipe' mouse-3 menu."
    ipe-test-menu-options
    nil
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox (|) over the lazy dog."
    (concat "<menu-bar> <edit> <ipe> <insert-pair> <mnemonic-(> "
	    "<mouse-3> <ipe-mouse-ia> <kill-text>")))

(ipe-test-def-kbd menu-mouse-3-ia-update-forward ()
  "Test `ipe-edit--ia-update-forward' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The quick [brown] fox (|jumps) over [the] lazy dog."
  "The quick [brown] fox (|jumps) over (the) lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-ia> <update-forward> [ "
	  "<mouse-3> <ipe-mouse-change-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-insert-pair>"))

(ipe-test-def-kbd menu-mouse-3-ia-update-backward ()
  "Test `ipe-edit--ia-update-backward' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The quick [brown] fox (|jumps) over [the] lazy dog."
  "The quick (brown) fox (|jumps) over [the] lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-ia> <update-backward> [ "
	  "<mouse-3> <ipe-mouse-change-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-insert-pair>"))

;; -------------------------------------------------------------------
;;;;; "Change PAIR >" tests.
;; -------------------------------------------------------------------

(ipe-test-def-kbd menu-mouse-3-change-pair ()
  "Test `ipe-edit--change-pair' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The quick brown fox (|jumps) over the lazy dog."
  "The quick brown fox [|jumps] over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-change-pair> <mnemonic-[> "
	  "<mouse-3> <ipe-mouse-insert-pair>"))

;; -------------------------------------------------------------------
;;;;; "Edit CONTENTS >" tests.
;; -------------------------------------------------------------------

;; Headless ERT has problems with Kill Ring <= 24.
(when (> emacs-major-version 24)
  (ipe-test-def-kbd menu-mouse-3-edit-contents-kill ()
    "Test `ipe-edit--contents-kill' using the `ipe' mouse-3 menu."
    ipe-test-menu-options
    nil
    "The quick brown fox (|jumps) over the lazy dog."
    "The quick brown fox (|) over the lazy dog."
    (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	    "<mouse-3> <ipe-mouse-edit-contents> <kill> "
	    "<mouse-3> <ipe-mouse-insert-pair>"))

  (ipe-test-def-kbd menu-mouse-3-edit-contents-copy ()
    "Test `ipe-edit--contents-copy' using the `ipe' mouse-3 menu."
    ipe-test-menu-options
    nil
    "The quick brown fox (|jumps) over the lazy dog."
    "The quick brown fox (jumps|jumps) over the lazy dog."
    (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	    "<mouse-3> <ipe-mouse-edit-contents> <copy> "
	    "<mouse-3> <ipe-mouse-insert-pair> "
	    "<menu-bar> <edit> <paste>"))

  (ipe-test-def-kbd menu-mouse-3-edit-contents-paste ()
    "Test `ipe-edit--contents-paste' using the `ipe' mouse-3 menu."
    ipe-test-menu-options
    nil
    "The quick brown fox (|jumps) over the lazy dog."
    "The quick brown fox |jumps (jumps) the lazy dog."
    (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	    "<mouse-3> <ipe-mouse-edit-contents> <copy> "
	    "<menu-bar> <ipe> <open-forward> "
	    "<mouse-3> <ipe-mouse-edit-contents> <paste> "
	    "<mouse-3> <ipe-mouse-insert-pair>")))

(ipe-test-def-kbd menu-mouse-3-edit-contents-replace ()
  "Test `ipe-edit--contents-replace' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The quick brown fox (|jumps) over the lazy dog."
  "The quick brown fox (|walks) over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-edit-contents> <replace> "
	  "walks RET "
	  "<mouse-3> <ipe-mouse-insert-pair>"))

(ipe-test-def-kbd menu-mouse-3-edit-contents-trim ()
  "Test `ipe-edit--contents-trim' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The quick brown fox (|   jumps   ) over the lazy dog."
  "The quick brown fox (|jumps) over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-edit-contents> <trim> "
	  "<mouse-3> <ipe-mouse-insert-pair>"))

(ipe-test-def-kbd menu-mouse-3-edit-contents-upcase ()
  "Test `ipe-edit--contents-upcase' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The quick brown (fox |jumps over) the lazy dog."
  "The quick brown (FOX |JUMPS OVER) the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-edit-contents> <upcase> "
	  "<mouse-3> <ipe-mouse-insert-pair>"))

(ipe-test-def-kbd menu-mouse-3-edit-contents-capitalize ()
  "Test `ipe-edit--contents-capitalize' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The quick brown (fox |jumps over) the lazy dog."
  "The quick brown (Fox |Jumps Over) the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-edit-contents> <capitalize> "
	  "<mouse-3> <ipe-mouse-insert-pair>"))

(ipe-test-def-kbd menu-mouse-3-edit-contents-downcase ()
  "Test `ipe-edit--contents-downcase' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "THE QUICK BROWN (FOX |JUMPS OVER) THE LAZY DOG."
  "THE QUICK BROWN (fox |jumps over) THE LAZY DOG."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-edit-contents> <downcase> "
	  "<mouse-3> <ipe-mouse-insert-pair>"))

;; -------------------------------------------------------------------
;;;;; "Next / Previous >" tests.
;; -------------------------------------------------------------------

(ipe-test-def-kbd menu-mouse-3-next-pair ()
  "Test `ipe-edit--update-next-pair' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The quick (brown) fox (|jumps) over (the) lazy dog."
  "The quick (brown) fox (|jumps) over [the] lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-next-prev> <next-pair> "
	  "<mouse-3> <ipe-mouse-change-pair> <mnemonic-[> "
	  "<mouse-3> <ipe-mouse-insert-pair>"))

(ipe-test-def-kbd menu-mouse-3-next-contents ()
  "Test `ipe-edit--update-next-contents' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "(the) quick brown fox |jumps over the lazy dog."
  "(the) quick brown fox |jumps over (the) lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-next-prev> <next-contents> "
	  "<mouse-3> <ipe-mouse-insert-pair>"))

(ipe-test-def-kbd menu-mouse-3-next-open ()
  "Test `ipe-edit--update-next-open' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The quick (brown (|fox (jumps) over) the) lazy dog."
  "The quick (brown (|fox [jumps) over] the) lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-next-prev> <next-open> "
	  "<mouse-3> <ipe-mouse-change-pair> <mnemonic-[> "
	  "<mouse-3> <ipe-mouse-insert-pair>"))

(ipe-test-def-kbd menu-mouse-3-next-close ()
  "Test `ipe-edit--update-next-close' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The quick (brown (|fox (jumps) over) the) lazy dog."
  "The quick (brown [|fox (jumps) over) the] lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<menu-bar> <ipe> <next-prev> <next-close> "
	  "<menu-bar> <ipe> <change-pair> <mnemonic-[> "
	  "<menu-bar> <ipe> <insert-pair>"))

(ipe-test-def-kbd menu-mouse-3-previous-pair ()
  "Test `ipe-edit--update-previous-pair' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The quick (brown) fox (|jumps) over (the) lazy dog."
  "The quick [brown] fox (|jumps) over (the) lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-next-prev> <previous-pair> "
	  "<mouse-3> <ipe-mouse-change-pair> <mnemonic-[> "
	  "<mouse-3> <ipe-mouse-insert-pair>"))

(ipe-test-def-kbd menu-mouse-3-previous-contents ()
  "Test `ipe-edit--update-previous-contents' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "the quick brown fox |jumps over (the) lazy dog."
  "(the) quick brown fox |jumps over (the) lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-next-prev> <previous-contents> "
	  "<mouse-3> <ipe-mouse-insert-pair>"))

(ipe-test-def-kbd menu-mouse-3-previous-open ()
  "Test `ipe-edit--update-previous-open' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The quick (brown (|fox (jumps) over) the) lazy dog."
  "The quick [brown (|fox (jumps) over] the) lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-next-prev> <previous-open> "
	  "<mouse-3> <ipe-mouse-change-pair> <mnemonic-[> "
	  "<mouse-3> <ipe-mouse-insert-pair>"))

(ipe-test-def-kbd menu-mouse-3-previous-close ()
  "Test `ipe-edit--update-previous-close' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The quick (brown (|fox (jumps) over) the) lazy dog."
  "The quick (brown [|fox (jumps] over) the) lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-next-prev> <previous-close> "
	  "<mouse-3> <ipe-mouse-change-pair> <mnemonic-[> "
	  "<mouse-3> <ipe-mouse-insert-pair>"))

;; -------------------------------------------------------------------
;;;;; "Delete PAIR" tests.
;; -------------------------------------------------------------------

(ipe-test-def-kbd menu-mouse-3-delete-pair ()
  "Test `ipe-edit--delete-all-pairs' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The quick brown fox (|jumps) over the lazy dog."
  "The quick brown fox |jumps over the lazy dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-delete-pair>"))

(ipe-test-def-kbd menu-mouse-3-delete-first-pair ()
  "Test `ipe-edit--delete-first-pair' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The (quick) |(brown) (fox) (jumps) (over) (the)@ (lazy) dog."
  "The (quick) |brown !(fox) !(jumps) !(over) !(the) (lazy) dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-delete> <first> "
	  "<mouse-3> <ipe-mouse-insert-pair>"))

(ipe-test-def-kbd menu-mouse-3-delete-all-pairs ()
  "Test `ipe-edit--delete-all-pairs' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The (quick) |(brown) (fox) (jumps) (over) (the)@ (lazy) dog."
  "The (quick) |brown fox jumps over the (lazy) dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-delete> <all>"))

(ipe-test-def-kbd menu-mouse-3-delete-last-pair ()
  "Test `ipe-edit--delete-last-pair' using the `ipe' mouse-3 menu."
  ipe-test-menu-options
  nil
  "The (quick) |(brown) (fox) (jumps) (over) (the)@ (lazy) dog."
  "The (quick) |(brown) !(fox) !(jumps) !(over) the (lazy) dog."
  (concat "<menu-bar> <edit> <ipe> <update-pair> <mnemonic-(> "
	  "<mouse-3> <ipe-mouse-next-prev> <previous-close> "
	  "<mouse-3> <ipe-mouse-delete> <last> "
	  "<mouse-3> <ipe-mouse-insert-pair>"))

(provide 'ipe-test-menu)

;;; ipe-test-menu.el ends here
