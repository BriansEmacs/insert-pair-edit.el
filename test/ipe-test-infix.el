;;; ipe-test-infix.el --- Insert Pair Edit - Infix Processing Tests
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
;; The tests within this file are used to test the processing
;; performed by 'Insert Pair Edit' when using PAIR Definitions which
;; include an :infix definition.

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'ipe-test)

(setq ipe-test-infix-options
      '((ipe-move-point-on-insert   nil)
        (ipe-prefix-moves-close-p   t)
        (ipe-edit--movement-keysets '(modifiers))
        (ipe-pairs                  '(("<" "<!-- " "  -->" (:movement line :infix "  -- "))
                                      ("[" "[" "]"         (:movement line :infix "-"))))
        (ipe-mode-pairs             nil)))

(ipe-test-def-kbd infix-basic-insert-1 ()
  "Test `insert-pair-edit' in an empty buffer.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  ""
  "<!-- |  -->"
  "M-( <")

(ipe-test-def-kbd infix-basic-insert-2 ()
  "Test `insert-pair-edit' at the start of buffer.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  "|The quick brown fox jumps over the lazy dog."
  "<!-- |The quick brown fox jumps over the lazy dog.  -->"
  "M-( < RET")

(ipe-test-def-kbd infix-basic-insert-3 ()
  "Test `insert-pair-edit' in the middle of buffer.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "<!-- The quick brown fox |jumps over the lazy dog.  -->"
  "M-( < RET")

(ipe-test-def-kbd infix-basic-insert-4 ()
  "Test `insert-pair-edit' at the end of buffer.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  "The quick brown fox jumps over the lazy dog.|"
  "<!-- The quick brown fox jumps over the lazy dog.|  -->"
  "M-( < RET")

(ipe-test-def-kbd infix-basic-insert-5 ()
  "Test `insert-pair-edit' at 'offset' beginning of buffer.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  "|  The quick brown fox jumps over the lazy dog."
  "<!-- |  The quick brown fox jumps over the lazy dog.  -->"
  "M-( < RET")

(ipe-test-def-kbd infix-basic-insert-6 ()
  "Test `insert-pair-edit' at an 'offset' end of buffer.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  "The quick brown fox jumps over the lazy dog.  |"
  "<!-- The quick brown fox jumps over the lazy dog.  |  -->"
  "M-( < RET")

(ipe-test-def-kbd infix-basic-prefix-insert-1 ()
  "Test `insert-pair-edit' with a numeric prefix.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("<!-- The quick brown fox |jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-1 M-( < RET")

(ipe-test-def-kbd infix-basic-prefix-insert-2 ()
  "Test `insert-pair-edit' with a '2' numeric prefix.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("<!-- The quick brown fox |jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-2 M-( < RET")

(ipe-test-def-kbd infix-basic-prefix-insert-3 ()
  "Test `insert-pair-edit' with a '3' numeric prefix.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("<!-- The quick brown fox |jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-3 M-( < RET")

(ipe-test-def-kbd infix-basic-prefix-insert-4 ()
  "Test `insert-pair-edit' with a '4' numeric prefix.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("<!-- The quick brown fox |jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "C-4 M-( < RET")

(ipe-test-def-kbd infix-basic-prefix-insert-5 ()
  "Test `insert-pair-edit' with a negative numeric prefix.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("<!-- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox |jumps over the lazy dog.  -->")
  "C-- C-4 M-( < RET")

(ipe-test-def-kbd infix-open-start-1 ()
  "Test `insert-pair-edit' OPEN start.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog.")
  '("<!-- "
    "  -- The quick brown fox |jumps over the lazy dog.  -->")
  "M-( < C-a RET")

(ipe-test-def-kbd infix-open-start-2 ()
  "Test `insert-pair-edit' OPEN start.

Using a 'line (+ :infix) PAIR at the beginning of buffer."
  ipe-test-infix-options
  nil
  '("|The quick brown fox jumps over the lazy dog.")
  '("<!-- |"
    "  -- The quick brown fox jumps over the lazy dog.  -->")
  "M-( < C-a C-a RET")

(ipe-test-def-kbd infix-open-start-3 ()
  "Test `insert-pair-edit' OPEN start.

Using a 'line (+ :infix) PAIR at the beginning of line."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!-- "
    "  -- The quick brown fox |jumps over the lazy dog.  -->")
  "M-( < C-a C-a RET")

(ipe-test-def-kbd infix-open-start-4 ()
  "Test `insert-pair-edit' OPEN start.

Using a 'line (+ :infix) PAIR with a numeric prefix."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("<!-- "
    "  -- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox |jumps over the lazy dog.  -->")
  "M-( < C-p 2 C-a RET")

(ipe-test-def-kbd infix-open-up-1 ()
  "Test `insert-pair-edit' OPEN up.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox |jumps over the lazy dog.  -->")
  "M-( < C-p RET")

(ipe-test-def-kbd infix-open-up-2 ()
  "Test `insert-pair-edit' OPEN up x2.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox |jumps over the lazy dog.  -->")
  "M-( < C-p C-p RET")

(ipe-test-def-kbd infix-open-up-3 ()
  "Test `insert-pair-edit' OPEN up x3.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox |jumps over the lazy dog.  -->")
  "M-( < 3 C-p RET")

(ipe-test-def-kbd infix-open-up-4 ()
  "Test `insert-pair-edit' OPEN up x4.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("<!-- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox |jumps over the lazy dog.  -->")
  "M-( < C-u C-p RET")

(ipe-test-def-kbd infix-open-up-blank ()
  "Test `insert-pair-edit' OPEN up with a blank line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!-- "
    "  -- The quick brown fox |jumps over the lazy dog.  -->")
  "M-( < C-p RET")

(ipe-test-def-kbd infix-open-up-blank-2 ()
  "Test `insert-pair-edit' OPEN up x2 with a blank line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog.")
  '("<!-- The quick brown fox jumps over the lazy dog."
    "  -- "
    "  -- The quick brown fox |jumps over the lazy dog.  -->")
  "M-( < C-p C-p RET")

(ipe-test-def-kbd infix-open-up-offset ()
  "Test `insert-pair-edit' OPEN up with an offset.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("  The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog."
    "  The quick brown fox |jumps over the lazy dog.")
  '("  The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog."
    "<!--   The quick brown fox jumps over the lazy dog."
    "  --   The quick brown fox |jumps over the lazy dog.  -->")
  "M-( < C-p RET")

(ipe-test-def-kbd infix-open-up-offset-beginning ()
  "Test `insert-pair-edit' OPEN up offset to beginning of line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "  |The quick brown fox jumps over the lazy dog.")
  '("<!-- The quick brown fox jumps over the lazy dog."
    "  --   |The quick brown fox jumps over the lazy dog.  -->")
  "M-( < C-p RET")

(ipe-test-def-kbd infix-open-up-offset-beginning-2 ()
  "Test `insert-pair-edit' OPEN up to offset beginning of line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("  The quick brown fox jumps over the lazy dog."
    "|The quick brown fox jumps over the lazy dog.")
  '("<!--   The quick brown fox jumps over the lazy dog."
    "  -- |The quick brown fox jumps over the lazy dog.  -->")
  "M-( < C-p RET")

(ipe-test-def-kbd infix-open-backward-1 ()
  "Test `insert-pair-edit' OPEN backward.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    " <!-- The quick brown fox jumps over the lazy dog."
    "   -- The quick brown fox |jumps over the lazy dog."
    "   -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-p C-b RET")

(ipe-test-def-kbd infix-open-backward-2 ()
  "Test `insert-pair-edit' OPEN backward x2.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "  <!-- The quick brown fox jumps over the lazy dog."
    "    -- The quick brown fox |jumps over the lazy dog."
    "    -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-p C-b C-b RET")

(ipe-test-def-kbd infix-open-backward-3 ()
  "Test `insert-pair-edit' OPEN backward x3.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "   <!-- The quick brown fox jumps over the lazy dog."
    "     -- The quick brown fox |jumps over the lazy dog."
    "     -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-p 3 C-b RET")

(ipe-test-def-kbd infix-open-backward-4 ()
  "Test `insert-pair-edit' OPEN backward x4.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "    <!-- The quick brown fox jumps over the lazy dog."
    "      -- The quick brown fox |jumps over the lazy dog."
    "      -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-p C-u C-b RET")

(ipe-test-def-kbd infix-open-backward-16 ()
  "Test `insert-pair-edit' OPEN backward x16.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "                <!-- The quick brown fox jumps over the lazy dog."
    "                  -- The quick brown fox |jumps over the lazy dog."
    "                  -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-p C-u C-u C-b RET")

(ipe-test-def-kbd infix-open-backward-beginning ()
  "Test `insert-pair-edit' OPEN backward at beginning of buffer.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  "|The quick brown fox jumps over the lazy dog."
  " <!-- |The quick brown fox jumps over the lazy dog.  -->"
  "M-( < C-b RET")

(ipe-test-def-kbd infix-open-backward-blank ()
  "Test `insert-pair-edit' OPEN backward with a blank line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "|"
    "The quick brown fox jumps over the lazy dog.")
  '(" <!-- The quick brown fox jumps over the lazy dog."
    "   -- |"
    "   -- The quick brown fox jumps over the lazy dog.  -->")
  "M-( < C-n C-p C-b RET")

(ipe-test-def-kbd infix-open-backward-blank-2 ()
  "Test `insert-pair-edit' OPEN backward x2 with a blank line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "|"
    "The quick brown fox jumps over the lazy dog.")
  '("  <!-- The quick brown fox jumps over the lazy dog."
    "    -- |"
    "    -- The quick brown fox jumps over the lazy dog.  -->")
  "M-( < C-n C-p C-b C-b RET")

(ipe-test-def-kbd infix-open-forward-1 ()
  "Test `insert-pair-edit' OPEN forward.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox |jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-p C-b M-f RET")

(ipe-test-def-kbd infix-open-forward-2 ()
  "Test `insert-pair-edit' OPEN forward x2.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox |jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-p C-b C-b M-f M-f RET")

(ipe-test-def-kbd infix-open-forward-3 ()
  "Test `insert-pair-edit' OPEN forward x3.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox |jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-p 3 C-b 3 M-f RET")

(ipe-test-def-kbd infix-open-forward-4 ()
  "Test `insert-pair-edit' OPEN forward x4.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox |jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-p C-u C-b C-u M-f RET")

(ipe-test-def-kbd infix-open-forward-16 ()
  "Test `insert-pair-edit' OPEN forward x16.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox |jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-p C-u C-u C-b C-u C-u M-f RET")

(ipe-test-def-kbd infix-open-forward-end ()
  "Test `insert-pair-edit' OPEN forward at the end of buffer.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog.|")
  '("<!-- The quick brown fox jumps over the lazy dog.|  -->")
  "M-( < M-f RET")

(ipe-test-def-kbd infix-open-forward-blank ()
  "Test `insert-pair-edit' OPEN forward with a blank line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "|"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!-- |  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-b M-f RET")

(ipe-test-def-kbd infix-open-forward-blank-2 ()
  "Test `insert-pair-edit' OPEN forward x2 with a blank line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "|"
    "The quick brown fox jumps over the lazy dog.")
  '("<!-- The quick brown fox jumps over the lazy dog."
    "  -- |"
    "  -- The quick brown fox jumps over the lazy dog.  -->")
  "M-( < C-n C-p M-f M-f RET")

(ipe-test-def-kbd infix-open-down-1 ()
  "Test `insert-pair-edit' OPEN down.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( < M-n RET")

(ipe-test-def-kbd infix-open-down-2 ()
  "Test `insert-pair-edit' OPEN down x2.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( < M-n M-n RET")

(ipe-test-def-kbd infix-open-down-3 ()
  "Test `insert-pair-edit' OPEN down x3.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < 3 M-n RET")

(ipe-test-def-kbd infix-open-down-4 ()
  "Test `insert-pair-edit' OPEN down x4.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
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
    "<!-- The quick brown fox jumps over the lazy dog.  -->")
  "M-( < C-u M-n RET")

(ipe-test-def-kbd infix-open-down-blank ()
  "Test `insert-pair-edit' OPEN down with a blank line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    "<!--   -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < M-n RET")

(ipe-test-def-kbd infix-open-down-blank-2 ()
  "Test `insert-pair-edit' OPEN down x2 with a blank line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox |jumps over the lazy dog."
    ""
    "<!-- The quick brown fox jumps over the lazy dog.  -->")
  "M-( < M-n M-n RET")

(ipe-test-def-kbd infix-open-down-offset ()
  "Test `insert-pair-edit' OPEN down with an offset.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog.  -->")
  "M-( < M-n RET")

(ipe-test-def-kbd infix-open-down-offset-beginning ()
  "Test `insert-pair-edit' OPEN down to offset beginning of line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("|The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog.")
  '("|The quick brown fox jumps over the lazy dog."
    "<!--   The quick brown fox jumps over the lazy dog.  -->")
  "M-( < M-n RET")

(ipe-test-def-kbd infix-open-down-offset-beginning-2 ()
  "Test `insert-pair-edit' OPEN down offset to beginning of line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("|  The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("|  The quick brown fox jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog.  -->")
  "M-( < M-n RET")

(ipe-test-def-kbd infix-open-end-1 ()
  "Test `insert-pair-edit' OPEN end.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog.")
  '("<!-- The quick brown fox |jumps over the lazy dog.  -->")
  "M-( < C-e M-e RET")

(ipe-test-def-kbd infix-open-end-2 ()
  "Test `insert-pair-edit' OPEN end.

Using a 'line (+ :infix) PAIR at end of buffer."
  ipe-test-infix-options
  nil
  "The quick brown fox jumps over the lazy dog.|"
  "<!-- The quick brown fox jumps over the lazy dog.|  -->"
  "M-( < C-e C-e M-e M-e RET")

(ipe-test-def-kbd infix-open-end-3 ()
  "Test `insert-pair-edit' OPEN end.

Using a 'line (+ :infix) PAIR at end of line."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("<!-- The quick brown fox |jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-e C-e M-e M-e RET")

(ipe-test-def-kbd infix-open-end-4 ()
  "Test `insert-pair-edit' OPEN end.

Using a 'line (+ :infix) PAIR with a numeric prefix."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("<!-- The quick brown fox |jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog.  -->")
  "M-( < C-n 2 M-e RET")

(ipe-test-def-kbd infix-close-start-1 ()
  "Test `insert-pair-edit' CLOSE start.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  "The quick brown fox |jumps over the lazy dog."
  "<!-- The quick brown fox |jumps over the lazy dog.  -->"
  "M-( < a M-a RET")

(ipe-test-def-kbd infix-close-start-2 ()
  "Test `insert-pair-edit' CLOSE start.

Using a 'line (+ :infix) PAIR at the start of buffer."
  ipe-test-infix-options
  nil
  "The quick brown fox jumps over the lazy dog."
  "<!-- |The quick brown fox jumps over the lazy dog.  -->"
  "M-( < a a M-a M-a RET")

(ipe-test-def-kbd infix-close-start-3 ()
  "Test `insert-pair-edit' CLOSE start.

Using a 'line (+ :infix) PAIR at the beginning of line."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("<!-- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox |jumps over the lazy dog.  -->")
  "M-( < C-p a a M-a M-a RET")

(ipe-test-def-kbd infix-close-start-4 ()
  "Test `insert-pair-edit' CLOSE start.

Using a 'line (+ :infix) PAIR with a numeric prefix."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!-- The quick brown fox |jumps over the lazy dog.  -->")
  "M-( < 2 a 2 M-a RET")

(ipe-test-def-kbd infix-close-up-1 ()
  "Test `insert-pair-edit' CLOSE up.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox |jumps over the lazy dog.")
  "M-( < M-p RET")

(ipe-test-def-kbd infix-close-up-2 ()
  "Test `insert-pair-edit' CLOSE up x2.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  "M-( < M-p M-p RET")

(ipe-test-def-kbd infix-close-up-3 ()
  "Test `insert-pair-edit' CLOSE up x3.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  "M-( < 3 M-p RET")

(ipe-test-def-kbd infix-close-up-4 ()
  "Test `insert-pair-edit' CLOSE up x4.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  '("<!-- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog.")
  "M-( < C-u M-p RET")

(ipe-test-def-kbd infix-close-up-blank ()
  "Test `insert-pair-edit' CLOSE up with a blank line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!--   -->"
    "The quick brown fox |jumps over the lazy dog.")
  "M-( < M-p RET")

(ipe-test-def-kbd infix-close-up-blank-2 ()
  "Test `insert-pair-edit' CLOSE up x2 with a blank line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    ""
    "The quick brown fox |jumps over the lazy dog.")
  '("<!-- The quick brown fox jumps over the lazy dog.  -->"
    ""
    "The quick brown fox |jumps over the lazy dog.")
  "M-( < M-p M-p RET")

(ipe-test-def-kbd infix-close-up-offset ()
  "Test `insert-pair-edit' CLOSE up with an offset.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("  The quick brown fox jumps over the lazy dog."
    "  The quick brown fox |jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog.")
  '("<!--   The quick brown fox jumps over the lazy dog.  -->"
    "  The quick brown fox |jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog.")
  "M-( < M-p RET")

(ipe-test-def-kbd infix-close-up-offset-beginning ()
  "Test `insert-pair-edit' CLOSE up offset to beginning of line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "|  The quick brown fox jumps over the lazy dog.")
  '("<!-- The quick brown fox jumps over the lazy dog.  -->"
    "|  The quick brown fox jumps over the lazy dog.")
  "M-( < M-p RET")

(ipe-test-def-kbd infix-close-up-offset-beginning-2 ()
  "Test `insert-pair-edit' CLOSE up to offset beginning of line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("  The quick brown fox jumps over the lazy dog."
    "|The quick brown fox jumps over the lazy dog.")
  '("<!--   The quick brown fox jumps over the lazy dog.  -->"
    "|The quick brown fox jumps over the lazy dog.")
  "M-( < M-p RET")

(ipe-test-def-kbd infix-close-backward-1 ()
  "Test `insert-pair-edit' CLOSE backward.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox |jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-p C-f M-b RET")

(ipe-test-def-kbd infix-close-backward-2 ()
  "Test `insert-pair-edit' CLOSE backward x2.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox |jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-p C-f C-f M-b M-b RET")

(ipe-test-def-kbd infix-close-backward-3 ()
  "Test `insert-pair-edit' CLOSE backward x3.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox |jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-p 3 C-f 3 M-b RET")

(ipe-test-def-kbd infix-close-backward-4 ()
  "Test `insert-pair-edit' CLOSE backward x4.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox |jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-p C-u C-f C-u M-b RET")

(ipe-test-def-kbd infix-close-backward-16 ()
  "Test `insert-pair-edit' CLOSE backward x16.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!-- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox |jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-p C-u C-u C-f C-u C-u M-b RET")

(ipe-test-def-kbd infix-close-backward-beginning ()
  "Test `insert-pair-edit' CLOSE backward at the beginning of buffer.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("|The quick brown fox jumps over the lazy dog.")
  '("<!-- |The quick brown fox jumps over the lazy dog.  -->")
  "M-( < M-b RET")

(ipe-test-def-kbd infix-close-backward-blank ()
  "Test `insert-pair-edit' CLOSE backward with a blank line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "|"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!-- |  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < M-b RET")

(ipe-test-def-kbd infix-close-backward-blank-2 ()
  "Test `insert-pair-edit' CLOSE backward x2 with a blank line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "|"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!-- |  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < M-b M-b RET")

(ipe-test-def-kbd infix-close-forward-1 ()
  "Test `insert-pair-edit' CLOSE forward.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!--  The quick brown fox jumps over the lazy dog."
    "  --  The quick brown fox |jumps over the lazy dog."
    "  --  The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-p C-f RET")

(ipe-test-def-kbd infix-close-forward-2 ()
  "Test `insert-pair-edit' CLOSE forward x2.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!--   The quick brown fox jumps over the lazy dog."
    "  --   The quick brown fox |jumps over the lazy dog."
    "  --   The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-p C-f C-f RET")

(ipe-test-def-kbd infix-close-forward-3 ()
  "Test `insert-pair-edit' CLOSE forward x3.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!--    The quick brown fox jumps over the lazy dog."
    "  --    The quick brown fox |jumps over the lazy dog."
    "  --    The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-p 3 C-f RET")

(ipe-test-def-kbd infix-close-forward-4 ()
  "Test `insert-pair-edit' CLOSE forward x4.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!--     The quick brown fox jumps over the lazy dog."
    "  --     The quick brown fox |jumps over the lazy dog."
    "  --     The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-p C-u C-f RET")

(ipe-test-def-kbd infix-close-forward-16 ()
  "Test `insert-pair-edit' CLOSE forward x16.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!--                 The quick brown fox jumps over the lazy dog."
    "  --                 The quick brown fox |jumps over the lazy dog."
    "  --                 The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-p C-u C-u C-f RET")

(ipe-test-def-kbd infix-close-forward-end ()
  "Test `insert-pair-edit' CLOSE forward at end of buffer.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  "The quick brown fox jumps over the lazy dog.|"
  "<!--  The quick brown fox jumps over the lazy dog|.  -->"
  "M-( < C-f RET")

(ipe-test-def-kbd infix-close-forward-blank ()
  "Test `insert-pair-edit' CLOSE forward with a blank line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "|"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "<!-- |   -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-f RET")

(ipe-test-def-kbd infix-close-forward-blank-2 ()
  "Test `insert-pair-edit' CLOSE forward x2 with a blank line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog."
    "|"
    "The quick brown fox jumps over the lazy dog.")
  '("<!--   The quick brown fox jumps over the lazy dog."
    "  --   |"
    "  --   The quick brown fox jumps over the lazy dog.  -->")
  "M-( < C-n C-p C-f C-f RET")

(ipe-test-def-kbd infix-close-down-1 ()
  "Test `insert-pair-edit' CLOSE down.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("<!-- The quick brown fox |jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n RET")

(ipe-test-def-kbd infix-close-down-2 ()
  "Test `insert-pair-edit' CLOSE down x2.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("<!-- The quick brown fox |jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n C-n RET")

(ipe-test-def-kbd infix-close-down-3 ()
  "Test `insert-pair-edit' CLOSE down x3.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("<!-- The quick brown fox |jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog.  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < 3 C-n RET")

(ipe-test-def-kbd infix-close-down-4 ()
  "Test `insert-pair-edit' CLOSE down x4.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("<!-- The quick brown fox |jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog.  -->")
  "M-( < C-u C-n RET")

(ipe-test-def-kbd infix-close-down-blank ()
  "Test `insert-pair-edit' CLOSE down with a blank line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("<!-- The quick brown fox |jumps over the lazy dog."
    "  --   -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < C-n RET")

(ipe-test-def-kbd infix-close-down-blank-2 ()
  "Test `insert-pair-edit' CLOSE down x2 with a blank line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    ""
    "The quick brown fox jumps over the lazy dog.")
  '("<!-- The quick brown fox |jumps over the lazy dog."
    "  -- "
    "  -- The quick brown fox jumps over the lazy dog.  -->")
  "M-( < C-n C-n RET")

(ipe-test-def-kbd infix-close-down-offset ()
  "Test `insert-pair-edit' CLOSE down with an offset.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("  The quick brown fox |jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog.")
  '("<!--   The quick brown fox |jumps over the lazy dog."
    "  --   The quick brown fox jumps over the lazy dog.  -->"
    "  The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog.")
  "M-( < C-n RET")

(ipe-test-def-kbd infix-close-down-offset-beginning ()
  "Test `insert-pair-edit' CLOSE down to offset beginning of line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("|The quick brown fox jumps over the lazy dog."
    "  The quick brown fox jumps over the lazy dog.")
  '("<!-- |The quick brown fox jumps over the lazy dog."
    "  --   The quick brown fox jumps over the lazy dog.  -->")
  "M-( < C-n RET")

(ipe-test-def-kbd infix-close-down-offset-beginning-2 ()
  "Test `insert-pair-edit' CLOSE down offset to beginning of line.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("|  The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("<!-- |  The quick brown fox jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog.  -->")
  "M-( < C-n RET")

(ipe-test-def-kbd infix-close-end-1 ()
  "Test `insert-pair-edit' CLOSE end.

Using a 'line (+ :infix) PAIR."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog.")
  '("<!-- The quick brown fox |jumps over the lazy dog."
    "  -->")
  "M-( < C-e RET")

(ipe-test-def-kbd infix-close-end-2 ()
  "Test `insert-pair-edit' CLOSE end.

Using a 'line (+ :infix) PAIR at the end of buffer."
  ipe-test-infix-options
  nil
  '("The quick brown fox jumps over the lazy dog.|")
  '("<!-- The quick brown fox jumps over the lazy dog.|"
    "  -->")
  "M-( < C-e C-e RET")

(ipe-test-def-kbd infix-close-end-3 ()
  "Test `insert-pair-edit' CLOSE end.

Using a 'line (+ :infix) PAIR at the end of line."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("<!-- The quick brown fox |jumps over the lazy dog."
    "  -- The quick brown fox jumps over the lazy dog."
    "  -->")
  "M-( < C-n C-e RET")

(ipe-test-def-kbd infix-close-end-4 ()
  "Test `insert-pair-edit' CLOSE end.

Using a 'line (+ :infix) PAIR with a numeric prefix."
  ipe-test-infix-options
  nil
  '("The quick brown fox |jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("<!-- The quick brown fox |jumps over the lazy dog."
    "  -->"
    "The quick brown fox jumps over the lazy dog.")
  "M-( < 2 C-e RET")

(provide 'ipe-test-infix)

;; ipe-test-infix.el ends here
