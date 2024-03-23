;;; ipe-test-contents.el --- Insert Pair Edit - CONTENT Editing Tests -*- lexical-binding: t; -*-
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
;; The tests within this file are used to test the functions which
;; edit the text between the OPEN and CLOSE strings of an 'Insert Pair
;; Edit' (ipe) PAIR when in `ipe-edit-mode'.  This is done via the
;; 'Edit CONTENTS' 'Insert Pair Edit' (ipe) `ipe-edit-mode' commands:
;;
;;   `ipe-edit--contents-kill'
;;   `ipe-edit--contents-copy'
;;   `ipe-edit--contents-paste'
;;   `ipe-edit--contents-replace'
;;   `ipe-edit--contents-upcase'
;;   `ipe-edit--contents-capitalize'
;;   `ipe-edit--contents-downcase'

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'ipe-test)

(defvar ipe-test-contents-options
  '((ipe-move-point-on-insert   nil)
    (ipe-prefix-moves-close-p   t)
    (ipe-edit--movement-keysets '(modifiers))
    (ipe-pairs
     '(("(" "(" ")")
       ("<" "<" ">" (:movement line))
       ("*" "" ""   (:movement line))))
    (ipe-mode-pairs nil))
  "Options used by `ipe-test-def-kbd' for `ipe-test-contents'.")

(ipe-test-def-kbd contents-kill-1 ()
  "Test `ipe-edit--contents-kill' function."
  ipe-test-contents-options
  nil
  "The quick brown |fox jumps over the lazy dog."
  "The quick brown (|) jumps over the lazy dog."
  "M-( ( C-k RET")

(ipe-test-def-kbd contents-kill-2 ()
  "Test `ipe-edit--contents-kill' function.

Delete all text."
  ipe-test-contents-options
  nil
  "The quick brown |fox jumps over the lazy dog."
  "<|>"
  "M-( < C-k")

(ipe-test-def-kbd contents-kill-3 ()
  "Test `ipe-edit--contents-kill' function.

Cursor at start of buffer."
  ipe-test-contents-options
  nil
  "|The quick brown (fox) jumps over the lazy dog."
  "|The quick brown () jumps over the lazy dog."
  "C-u M-( ( C-k RET")

(ipe-test-def-kbd contents-kill-4 ()
  "Test `ipe-edit--contents-kill' function.

Cursor at end of buffer."
  ipe-test-contents-options
  nil
  "The quick brown (fox) jumps over the lazy dog.|"
  "The quick brown () jumps over the lazy dog.|"
  "C-u M-( ( C-k RET")

(ipe-test-def-kbd contents-copy-1 ()
  "Test `ipe-edit--contents-copy' function."
  ipe-test-contents-options
  nil
  "The quick brown |fox jumps over the lazy dog."
  "The quick brown (fox|fox) jumps over the lazy dog."
  "M-( ( M-w RET C-y")

(ipe-test-def-kbd contents-copy-2 ()
  "Test `ipe-edit--contents-copy' function."
  ipe-test-contents-options
  nil
  "|The quick brown fox jumps over the lazy dog."
  "The quick brown fox jumps over the lazy dog.|The quick brown fox jumps over the lazy dog."
  "M-( < M-w C-d C-y")

(ipe-test-def-kbd contents-copy-3 ()
  "Test `ipe-edit--contents-copy' function.

Cursor at start of buffer."
  ipe-test-contents-options
  nil
  "|The quick brown (fox) jumps over the lazy dog."
  "fox|The quick brown (fox) jumps over the lazy dog."
  "C-u M-( ( M-w RET C-y")

(ipe-test-def-kbd contents-copy-4 ()
  "Test `ipe-edit--contents-copy' function.

Cursor at end of buffer."
  ipe-test-contents-options
  nil
  "The quick brown (fox) jumps over the lazy dog.|"
  "The quick brown (fox) jumps over the lazy dog.fox|"
  "C-u M-( ( M-w RET C-y")

(ipe-test-def-kbd contents-paste-1 ()
  "Test `ipe-edit--contents-paste' function."
  ipe-test-contents-options
  nil
  "The quick brown |fox jumps over the lazy dog."
  "The quick brown |fox (fox) over the lazy dog."
  "M-( ( M-w C-f M-f C-y RET")

(ipe-test-def-kbd contents-paste-2 ()
  "Test `ipe-edit--contents-paste' function."
  ipe-test-contents-options
  nil
  '("The quick brown |fox jumps over the lazy dog."
    "xxx"
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( < M-w M-n C-y C-d")

(ipe-test-def-kbd contents-paste-3 ()
  "Test `ipe-edit--contents-paste' function.

Cursor at beginning of buffer."
  ipe-test-contents-options
  nil
  "|The quick brown (fox) jumps over the lazy dog."
  "|The quick brown fox (fox) over the lazy dog."
  "C-u M-( ( M-w C-f M-f C-y RET")

(ipe-test-def-kbd contents-paste-4 ()
  "Test `ipe-edit--contents-paste' function.

Cursor at end of buffer."
  ipe-test-contents-options
  nil
  "The quick brown (fox) jumps over the lazy dog.|"
  "The quick brown fox (fox) over the lazy dog.|"
  "C-u M-( ( M-w C-f M-f C-y RET")

(ipe-test-def-kbd contents-replace-1 ()
  "Test `ipe-edit--contents-replace' function."
  ipe-test-contents-options
  nil
  "The quick brown |fox jumps over the lazy dog."
  "The quick brown (|dog) jumps over the lazy dog."
  "M-( ( % x C-a C-k dog RET RET")

(ipe-test-def-kbd contents-replace-2 ()
  "Test `ipe-edit--contents-replace' function.

Replace all with empty text."
  ipe-test-contents-options
  nil
  "The quick brown |fox jumps over the lazy dog."
  "<|>"
  "M-( < % x C-a C-k RET")

(ipe-test-def-kbd contents-replace-3 ()
  "Test `ipe-edit--contents-replace' function.

Replace with a shorter string."
  ipe-test-contents-options
  nil
  "The quick brown fo|x jumps over the lazy dog."
  "The quick brown (x|) jumps over the lazy dog."
  "M-( ( % x C-a C-k x RET RET")

(ipe-test-def-kbd contents-replace-4 ()
  "Test `ipe-edit--contents-replace' function.

Replace with a longer string."
  ipe-test-contents-options
  nil
  "The quick brown fo|x jumps over the lazy dog."
  "The quick brown (wo|lf) jumps over the lazy dog."
  "M-( ( % x C-a C-k wolf RET RET")

(ipe-test-def-kbd contents-replace-5 ()
  "Test `ipe-edit--contents-replace' function.

Replace with a longer string."
  ipe-test-contents-options
  nil
  "The quick brown fox| jumps over the lazy dog."
  "The quick brown (Fantastic_Mr_Fox|) jumps over the lazy dog."
  "M-( ( % x C-a C-k Fantastic_Mr_Fox RET RET")

(ipe-test-def-kbd contents-replace-6 ()
  "Test `ipe-edit--contents-replace' function.

Replace entire buffer.

Cursor at beginning of buffer."
  ipe-test-contents-options
  nil
  "|12345"
  "|67890"
  "M-( * % x C-a C-k 67890 RET RET")

(ipe-test-def-kbd contents-replace-7 ()
  "Test `ipe-edit--contents-replace' function.

Replace entire buffer.

Cursor in middle of buffer."
  ipe-test-contents-options
  nil
  "12|345"
  "67|890"
  "M-( * % x C-a C-k 67890 RET RET")

(ipe-test-def-kbd contents-replace-8 ()
  "Test `ipe-edit--contents-replace' function.

Replace entire buffer.

Cursor at end of buffer."
  ipe-test-contents-options
  nil
  "12345|"
  "67890|"
  "M-( * % x C-a C-k 67890 RET RET")

(ipe-test-def-kbd contents-upcase-1 ()
  "Test `ipe-edit--contents-upcase' function."
  ipe-test-contents-options
  nil
  "The quick brown |fox jumps over the lazy dog."
  "<THE QUICK BROWN |FOX JUMPS OVER THE LAZY DOG.>"
  "M-( < M-u RET")

(ipe-test-def-kbd contents-capitalize-1 ()
  "Test `ipe-edit--contents-capitalize' function."
  ipe-test-contents-options
  nil
  "The quick brown |fox jumps over the lazy dog."
  "<The Quick Brown |Fox Jumps Over The Lazy Dog.>"
  "M-( < M-c RET")

(ipe-test-def-kbd contents-downcase-1 ()
  "Test `ipe-edit--contents-downcase' function."
  ipe-test-contents-options
  nil
  "The QUICK Brown |FOX Jumps OVER The LAZY Dog."
  "<the quick brown |fox jumps over the lazy dog.>"
  "M-( < M-l RET")

(provide 'ipe-test-contents)

;;; ipe-test-contents.el ends here
