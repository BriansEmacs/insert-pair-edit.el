;;; ipe-test-defn.el --- Insert Pair Edit - Edit PAIR Definitions Tests
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
;; The tests within this file are used to test the 'on-the-fly'
;; editing of an 'Insert Pair Edit' (ipe) PAIR definition. This is
;; done via the 'Edit PAIR Definitions' 'Insert Pair Edit'
;; `ipe-edit-mode' commands:
;;
;;   `ipe-defn--add-pair'
;;   `ipe-defn--delete-pair'
;;   `ipe-defn--add-mode-pair'
;;   `ipe-defn--delete-mode-pair'

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'ipe-test)

(setq ipe-test-defn-options
      '((ipe-move-point-on-insert   nil)
        (ipe-prefix-moves-close-p   t)
        (ipe-edit--movement-keysets '(modifiers))
        (ipe-pairs                  '(("(" "(" ")")
                                      ("[" "[" "]")
                                      ("<" "<" ">")))
        (ipe-emacs-lisp-pairs       '(("<" "**" "**")))
        (ipe-mode-pairs             '((html-mode (("<" "&lt;" "&gt;")))
                                      (emacs-lisp-mode ipe-emacs-lisp-pairs)))))

(defun ipe-test-defn--setup ()
  "Override `customize-set-variable' / `customize-save-customized'.

This function is used to provide alternative 'empty' functions for the
'customize-set-variable' / 'customize-save-customized' functions when
running tests of the 'ipe-defn--*' functions so that the tests to not
permanently write changes to the custom.el files."
  (fset 'ipe-test-defn--customize-set-variable
        (symbol-function 'customize-set-variable))
  (fset 'customize-set-variable (lambda (x y)))
  (fset 'ipe-test-defn--customize-save-customized
        (symbol-function 'customize-save-customized))
  (fset 'customize-save-customized (lambda ()))
  (when (functionp 'icy-mode)
    (funcall 'icy-mode -1)))

(defun ipe-test-defn--teardown ()
  "Restore `customize-set-variable' / `customize-save-customized'.

This functions restores the `customize-set-variable' /
`customize-save-customized' functions overridden by
`ipe-test-defn--setup' to their original values."
  (fset 'customize-set-variable
        (symbol-function 'ipe-test-defn--customize-set-variable))
  (fset 'customize-save-customized
        (symbol-function 'ipe-test-defn--customize-save-customized)))

(setq ipe-test-defn--teardown 'ipe-test-defn--teardown)

(ipe-test-def-kbd defn-add-pair-1 ()
  "Test `insert-pair-edit' `ipe-defn--add-pair' function."
  ipe-test-defn-options
  (lambda () (ipe-test-defn--setup))
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox {|jumps} over the lazy dog."
  "M-( ( C-+ { RET { RET } RET RET ( { RET"
  (lambda () (ipe-test-defn--teardown)))

(ipe-test-def-kbd defn-add-pair-2 ()
  "Test `insert-pair-edit' `ipe-defn--add-pair' function.

This test overwrites an existing MNEMONIC definition."
  ipe-test-defn-options
  (lambda () (ipe-test-defn--setup))
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox [|[jumps]] over the lazy dog."
  "M-( ( C-+ [ RET y RET x C-a C-k [[ RET x C-a C-k ]] RET n RET ( [ RET"
  (lambda () (ipe-test-defn--teardown)))

(ipe-test-def-kbd defn-delete-pair-1 ()
  "Test `insert-pair-edit' `ipe-defn--delete-pair' function."
  ipe-test-defn-options
  (lambda () (ipe-test-defn--setup))
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jumps) over the lazy dog."
  "M-( C-* [ RET y RET ( RET"
  (lambda () (ipe-test-defn--teardown)))

(ipe-test-def-kbd defn-add-mode-pair-1 ()
  "Test `insert-pair-edit' `ipe-defn--add-mode-pair' function.

This tests a mode that uses a separate variable to define its pair
definitions."
  ipe-test-defn-options
  (lambda () (emacs-lisp-mode) (ipe-test-defn--setup))
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox xx|xjumpsxxx over the lazy dog."
  "M-( < M-+ x C-a C-k emacs-lisp-mode RET t RET xxx RET xxx RET n RET ( t RET"
  (lambda () (ipe-test-defn--teardown)))

(ipe-test-def-kbd defn-add-mode-pair-2 ()
  "Test `insert-pair-edit' `ipe-defn--add-mode-pair' function.

This test overwrites an existing 'Mode-Specific' MNEMONIC definition.

This tests a mode that uses a separate variable to define its pair
definitions."
  ipe-test-defn-options
  (lambda () (emacs-lisp-mode) (ipe-test-defn--setup))
  "The quick brown fox |jumps over the lazy dog."
  "&lt;The quick brown fox |jumps over the lazy dog.&gt;"
  "M-( < M-+ x C-a C-k emacs-lisp-mode RET < RET y RET y RET C-a C-k &lt; RET C-a C-k &gt; RET y RET C-a C-k lines RET RET RET RET RET ( < RET"
  (lambda () (ipe-test-defn--teardown)))

(ipe-test-def-kbd defn-add-mode-pair-3 ()
  "Test `insert-pair-edit' `ipe-defn--add-mode-pair' function.

This tests a mode with 'inline' PAIR definitions."
  ipe-test-defn-options
  (lambda () (html-mode) (ipe-test-defn--setup))
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox xxx|jumpsxxx over the lazy dog."
  "M-( < M-+ x C-a C-k html-mode RET t RET xxx RET xxx RET RET ( t RET"
  (lambda () (ipe-test-defn--teardown)))

(ipe-test-def-kbd defn-add-mode-pair-4 ()
  "Test `insert-pair-edit' `ipe-defn--add-mode-pair' function.

This test overwrites an existing 'Mode-Specific' MNEMONIC definition.

This tests a mode with 'inline' PAIR definitions."
  ipe-test-defn-options
  (lambda () (html-mode) (ipe-test-defn--setup))
  "The quick brown fox |jumps over the lazy dog."
  "&lt;The quick brown fox |jumps over the lazy dog.&gt;"
  "M-( < M-+ x C-a C-k html-mode RET < RET y RET y RET C-a C-k &lt; RET C-a C-k &gt; RET y RET C-a C-k lines RET RET RET RET RET ( < RET"
  (lambda () (ipe-test-defn--teardown)))

(ipe-test-def-kbd defn-delete-mode-pair-1 ()
  "Test `insert-pair-edit' `ipe-defn--add-mode-pair' function.

This tests a mode that uses a separate variable to define its PAIR
definitions."
  ipe-test-defn-options
  (lambda () (emacs-lisp-mode) (ipe-test-defn--setup))
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox <|jumps> over the lazy dog."
  "M-( < M-* x C-a C-k emacs-lisp-mode RET < RET y RET ( < RET"
  (lambda () (ipe-test-defn--teardown)))

(ipe-test-def-kbd defn-delete-mode-pair-2 ()
  "Test `insert-pair-edit' `ipe-defn--delete-mode-pair' function.

This tests a mode with 'inline' PAIR definitions."
  ipe-test-defn-options
  (lambda () (html-mode) (ipe-test-defn--setup))
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox <|jumps> over the lazy dog."
  "M-( < M-* x C-a C-k html-mode RET < RET y RET ( < RET"
  (lambda () (ipe-test-defn--teardown)))

(provide 'ipe-test-defn)

;; ipe-test-defn.el ends here