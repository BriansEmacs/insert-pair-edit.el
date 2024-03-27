;;; ipe-test-options.el --- Insert Pair Edit - Global Option Tests -*- lexical-binding: t; -*-
;; Copyright (C) 2023 Brian Kavanagh

;; Author: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Maintainer: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Created: 28 June, 2020
;; Version: 1.1
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
;; The tests within this file are used to test the processing
;; performed by 'Insert Pair Edit' when various Global Options are
;; set.  This is done by providing alternate values for the following
;; 'Insert Pair Edit' `customize'-able variables:
;;
;;   `ipe-edit-movement-keysets'
;;   `ipe-mnemonic-prompt-shortcut-p'
;;   `ipe-move-point-on-insert'
;;   `ipe-prefix-moves-close-p'
;;   `ipe-set-mark-on-insert'
;;

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'ipe)
(require 'ipe-test)

;; 'Display Only' options - not tested.
;;
;; (ipe-elide-description       0)
;; (ipe-empty-close-string      ")")
;; (ipe-empty-open-string       "(")
;; (ipe-menu-display-in-edit-p  t)
;; (ipe-menu-support-p          t)
;; (ipe-mouse-support-p         t)
;; (ipe-pair-sort               'mnemonic)
;;
(defvar ipe-test-options-base
  '(
    (ipe-edit--movement-keysets     '(modifiers))
    (ipe-mnemonic-prompt-shortcut-p t)
    (ipe-move-point-on-insert       'resume)
    (ipe-prefix-moves-close-p       nil)
    (ipe-set-mark-on-insert         nil)
    (ipe-pairs
     '(("(" "("     ")")
       ("<" "<"     ">"     (:movement   char))
       ("{" "{"     "}"     (:movement   word))
       ("[" "["     "]"     (:movement   line))
       ("0" "(((((" ")))))")
       ("1" "(((((" ")))))" (:move-point open-beg))
       ("2" "(((((" ")))))" (:move-point open-end))
       ("3" "(((((" ")))))" (:move-point close-beg))
       ("4" "(((((" ")))))" (:move-point close-end))
       ("5" "(((((" ")))))" (:move-point 3))
       ("6" "(((((" ")))))" (:move-point -3))
       ("7" "("     ")"     (:auto-insert t))))
    (ipe-mode-pairs           nil))
  "Options used by `ipe-test-def-kbd' for `ipe-test-options'.")

(ipe-test-def-kbd options-movement-keyset-modifiers-1 ()
  "Test `ipe-insert-pair-edit' with `ipe-edit-movement-keysets' 'modifiers.

Moving outwards."
  (ipe--alist-update ipe-test-options-base 'ipe-edit--movement-keysets ''(modifiers))
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick (brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps) over the lazy dog.")
  "M-( ( C-n C-f C-b C-p RET")

(ipe-test-def-kbd options-movement-keyset-modifiers-2 ()
  "Test `ipe-insert-pair-edit' with `ipe-edit-movement-keysets' 'modifiers.

Moving inwards."
  (ipe--alist-update ipe-test-options-base 'ipe-edit--movement-keysets ''(modifiers))
  nil
  '("The quick (brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps) over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown (|fox) jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ( M-n M-f M-b M-p RET")

(ipe-test-def-kbd options-movement-keyset-alpha-1 ()
  "Test `ipe-insert-pair-edit' with `ipe-edit-movement-keysets' 'alpha.

Test that the 'alpha characters do NOT move OPEN and close when the
`ipe-edit--movement-keysets' is NOT 'alpha.

Movement outwards."
  (ipe--alist-update ipe-test-options-base 'ipe-edit--movement-keysets ''(modifiers))
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown (|fox) jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( n f b p RET")

(ipe-test-def-kbd options-movement-keyset-alpha-2 ()
  "Test `ipe-insert-pair-edit' with `ipe-edit-movement-keysets' 'with alpha.

Test that the 'alpha characters do NOT move OPEN and close when the
`ipe-edit--movement-keysets' is NOT 'alpha.

Movement inwards."
  (ipe--alist-update ipe-test-options-base 'ipe-edit--movement-keysets ''(modifiers))
  nil
  '("The quick (brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps) over the lazy dog.")
  '("The quick (brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps) over the lazy dog.")
  "C-u M-( ( N F B P RET")

(ipe-test-def-kbd options-movement-keyset-alpha-3 ()
  "Test `ipe-insert-pair-edit' with `ipe-edit-movement-keysets' 'alpha.

Test that the 'alpha characters do move OPEN and CLOSE when the
`ipe-edit--movement-keysets' _is_ 'alpha.

Movement outwards."
  (ipe--alist-update ipe-test-options-base 'ipe-edit--movement-keysets ''(alpha))
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick (brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps) over the lazy dog.")
  "M-( ( n f b p RET")

(ipe-test-def-kbd options-movement-keyset-alpha-4 ()
  "Test `ipe-insert-pair-edit' with `ipe-edit-movement-keysets' 'alpha.

Test that the 'alpha characters do move OPEN and CLOSE when the
`ipe-edit--movement-keysets' _is_ 'alpha.

Movement inwards."
  (ipe--alist-update ipe-test-options-base 'ipe-edit--movement-keysets ''(alpha))
  nil
  '("The quick (brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps) over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown (|fox) jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ( N F B P RET")

(ipe-test-def-kbd options-movement-keyset-arrow-1 ()
  "Test `ipe-insert-pair-edit' with `ipe-edit-movement-keysets' 'arrow.

Test that the 'arrow characters do NOT move OPEN and close when the
`ipe-edit--movement-keysets' is NOT 'arrow.

Movement outwards."
  (ipe--alist-update ipe-test-options-base 'ipe-edit--movement-keysets ''(modifiers))
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown (|fox) jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( <down> <right> <left> <up> RET")

(ipe-test-def-kbd options-movement-keyset-arrow-2 ()
  "Test `ipe-insert-pair-edit' with `ipe-edit-movement-keysets' 'arrow.

Test that the 'arrow characters do NOT move OPEN and close when the
`ipe-edit--movement-keysets' is NOT 'arrow.

Movement inwards."
  (ipe--alist-update ipe-test-options-base 'ipe-edit--movement-keysets ''(modifiers))
  nil
  '("The quick (brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps) over the lazy dog.")
  '("The quick (brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps) over the lazy dog.")
  "C-u M-( ( <C-down> <C-right> <C-left> <C-up> RET")

(ipe-test-def-kbd options-movement-keyset-arrow-3 ()
  "Test `ipe-insert-pair-edit' with `ipe-edit-movement-keysets' 'arrow.

Test that the 'arrow characters do move OPEN and CLOSE when the
`ipe-edit--movement-keysets' _is_ 'arrow.

Movement outwards."
  (ipe--alist-update ipe-test-options-base 'ipe-edit--movement-keysets ''(arrow))
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick (brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps) over the lazy dog.")
  "M-( ( <down> <right> <left> <up> RET")

(ipe-test-def-kbd options-movement-keyset-arrow-4 ()
  "Test `ipe-insert-pair-edit' with `ipe-edit-movement-keysets' 'arrow.

Test that the 'arrow characters do move OPEN and CLOSE when the
`ipe-edit--movement-keysets' _is_ 'arrow.

Movement inwards."
  (ipe--alist-update ipe-test-options-base 'ipe-edit--movement-keysets ''(arrow))
  nil
  '("The quick (brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps) over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown (|fox) jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ( <C-down> <C-right> <C-left> <C-up> RET")

(ipe-test-def-kbd options-movement-keyset-wasd-1 ()
  "Test `ipe-insert-pair-edit' with `ipe-edit-movement-keysets' 'wasd.

Test that the 'wasd characters do NOT move OPEN and close when the
`ipe-edit--movement-keysets' is NOT 'wasd.

Movement outwards."
  (ipe--alist-update ipe-test-options-base 'ipe-edit--movement-keysets ''(modifiers))
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown (|fox) jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( d a w RET")

(ipe-test-def-kbd options-movement-keyset-wasd-2 ()
  "Test `ipe-insert-pair-edit' with `ipe-edit-movement-keysets' 'wasd.

Test that the 'wasd characters do NOT move OPEN and close when the
`ipe-edit--movement-keysets' is NOT 'wasd.

Movement inwards."
  (ipe--alist-update ipe-test-options-base 'ipe-edit--movement-keysets ''(modifiers))
  nil
  '("The quick (brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps) over the lazy dog.")
  '("The quick (brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps) over the lazy dog.")
  "C-u M-( ( S D A W RET")

(ipe-test-def-kbd options-movement-keyset-wasd-3 ()
  "Test `ipe-insert-pair-edit' with `ipe-edit-movement-keysets' 'wasd.

Test that the 'wasd characters do move OPEN and CLOSE when the
`ipe-edit--movement-keysets' _is_ 'wasd.

Movement outwards."
  (ipe--alist-update ipe-test-options-base 'ipe-edit--movement-keysets ''(wasd))
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick (brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps) over the lazy dog.")
  "M-( ( s d a w RET")

(ipe-test-def-kbd options-movement-keyset-wasd-4 ()
  "Test `ipe-insert-pair-edit' with `ipe-edit-movement-keysets' 'wasd.

Test that the 'wasd characters do move OPEN and CLOSE when the
`ipe-edit--movement-keysets' _is_ 'wasd.

Movement inwards."
  (ipe--alist-update ipe-test-options-base 'ipe-edit--movement-keysets ''(wasd))
  nil
  '("The quick (brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps) over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown (|fox) jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ( S D A W RET")

(ipe-test-def-kbd options-movement-keyset-vi-1 ()
  "Test `ipe-insert-pair-edit' with `ipe-edit-movement-keysets' 'vi.

Test that the 'vi characters do NOT move OPEN and close when the
`ipe-edit--movement-keysets' is NOT 'vi.

Movement outwards."
  (ipe--alist-update ipe-test-options-base 'ipe-edit--movement-keysets ''(modifiers))
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown (|fox) jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "M-( ( j l h k RET")

(ipe-test-def-kbd options-movement-keyset-vi-2 ()
  "Test `ipe-insert-pair-edit' with `ipe-edit-movement-keysets' 'vi.

Test that the 'vi characters do NOT move OPEN and close when the
`ipe-edit--movement-keysets' is NOT 'vi.

Movement inwards."
  (ipe--alist-update ipe-test-options-base 'ipe-edit--movement-keysets ''(modifiers))
  nil
  '("The quick (brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps) over the lazy dog.")
  '("The quick ("
    "|) over the lazy dog.")
  "C-u M-( ( J L H K RET")

(ipe-test-def-kbd options-movement-keyset-vi-3 ()
  "Test `ipe-insert-pair-edit' with `ipe-edit-movement-keysets' 'vi.

Test that the 'vi characters do move OPEN and CLOSE when the
`ipe-edit--movement-keysets' _is_ 'vi.

Movement outwards."
  (ipe--alist-update ipe-test-options-base 'ipe-edit--movement-keysets ''(vi))
  nil
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  '("The quick (brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps) over the lazy dog.")
  "M-( ( j l h k RET")

(ipe-test-def-kbd options-movement-keyset-vi-4 ()
  "Test `ipe-insert-pair-edit' with `ipe-edit-movement-keysets' 'vi.

Test that the 'vi characters do move OPEN and CLOSE when the
`ipe-edit--movement-keysets' _is_ 'vi.

Movement inwards."
  (ipe--alist-update ipe-test-options-base 'ipe-edit--movement-keysets ''(vi))
  nil
  '("The quick (brown fox jumps over the lazy dog."
    "The quick brown |fox jumps over the lazy dog."
    "The quick brown fox jumps) over the lazy dog.")
  '("The quick brown fox jumps over the lazy dog."
    "The quick brown (|fox) jumps over the lazy dog."
    "The quick brown fox jumps over the lazy dog.")
  "C-u M-( ( J L H K RET")

(ipe-test-def-kbd options-mnemonic-prompt-shortcut-1 ()
  "Test `ipe-insert-pair-edit' with `ipe-mnemonic-prompt-shortcut-p' on."
  (ipe--alist-update ipe-test-options-base 'ipe-mnemonic-prompt-shortcut-p t)
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jumps) over the lazy dog."
  "M-( ( RET")

(ipe-test-def-kbd options-mnemonic-prompt-shortcut-2 ()
  "Test `ipe-insert-pair-edit' with `ipe-mnemonic-prompt-shortcut-p' off."
  (ipe--alist-update ipe-test-options-base 'ipe-mnemonic-prompt-shortcut-p nil)
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jumps) over the lazy dog."
  "M-( ( RET RET")

(ipe-test-def-kbd options-move-point-on-insert-1 ()
  "Test `ipe-insert-pair-edit' with `ipe-move-point-on-insert' 'resume."
  (ipe--alist-update ipe-test-options-base 'ipe-move-point-on-insert ''resume)
  nil
  "The quick brown fox jum|ps over the lazy dog."
  "The quick brown fox (jum|ps) over the lazy dog."
  "M-( ( RET")

(ipe-test-def-kbd options-move-point-on-insert-2 ()
  "Test `ipe-insert-pair-edit' with `ipe-move-point-on-insert' 'open-beg."
  (ipe--alist-update ipe-test-options-base 'ipe-move-point-on-insert ''open-beg)
  nil
  "The quick brown fox jum|ps over the lazy dog."
  "The quick brown fox |(jumps) over the lazy dog."
  "M-( ( RET")

(ipe-test-def-kbd options-move-point-on-insert-3 ()
  "Test `ipe-insert-pair-edit' with `ipe-move-point-on-insert' 'open-end."
  (ipe--alist-update ipe-test-options-base 'ipe-move-point-on-insert ''open-end)
  nil
  "The quick brown fox jum|ps over the lazy dog."
  "The quick brown fox (|jumps) over the lazy dog."
  "M-( ( RET")

(ipe-test-def-kbd options-move-point-on-insert-4 ()
  "Test `ipe-insert-pair-edit' with `ipe-move-point-on-insert' 'close-beg."
  (ipe--alist-update ipe-test-options-base 'ipe-move-point-on-insert ''close-beg)
  nil
  "The quick brown fox jum|ps over the lazy dog."
  "The quick brown fox (jumps|) over the lazy dog."
  "M-( ( RET")

(ipe-test-def-kbd options-move-point-on-insert-5 ()
  "Test `ipe-insert-pair-edit' with `ipe-move-point-on-insert' 'close-end."
  (ipe--alist-update ipe-test-options-base 'ipe-move-point-on-insert ''close-end)
  nil
  "The quick brown fox jum|ps over the lazy dog."
  "The quick brown fox (jumps)| over the lazy dog."
  "M-( ( RET")

(ipe-test-def-kbd options-move-point-open-beg ()
  "Test `ipe-insert-pair-edit' with `ipe-move-point-on-insert' 'open-beg."
  (ipe--alist-update ipe-test-options-base 'ipe-move-point-on-insert ''open-beg)
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |(((((jumps))))) over the lazy dog."
  "M-( 0 RET")

(ipe-test-def-kbd options-move-point-open-end ()
  "Test `ipe-insert-pair-edit' with `ipe-move-point-on-insert' 'open-end."
  (ipe--alist-update ipe-test-options-base 'ipe-move-point-on-insert ''open-end)
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (((((|jumps))))) over the lazy dog."
  "M-( 0 RET")

(ipe-test-def-kbd options-move-point-close-beg ()
  "Test `ipe-insert-pair-edit' with `ipe-move-point-on-insert' 'close-beg."
  (ipe--alist-update ipe-test-options-base 'ipe-move-point-on-insert ''close-beg)
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (((((jumps|))))) over the lazy dog."
  "M-( 0 RET")

(ipe-test-def-kbd options-move-point-close-end ()
  "Test `ipe-insert-pair-edit' with `ipe-move-point-on-insert' 'close-end."
  (ipe--alist-update ipe-test-options-base 'ipe-move-point-on-insert ''close-end)
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (((((jumps)))))| over the lazy dog."
  "M-( 0 RET")

(ipe-test-def-kbd options-move-point-positive-number ()
  "Test `ipe-insert-pair-edit' with `ipe-move-point-on-insert' = 3."
  (ipe--alist-update ipe-test-options-base 'ipe-move-point-on-insert 3)
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (((|((jumps))))) over the lazy dog."
  "M-( 0 RET")

(ipe-test-def-kbd options-move-point-negative-number ()
  "Test `ipe-insert-pair-edit' with `ipe-move-point-on-insert' = -3."
  (ipe--alist-update ipe-test-options-base 'ipe-move-point-on-insert -3)
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (((((jumps)))|)) over the lazy dog."
  "M-( 0 RET")

(ipe-test-def-kbd options-prefix-moves-close-on-1 ()
  "Test `ipe-insert-pair-edit' with `ipe-prefix-prefix-moves-close-p' on.

Movement (x2)."
  (ipe--alist-update ipe-test-options-base 'ipe-prefix-moves-close-p t)
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jumps over) the lazy dog."
  "M-2 M-( ( RET")

(ipe-test-def-kbd options-prefix-moves-close-on-2 ()
  "Test `ipe-insert-pair-edit' with `ipe-prefix-prefix-moves-close-p' on.

Movement (x4)."
  (ipe--alist-update ipe-test-options-base 'ipe-prefix-moves-close-p t)
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jumps over the lazy) dog."
  "M-4 M-( ( RET")

(ipe-test-def-kbd options-prefix-moves-close-off-1 ()
  "Test `ipe-insert-pair-edit' with `ipe-prefix-prefix-moves-close-p' on.

Movement (x2)."
  (ipe--alist-update ipe-test-options-base 'ipe-prefix-moves-close-p nil)
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown (fox |jumps) over the lazy dog."
  "M-2 M-( ( RET")

(ipe-test-def-kbd options-prefix-moves-close-off-2 ()
  "Test `ipe-insert-pair-edit' with `ipe-prefix-prefix-moves-close-p' on.

Movement (x4)."
  (ipe--alist-update ipe-test-options-base 'ipe-prefix-moves-close-p nil)
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The (quick brown fox |jumps) over the lazy dog."
  "M-4 M-( ( RET")

(ipe-test-def-kbd options-set-mark-nil ()
  "Test `ipe-insert-pair-edit' with `ipe-set-mark-on-insert' nil."
  (ipe--alist-update ipe-test-options-base 'ipe-set-mark-on-insert nil)
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |jumps (((((over))))) the lazy dog."
  "C-SPC M-( 0 M-f RET C-w")

(ipe-test-def-kbd options-set-mark-open-beg ()
  "Test `ipe-insert-pair-edit' with `ipe-set-mark-on-insert' 'open-beg."
  (ipe--alist-update ipe-test-options-base 'ipe-set-mark-on-insert ''open-beg)
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |(((((over))))) the lazy dog."
  "M-( 0 M-f RET C-w")

(ipe-test-def-kbd options-set-mark-open-end ()
  "Test `ipe-insert-pair-edit' with `ipe-set-mark-on-insert' 'open-end."
  (ipe--alist-update ipe-test-options-base 'ipe-set-mark-on-insert ''open-end)
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |over))))) the lazy dog."
  "M-( 0 M-f RET C-w")

(ipe-test-def-kbd options-set-mark-close-beg ()
  "Test `ipe-insert-pair-edit' with `ipe-set-mark-on-insert' 'close-beg."
  (ipe--alist-update ipe-test-options-base 'ipe-set-mark-on-insert ''close-beg)
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |))))) the lazy dog."
  "M-( 0 M-f RET C-w")

(ipe-test-def-kbd options-set-mark-close-end ()
  "Test `ipe-insert-pair-edit' with `ipe-set-mark-on-insert' 'close-end."
  (ipe--alist-update ipe-test-options-base 'ipe-set-mark-on-insert ''close-end)
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox | the lazy dog."
  "M-( 0 M-f RET C-w")

;; -------------------------------------------------------------------
;; Per PAIR Definition options.

(ipe-test-def-kbd options-move-point-override-open-beg ()
  "Test `ipe-insert-pair-edit' with :move-point 'open-beg."
  ipe-test-options-base
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox |(((((jumps))))) over the lazy dog."
  "M-( 1 RET")

(ipe-test-def-kbd options-move-point-override-open-beg-empty ()
  "Test `ipe-insert-pair-edit' with :move-point 'open-beg.

In empty buffer."
  ipe-test-options-base
  nil
  "|"
  "|((((()))))"
  "M-( 1")

(ipe-test-def-kbd options-move-point-override-open-end ()
  "Test `ipe-insert-pair-edit' with :move-point 'open-end."
  ipe-test-options-base
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (((((|jumps))))) over the lazy dog."
  "M-( 2 RET")

(ipe-test-def-kbd options-move-point-override-open-end-empty ()
  "Test `ipe-insert-pair-edit' with :move-point 'open-end.

In empty buffer."
  ipe-test-options-base
  nil
  "|"
  "(((((|)))))"
  "M-( 2")

(ipe-test-def-kbd options-move-point-override-close-beg ()
  "Test `ipe-insert-pair-edit' with :move-point 'close-beg."
  ipe-test-options-base
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (((((jumps|))))) over the lazy dog."
  "M-( 3 RET")

(ipe-test-def-kbd options-move-point-override-close-beg-empty ()
  "Test `ipe-insert-pair-edit' with :move-point 'close-beg.

In empty buffer."
  ipe-test-options-base
  nil
  "|"
  "(((((|)))))"
  "M-( 3")

(ipe-test-def-kbd options-move-point-override-close-end ()
  "Test `ipe-insert-pair-edit' with :move-point 'close-end."
  ipe-test-options-base
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (((((jumps)))))| over the lazy dog."
  "M-( 4 RET")

(ipe-test-def-kbd options-move-point-override-close-end-empty ()
  "Test `ipe-insert-pair-edit' with :move-point 'close-end.

In empty buffer."
  ipe-test-options-base
  nil
  "|"
  "((((()))))|"
  "M-( 4")

(ipe-test-def-kbd options-move-point-override-positive-number ()
  "Test `ipe-insert-pair-edit' with :move-point positive number."
  ipe-test-options-base
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (((|((jumps))))) over the lazy dog."
  "M-( 5 RET")

(ipe-test-def-kbd options-move-point-override-negative-number ()
  "Test `ipe-insert-pair-edit' with :move-point negative number."
  ipe-test-options-base
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (((((jumps)))|)) over the lazy dog."
  "M-( 6 RET")

(ipe-test-def-kbd options-auto-insert ()
  "Test `ipe-insert-pair-edit' with :auto-insert on."
  ipe-test-options-base
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jumps) over the lazy dog."
  "M-( 7")

(ipe-test-def-kbd options-single-pair-1 ()
  "Test `ipe-insert-pair-edit' with single pair definition."
  (ipe--alist-update ipe-test-options-base 'ipe-pairs ''(("(" "(" ")")))
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jumps) over the lazy dog."
  "M-( RET")

(ipe-test-def-kbd options-single-pair-2 ()
  "Test `ipe-insert-pair-edit' with single pair (:auto-insert) definition."
  (ipe--alist-update ipe-test-options-base
		     'ipe-pairs
		     ''(("(" "(" ")" (:auto-insert t))))
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jumps) over the lazy dog."
  "M-(")

(ipe-test-def-kbd options-single-pair-3 ()
  "Test `ipe-insert-pair-edit' with single pair (:auto-insert) definition.

With (:auto-insert t) & `ipe-prefix-moves-close-p' = nil set."
  (ipe--alist-update
   (ipe--alist-update ipe-test-options-base
		      'ipe-pairs
		      ''(("(" "(" ")" (:auto-insert t))))
   'ipe-prefix-moves-close-p nil)
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown (fox |jumps) over the lazy dog."
  "M-2 M-(")

(ipe-test-def-kbd options-single-pair-4 ()
  "Test `ipe-insert-pair-edit' with single pair (:auto-insert) definition.

With (:auto-insert t) & `ipe-prefix-moves-close-p' set.

(This mimics the behaviour of `insert-parentheses'.)"
  (ipe--alist-update
   (ipe--alist-update ipe-test-options-base
		      'ipe-pairs
		      ''(("(" "(" ")" (:auto-insert t))))
   'ipe-prefix-moves-close-p t)
  nil
  "The quick brown fox |jumps over the lazy dog."
  "The quick brown fox (|jumps over) the lazy dog."
  "M-2 M-(")

(provide 'ipe-test-options)

;;; ipe-test-options.el ends here
