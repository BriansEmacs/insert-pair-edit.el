;;; ipe-test-escape.el --- Insert Pair Edit - ESCAPE Processing Tests
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
;; The tests within this file are used to test the processing
;; performed by 'Insert Pair Edit' when using PAIR Definitions which
;; include an :escapes list.

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'ipe-test)

(setq ipe-test-escape-options
      '((ipe-move-point-on-insert   nil)
        (ipe--escapes-show-p        t)
        (ipe-prefix-moves-close-p   t)
        (ipe-edit--movement-keysets '(modifiers))
        (ipe-pairs
         '(
           ("'"  "'"      "'"        (:escapes (("'" "\\'"))))
           ("\"" "\""     "\""       (:escapes (("\"" "\\\"") ("\\" "\\\\"))))
           ("`"  "`"      "`"        (:escapes (("`" "\\`")   ("\\" "\\\\")) :movement char))
           ("t"  "<code>" "</code>"  (:escapes (("<" "&lt;")  (">" "&gt;") ("&" "&amp;"))))
           ("1"  ""       ""         (:escapes (("\\" "\\\\"))))
           ("2"  "("      ")"        (:escapes (("1"  "11") ("2" "22") ("12" "33"))))
           ("3"  "("      ")"        (:escapes (("12" "33") ("1" "11") ("2"  "22"))))
           ("4"  "("      ")"        (:escapes (("1" "22222") ("3" "4444") ("5"  "666") ("7" "88") ("9" "0"))))
           ("5"  "("      ")"        (:escapes (("22222" "1") ("4444" "3") ("666"  "5") ("88" "7") ("0" "9"))))
           ("6"  ""       ""         (:escapes (("11" "22222")) :movement line))
           ("7"  ""       ""         (:escapes (("22222" "11")) :movement line))
           ))
        (ipe-mode-pairs             nil)))

(ipe-test-def-kbd escape-insert-1 ()
  "Test `insert-pair-edit' with :escapes.

Test single quote (') escape."
  ipe-test-escape-options
  nil
  "The quick brown |fox' jumps over the lazy dog."
  "The quick brown '|fox\\' jumps' over the lazy dog."
  "M-( ' C-f RET")

(ipe-test-def-kbd escape-insert-2 ()
  "Test `insert-pair-edit' with :escapes.

Test multiple quote (') escapes."
  ipe-test-escape-options
  nil
  "The 'quick' brown |'fox' jumps 'over' the lazy dog."
  "'The \\'quick\\' brown |\\'fox\\' jumps \\'over\\' the' lazy dog."
  "M-( ' C-f C-f C-f C-b C-b C-b RET")

(ipe-test-def-kbd escape-insert-multiple-1 ()
  "Test `insert-pair-edit' with :escapes.

Test single HTML (&..;) :escapes."
  ipe-test-escape-options
  nil
  "The <quick> brown <|fox> jumps <over> the lazy dog."
  "The <quick> <code>brown &lt;|fox&gt; jumps</code> <over> the lazy dog."
  "M-( t C-f C-b RET")

(ipe-test-def-kbd escape-insert-multiple-2 ()
  "Test `insert-pair-edit' with :escapes.

Test multiple HTML (&..;) :escapes."
  ipe-test-escape-options
  nil
  "The <quick> & brown <|fox> jumps <over> the lazy dog."
  "<code>The &lt;quick&gt; &amp; brown &lt;|fox&gt; jumps &lt;over&gt; the</code> lazy dog."
  "M-( t C-f C-f C-f C-b C-b C-b RET")

(ipe-test-def-kbd escape-insert-escape-1 ()
  "Test `insert-pair-edit' with :escapes.

Test escaping :escapes."
  ipe-test-escape-options
  nil
  "The \"quick\" brown \\\"fo|x\\\" jumps \"over\" the lazy dog."
  "The \"quick\" \"brown \\\\\\\"fo|x\\\\\\\" jumps\" \"over\" the lazy dog."
  "M-( \" C-f C-b RET")

(ipe-test-def-kbd escape-insert-missing ()
  "Test `insert-pair-edit' with :escapes.

Test missing OPEN and CLOSE with :escapes."
  ipe-test-escape-options
  nil
  "@The \\quick brown \\fox jumps \\over the lazy dog.|"
  "The \\\\quick brown \\\\fox jumps \\\\over the lazy dog.|"
  "M-( 1 RET")

(ipe-test-def-kbd escape-insert-open-escape-1 ()
  "Test `insert-pair-edit' with :escapes.

Test :escapes at OPEN."
  ipe-test-escape-options
  nil
  "|```The quick brown fox jumps over the lazy dog."
  "`|\\````The quick brown fox jumps over the lazy dog."
  "M-( ` C-f RET")

(ipe-test-def-kbd escape-insert-open-escape-2 ()
  "Test `insert-pair-edit' with :escapes.

Test multiple (x2) :escapes at OPEN."
  ipe-test-escape-options
  nil
  "|```The quick brown fox jumps over the lazy dog."
  "`|\\`\\```The quick brown fox jumps over the lazy dog."
  "M-( ` C-f C-f RET")

(ipe-test-def-kbd escape-insert-open-escape-3 ()
  "Test `insert-pair-edit' with :escapes.

Test multiple (x3) :escapes at OPEN."
  ipe-test-escape-options
  nil
  "|```The quick brown fox jumps over the lazy dog."
  "`|\\`\\`\\``The quick brown fox jumps over the lazy dog."
  "M-( ` C-f C-f C-f RET")

(ipe-test-def-kbd escape-insert-close-escape-1 ()
  "Test `insert-pair-edit' with :escapes.

Test :escapes at CLOSE."
  ipe-test-escape-options
  nil
  "The quick brown fox jumps over the lazy dog.```|"
  "The quick brown fox jumps over the lazy dog.```\\`|`"
  "M-( ` C-b RET")

(ipe-test-def-kbd escape-insert-close-escape-2 ()
  "Test `insert-pair-edit' with :escapes.

Test multiple (x2) :escapes at CLOSE."
  ipe-test-escape-options
  nil
  "The quick brown fox jumps over the lazy dog.```|"
  "The quick brown fox jumps over the lazy dog.``\\`\\``|"
  "M-( ` C-b C-b RET")

(ipe-test-def-kbd escape-insert-close-escape-3 ()
  "Test `insert-pair-edit' with :escapes.

Test multiple (x3) :escapes at CLOSE."
  ipe-test-escape-options
  nil
  "The quick brown fox jumps over the lazy dog.```|"
  "The quick brown fox jumps over the lazy dog.`\\`\\`\\``|"
  "M-( ` C-b C-b C-b RET")

(ipe-test-def-kbd escape-insert-overlaps-1 ()
  "Test `insert-pair-edit' with :escapes.

Test :escapes which overlap."
  ipe-test-escape-options
  nil
  "The quick brown 12 fox |jumps 12 over 12 the lazy dog."
  "The quick brown (1122 fox |jumps 1122 over 1122) the lazy dog."
  "M-( 2 C-f C-f C-f C-b C-b RET")

(ipe-test-def-kbd escape-insert-overlaps-2 ()
  "Test `insert-pair-edit' with :escapes.

Test :escapes which overlap."
  ipe-test-escape-options
  nil
  "The quick brown 12 fox |jumps 12 over 12 the lazy dog."
  "The quick brown (33 fox |jumps 33 over 33) the lazy dog."
  "M-( 3 C-f C-f C-f C-b C-b RET")

(ipe-test-def-kbd escape-insert-sizes-1 ()
  "Test `insert-pair-edit' with :escapes.

Test changing sizes of :escapes.  Small -> large.

Empty OPEN & CLOSE.

Beginning of line."
  ipe-test-escape-options
  nil
  "|11"
  "|22222"
  "M-( 6 RET")

(ipe-test-def-kbd escape-insert-sizes-2 ()
  "Test `insert-pair-edit' with :escapes.

Test changing sizes of :escapes.  Large -> small.

Empty OPEN & CLOSE.

Beginning of line."
  ipe-test-escape-options
  nil
  "|22222"
  "|11"
  "M-( 7 RET")

(ipe-test-def-kbd escape-update-simple-1 ()
  "Test `insert-pair-edit-update' with :escapes.

Update simple PAIR, cursor before PAIR, abort."
  ipe-test-escape-options
  nil
  "|`\\``"
  "|`"
  "C-u M-( ` C-d")

(ipe-test-def-kbd escape-update-simple-2 ()
  "Test `insert-pair-edit-update' with :escapes.

Update Simple PAIR, cursor before PAIR, insert."
  ipe-test-escape-options
  nil
  "|`\\``"
  "|`\\``"
  "C-u M-( ` RET")

(ipe-test-def-kbd escape-update-simple-3 ()
  "Test `insert-pair-edit-update' with :escapes.

Update Simple PAIR, cursor before PAIR, move forward."
  ipe-test-escape-options
  nil
  "|`\\``"
  "|```"
  "C-u M-( ` M-f RET")

(ipe-test-def-kbd escape-update-simple-4 ()
  "Test `insert-pair-edit-update' with :escapes.

Update Simple PAIR, cursor before PAIR, move backward."
  ipe-test-escape-options
  nil
  "|`\\``"
  "|```"
  "C-u M-( ` M-b RET")

(ipe-test-def-kbd escape-update-simple-5 ()
  "Test `insert-pair-edit-update' with :escapes.

Update Simple PAIR, cursor after OPEN, abort."
  ipe-test-escape-options
  nil
  "`|\\``"
  "|`"
  "C-u M-( ` C-d")

(ipe-test-def-kbd escape-update-simple-6 ()
  "Test `insert-pair-edit-update' with :escapes.

Update Simple PAIR, cursor after OPEN, insert."
  ipe-test-escape-options
  nil
  "`|\\``"
  "`|\\``"
  "C-u M-( ` RET")

(ipe-test-def-kbd escape-update-simple-7 ()
  "Test `insert-pair-edit-update' with :escapes.

Update Simple PAIR, cursor after OPEN, move forward."
  ipe-test-escape-options
  nil
  "`|\\``"
  "|```"
  "C-u M-( ` M-f RET")

(ipe-test-def-kbd escape-update-simple-8 ()
  "Test `insert-pair-edit-update' with :escapes.

Update Simple PAIR, cursor after OPEN, move backward."
  ipe-test-escape-options
  nil
  "`|\\``"
  "`|``"
  "C-u M-( ` M-b RET")

(ipe-test-def-kbd escape-update-simple-9 ()
  "Test `insert-pair-edit-update' with :escapes.

Update Simple PAIR, cursor before CLOSE, abort."
  ipe-test-escape-options
  nil
  "`\\`|`"
  "`|"
  "C-u M-( ` C-d")

(ipe-test-def-kbd escape-update-simple-10 ()
  "Test `insert-pair-edit-update' with :escapes.

Update Simple PAIR, cursor before CLOSE, insert."
  ipe-test-escape-options
  nil
  "`\\`|`"
  "`\\`|`"
  "C-u M-( ` RET")

(ipe-test-def-kbd escape-update-simple-11 ()
  "Test `insert-pair-edit-update' with :escapes.

Update Simple PAIR, cursor before CLOSE, move forward."
  ipe-test-escape-options
  nil
  "`\\`|`"
  "``|`"
  "C-u M-( ` M-f RET")

(ipe-test-def-kbd escape-update-simple-12 ()
  "Test `insert-pair-edit-update' with :escapes.

Update Simple PAIR, cursor before CLOSE, move backward."
  ipe-test-escape-options
  nil
  "`\\`|`"
  "```|"
  "C-u M-( ` M-b RET")

(ipe-test-def-kbd escape-update-simple-13 ()
  "Test `insert-pair-edit-update' with :escapes.

Update Simple PAIR, cursor after CLOSE, abort."
  ipe-test-escape-options
  nil
  "`\\``|"
  "`|"
  "C-u M-( ` C-d")

(ipe-test-def-kbd escape-update-simple-14 ()
  "Test `insert-pair-edit-update' with :escapes.

Update Simple PAIR, cursor after CLOSE, insert."
  ipe-test-escape-options
  nil
  "`\\``|"
  "`\\``|"
  "C-u M-( ` RET")

(ipe-test-def-kbd escape-update-simple-15 ()
  "Test `insert-pair-edit-update' with :escapes.

Update Simple PAIR, cursor after CLOSE, move forward."
  ipe-test-escape-options
  nil
  "`\\``|"
  "```|"
  "C-u M-( ` M-f RET")

(ipe-test-def-kbd escape-update-simple-16 ()
  "Test `insert-pair-edit-update' with :escapes.

Update Simple PAIR, cursor after CLOSE, move backward."
  ipe-test-escape-options
  nil
  "`\\``|"
  "```|"
  "C-u M-( ` M-b RET")

(ipe-test-def-kbd escape-update-single-1 ()
  "Test `insert-pair-edit-update' with :escapes.

Test update with cursor at beginning of buffer."
  ipe-test-escape-options
  nil
  "|The quick brown 'fox \\'jumps\\' over' the lazy dog."
  "|The quick brown fox 'jumps' over the lazy dog."
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-single-2 ()
  "Test `insert-pair-edit-update' with :escapes.

Test update with cursor at start of PAIR."
  ipe-test-escape-options
  nil
  "The quick brown |'fox \\'jumps\\' over' the lazy dog."
  "The quick brown |fox 'jumps' over the lazy dog."
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-single-3 ()
  "Test `insert-pair-edit-update' with :escapes.

Test update with cursor immediately after OPEN."
  ipe-test-escape-options
  nil
  "The quick brown '|fox \\'jumps\\' over' the lazy dog."
  "The quick brown |fox 'jumps' over the lazy dog."
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-single-4 ()
  "Test `insert-pair-edit-update' with :escapes.

Test update with cursor at start of :escape."
  ipe-test-escape-options
  nil
  "The quick brown 'fox |\\'jumps\\' over' the lazy dog."
  "The quick brown fox |'jumps' over the lazy dog."
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-single-5 ()
  "Test `insert-pair-edit-update' with :escapes.

Test update with cursor immediately after :escape."
  ipe-test-escape-options
  nil
  "The quick brown 'fox \\'|jumps\\' over' the lazy dog."
  "The quick brown fox '|jumps' over the lazy dog."
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-single-6 ()
  "Test `insert-pair-edit-update' with :escapes.

Test update with cursor inside :escape'd PAIR."
  ipe-test-escape-options
  nil
  "The quick brown 'fox \\'ju|mps\\' over' the lazy dog."
  "The quick brown fox 'ju|mps' over the lazy dog."
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-single-7 ()
  "Test `insert-pair-edit-update' with :escapes.

Test update with cursor at end of :escape'd PAIR."
  ipe-test-escape-options
  nil
  "The quick brown 'fox \\'jumps|\\' over' the lazy dog."
  "The quick brown fox 'jumps|' over the lazy dog."
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-single-8 ()
  "Test `insert-pair-edit-update' with :escapes.

Test update with cursor after :escape'd PAIR."
  ipe-test-escape-options
  nil
  "The quick brown 'fox \\'jumps\\'| over' the lazy dog."
  "The quick brown fox 'jumps'| over the lazy dog."
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-single-9 ()
  "Test `insert-pair-edit-update' with :escapes.

Test update with cursor immediately before CLOSE."
  ipe-test-escape-options
  nil
  "The quick brown 'fox \\'jumps\\' over|' the lazy dog."
  "The quick brown fox 'jumps' over| the lazy dog."
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-single-10 ()
  "Test `insert-pair-edit-update' with :escapes.

Test update with cursor after CLOSE."
  ipe-test-escape-options
  nil
  "The quick brown 'fox \\'jumps\\' over'| the lazy dog."
  "The quick brown fox 'jumps' over| the lazy dog."
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-single-11 ()
  "Test `insert-pair-edit-update' with :escapes.

Test update with cursor at end-of-buffer."
  ipe-test-escape-options
  nil
  "The quick brown 'fox \\'jumps\\' over' the lazy dog.|"
  "The quick brown fox 'jumps' over the lazy dog.|"
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-double-1 ()
  "Test `insert-pair-edit-update' with :escapes.

Test nested escape update with cursor at beginning of buffer."
  ipe-test-escape-options
  nil
  "|The quick 'brown \\'fox \\\\'jumps\\\\' over\\' the' lazy dog."
  "|The quick brown 'fox \\'jumps\\' over' the lazy dog."
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-double-2 ()
  "Test `insert-pair-edit-update' with :escapes.

Test nested escape update with cursor at start of PAIR."
  ipe-test-escape-options
  nil
  "The quick 'brown |\\'fox \\\\'jumps\\\\' over\\' the' lazy dog."
  "The quick brown |'fox \\'jumps\\' over' the lazy dog."
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-double-3 ()
  "Test `insert-pair-edit-update' with :escapes.

Test nested escape update with cursor immediately after OPEN."
  ipe-test-escape-options
  nil
  "The quick 'brown \\'|fox \\\\'jumps\\\\' over\\' the' lazy dog."
  "The quick brown '|fox \\'jumps\\' over' the lazy dog."
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-double-4 ()
  "Test `insert-pair-edit-update' with :escapes.

Test nested escape update with cursor at start of :escape."
  ipe-test-escape-options
  nil
  "The quick 'brown \\'|fox \\\\'jumps\\\\' over\\' the' lazy dog."
  "The quick brown '|fox \\'jumps\\' over' the lazy dog."
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-double-5 ()
  "Test `insert-pair-edit-update' with :escapes.

Test nested escape update with cursor at immediately after :escape."
  ipe-test-escape-options
  nil
  "The quick 'brown \\'fox |\\\\'jumps\\\\' over\\' the' lazy dog."
  "The quick brown 'fox |\\'jumps\\' over' the lazy dog."
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-double-6 ()
  "Test `insert-pair-edit-update' with :escapes.

Test nested escape update with cursor inside :escape'd PAIR."
  ipe-test-escape-options
  nil
  "The quick 'brown \\'fox \\\\'|jumps\\\\' over\\' the' lazy dog."
  "The quick brown 'fox \\'|jumps\\' over' the lazy dog."
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-double-7 ()
  "Test `insert-pair-edit-update' with :escapes.

Test nested escape update with cursor at end of :escape'd PAIR."
  ipe-test-escape-options
  nil
  "The quick 'brown \\'fox \\\\'jumps|\\\\' over\\' the' lazy dog."
  "The quick brown 'fox \\'jumps|\\' over' the lazy dog."
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-double-8 ()
  "Test `insert-pair-edit-update' with :escapes.

Test nested escape update with cursor after :escape'd PAIR."
  ipe-test-escape-options
  nil
  "The quick 'brown \\'fox \\\\'jumps\\\\'| over\\' the' lazy dog."
  "The quick brown 'fox \\'jumps\\'| over' the lazy dog."
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-double-9 ()
  "Test `insert-pair-edit-update' with :escapes.

Test nested escape update with cursor immediately before CLOSE."
  ipe-test-escape-options
  nil
  "The quick 'brown \\'fox \\\\'jumps\\\\' over|\\' the' lazy dog."
  "The quick brown 'fox \\'jumps\\' over|' the lazy dog."
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-double-10 ()
  "Test `insert-pair-edit-update' with :escapes.

Test nested escape update with cursor after CLOSE."
  ipe-test-escape-options
  nil
  "The quick 'brown \\'fox \\\\'jumps\\\\' over\\'| the' lazy dog."
  "The quick brown 'fox \\'jumps\\' over'| the lazy dog."
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-double-11 ()
  "Test `insert-pair-edit-update' with :escapes.

Test nested escape update with cursor at end-of-buffer."
  ipe-test-escape-options
  nil
  "The quick 'brown \\'fox \\\\'jumps\\\\' over\\' the' lazy dog.|"
  "The quick brown 'fox \\'jumps\\' over' the lazy dog.|"
  "C-u M-( ' C-d")

(ipe-test-def-kbd escape-update-sizes-1 ()
  "Test `insert-pair-edit-update' with :escapes.

Test changing sizes of :escapes.  Large -> small."
  ipe-test-escape-options
  nil
  "The quick (brown 22222 fox |444466688 jumps 0 over the) lazy dog."
  "The quick brown 1 fox |357 jumps 9 over the lazy dog."
  "C-u M-( 4 C-d")

(ipe-test-def-kbd escape-update-sizes-2 ()
  "Test `insert-pair-edit-update' with :escapes.

Test changing sizes of :escapes.  Large -> small side-by-side.
beginning of line."
  ipe-test-escape-options
  nil
  "|(222224444666880)"
  "|13579"
  "C-u M-( 4 C-d")

(ipe-test-def-kbd escape-update-sizes-3 ()
  "Test `insert-pair-edit-update' with :escapes.

Test changing sizes of :escapes.  Large -> small side-by-side,
middle of line."
  ipe-test-escape-options
  nil
  "(222224444|666880)"
  "13|579"
  "C-u M-( 4 C-d")

(ipe-test-def-kbd escape-update-sizes-4 ()
  "Test `insert-pair-edit-update' with :escapes.

Test changing sizes of :escapes.  Large -> small side-by-side.
end of line."
  ipe-test-escape-options
  nil
  "(222224444666880)|"
  "13579|"
  "C-u M-( 4 C-d")

(ipe-test-def-kbd escape-update-sizes-5 ()
  "Test `insert-pair-edit-update' with :escapes.

Test changing sizes of :escapes.  Small -> large."
  ipe-test-escape-options
  nil
  "The quick (brown 1 fox |357 jumps 9 over the) lazy dog."
  "The quick brown 22222 fox |444466688 jumps 0 over the lazy dog."
  "C-u M-( 5 C-d")

(ipe-test-def-kbd escape-update-sizes-6 ()
  "Test `insert-pair-edit-update' with :escapes.

Test changing sizes of :escapes.  Small -> large side-by-side.
beginning of line."
  ipe-test-escape-options
  nil
  "|(13579)"
  "|222224444666880"
  "C-u M-( 5 C-d")

(ipe-test-def-kbd escape-update-sizes-7 ()
  "Test `insert-pair-edit-update' with :escapes.

Test changing sizes of :escapes.  Small -> large side-by-side.
middle of line."
  ipe-test-escape-options
  nil
  "(13|579)"
  "222224444|666880"
  "C-u M-( 5 C-d")

(ipe-test-def-kbd escape-update-sizes-8 ()
  "Test `insert-pair-edit-update' with :escapes.

Test changing sizes of :escapes.  Small -> large side-by-side.
end of line."
  ipe-test-escape-options
  nil
  "(13579)|"
  "222224444666880|"
  "C-u M-( 5 C-d")

(ipe-test-def-kbd escape-delete-inner-1 ()
  "Test `insert-pair-edit-delete' with :escapes.

Test single delete."
  ipe-test-escape-options
  nil
  "The quick 'brown \\'fox \\\\''|jumps'\\\\' over\\' the' lazy dog."
  "The quick 'brown \\'fox \\\\'|jumps\\\\' over\\' the' lazy dog."
  "C-u C-u M-( '")

(ipe-test-def-kbd escape-delete-inner-2 ()
  "Test `insert-pair-edit-delete' with :escapes.

Test multiple deletes (x2)."
  ipe-test-escape-options
  nil
  "The quick 'brown \\'fox \\\\''|jumps'\\\\' over\\' the' lazy dog."
  "The quick brown 'fox \\'|jumps\\' over' the lazy dog."
  "C-u C-u M-( ' C-u C-u M-( '")

(ipe-test-def-kbd escape-delete-inner-3 ()
  "Test `insert-pair-edit-delete' with :escapes.

Test multiple deletes (x3)."
  ipe-test-escape-options
  nil
  "The quick 'brown \\'fox \\\\''|jumps'\\\\' over\\' the' lazy dog."
  "The quick brown fox '|jumps' over the lazy dog."
  "C-u C-u M-( ' C-u C-u M-( ' C-u C-u M-( '")

(ipe-test-def-kbd escape-delete-inner-4 ()
  "Test `insert-pair-edit-delete' with :escapes.

Test multiple deletes (x4)."
  ipe-test-escape-options
  nil
  "The quick 'brown \\'fox \\\\''|jumps'\\\\' over\\' the' lazy dog."
  "The quick brown fox |jumps over the lazy dog."
  "C-u C-u M-( ' C-u C-u M-( ' C-u C-u M-( ' C-u C-u M-( '")

(provide 'ipe-test-escape)

;; ipe-test-escape.el ends here
