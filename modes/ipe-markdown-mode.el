;;; ipe-markdown-mode.el --- Insert Pair Edit - markdown-mode definitions -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Brian Kavanagh

;; Author: Brian Kavanagh <brians.emacs@gmail.com>
;; Maintainer: Brian Kavanagh <brians.emacs@gmail.com>
;; Created: 28 June, 2020
;; Version: 1.1
;; Package: ipe
;; Keywords: convenience, tools
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
;; This file defines a set of Mode-Specific 'Insert Pair Edit' (ipe)
;; definitions for `markdown-mode'.

;; -------------------------------------------------------------------
;;; Code:

(require 'ipe)

(defcustom
  ipe-markdown-pairs
  (ipe-custom--pair-list-get
   '(("1" "# "      " #"      (:menu "Headings"       :movement word))
     ("2" "## "     " ##"     (:menu "Headings"       :movement word))
     ("3" "### "    " ###"    (:menu "Headings"       :movement word))
     ("4" "#### "   " ####"   (:menu "Headings"       :movement word))
     ("5" "##### "  " #####"  (:menu "Headings"       :movement word))
     ("6" "###### " " ######" (:menu "Headings"       :movement word))

     (">" "> "      "\n> "
      (:menu "Blocks"         :movement line :infix "> "))
     ("f" "```"     "```"
      (:menu "Blocks"         :movement line))

     ("|" "|"       "|"
      (:menu "Lists & Tables" :movement word))
     ("-" " - "     ""
      (:menu "Lists & Tables" :movement line :infix " - "))
     ("x" " - [ ] " ""
      (:menu "Lists & Tables" :movement line :infix " - [ ] "))

     ("*" "**"      "**"
      (:menu "Text Styles"    :movement word))
     ("i" "*"       "*"
      (:menu "Text Styles"    :movement word))
     ("`" "`"       "`"
      (:menu "Text Styles"    :movement word))
     ("_" "_"       "_"
      (:menu "Text Styles"    :movement word))
     ("~" "~~"      "~~"
      (:menu "Text Styles"    :movement char))
     ("v" "~"       "~"
      (:menu "Text Styles"    :movement char))
     ("^" "^"       "^"
      (:menu "Text Styles"    :movement char))

     ("[" "["       "]"       (:menu "Other"))

     ;; ipe-html-pair equivalents.
     ("a" "["       "]"
      (:menu "HTML Equiv"     :movement word))
     ("b" "**"      "**"
      (:menu "HTML Equiv"     :movement word))
     ("c" "`"       "`"
      (:menu "HTML Equiv"     :movement word))
     ("e" "_"       "_"
      (:menu "HTML Equiv"     :movement word))
     ("h" "|"       "|"
      (:menu "HTML Equiv"     :movement word))
     ("l" " - "     ""
      (:menu "HTML Equiv"     :movement line :infix " - "))
     ("t" " - [ ] " ""
      (:menu "HTML Equiv"     :movement line :infix " - [ ] "))
     ("q" "> "      "\n> "
      (:menu "HTML Equiv"     :movement line :infix "> "))))

  "`insert-pair-edit' customizations for `markdown-mode'."
  :group 'ipe-mode-pairs
  :tag   "Insert Pair Edit - markdown-mode PAIRs"
  :link  '(function-link insert-pair-edit)
  :set   'ipe-custom-pair-list-set
  :get   'ipe-custom-pair-list-get
  :type  '(ipe-custom-pair-list
	   :tag "Markdown PAIRs used by `insert-pair-edit' in \
`markdown-mode'."))

(ipe-mode-pairs-add 'markdown-mode 'ipe-markdown-pairs)

(provide 'ipe-markdown-mode)
;;; ipe-markdown-mode.el ends here
