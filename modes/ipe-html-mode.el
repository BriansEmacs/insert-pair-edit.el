;;; ipe-html-mode.el --- Insert Pair Edit - html-mode definitions -*- lexical-binding: t; -*-
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
;; definitions for `html-mode'.

;; -------------------------------------------------------------------
;;; Code:

(require 'ipe-custom)
(require 'ipe)

(defcustom
  ipe-html-pairs
  (ipe-custom--pair-list-get
   '(
     ;; Structure
     ("H" "<html>"   "</html>"
      (:movement char :menu "Structure"))
     ("D" "<head>"   "</head>"
      (:movement char :menu "Structure"))
     ("T" "<title>"   "</title>"
      (:movement char :menu "Structure"))
     ("B" "<body>"   "</body>"
      (:movement char :menu "Structure"))
     ("S" "<script>" "</script>"
      (:movement char :menu "Structure"))
     ("M" "<meta>"   "</meta>"
      (:movement char :menu "Structure"))

     ;; Headings
     ("1" "<h1>" "</h1>"
      (:movement word :menu "Headings"))
     ("2" "<h2>" "</h2>"
      (:movement word :menu "Headings"))
     ("3" "<h3>" "</h3>"
      (:movement word :menu "Headings"))
     ("4" "<h4>" "</h4>"
      (:movement word :menu "Headings"))
     ("5" "<h5>" "</h5>"
      (:movement word :menu "Headings"))
     ("6" "<h6>" "</h6>"
      (:movement word :menu "Headings"))

     ;; Blocks
     ("p" "<p>"    "</p>"
      (:movement char :menu "Blocks"))
     ("f" "<pre>"  "\n</pre>"
      (:movement char :menu "Blocks"))
     ("d" "<div>"  "\n</div>"
      (:movement char :menu "Blocks"
		 :infix "    "
		 :indent-function previous))
     ("n" "<span>" "</span>"
      (:movement  word :menu "Blocks"))

     ;; Lists and Tables
     ("o" "<ol>"    "</ol>"
      (:movement char :menu "Lists & Tables/Lists"))
     ("u" "<ul>"    "</ul>"
      (:movement char :menu "Lists & Tables/Lists"))
     ("l" "<li>"    "</li>"
      (:movement word :menu "Lists & Tables/Lists"))
     ("dl" "<dl>"    "</dl>"
      (:movement word :menu "Lists & Tables/Lists"))
     ("dd" "<dd>"    "</dd>"
      (:movement word :menu "Lists & Tables/Lists"))
     ("dt" "<dt>"    "</dt>"
      (:movement word :menu "Lists & Tables/Lists"))

     ("t" "<table>" "</table>"
      (:movement char :menu "Lists & Tables/Tables"))
     ("h" "<th>"    "</th>"
      (:movement word :menu "Lists & Tables/Tables"))
     ("r" "<tr>"    "</tr>"
      (:movement word :menu "Lists & Tables/Tables"))

     ;; Text Styles
     ("b" "<b>"      "</b>"
      (:movement word :menu "Text Styles"))
     ("i" "<i>"      "</i>"
      (:movement word :menu "Text Styles"))
     ("_" "<u>"      "</u>"
      (:movement word :menu "Text Styles"))
     ("e" "<em>"     "</em>"
      (:movement word :menu "Text Styles"))
     ("q" "<q>"      "</q>"
      (:movement word :menu "Text Styles"))
     ("s" "<strong>" "</strong>"
      (:movement word :menu "Text Styles"))
     ("k" "<kbd>"    "</kbd>"
      (:movement word :menu "Text Styles"))
     ("c" "<code>"   "</code>"
      (:movement word :menu "Text Styles"))
     ("m" "<mark>"   "</mark>"
      (:movement char :menu "Text Styles"))

     ("^" "<sup>"    "</sup>"
      (:movement char :menu "Text Styles"))
     ("v" "<sub>"    "</sub>"
      (:movement char :menu "Text Styles"))
     ("-" "<del>"    "</del>"
      (:movement char :menu "Text Styles"))

     ;; Other
     ("a" "<a href=\"\">" "</a>"
      (:movement word :menu "Other"))
     ("|" "<br>"     "</br>"
      (:movement char :menu "Other"))
     ("~" "<hr>"     "</hr>"
      (:movement char :menu "Other"))
     ("<" "<![CDATA["     "]]>"
      (:movement line :menu "Other"))
     (";" "<!-- "         "\n  -->"
      (:movement char :menu "Other"
		 :infix "  -- "
		 :indent-function previous))))

  "`insert-pair-edit' customizations for `html-mode'."
  :group 'ipe-mode-pairs
  :tag   "Insert Pair Edit - html-mode PAIRs"
  :link  '(function-link insert-pair-edit)
  :set   'ipe-custom-pair-list-set
  :get   'ipe-custom-pair-list-get
  :type  '(ipe-custom-pair-list
	   :tag "HTML PAIRs used by `insert-pair-edit' in `html-mode'."))

(ipe-mode-pairs-add 'html-mode  'ipe-html-pairs)
(ipe-mode-pairs-add 'mhtml-mode 'ipe-html-pairs)

(provide 'ipe-html-mode)

;;; ipe-html-mode.el ends here
