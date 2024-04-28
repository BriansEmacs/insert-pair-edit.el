;;; ipe-texinfo-mode.el --- Insert Pair Edit - texinfo-mode definitions -*- lexical-binding: t; -*-
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
;; definitions for `texinfo-mode'.

;; -------------------------------------------------------------------
;;; Code:

(require 'ipe-custom)
(require 'ipe)

(defcustom
  ipe-texinfo-pairs
  (ipe-custom--pair-list-get
   '(
     ;; Structure
     ("@" "@copying\n"    "\n@end copying"
      (:menu "Structure" :movement line))
     ("M" "@menu\n"       "\n@end menu\n* "
      (:menu "Structure" :movement line :infix "* " ))

     ;; Headings
     ("0" "@part"          ""
      (:menu "Headings" :movement line))
     ("1" "@chapter"       ""
      (:menu "Headings" :movement line))
     ("2" "@section"       ""
      (:menu "Headings" :movement line))
     ("3" "@subsection"    ""
      (:menu "Headings" :movement line))
     ("4" "@subsubsection" ""
      (:menu "Headings" :movement line))

     ;; Cross References
     ("*" "@footnote{"          "}"
      (:menu "Cross References" :movement word))
     ("A" "@anchor{"            "}"
      (:menu "Cross References" :movement word))
     ("R" "@ref{"               "}"
      (:menu "Cross References" :movement char))
     ("l" "@link{"              "}"
      (:menu "Cross References" :movement word))
     ("u" "@url{"               "}"
      (:menu "Cross References" :movement char))
     ("x" "@xref{"              "}"
      (:menu "Cross References" :movement char))

     ;; Blocks
     ("<" "@exdent\n"     "\n@end exdent"
      (:menu "Blocks"     :movement line))
     (">" "@indent\n"     "\n@end indent"
      (:menu "Blocks"     :movement line))
     ("D" "@display\n"    "\n@end display"
      (:menu "Blocks"     :movement line))
     ("E" "@example\n"    "\n@end example"
      (:menu "Blocks"     :movement line))
     ("F" "@format\n"     "\n@end format"
      (:menu "Blocks"     :movement line))
     ("L"  "@lisp\n"      "\n@end lisp"
      (:menu "Blocks"     :movement line))
     ("q" "@quotation\n"  "\n@end quotation"
      (:menu "Blocks"     :movement line))

     ;; Lists and Tables
     ("9" "@enumerate\n"  "\n@end enumerate"
      (:menu "Lists & Tables" :movement line :infix "* "))
     ("I" "@itemize\n"    "\n@end itemize"
      (:menu "Lists & Tables" :movement line :infix "@item\n"))
     ("T" "@table\n"      "\n@end table"
      (:menu "Lists & Tables" :movement line :infix "@item\n"))

     ;; Definitions
     ("C" "@command{"      "}"
      (:menu "Definitions" :movement word))
     ("K" "@key{"          "}"
      (:menu "Definitions" :movement char))
     ("V" "@verb{"         "}"
      (:menu "Definitions" :movement word))
     ("a" "@abbr{"         "}"
      (:menu "Definitions" :movement word))
     ("c" "@code{"         "}"
      (:menu "Definitions" :movement word))
     ("df" "@defun"        "\n@end defun"
      (:menu "Definitions" :movement line))
     ("dm" "@defmac{"      "\n@end defmac"
      (:menu "Definitions" :movement line))
     ("do" "@defopt{"      "\n@end defopt"
      (:menu "Definitions" :movement line))
     ("ds" "@defspec{"     "\n@end defspec"
      (:menu "Definitions" :movement line))
     ("dv" "@defvar{"      "\n@end defvar"
      (:menu "Definitions" :movement line))
     ("f" "@file{"         "}"
      (:menu "Definitions" :movement word))
     ("k" "@kbd{"          "}"
      (:menu "Definitions" :movement word))
     ("m" "@email{"        "}"
      (:menu "Definitions" :movement word))
     ("o" "@option{"       "}"
      (:menu "Definitions" :movement word))
     ("v" "@var{"          "}"
      (:menu "Definitions" :movement word))

     ;; Text Styles
     ("S" "@sc{"           "}"
      (:menu "Text Styles" :movement word))
     ("^" "@sup{"          "}"
      (:menu "Text Styles" :movement char))
     ("_" "@sub{"          "}"
      (:menu "Text Styles" :movement char))
     ("b" "@b{"            "}"
      (:menu "Text Styles" :movement word))
     ("e" "@emph{"         "}"
      (:menu "Text Styles" :movement word))
     ("i" "@i{"            "}"
      (:menu "Text Styles" :movement word))
     ("s" "@strong{"       "}"
      (:menu "Text Styles" :movement word))

     (";" "@c "            ""
      (:menu "Other"       :movement line :infix "@c "))
     ("w" "@w{"            "}"
      (:menu "Other"       :movement word))))

  "`insert-pair-edit' customizations for `texinfo-mode'."
  :group 'ipe-mode-pairs
  :tag   "Insert Pair Edit - texinfo-mode PAIRs"
  :link  '(function-link insert-pair-edit)
  :set   'ipe-custom-pair-list-set
  :get   'ipe-custom-pair-list-get
  :type  '(ipe-custom-pair-list
	   :tag "TEXINFO PAIRs used by `insert-pair-edit' in\
 `texinfo-mode'."))

(ipe-mode-pairs-add 'texinfo-mode 'ipe-texinfo-pairs)

(provide 'ipe-texinfo-mode)

;;; ipe-texinfo-mode.el ends here

