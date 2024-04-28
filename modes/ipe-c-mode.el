;;; ipe-c-mode.el --- Insert Pair Edit - c-mode definitions -*- lexical-binding: t; -*-
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
;; definitions for `c-mode'.

;; -------------------------------------------------------------------
;;; Code:

(require 'ipe-custom)
(require 'ipe)

(defcustom
  ipe-c-pairs
  (ipe-custom--pair-list-get
   '(("m" "int main(int args, char* argv[]) {" "}"
      (:movement line
		 :infix "    "
		 :indent-function previous))
     ("{" "{    " "}"
      (:movement line
		 :infix "    "
		 :indent-function previous))
     ("d" "do {    " "} while ();"
      (:movement line
		 :infix "    "
		 :indent-function previous))
     ("f" "for (;;) {    " "}"
      (:movement line
		 :infix "    "
		 :indent-function previous))
     ("i" "if () {    " "}"
      (:movement line
		 :infix "    "
		 :indent-function previous))
     ("w" "while () {    " "}"
      (:movement line
		 :infix "    "
		 :indent-function previous))
     (";" "/* " " */"
      (:movement line
		 :infix " * "
		 :indent-function previous))
     ("/" "// " ""
      (:movement line
		 :infix "// "
		 :indent-function previous))
     ("#" "#ifdef " "\n#endif"
      (:movement line))))

  "`insert-pair-edit' customizations for `c-mode'."
  :group 'ipe-mode-pairs
  :tag   "Insert Pair Edit - c-mode PAIRs"
  :link  '(function-link insert-pair-edit)
  :set   'ipe-custom-pair-list-set
  :get   'ipe-custom-pair-list-get
  :type  '(ipe-custom-pair-list
	   :tag "PAIRs used by `insert-pair-edit' in `c-mode'."))

(ipe-mode-pairs-add 'c-mode 'ipe-c-pairs)

(provide 'ipe-c-mode)
;;; ipe-c-mode.el ends here
