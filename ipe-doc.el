;;; ipe-doc.el --- Insert Pair Edit - Functions to generate doc gifs -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Brian Kavanagh

;; Author: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Maintainer: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Created: 23 March, 2024
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
;; This file defines functions used by the 'Insert Pair Edit' package
;; to generate documentation gifs.

;; -------------------------------------------------------------------
;;; Code:

(require 'ipe)

(defun ipe-doc-logo ()
  "Display the \"Insert Pair Edit (ipe)\" logo."

  (interactive)

  (let* ((logo-buffer  (get-buffer-create "**ipe-logo**"))
	 (logo         "Insert Pair Edit (ipe)")
	 (cursor-color (face-attribute 'cursor :background))
	 (delay 0.5)
	 (time 0))

    (switch-to-buffer logo-buffer)
    (buffer-face-set 'ipe--logo-face)
    (blink-cursor-mode 0)
    (set-cursor-color "grey10")

    (setq-local ipe-pairs '(("(" "(" ")")
			    ("[" "[" "]")
			    ("{" "{" "}")
			    ("\"" "\"" "\"")
			    ("'" "'" "'")))
    (setq-local ipe-mode-pairs nil)

    (delete-region (point-min) (point-max))

    (insert "\n\n\n\n")
    (insert logo)

    (center-paragraph)
    (goto-char (point-max))

    (dolist (command
	     (list
	      '(ipe-insert-pair-edit-update "(")
	      '(ipe-edit--close-backward nil)
	      '(ipe-edit--close-backward nil)
	      '(ipe-edit--close-backward nil)
	      '(ipe-edit--change-pair "[")
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--close-backward nil)
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--close-backward nil)
	      '(ipe-edit--change-pair "{")
	      '(ipe-edit--change-pair "\"")
	      '(ipe-edit--change-pair "'")
	      '(ipe-edit--change-pair "(")
	      '(ipe-edit--contents-upcase)
	      '(ipe-edit--contents-downcase)
	      '(ipe-edit--contents-capitalize)
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--abort)))

      (run-at-time (setq time (+ time delay))
		   0
		   'apply
		   command))

    (set-cursor-color cursor-color)))

(defun ipe-doc-markdown-demo (&optional speed)
  "Display the \"Insert Pair Edit (ipe)\" markdown-demo.

If specified, SPEED represents the delay between actions
within the demo."

  (interactive)

  (let* ((demo-buffer  (get-buffer-create "**ipe-markdown-demo**"))
	 (cursor-color (face-attribute 'cursor :background))
	 (delay        (or speed 0.40))
	 (time         0))

    (switch-to-buffer demo-buffer)

    (setq-local ipe-pairs
		'(("(" "(" ")")
		  ("'" "'" "'")
		  ("1" "# "      " #")
		  ("2" "## "     " ##")
		  ("*" "**"      "**")
		  ("_" "_"       "_")
		  ("`" "`"       "`")
		  ("~" "~~"      "~~"  (:movement char))
		  ("-" " - "     ""    (:movement line :infix " - "))
		  ("x" " - [ ] " ""    (:movement line :infix " - [ ] "))
		  ("f" "```"     "```" (:movement line))))

    (ipe--safecall 'markdown-mode)
    (blink-cursor-mode 0)
    (set-cursor-color "grey30")

    (delete-region (point-min) (point-max))

    (insert
     "Insert Pair Edit ipe - markdown mode\n"
     "\n"
     "Text Styles\n"
     "Bold\n"
     "Italic\n"
     "Monospace\n"
     "Strikethrough\n"
     "\n"
     "Lists\n"
     "One\n"
     "Two\n"
     "Three\n"
     "\n"
     "Checkboxes\n"
     "Item #1\n"
     "Item #2\n"
     "Item #3\n"
     "\n"
     "Blocks\n"
     "The quick brown\n"
     "fox jumps over\n"
     "the lazy dog.\n")

    (goto-char (point-min))

    (dolist (command
	     (list
	      ;; Edit the title line.
	      '(ipe-insert-pair-edit nil "(")
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--insert-pair)
	      '(ipe-insert-pair-edit nil "'")
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--insert-pair)
	      '(ipe-insert-pair-edit nil "1")
	      '(ipe-edit--close-end nil)
	      '(ipe-edit--insert-pair)
	      '(beginning-of-line)

	      ;; Edit the 'Text Styles'.
	      '(next-line)
	      '(next-line)
	      '(ipe-insert-pair-edit nil "2")
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--insert-pair)
	      '(beginning-of-line)
	      '(next-line)
	      '(ipe-insert-pair-edit nil "*")
	      '(ipe-edit--insert-pair)
	      '(beginning-of-line)
	      '(next-line)
	      '(ipe-insert-pair-edit nil "_")
	      '(ipe-edit--insert-pair)
	      '(beginning-of-line)
	      '(next-line)
	      '(ipe-insert-pair-edit nil "`")
	      '(ipe-edit--insert-pair)
	      '(beginning-of-line)
	      '(next-line)
	      '(ipe-insert-pair-edit nil "~")
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--insert-pair)
	      '(beginning-of-line)

	      ;; Edit the 'Lists'.
	      '(next-line)
	      '(next-line)
	      '(ipe-insert-pair-edit nil "2")
	      '(ipe-edit--insert-pair)
	      '(beginning-of-line)
	      '(next-line)
	      '(ipe-insert-pair-edit nil "-")
	      '(ipe-edit--close-down nil)
	      '(ipe-edit--close-down nil)
	      '(ipe-edit--insert-pair)
	      '(beginning-of-line)
	      '(next-line)
	      '(next-line)
	      '(next-line)

	      ;; Edit the 'Checkboxes'.
	      '(next-line)
	      '(ipe-insert-pair-edit nil "2")
	      '(ipe-edit--insert-pair)
	      '(beginning-of-line)
	      '(next-line)
	      '(ipe-insert-pair-edit nil "x")
	      '(ipe-edit--close-down nil)
	      '(ipe-edit--close-down nil)
	      '(ipe-edit--open-backward nil)
	      '(ipe-edit--open-backward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--insert-pair)
	      '(beginning-of-line)
	      '(next-line)
	      '(next-line)
	      '(next-line)

	      ;; Edit the 'Blocks'
	      '(next-line)
	      '(ipe-insert-pair-edit nil "2")
	      '(ipe-edit--insert-pair)
	      '(beginning-of-line)
	      '(next-line)
	      '(ipe-insert-pair-edit nil "f")
	      '(ipe-edit--open-beg nil)
	      '(ipe-edit--close-down nil)
	      '(ipe-edit--close-down nil)
	      '(ipe-edit--close-end nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--close-forward nil)
	      '(next-line)
	      '(ipe-edit--insert-pair)

	      ;; Done.
	      '(beginning-of-buffer)))

      (run-at-time (setq time (+ time delay))
		   0
		   'apply
		   command))

    (set-cursor-color cursor-color)))

(provide 'ipe-doc)

;;; ipe-doc.el ends here
