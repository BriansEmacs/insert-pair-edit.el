;;; ipe-doc.el --- Insert Pair Edit - Functions to generate doc gifs -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Brian Kavanagh

;; Author: Brian Kavanagh <brians.emacs@gmail.com>
;; Maintainer: Brian Kavanagh <brians.emacs@gmail.com>
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

  (let* ((logo-buffer  (get-buffer-create
			"**ipe-logo**"))
	 (logo         "Insert Pair Edit (ipe)")
	 (cursor-color (face-attribute 'cursor :background))
	 (delay        0.5)
	 (time         2.0))

    ;; Set up some (ipe) configuration.
    (switch-to-buffer logo-buffer)
    (setq-local ipe-pairs '(("(" "(" ")")
			    ("[" "[" "]")
			    ("{" "{" "}")
			    ("\"" "\"" "\"")
			    ("'" "'" "'")))
    (setq-local ipe-mode-pairs nil)

    ;; Set up the demo buffer.
    (blink-cursor-mode 0)
    (set-cursor-color "grey10")
    (delete-region (point-min) (point-max))
    (buffer-face-set 'ipe--logo-face)
    (insert "\n\n\n\n")
    (insert logo)
    (center-paragraph)
    (goto-char (point-max))

    ;; Run some demonstration (ipe) commands.
    (dolist
	(command
	 (list
	  ;; Update the '(' / ')' PAIR around ipe.
	  '(ipe-insert-pair-edit-update "(")

	  ;; Move it backward.
	  '(ipe-edit--close-backward nil)
	  '(ipe-edit--close-backward nil)
	  '(ipe-edit--close-backward nil)

	  ;; Change it to a '[' / ']' PAIR.
	  '(ipe-edit--change-pair "[")

	  ;; Do some more movement.
	  '(ipe-edit--close-forward nil)
	  '(ipe-edit--close-forward nil)
	  '(ipe-edit--close-forward nil)
	  '(ipe-edit--close-backward nil)
	  '(ipe-edit--open-forward nil)
	  '(ipe-edit--close-backward nil)

	  ;; Cycle through some changes.
	  '(ipe-edit--change-pair "{")
	  '(ipe-edit--change-pair "\"")
	  '(ipe-edit--change-pair "'")
	  '(ipe-edit--change-pair "(")

	  ;; Cycle through some CONTENT changes.
	  '(ipe-edit--contents-upcase)
	  '(ipe-edit--contents-downcase)
	  '(ipe-edit--contents-capitalize)

	  ;; Restore us to the original text.
	  '(ipe-edit--open-forward nil)
	  '(ipe-edit--open-forward nil)
	  '(ipe-edit--abort)))

      (run-at-time (setq time (+ time delay))
		   0
		   'apply
		   (lambda (x)
		     (when (buffer-live-p logo-buffer)
		       (with-current-buffer logo-buffer
			 (apply x))))
		   (list command)))

    (set-cursor-color cursor-color)))

(defun ipe-doc-change-pair-demo (&optional speed)
  "Display the \"Insert Pair Edit (ipe)\" Change Pair demo.

If specified, SPEED represents the delay between actions
within the demo."

  (interactive)

  (let* ((demo-buffer  (get-buffer-create
			"**ipe-change-pair-demo**"))
	 (cursor-color (face-attribute 'cursor :background))
	 (delay        (or speed 0.6))
	 (time         3.0))

    ;; Set up some (ipe) configuration.
    (switch-to-buffer demo-buffer)
    (setq-local ipe-pairs
		'(("b" "<b>"      "</b>")
		  ("s" "<strong>" "</strong>")
		  ("i" "<i>"      "</i>")
		  ("e" "<em>"     "</em>")
		  ("B" "<big>"    "</big>")
		  ("l" "<span style=\"font-size: 120%\">" "</span>")))

    ;; Set up the demo buffer.
    (blink-cursor-mode 0)
    (set-cursor-color "grey30")
    (delete-region (point-min) (point-max))
    (insert
     "\n"
     "    The <b>quick</b> brown <i>fox</i> jumps <big>over</big> the\
 lazy dog.\n"
     "\n")
    (goto-char (+ (point-min) 1))

    ;; Run some demonstration (ipe) commands.
    (dolist (command
	     (list
	      ;; Change the <b></b> tag to <strong></strong>.
	      '(ipe-insert-pair-edit-update "b")
	      '(ipe-edit--change-pair "s")
	      '(ipe-edit--insert-pair)

	      ;; Change the <i></i> tag to <em></em>.
	      '(ipe-insert-pair-edit-update "i")
	      '(ipe-edit--change-pair "e")
	      '(ipe-edit--insert-pair)

	      ;; Change the <big></big> tag to <span
	      ;; style="font-size: 120%"></span>.
	      '(ipe-insert-pair-edit-update "B")
	      '(ipe-edit--change-pair "l")
	      '(ipe-edit--insert-pair)

	      ;; Change the <strong></strong> tag to <b></b>.
	      '(ipe-insert-pair-edit-update "s")
	      '(ipe-edit--change-pair "b")
	      '(ipe-edit--insert-pair)

	      ;; Change the <em></em> tag to <i></i>.
	      '(ipe-insert-pair-edit-update "e")
	      '(ipe-edit--change-pair "i")
	      '(ipe-edit--insert-pair)

	      ;; Change the <span style="font-size: 120"></span>
	      ;; tag to <big></big>.
	      '(ipe-insert-pair-edit-update "l")
	      '(ipe-edit--change-pair "B")
	      '(ipe-edit--insert-pair)))

      (run-at-time (setq time (+ time delay))
		   0
		   'apply
		   (lambda (x)
		     (when (buffer-live-p demo-buffer)
		       (with-current-buffer demo-buffer
			 (apply x))))
		   (list command)))

    (set-cursor-color cursor-color)))

(defun ipe-doc-lexical-units-demo (&optional speed)
  "Display the \"Insert Pair Edit (ipe)\" Lexical Units demo.

If specified, SPEED represents the delay between actions
within the demo."

  (interactive)

  (let* ((demo-buffer  (get-buffer-create
			"**ipe-lexical-units-demo**"))
	 (cursor-color (face-attribute 'cursor :background))
	 (delay        (or speed 0.6))
	 (time         3.0))

    ;; Set up some (ipe) configuration.
    (switch-to-buffer demo-buffer)
    (setq-local ipe-pairs '(("/" "/*" "*/")))

    ;; Set up the demo buffer.
    (blink-cursor-mode 0)
    (set-cursor-color "grey30")
    (delete-region (point-min) (point-max))
    (insert
     "\n"
     "    The quick brown fox /*jumps*/ over the lazy dog.\n"
     "\n")
    (goto-char (+ (point-min) 1))

    ;; Run some demonstration (ipe) commands.
    (dolist (command
	     (list
	      ;; Edit the '/*' '*/' PAIR.
	      '(ipe-insert-pair-edit-update "/")

	      ;; Change the movement to 'char.
	      '(ipe-edit--movement-by-char)
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--close-backward nil)
	      '(ipe-edit--close-backward nil)
	      '(ipe-edit--close-backward nil)

	      ;; Change the movement to 'word.
	      '(ipe-edit--movement-by-word)
	      '(ipe-edit--open-backward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--open-backward nil)
	      '(ipe-edit--close-forward nil)

	      ;; Change the movement to 'line.
	      '(ipe-edit--movement-by-line)
	      '(ipe-edit--open-beg nil)
	      '(ipe-edit--close-end nil)
	      '(ipe-edit--open-backward nil)
	      '(ipe-edit--open-backward nil)
	      '(ipe-edit--open-backward nil)
	      '(ipe-edit--open-backward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--open-end nil)
	      '(ipe-edit--close-beg nil)

	      ;; Change back to 'words.
	      '(ipe-edit--movement-by-word)
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--close-backward nil)
	      '(ipe-edit--close-backward nil)
	      '(ipe-edit--close-backward nil)
	      '(ipe-edit--close-backward nil)

	      '(ipe-edit--abort)))

      (run-at-time (setq time (+ time delay))
		   0
		   'apply
		   (lambda (x)
		     (when (buffer-live-p demo-buffer)
		       (with-current-buffer demo-buffer
			 (apply x))))
		   (list command)))

    (set-cursor-color cursor-color)))

(defun ipe-doc-change-contents-demo (&optional speed)
  "Display the \"Insert Pair Edit (ipe)\" Lexical Units demo.

If specified, SPEED represents the delay between actions
within the demo."

  (interactive)

  (let* ((demo-buffer  (get-buffer-create
			"**ipe-change-contents-demo**"))
	 (cursor-color (face-attribute 'cursor :background))
	 (delay        (or speed 0.6))
	 (time         3.0))

    ;; Set up some (ipe) configuration.
    (switch-to-buffer demo-buffer)
    (setq-local ipe-pairs '(("(" "(" ")")))
    (setq-local ipe-update-forward-first-p t)

    ;; Set up the demo buffer.
    (blink-cursor-mode 0)
    (set-cursor-color "grey30")
    (delete-region (point-min) (point-max))
    (insert
     "\n"
     "    The quick (brown) fox (jumps) over (the) lazy dog.\n"
     "\n")
    (goto-char (+ (point-min) 1))

    ;; Run some demonstration (ipe) commands.
    (dolist (command
	     (list
	      ;; Edit the '(' ')' PAIR.
	      '(ipe-insert-pair-edit-update "(")

	      ;; Do some case conversions.
	      '(ipe-edit--contents-upcase)
	      '(ipe-edit--contents-capitalize)
	      '(ipe-edit--contents-downcase)
	      '(ipe-edit--ia-goto-close nil)

	      ;; Do some replacements.
	      '(ipe-insert-pair-edit-update "(")
	      '(ipe-edit--contents-replace nil)
	      '(insert "walks")
	      '(exit-minibuffer)

	      '(ipe-edit--contents-replace nil)
	      '(insert "runs")
	      '(exit-minibuffer)

	      '(ipe-edit--contents-replace nil)
	      '(insert "jumps")
	      '(exit-minibuffer)
	      '(ipe-edit--ia-goto-close nil)

	      ;; Delete.
	      '(ipe-insert-pair-edit-update "(")
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--contents-kill)
	      '(ipe-edit--contents-yank)
	      '(ipe-edit--close-backward nil)
	      '(ipe-edit--close-backward nil)

	      '(ipe-edit--abort)
	      '(beginning-of-line)))

      (run-at-time (setq time (+ time delay))
		   0
		   'apply
		   (lambda (x)
		     (when (buffer-live-p demo-buffer)
		       (if (minibuffer-window-active-p
			    (get-buffer-window (current-buffer)))
			   (apply x)
			 (with-current-buffer demo-buffer
			   (apply x)))))
		   (list command)))

    (set-cursor-color cursor-color)))

(defun ipe-doc-other-pairs-demo (&optional speed)
  "Display the \"Insert Pair Edit (ipe)\" Other Pair demo.

If specified, SPEED represents the delay between actions
within the demo."

  (interactive)

  (let* ((demo-buffer  (get-buffer-create
			"**ipe-other-pairs-demo**"))
	 (cursor-color (face-attribute 'cursor :background))
	 (delay        (or speed 0.6))
	 (time         3.0))

    ;; Set up some (ipe) configuration.
    (switch-to-buffer demo-buffer)
    (setq-local ipe-pairs
		'(("e" "@emph{" "}" (:movement char))))

    ;; Set up the demo buffer.
    (blink-cursor-mode 0)
    (set-cursor-color "grey30")
    (delete-region (point-min) (point-max))
    (insert
     "\n"
     "    The @emph{quick} brown @emph{fox} jumps @emph{over} the\
 lazy dog.\n"
     "    The quick brown fox jumps over the lazy dog.\n"
     "    The quick brown fox jumps over the lazy dog.\n"
     "\n")
    (goto-char (+ (point-min) 1))

    ;; Run some demonstration (ipe) commands.
    (dolist (command
	     (list
	      ;; Edit the '@emph{' '}' PAIR.
	      '(ipe-insert-pair-edit-update "e")

	      ;; Search forward.
	      '(ipe-edit--close-backward 4)
	      '(ipe-edit--update-next-pair nil)
	      '(ipe-edit--close-backward 2)
	      '(ipe-edit--update-next-pair nil)
	      '(ipe-edit--close-backward 3)

	      ;; Search backward.
	      '(ipe-edit--close-forward 3)
	      '(ipe-edit--update-previous-pair nil)
	      '(ipe-edit--close-forward 2)
	      '(ipe-edit--update-previous-pair nil)
	      '(ipe-edit--close-forward 4)

	      ;; OPEN / CLOSE movement.
	      '(ipe-edit--update-next-close nil)
	      '(ipe-edit--update-next-close nil)
	      '(ipe-edit--update-next-open nil)
	      '(ipe-edit--update-next-open nil)

	      ;; Search for contents.
	      '(ipe-edit--update-previous-pair nil)
	      '(ipe-edit--update-previous-pair nil)
	      '(ipe-edit--close-backward 4)
	      '(ipe-edit--update-next-contents)
	      '(ipe-edit--update-next-contents)
	      '(ipe-edit--update-previous-pair nil)
	      '(ipe-edit--update-previous-pair nil)
	      '(ipe-edit--update-previous-pair nil)
	      '(ipe-edit--update-previous-pair nil)
	      '(ipe-edit--abort)
	      '(beginning-of-line)))

      (run-at-time (setq time (+ time delay))
		   0
		   'apply
		   (lambda (x)
		     (when (buffer-live-p demo-buffer)
		       (with-current-buffer demo-buffer
			 (apply x))))
		   (list command)))

    (set-cursor-color cursor-color)))

(defun ipe-doc-multiple-pairs-demo (&optional speed)
  "Display the \"Insert Pair Edit (ipe)\" Other Pair demo.

If specified, SPEED represents the delay between actions
within the demo."

  (interactive)

  (let* ((demo-buffer  (get-buffer-create
			"**ipe-multiple-pairs-demo**"))
	 (cursor-color (face-attribute 'cursor :background))
	 (delay        (or speed 0.6))
	 (time         3.0))

    ;; Set up some (ipe) configuration.
    (switch-to-buffer demo-buffer)
    (setq-local ipe-pairs
		'(("b" "<b>"      "</b>")
		  ("s" "<strong>" "</strong>)")
		  ("i" "<i>"      "</i>")
		  ("e" "<em>"     "</em>)")))

    ;; Set up the demo buffer.
    (blink-cursor-mode 0)
    (set-cursor-color "grey30")
    (delete-region (point-min) (point-max))
    (insert
     "\n"
     "    The <i>quick</i> brown fox jumps <i>over</i> the lazy\
 dog.\n"
     "    The <b>quick</b> brown fox jumps <b>over</b> the lazy\
 dog.\n"
     "    The <b>quick</b> brown fox jumps <i>over</i> the lazy\
 dog.\n"
     "\n")
    (goto-char (point-min))

    ;; Run some demonstration (ipe) commands.
    (dolist (command
	     (list
	      ;; Edit all of the <i> / </i> PAIRs.
	      '(mark-whole-buffer)

	      '(ipe-insert-pair-edit-update "i")
	      '(ipe-edit--change-pair "e")
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--open-backward nil)
	      '(ipe-edit--insert-pair)

	      ;; Edit all of the <b> / </b> PAIRs.
	      '(mark-whole-buffer)

	      '(ipe-insert-pair-edit-update "b")
	      '(ipe-edit--change-pair "s")
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--open-backward nil)
	      '(ipe-edit--insert-pair)

	      ;; Edit all of the <i> / </i> PAIRs.
	      '(mark-whole-buffer)

	      '(ipe-insert-pair-edit-update "e")
	      '(ipe-edit--change-pair "i")
	      '(ipe-edit--close-backward nil)
	      '(ipe-edit--insert-pair)

	      ;; Edit all of the <b> / </b> PAIRs.
	      '(mark-whole-buffer)

	      '(ipe-insert-pair-edit-update "s")
	      '(ipe-edit--change-pair "b")
	      '(ipe-edit--close-backward nil)
	      '(ipe-edit--insert-pair)

	      '(beginning-of-line)))

      (run-at-time (setq time (+ time delay))
		   0
		   'apply
		   (lambda (x)
		     (when (buffer-live-p demo-buffer)
		       (with-current-buffer demo-buffer
			 (apply x))))
		   (list command)))

    (set-cursor-color cursor-color)))

(defun ipe-doc-escape-demo (&optional speed)
  "Display the \"Insert Pair Edit (ipe)\" Escape demo.

If specified, SPEED represents the delay between actions
within the demo."

  (interactive)

  (let* ((demo-buffer  (get-buffer-create
			"**ipe-escape-demo**"))
	 (cursor-color (face-attribute 'cursor :background))
	 (delay        (or speed 0.6))
	 (time         3.0))

    ;; Set up some (ipe) configuration.
    (switch-to-buffer demo-buffer)
    (setq-local ipe-pairs
		'(("\"" "\""      "\""
		   (
		    :escapes (("\"" "\\\"")
			      ("\\" "\\\\"))))
		  ("<" "<!--  " "  -->"
		   (
		    :movement line
		    :infix    "  -- "
		    :escapes  (("<" "&lt;")
			       (">" "&gt;")
			       ("&" "&amp;"))))))
    (setq-local ipe--escapes-show-p t)

    ;; Set up the demo buffer.
    (blink-cursor-mode 0)
    (set-cursor-color "grey30")
    (delete-region (point-min) (point-max))
    (insert
     "\n"
     "    The \"quick\" brown \"fox \\\"jumps\\\" over\" the lazy\
 dog.\n"
     "\n"
     "    <!-- The quick & brown fox <jumps> over the lazy dog.\
 -->\n"
     "\n")
    (goto-char (+ (point-min) 1))

    ;; Run some demonstration (ipe) commands.
    (dolist (command
	     (list
	      ;; Insert a " / " PAIR.
	      '(ipe-insert-pair-edit nil "\"")
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--close-forward nil)
	      '(ipe-edit--toggle-escapes 0)
	      '(ipe-edit--toggle-escapes 1)
	      '(ipe-edit--insert-pair)

	      ;; Update the <!-- / --> PAIR.
	      '(forward-line 1)
	      '(ipe-insert-pair-edit nil "<")
	      '(ipe-edit--close-down nil)
	      '(ipe-edit--close-end nil)
	      '(ipe-edit--toggle-escapes 0)
	      '(ipe-edit--toggle-escapes 1)
	      '(ipe-edit--ia-goto-close nil)

	      ;; Update the " / " PAIR.
	      '(ipe-insert-pair-edit-update "\"")
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--open-forward nil)
	      '(ipe-edit--delete-all-pairs)

	      ;; Update the <!-- / --> PAIR.
	      '(ipe-insert-pair-edit-update "<")))

      (run-at-time (setq time (+ time delay))
		   0
		   'apply
		   (lambda (x)
		     (when (buffer-live-p demo-buffer)
		       (with-current-buffer demo-buffer
			 (apply x))))
		   (list command)))

    (set-cursor-color cursor-color)))

(defun ipe-doc-markdown-demo (&optional speed)
  "Display the \"Insert Pair Edit (ipe)\" markdown-demo.

If specified, SPEED represents the delay between actions
within the demo."

  (interactive)

  (let* ((demo-buffer  (get-buffer-create
			"**ipe-markdown-demo**"))
	 (cursor-color (face-attribute 'cursor :background))
	 (delay        (or speed 0.4))
	 (time         2.0))

    ;; Set up some (ipe) configuration.
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

    ;; Set up the demo buffer.
    (blink-cursor-mode 0)
    (set-cursor-color "grey30")
    (ipe--safecall 'markdown-mode)
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

    ;; Run some demonstration (ipe) commands.
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
		   (lambda (x)
		     (when (buffer-live-p demo-buffer)
		       (with-current-buffer demo-buffer
			 (apply x))))
		   (list command)))

    (set-cursor-color cursor-color)))

(provide 'ipe-doc)

;;; ipe-doc.el ends here
