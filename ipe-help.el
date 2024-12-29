;;; ipe-help.el --- Insert Pair Edit - help text / functions -*- lexical-binding: t; -*-
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
;; This file defines the help text / functions used by the 'Insert
;; Pair Edit' package.  The 'Insert Pair Edit' package supplies the
;; `ipe-insert-pair-edit' command which offers a more feature rich
;; alternative to the standard 'M-(' Emacs keybinding
;; (`insert-parentheses').
;;

(require 'help-mode)

;; -------------------------------------------------------------------
;;; Code:

(defconst ipe-help--help-text
  "The 'Insert Pair Edit' (ipe) Package
------------------------------------

This package defines commands that allow for the interactive,
insertion, update and deletion of `customize'-able, mode-dependent
PAIRs via the use of overlays.

PAIRs can be inserted, updated, deleted, and replaced by:

  \\<global-map>'\\[ipe-insert-pair-edit]' -\
 (command: `ipe-insert-pair-edit')
  `\\[ipe-insert-pair-edit-update]' -\
 (command: `ipe-insert-pair-edit-update')
  `\\[ipe-insert-pair-edit-delete]' -\
 (command: `ipe-insert-pair-edit-delete')
  `\\[ipe-insert-pair-edit-replace]' -\
 (command: `ipe-insert-pair-edit-replace')

These commands prompt for, and then insert, update, delete or replace,
PAIRs from the:

  `ipe-pairs'
  `ipe-mode-pairs'

`customize'-able variables.

Once inserted, `ipe-edit-mode' minor mode is activated, and the
OPEN and CLOSE strings of the PAIR can be moved interactively and
independently around the buffer using overlays before being
inserted into position.

Key-bindings for Insert Pair Edit (ipe) commands can be `customize'd
via the command:

  \\[customize-group] ipe-keys

")

(defconst ipe-help--help-more-1
  "The 'Insert Pair Edit' (ipe) Package - Additional Commands #1
-------------------------------------------------------------

When in `ipe-edit-mode' the following commands are available:

'Insert Pair Edit' minor mode - Movement: \\<ipe-edit-mode-map>

  \\<ipe-edit-mode-map>`\\[ipe-edit--open-beg]' -\
 Move OPEN to 'beginning'. (command: `ipe-edit--open-beg')
  `\\[ipe-edit--open-up]' -\
 Move OPEN 'up'.           (command: `ipe-edit--open-up')
  `\\[ipe-edit--open-backward]' -\
 Move OPEN 'backward'.     (command: `ipe-edit--open-backward')
  `\\[ipe-edit--open-forward]' -\
 Move OPEN 'forward'.      (command: `ipe-edit--open-forward')
  `\\[ipe-edit--open-down]' -\
 Move OPEN 'down'.         (command: `ipe-edit--open-down')
  `\\[ipe-edit--open-end]' -\
 Move OPEN to the 'end'.   (command: `ipe-edit--open-end')

  `\\[ipe-edit--close-beg]' -\
 Move CLOSE to 'beginning'.(command: `ipe-edit--close-beg')
  `\\[ipe-edit--close-up]' -\
 Move CLOSE 'up'.          (command: `ipe-edit--close-up')
  `\\[ipe-edit--close-backward]' -\
 Move CLOSE 'backward'.    (command: `ipe-edit--close-backward')
  `\\[ipe-edit--close-forward]' -\
 Move CLOSE 'forward'.     (command: `ipe-edit--close-forward')
  `\\[ipe-edit--close-down]' -\
 Move CLOSE 'down'.        (command: `ipe-edit--close-down')
  `\\[ipe-edit--close-end]' -\
 Move CLOSE to the 'end'.  (command: `ipe-edit--close-end')

All basic 'ipe-edit-mode' movement key bindings can be edited via:
  `\\[customize-variable] ipe-edit-movement-keysets'

'Insert Pair Edit' minor mode - Insert and Abort:

  `\\[ipe-edit--insert-pair]' - Insert OPEN and CLOSE and exit\
 'ipe-edit-mode'.
	  (command: `ipe-edit--insert-pair')
  `\\[ipe-edit--abort]' - Exit 'ipe-edit-mode' without inserting\
 OPEN and CLOSE.
	  (command: `ipe-edit--abort')

All 'ipe-edit-mode' key bindings can be edited via:
  `\\[customize-variable] ipe-edit-mode-keys'"
  "Help text for `ipe-help'.")

(defconst ipe-help--help-more-2
  "The 'Insert Pair Edit' (ipe) Package - Additional Commands #2
-------------------------------------------------------------

'Insert Pair Edit' minor mode - Insert And... 'Action':

  \\<ipe-edit-mode-map>`\\[ipe-edit--ia-goto-open]' - Insert\
 OPEN and CLOSE, exit 'ipe-edit-mode', and move the
	cursor to the end of the OPEN string.
	(command: `ipe-edit--ia-goto-open')
  `\\[ipe-edit--ia-goto-close]' - Insert OPEN and CLOSE, exit\
 'ipe-edit-mode', and move the
	cursor to the end of the CLOSE string.
	(command: `ipe-edit--ia-goto-close')
  `\\[ipe-edit--ia-resume]' - Insert OPEN and CLOSE, exit\
 'ipe-edit-mode', and leave the
	cursor at its current position.
	(command: `ipe-edit--ia-resume')
  `\\[ipe-edit--ia-copy-text]' - Insert OPEN and CLOSE, exit\
 'ipe-edit-mode', and copy the text
	between the OPEN and CLOSE to the `kill-ring'.
	(command: `ipe-edit--ia-copy-text')
  `\\[ipe-edit--ia-kill-text]' - Insert OPEN and CLOSE, exit\
 'ipe-edit-mode', and kill the text
	between the OPEN and CLOSE.
	(command: `ipe-edit--ia-kill-text')
  `\\[ipe-edit--ia-update-forward]' - Insert OPEN and CLOSE,\
 prompt for another MNEMONIC, and
	update the next PAIR that matches the PAIR associated with
	the MNEMONIC.
	(command: `ipe-edit--ia-update-forward')
  `\\[ipe-edit--ia-update-backward]' - Insert OPEN and CLOSE,\
 prompt for another MNEMONIC, and
	update the previous PAIR that matches the PAIR associated
	with the MNEMONIC.
	(command: `ipe-edit--ia-update-backward')"
  "Help text for `ipe-help'.")

(defconst ipe-help--help-more-3
  "The 'Insert Pair Edit' (ipe) Package - Additional Commands #3
-------------------------------------------------------------

'Insert Pair Edit' minor mode - Change Movement:

  \\<ipe-edit-mode-map>`\\[ipe-edit--movement-by-char]' -\
 Change the movement of the OPEN and CLOSE by the
	`Insert Pair Edit' commands to be 'by char'.
	(command: `ipe-edit--movement-by-char')
  `\\[ipe-edit--movement-by-word]' -\
 Change the movement of the OPEN and CLOSE by the
	`Insert Pair Edit' commands to be 'by word'.
	(command: `ipe-edit--movement-by-word')
  `\\[ipe-edit--movement-by-line]' -\
 Change the movement of the OPEN and CLOSE by the
	`Insert Pair Edit' commands to be 'by line'.
	(command: `ipe-edit--movement-by-line')
  `\\[ipe-edit--movement-by-list]' -\
 Change the movement of the OPEN and CLOSE by the
	`Insert Pair Edit' commands to be 'by list'.
	(command: `ipe-edit--movement-by-list')"
  "Help text for `ipe-help'.")

(defconst ipe-help--help-more-4
  "The 'Insert Pair Edit' (ipe) Package - Additional Commands #4
-------------------------------------------------------------

'Insert Pair Edit' minor mode - Edit CONTENTs:

  \\<ipe-edit-mode-map>`\\[ipe-edit--contents-kill]' - Kill the text\
 enclosed by the OPEN and CLOSE.
	(command: `ipe-edit--contents-kill')
  `\\[ipe-edit--contents-copy]' - Copy the text enclosed by the OPEN\
 and CLOSE to the
	`kill-ring'.
	(command: `ipe-edit--contents-copy')
  `\\[ipe-edit--contents-yank]' - Replace the text enclosed by the\
 OPEN and CLOSE with the
	last killed text.
	(command: `ipe-edit--contents-yank')
  `\\[ipe-edit--contents-replace]' - Replace the text enclosed by the\
 OPEN and CLOSE with input
	text.
	(command: `ipe-edit--contents-replace')
  `\\[ipe-edit--contents-trim]' - Trim whitespace from around the\
 text enclosed by the
	OPEN and CLOSE.
	(command: `ipe-edit--contents-trim')
  `\\[ipe-edit--contents-upcase]' - Convert the text enclosed by the\
 OPEN and CLOSE to UPPERCASE.
	(command: `ipe-edit--contents-upcase')
  `\\[ipe-edit--contents-capitalize]' - Convert the text enclosed by\
 the OPEN and CLOSE to Capital
	Case.
	(command: `ipe-edit--contents-capitalize')
  `\\[ipe-edit--contents-downcase]' - Convert the text enclosed by\
 the OPEN and CLOSE to lowercase.
	(command: `ipe-edit--contents-downcase')"
  "Help text for `ipe-help'.")

(defconst ipe-help--help-more-5
  "The 'Insert Pair Edit' (ipe) Package - Additional Commands #5
-------------------------------------------------------------

'Insert Pair Edit' minor mode - Next / Previous:

  \\<ipe-edit-mode-map>`\\[ipe-edit--update-next-pair]' -\
 Insert the current PAIR and search forward for next matching
	PAIR.
	(command: `ipe-edit--update-next-pair')
  `\\[ipe-edit--update-next-contents]' - Insert the current PAIR,\
 search forward for the text
	between OPEN and CLOSE, and create another PAIR.
	(command: `ipe-edit--update-next-contents')
  `\\[ipe-edit--update-next-open]' - Insert current OPEN and search\
 forward for the next OPEN.
	(command: `ipe-edit--update-next-open')
  `\\[ipe-edit--update-next-close]' - Insert current CLOSE and search\
 forward for the next CLOSE.
	(command: `ipe-edit--update-next-close')

  `\\[ipe-edit--update-previous-pair]' - Insert current PAIR and\
 search backward for the previous
	matching PAIR.
	(command: `ipe-edit--update-previous-pair')
  `\\[ipe-edit--update-previous-contents]' - Insert the current PAIR,\
 search backward for the text
	between OPEN and CLOSE, and create another PAIR.
	(command: `ipe-edit--update-previous-contents')
  `\\[ipe-edit--update-previous-open]' - Insert the current OPEN and\
 search backward for the previous
	OPEN.
	(command: `ipe-edit--update-previous-open')
  `\\[ipe-edit--update-previous-close]' - Insert the current CLOSE\
 and search forward for the previous
	CLOSE.
	(command: `ipe-edit--update-previous-close')"
  "Help text for `ipe-help'.")

(defconst ipe-help--help-more-6
  "The 'Insert Pair Edit' (ipe) Package - Additional Commands #6
-------------------------------------------------------------

'Insert Pair Edit' minor mode - Multiple PAIRs:

  \\<ipe-edit-mode-map>`\\[ipe-edit--add-next-pair]' -\
 Add another PAIR to the buffer by searching forward for the
	next OPEN and CLOSE, deleting them from the buffer, and
	replacing them with a PAIR.
	(command: `ipe-edit--add-next-pair')
  `\\[ipe-edit--add-previous-pair]' -\
 Add another PAIR to the buffer by searching backward for the
	previous OPEN and CLOSE, deleting them from the buffer, and
	replacing them with a PAIR.
	(command: `ipe-edit--add-previous-pair')
  `\\[ipe-edit--add-next-contents]' -\
 Add another PAIR to the buffer by searching forward for text
	between OPEN and CLOSE.
	(command: `ipe-edit--add-next-contents')
  `\\[ipe-edit--add-previous-contents]' -\
 Add another PAIR to the buffer by searching backward for text
	between OPEN and CLOSE.
	(command: `ipe-edit--add-previous-contents')

  `\\[ipe-edit--insert-first-pair]' -\
 Add the first PAIR to the buffer.
	(command: `ipe-edit--insert-first-pair')
  `\\[ipe-edit--insert-last-pair]' -\
 Add the last PAIR to the buffer.
	(command: `ipe-edit--insert-last-pair')

  `\\[ipe-edit--delete-first-pair]' -\
 Remove the first PAIR from the buffer.
	(command: `ipe-edit--delete-first-pair')
  `\\[ipe-edit--delete-all-pairs]' -\
 Remove all PAIRs from the buffer.
	(command: `ipe-edit--delete-all-pairs')
  `\\[ipe-edit--delete-last-pair]' -\
 Remove the last PAIR from the buffer.
	(command: `ipe-edit--delete-last-pair')"
  "Help text for `ipe-help'.")

(defconst ipe-help--help-more-7
  "The 'Insert Pair Edit' (ipe) Package - Additional Commands #7
-------------------------------------------------------------

'Insert Pair Edit' minor mode - Edit PAIR Definitions:

  \\<ipe-edit-mode-map>`\\[ipe-edit--edit-current-pair]' - \
 Edit the definition of the currently active PAIR.
	(command: `ipe-edit--edit-current-pair')
  `\\[ipe-defn--edit-pair]' - Add / Edit the\
 definition of a global PAIR.
	(command: `ipe-defn--edit-pair')
  `\\[ipe-defn--edit-mode-pair]' - Add / Edit the definition of a\
 modal PAIR.
	(command: `ipe-defn--edit-mode-pair')
  `\\[ipe-defn--change-pair-mnemonic]' - Change the MNEMONIC of\
 an existing global PAIR.
	(command: `ipe-defn--change-pair-mnemonic')
  `\\[ipe-defn--change-mode-pair-mnemonic]' - Change the MNEMONIC\
 of an existing model PAIR.
	(command: `ipe-defn--change-mode-pair-mnemonic'
  `\\[ipe-defn--delete-pair]' - Remove the definition of a global\
 PAIR.
	(command: `ipe-defn--delete-pair')
  `\\[ipe-defn--delete-mode-pair]' - Remove the definition of a\
 modal PAIR.
	(command: `ipe-defn--delete-mode-pair')"
  "Help text for `ipe-help'.")

(defconst ipe-help--help-more-8
  "The 'Insert Pair Edit' (ipe) Package - Additional Commands #8
-------------------------------------------------------------

'Insert Pair Edit' minor mode - Other:

  \\<ipe-edit-mode-map>`\\[ipe-edit--change-pair]' - Replace the\
 current PAIR with a different one.
	(command: `ipe-edit--change-pair')
  `\\[ipe-edit--toggle-escapes]' - Turn on/off the processing\
 of ESCAPES for the current PAIR.
	(command: `ipe-edit--toggle-escapes')
  `\\[ipe-edit--recenter-pair]' - Recenter the display around\
 the current PAIR.
	(command: `ipe-edit--recenter-pair')

  `\\[ipe-options]' - Display the 'Insert Pair Edit'\
 `customize'-ation options.
	(command: `ipe-options')
  `\\[ipe-help-info]' - Display the 'Insert Pair Edit' (ipe)\
 info file.
	(command: `ipe-help-info')
  `\\[ipe-help-edit-mode]' - Display help on the commands\
 available within `ipe-edit-mode'.
	(command: `ipe-help-edit-mode')

PAIRs are highlighted using:

- OPEN   - is highlighted with: (face: `ipe-open-highlight')
- CLOSE  - is highlighted with: (face: `ipe-close-highlight')
- INFIX  - is highlighted with: (face: `ipe-infix-highlight')
- ESCAPE - is highlighted with: (face: `ipe-escape-highlight')

If the `ipe-delete-action' is set to (`highlight', `fade' or
`prompt', PAIRs to be deleted are highlighted using:

  (face: `ipe-delete-highlight')"

  "Help text for `ipe-help'.")

(defun ipe-help ()
  "Display help for the 'Insert Pair Edit' package.

\(command `ipe-edit-mode')"

  (interactive)

  (with-help-window "*Help*"
    (set-buffer standard-output)
    (setq-local tab-width 8)
    (help-setup-xref (list #'ipe-help) t)
    (insert (substitute-command-keys ipe-help--help-text)))

  (with-current-buffer (get-buffer "*Help*")
    (let ((buffer-read-only nil))
      (setq-local tab-width 8)
      (goto-char (point-min))
      (forward-line 3)
      (help-insert-xref-button
       "[More 'Insert Pair Edit' Commands (1)]"
       'ipe-help--more-1-button)
      (insert "       ")
      (help-insert-xref-button "['Insert Pair Edit' Info]"
			       'ipe-help--info-button)
      (insert " \n\n")

      (goto-char (point-min))
      (forward-button 1)
      (set-window-point
       (get-buffer-window (current-buffer)) (point))))
  "")

(defun ipe-help--more-1 ()
  "Display more help for the 'Insert Pair Edit' package.

\(command `ipe-edit-mode')"

  (interactive)

  (with-help-window "*Help*"
    (set-buffer standard-output)
    (setq-local tab-width 8)
    (help-setup-xref (list #'ipe-help--more-1) t)
    (insert (substitute-command-keys ipe-help--help-more-1)))

  (with-current-buffer (get-buffer "*Help*")
    (let ((buffer-read-only nil))
      (setq-local tab-width 8)
      (goto-char (point-min))
      (forward-line 3)
      (help-insert-xref-button
       "[More 'Insert Pair Edit' Commands (2)]"
       'ipe-help--more-2-button)
      (insert "       ")
      (help-insert-xref-button "['Insert Pair Edit' Info]"
			       'ipe-help--info-button)
      (insert " \n\n")

      (goto-char (point-min))
      (forward-button 1)
      (set-window-point
       (get-buffer-window (current-buffer)) (point)))))

(defun ipe-help--more-2 ()
  "Display more help for the 'Insert Pair Edit' package.

\(command `ipe-edit-mode')"

  (interactive)

  (with-help-window "*Help*"
    (set-buffer standard-output)
    (setq-local tab-width 8)
    (help-setup-xref (list #'ipe-help--more-2) t)
    (insert (substitute-command-keys ipe-help--help-more-2)))

  (with-current-buffer (get-buffer "*Help*")
    (let ((buffer-read-only nil))
      (setq-local tab-width 8)
      (goto-char (point-min))
      (forward-line 3)
      (help-insert-xref-button
       "[More 'Insert Pair Edit' Commands (3)]"
       'ipe-help--more-3-button)
      (insert "       ")
      (help-insert-xref-button "['Insert Pair Edit' Info]"
			       'ipe-help--info-button)
      (insert " \n\n")

      (goto-char (point-min))
      (forward-button 1)
      (set-window-point
       (get-buffer-window (current-buffer)) (point)))))

(defun ipe-help--more-3 ()
  "Display more help for the 'Insert Pair Edit' package.

\(command `ipe-edit-mode')"

  (interactive)

  (with-help-window "*Help*"
    (set-buffer standard-output)
    (setq-local tab-width 8)
    (help-setup-xref (list #'ipe-help--more-3) t)
    (insert (substitute-command-keys ipe-help--help-more-3)))

  (with-current-buffer (get-buffer "*Help*")
    (let ((buffer-read-only nil))
      (setq-local tab-width 8)
      (goto-char (point-min))
      (forward-line 3)
      (help-insert-xref-button
       "[More 'Insert Pair Edit' Commands (4)]"
       'ipe-help--more-4-button)
      (insert "       ")
      (help-insert-xref-button "['Insert Pair Edit' Info]"
			       'ipe-help--info-button)
      (insert " \n\n")

      (goto-char (point-min))
      (forward-button 1)
      (set-window-point
       (get-buffer-window (current-buffer)) (point)))))

(defun ipe-help--more-4 ()
  "Display more help for the 'Insert Pair Edit' package.

\(command `ipe-edit-mode')"

  (interactive)

  (with-help-window "*Help*"
    (set-buffer standard-output)
    (setq-local tab-width 8)
    (help-setup-xref (list #'ipe-help--more-4) t)
    (insert (substitute-command-keys ipe-help--help-more-4)))

  (with-current-buffer (get-buffer "*Help*")
    (let ((buffer-read-only nil))
      (setq-local tab-width 8)
      (goto-char (point-min))
      (forward-line 3)
      (help-insert-xref-button
       "[More 'Insert Pair Edit' Commands (5)]"
       'ipe-help--more-5-button)
      (insert "       ")
      (help-insert-xref-button "['Insert Pair Edit' Info]"
			       'ipe-help--info-button)
      (insert " \n\n")

      (goto-char (point-min))
      (forward-button 1)
      (set-window-point
       (get-buffer-window (current-buffer)) (point)))))

(defun ipe-help--more-5 ()
  "Display more help for the 'Insert Pair Edit' package.

\(command `ipe-edit-mode')"

  (interactive)

  (with-help-window "*Help*"
    (set-buffer standard-output)
    (setq-local tab-width 8)
    (help-setup-xref (list #'ipe-help--more-5) t)
    (insert (substitute-command-keys ipe-help--help-more-5)))

  (with-current-buffer (get-buffer "*Help*")
    (let ((buffer-read-only nil))
      (setq-local tab-width 8)
      (goto-char (point-min))
      (forward-line 3)
      (help-insert-xref-button
       "[More 'Insert Pair Edit' Commands (6)]"
       'ipe-help--more-6-button)
      (insert "       ")
      (help-insert-xref-button "['Insert Pair Edit' Info]"
			       'ipe-help--info-button)
      (insert " \n\n")

      (goto-char (point-min))
      (forward-button 1)
      (set-window-point
       (get-buffer-window (current-buffer)) (point)))))

(defun ipe-help--more-6 ()
  "Display more help for the 'Insert Pair Edit' package.

\(command `ipe-edit-mode')"

  (interactive)

  (with-help-window "*Help*"
    (set-buffer standard-output)
    (setq-local tab-width 8)
    (help-setup-xref (list #'ipe-help--more-6) t)
    (insert (substitute-command-keys ipe-help--help-more-6))
    (goto-char (point-max)))

  (with-current-buffer (get-buffer "*Help*")
    (let ((buffer-read-only nil))
      (setq-local tab-width 8)
      (goto-char (point-min))
      (forward-line 3)
      (help-insert-xref-button
       "[More 'Insert Pair Edit' Commands (7)]"
       'ipe-help--more-7-button)
      (insert "       ")
      (help-insert-xref-button "['Insert Pair Edit' Info]"
			       'ipe-help--info-button)
      (insert " \n\n")

      (goto-char (point-min))
      (forward-button 1)
      (set-window-point
       (get-buffer-window (current-buffer)) (point)))))

(defun ipe-help--more-7 ()
  "Display more help for the 'Insert Pair Edit' package.

\(command `ipe-edit-mode')"

  (interactive)

  (with-help-window "*Help*"
    (set-buffer standard-output)
    (setq-local tab-width 8)
    (help-setup-xref (list #'ipe-help--more-7) t)
    (insert (substitute-command-keys ipe-help--help-more-7)))

  (with-current-buffer (get-buffer "*Help*")
    (let ((buffer-read-only nil))
      (setq-local tab-width 8)
      (goto-char (point-min))
      (forward-line 3)
      (help-insert-xref-button
       "[More 'Insert Pair Edit' Commands (8)]"
       'ipe-help--more-8-button)
      (insert "       ")
      (help-insert-xref-button "['Insert Pair Edit' Info]"
			       'ipe-help--info-button)
      (insert " \n\n")

      (goto-char (point-min))
      (forward-button 1)
      (set-window-point
       (get-buffer-window (current-buffer)) (point)))))

(defun ipe-help--more-8 ()
  "Display more help for the 'Insert Pair Edit' package.

\(command `ipe-edit-mode')"

  (interactive)

  (with-help-window "*Help*"
    (set-buffer standard-output)
    (setq-local tab-width 8)
    (help-setup-xref (list #'ipe-help--more-8) t)
    (insert (substitute-command-keys ipe-help--help-more-8)))

  (with-current-buffer (get-buffer "*Help*")
    (let ((buffer-read-only nil))
      (setq-local tab-width 8)
      (goto-char (point-min))
      (forward-line 3)
      (help-insert-xref-button "['Insert Pair Edit' Help]"
			       'ipe-help--help-button)
      (insert "                    ")
      (help-insert-xref-button "['Insert Pair Edit' Info]"
			       'ipe-help--info-button)
      (insert " \n\n")

      (goto-char (point-min))
      (forward-button 1)
      (set-window-point
       (get-buffer-window (current-buffer)) (point)))))

(defun ipe-help-edit-mode ()
  "Display help for the 'Insert Pair Edit' minor mode.

\(command `ipe-edit-mode')"

  (interactive)

  (let ((edit-mode ipe-edit-mode))
    (with-help-window "*Help*"
      (set-buffer standard-output)
      (setq-local tab-width 8)
      (help-setup-xref (list #'ipe-help-edit-mode) t)
      (insert (substitute-command-keys
	       (concat "'Insert Pair Edit' Help
------------------------
"

		       (if edit-mode "
You are currently in the 'Insert Pair Edit' minor mode:"
			 "
The help describes the:
")
		       "
      (command: `ipe-edit-mode').

This mode is generally launched by the function
`ipe-insert-pair-edit', by typing:

   \\<global-map>`\\[ipe-insert-pair-edit]'

Or from the 'Edit -> Pairs' menu.

To exit this minor mode, type:

   \\<ipe-edit-mode-map>`\\[ipe-edit--abort]'.

This minor mode forms part of the 'Insert Pair Edit' (ipe) package.

" ipe-help--help-text "
---

" ipe-help--help-more-1 "
---

" ipe-help--help-more-2 "
---

" ipe-help--help-more-3 "
---

" ipe-help--help-more-4 "
---

" ipe-help--help-more-5 "
---

" ipe-help--help-more-6 "
---

" ipe-help--help-more-7 "
---

" ipe-help--help-more-8)))
      "")))

(defun ipe-help-prompt ()
  "Display the 'Insert Pair Edit' prompt help."

  (interactive)

  (let ((help-text (substitute-command-keys
		    (concat "'Insert Pair Edit' Prompt Help
------------------------------

This prompt is generally launched by the function
`ipe-insert-pair-edit', by typing:

    '\\<global-map>\\[ipe-insert-pair-edit]'

At this prompt you may either:

    Enter a MNEMONIC from the `ipe-pairs' / `ipe-mode-pairs'
    variable.

Or, use one the following key bindings:

    \\<ipe-read--minibuffer-keymap>'\\[minibuffer-complete]' -\
 Display the list of available MNEMONICS.
	    (command: `minibuffer-complete')
    '\\[abort-recursive-edit]' -\
 Exit the 'Insert Pair Edit' prompt without selecting a
	    PAIR.
	    (command: `minibuffer-keyboard-quit')

    '\\[ipe-help-prompt]' -\
 Display this Help.
	    (command: `ipe-help-prompt')
    '\\[ipe-help-info]' -\
 Display the 'Insert Pair Edit' Info.
	    (command: `ipe-help-info')
    '\\[ipe-options]' -\
 Customize the 'Insert Pair Edit' options.
	    (command: `ipe-options')

    '\\[ipe-defn--edit-pair]' -\
 Create a new 'Insert Pair Edit' PAIR Definition.
	    (command: `ipe-defn--edit-pair')
    '\\[ipe-defn--edit-mode-pair]' -\
 Create a new Mode-Specific 'Insert Pair Edit' PAIR
	    Definition.
	    (command: `ipe-defn--edit-mode-pair')
    '\\[ipe-defn--change-pair-mnemonic]' -\
 Change the MNEMONIC for an existing 'Insert Pair Edit'
	    PAIR Definition.
	    (command: `ipe-defn--change-pair-mnemonic')
    '\\[ipe-defn--change-mode-pair-mnemonic]' -\
 Change the MNEMONIC for an existing Mode-Specific 'Insert
	    Pair Edit' PAIR Definition.
	    (command: `ipe-defn--change-mode-pair-mnemonic')
    '\\[ipe-defn--delete-pair]' -\
 Delete an 'Insert Pair Edit' PAIR Definition.
	    (command: `ipe-defn--delete-pair')
    '\\[ipe-defn--delete-mode-pair]' -\
 Delete a Mode-Specific 'Insert Pair Edit' PAIR Definition.
	    (command: `ipe-defn--delete-mode-pair')

"))))

    (with-help-window "*Help*"
      (set-buffer standard-output)
      (setq-local tab-width 8)
      (help-setup-xref (list #'ipe-help-prompt) t)
      (insert help-text)
      "")))

(defun ipe-help-info ()
  "Display the 'Insert Pair Edit' info help."

  (interactive)

  (let ((dir (if (symbol-file 'ipe-help--help-text)
		 (concat "/" (file-name-directory (symbol-file
						   'ipe-help--help-text)))
	       "")))
    (message (concat "loading ipe.info from [" dir "]"))
    (info (concat dir "ipe.info"))))

(define-button-type 'ipe-help--help-button
  :supertype 'help-xref
  'help-function 'ipe-help)

(define-button-type 'ipe-help--more-1-button
  :supertype 'help-xref
  'help-function 'ipe-help--more-1)

(define-button-type 'ipe-help--more-2-button
  :supertype 'help-xref
  'help-function 'ipe-help--more-2)

(define-button-type 'ipe-help--more-3-button
  :supertype 'help-xref
  'help-function 'ipe-help--more-3)

(define-button-type 'ipe-help--more-4-button
  :supertype 'help-xref
  'help-function 'ipe-help--more-4)

(define-button-type 'ipe-help--more-5-button
  :supertype 'help-xref
  'help-function 'ipe-help--more-5)

(define-button-type 'ipe-help--more-6-button
  :supertype 'help-xref
  'help-function 'ipe-help--more-6)

(define-button-type 'ipe-help--more-7-button
  :supertype 'help-xref
  'help-function 'ipe-help--more-7)

(define-button-type 'ipe-help--more-8-button
  :supertype 'help-xref
  'help-function 'ipe-help--more-8)

(define-button-type 'ipe-help--info-button
  :supertype 'help-xref
  'help-function 'ipe-help-info)

(provide 'ipe-help)

;;; ipe-help.el ends here
