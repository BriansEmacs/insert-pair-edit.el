;;; ipe-edit.el --- Insert Pair Edit - editing minor mode -*- lexical-binding: t; -*-
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
;; This file defines the minor mode `ipe-edit-mode' started by the
;; `ipe-insert-pair-edit' command.
;;
;; The `ipe-edit-mode' supplies commands to interactively and
;; independently move the overlays representing the OPEN and CLOSE
;; strings for a PAIR created by one of the `ipe-insert-pair-edit'
;; commands.
;;
;; `ipe-insert-pair-edit' will prompt the user to enter a
;; `customize'-able MNEMONIC that selects a 'major-mode dependent'
;; PAIR to be inserted around point.  The PAIR consists of OPEN and
;; CLOSE strings which delimit text in some fashion.
;;
;; These OPEN and CLOSE strings are inserted into the current buffer
;; as overlays by the `ipe-insert-pair-edit' command.  The
;; `ipe-insert-pair-edit' then starts the `ipe-edit-mode' to enter the
;; 'Insert Pair Edit' (ipe) minor mode.
;;
;; Commands are supplied by this minor mode to move these OPEN and
;; CLOSE overlays about the buffer, and, to either: insert
;; (`ipe-edit--insert-pair'), or discard (`ipe-edit--abort') them once
;; they have been correctly positioned.
;;
;; Movement of the OPEN and CLOSE overlays is based upon 'lexical
;; units'.  The 'Lexical units' are either: characters, words, lines,
;; or lists (S-expressions).
;;
;; By default, movement will be by 'words', but this can be changed
;; interactively via the: `ipe-edit--movement-by-*' commands or by
;; `ipe-pairs' / `ipe-mode-pairs' `customize'-ations.
;;
;; Additional commands are supplied to:
;;
;; * Perform extra operations when inserting the OPEN and CLOSE
;;   strings.  ('Insert And...' Commands.)
;;
;; * Change the OPEN and CLOSE strings used as overlays.  ('Change
;;   PAIR' Command.)
;;
;; * Change the lexical unit used by the movement commands.  ('Change
;;   Movement' Commands.)
;;
;; * Operate on the CONTENTS of the PAIR (i.e. the text enclosed
;;   between the OPEN and CLOSE overlays.) Text can be copied,
;;   deleted, replaced and case converted.  ('Edit CONTENTS'
;;   Commands.)
;;
;; * Search for 'other' PAIRs ('Next / Previous' Commands.)
;;
;; * Add / delete extra PAIRs ('Multiple' Commands.)
;;
;; Customizations for the mode can be found under the `ipe' group.

;; -------------------------------------------------------------------
;;; Code:

(require 'ipe-core)
(require 'ipe-read)
(require 'ipe-updt)
(require 'ipe-defn)
(require 'ipe-help)
(require 'ipe-char)
(require 'ipe-word)
(require 'ipe-line)
(require 'ipe-list)

;; -------------------------------------------------------------------
;;;; Internal Variables.
;; -------------------------------------------------------------------

(defvar ipe-edit--movement-keysets '(modifiers arrow)
  "Internal variable backing `ipe-edit-movement-keysets'.

A list of one or more symbols:

- `'modifiers'
- `'alpha'
- `'arrow'
- `'wasd'
- `'vi'
- `'custom'

That specify the pre-defined movement keysets to be used with the
Insert Pair Edit (ipe) minor-mode (command: `ipe-edit-mode').  The
value of this variable is set on change of the `customize'-able
`ipe-edit-movement-keysets' variable by
`ipe-edit--movement-keysets-set'.")

(defvar ipe-edit--custom-movement-keyset nil
  "Internal variable backing `ipe-edit-movement-keysets'.

If `ipe-edit--movement-keysets' contains `'custom', this variable
should be a list of 12 key bindings.  These 12 key-bindings map to:

- Move OPEN Beginning  (command: `ipe-edit--open-beg')
- Move OPEN Up         (command: `ipe-edit--open-up')
- Move OPEN Backward   (command: `ipe-edit--open-backward')
- Move OPEN Forward    (command: `ipe-edit--open-forward')
- Move OPEN Down       (command: `ipe-edit--open-down')
- Move OPEN End        (command: `ipe-edit--open-end')

- Move CLOSE Beginning (command: `ipe-edit--close-beg')
- Move CLOSE Up        (command: `ipe-edit--close-up')
- Move CLOSE Backward  (command: `ipe-edit--close-forward')
- Move CLOSE Forward   (command: `ipe-edit--close-backward')
- Move CLOSE Down      (command: `ipe-edit--close-down')
- Move CLOSE End       (command: `ipe-edit--close-end')

The value of this variable is set on change of the `customize'-able
`ipe-edit-movement-keysets' variable by
`ipe-edit--movement-keysets-set'.")

;; -------------------------------------------------------------------
;;;; `ipe-edit-mode' minor mode.
;; -------------------------------------------------------------------

(defvar ipe-edit-mode-map
  (make-sparse-keymap "Insert Pair Edit (ipe) - minor mode Keymap")
  "The Keymap for the Insert Pair Edit (ipe) minor mode.

\(command: `ipe-edit-mode')")

(defvar ipe-edit-mode-movement-map
  (make-sparse-keymap "Insert Pair Edit (ipe) - movement Keymap")
  "The Keymap for Insert Pair Edit (ipe) change movement functions.

\(command: `ipe-edit-mode')")

(defvar ipe-edit-insert-hook '()
  "This hook is called before the `ipe-edit--insert-pair' command.")

(define-minor-mode ipe-edit-mode
  "Toggle movement of inserted Insert Pair Edit (ipe) PAIRs.

This mode is launched programmatically by a set of commands that allow
for the interactive, mode-dependent, insertion and movement of the
opening (OPEN) and closing (CLOSE) strings of a PAIR.

PAIRs can be inserted, updated, deleted, and replaced by:

  `\\[ipe-insert-pair-edit]' -\
 (command: `ipe-insert-pair-edit')
  `\\[ipe-insert-pair-edit-update]' -\
 (command: `ipe-insert-pair-edit-update')
  `\\[ipe-insert-pair-edit-delete]' -\
 (command: `ipe-insert-pair-edit-delete')
  `\\[ipe-insert-pair-edit-replace]' -\
 (command: `ipe-insert-pair-edit-replace')

Which prompt for, and then insert, update, delete or replace, PAIRs
from the:

  `ipe-pairs'
  `ipe-mode-pairs'

`customize'-able variables.

Once inserted, `ipe-edit-mode' minor mode is activated, and the OPEN
and CLOSE strings of the PAIR can be moved interactively and
independently around the buffer using overlays before being inserted
into position.

Insert Pair Edit minor mode - Movement: \\<ipe-edit-mode-map>

  \\<ipe-edit-mode-map>`\\[ipe-edit--open-beg]' -\
 Move OPEN to `beginning'.   (command: `ipe-edit--open-beg')
  `\\[ipe-edit--open-up]' -\
 Move OPEN `up'.             (command: `ipe-edit--open-up')
  `\\[ipe-edit--open-backward]' -\
 Move OPEN `backward'.       (command: `ipe-edit--open-backward')
  `\\[ipe-edit--open-forward]' -\
 Move OPEN `forward'.        (command: `ipe-edit--open-forward')
  `\\[ipe-edit--open-down]' -\
 Move OPEN `down'.           (command: `ipe-edit--open-down')
  `\\[ipe-edit--open-end]' -\
 Move OPEN to the `end'.     (command: `ipe-edit--open-end')

  `\\[ipe-edit--close-beg]' -\
 Move CLOSE to `beginning'.  (command: `ipe-edit--close-beg')
  `\\[ipe-edit--close-up]' -\
 Move CLOSE `up'.            (command: `ipe-edit--close-up')
  `\\[ipe-edit--close-backward]' -\
 Move CLOSE `backward'.      (command: `ipe-edit--close-backward')
  `\\[ipe-edit--close-forward]' -\
 Move CLOSE `forward'.       (command: `ipe-edit--close-forward')
  `\\[ipe-edit--close-down]' -\
 Move CLOSE `down'.          (command: `ipe-edit--close-down')
  `\\[ipe-edit--close-end]' -\
 Move CLOSE to the `end'.    (command: `ipe-edit--close-end')

All basic `ipe-edit-mode' movement key bindings can be edited via:
 `customize-variable' `ipe-edit-movement-keysets'

Insert Pair Edit minor mode - Insert and Abort:

  `\\[ipe-edit--insert-pair]' - Insert OPEN and CLOSE and exit\
 `ipe-edit-mode'.
	  (command: `ipe-edit--insert-pair')
  `\\[ipe-edit--abort]' - Exit `ipe-edit-mode' without inserting\
 OPEN and CLOSE .
	  (command: `ipe-edit--abort')

All `ipe-edit-mode' key bindings can be edited via:
  `customize-variable' `ipe-edit-mode-keys'

Insert Pair Edit minor mode - Insert And... - Action:

  \\<ipe-edit-mode-map>`\\[ipe-edit--ia-goto-open]' - Insert\
 OPEN and CLOSE, exit `ipe-edit-mode', and move the
	cursor to the end of the OPEN string.
	(command: `ipe-edit--ia-goto-open')
  `\\[ipe-edit--ia-goto-close]' - Insert OPEN and CLOSE, exit\
 `ipe-edit-mode', and move the
	cursor to the end of the CLOSE string.
	(command: `ipe-edit--ia-goto-close')
  `\\[ipe-edit--ia-resume]' - Insert OPEN and CLOSE, exit\
 `ipe-edit-mode', and leave the
	cursor at its current position.
	(command: `ipe-edit--ia-resume')
  `\\[ipe-edit--ia-copy-text]' - Insert OPEN and CLOSE, exit\
 `ipe-edit-mode', and copy the text
	between the OPEN and CLOSE to the `kill-ring'.
	(command: `ipe-edit--ia-copy-text')
  `\\[ipe-edit--ia-kill-text]' - Insert OPEN and CLOSE, exit\
 `ipe-edit-mode', and kill the text
	between the OPEN and CLOSE.
	(command: `ipe-edit--ia-kill-text')
  `\\[ipe-edit--ia-update-forward]' - Insert OPEN and CLOSE,\
 prompt for another MNEMONIC, and
	update the next PAIR that matches MNEMONIC.
	(command: `ipe-edit--ia-update-forward')
  `\\[ipe-edit--ia-update-backward]' - Insert OPEN and CLOSE,\
 prompt for another MNEMONIC, and
	update the previous PAIR that matches MNEMONIC.
	(command: `ipe-edit--ia-update-backward')

Insert Pair Edit minor mode - Change Movement:

  \\<ipe-edit-mode-map>`\\[ipe-edit--movement-by-char]' -\
 Change the movement of PAIRs by the Insert Pair Edit
	commands to be `by char'.
	(command: `ipe-edit--movement-by-char')
  `\\[ipe-edit--movement-by-word]' -\
 Change the movement of PAIRs by the Insert Pair Edit
	commands to be `by word'.
	(command: `ipe-edit--movement-by-word')
  `\\[ipe-edit--movement-by-line]' -\
 Change the movement of PAIRs by the Insert Pair Edit
	commands to be `by line'.
	(command: `ipe-edit--movement-by-line')
  `\\[ipe-edit--movement-by-list]' -\
 Change the movement of PAIRs by
	the Insert Pair Edit commands to be `by list'.
	(command: `ipe-edit--movement-by-list')

Insert Pair Edit minor mode - Edit CONTENTs:

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
 the OPEN and CLOSE to
	lowercase.
	(command: `ipe-edit--contents-downcase')

Insert Pair Edit minor mode - Next / Previous:

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
	(command: `ipe-edit--update-previous-close')

Insert Pair Edit minor mode - Multiple PAIRs:

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
 Insert the first PAIR into the buffer.
    (command: `ipe-edit--insert-first-pair')
  `\\[ipe-edit--insert-last-pair]' -\
 Insert the last PAIR into the buffer.
    (command: `ipe-edit--insert-last-pair')
  `\\[ipe-edit--delete-first-pair]' -\
 Remove the first PAIR from the buffer.
	(command: `ipe-edit--delete-first-pair')
  `\\[ipe-edit--delete-all-pairs]' -\
 Remove all PAIRs from the buffer.
	(command: `ipe-edit--delete-all-pairs')
  `\\[ipe-edit--delete-last-pair]' -\
 Remove the last PAIR from the buffer.
	(command: `ipe-edit--delete-last-pair')
  `\\[ipe-edit--recenter-pair]' -\
 Recenter the window on a PAIR.
	(command: `ipe-edit--recenter-pair')

Insert Pair Edit minor mode - Edit PAIR Definitions:

  \\<ipe-edit-mode-map>`\\[ipe-defn--edit-pair]' - Add / Edit the\
 definition of a global PAIR.
	(command: `ipe-defn--edit-pair')
  `\\[ipe-defn--edit-mode-pair]' - Add / Edit the definition of a\
 modal PAIR.
	(command: `ipe-defn--edit-mode-pair')
  `\\[ipe-edit--edit-current-pair]' - Edit the definition of the\
 currently active PAIR.
	(command: `ipe-edit--edit-current-pair')
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
	(command: `ipe-defn--delete-mode-pair')

Insert Pair Edit minor mode - Other:

  \\<ipe-edit-mode-map>`\\[ipe-edit--change-pair]' - Replace the\
 current PAIR with a different one.
	(command: `ipe-edit--change-pair')
  `\\[ipe-edit--toggle-escapes]' - Turn on/off the processing\
 of the :escapes option for the
	current PAIR.
	(command: `ipe-edit--toggle-escapes')
  `\\[ipe-options]' - Display the Insert Pair Edit\
 `customize'-ation options.
	(command: `ipe-options')
  `\\[ipe-help-edit-mode]' - Display help on the commands\
 available within `ipe-edit-mode'.
	(command: `ipe-help-edit-mode')
  `\\[ipe-help-info]' - Display the Insert Pair Edit (ipe)\
 info file.
	(command: `ipe-help-info')

PAIRs are highlighted using:

- OPEN   - is highlighted with: (face: `ipe-open-highlight')
- CLOSE  - is highlighted with: (face: `ipe-close-highlight')
- INFIX  - is highlighted with: (face: `ipe-infix-highlight')
- ESCAPE - is highlighted with: (face: `ipe-escape-highlight')"
  :init-val nil
  :lighter  " (<->)"
  :keymap   ipe-edit-mode-map

  (add-hook 'ipe-defn--update-hook #'ipe-edit--defn-update 0 t)

  (when ipe-edit-mode
    (when (not (and ipe--mnemonic
		    ipe--movement
		    (ipe--pos-open 0)
		    (ipe--pos-close 0)))
      (message "This mode should only be launched via\
 'ipe-insert-pair-edit-*' commands.  See command:\
 `ipe-insert-pair-edit'.")
      (ipe-edit-mode -1))))

;; -------------------------------------------------------------------
;;;;; Utility functions.
;; -------------------------------------------------------------------

(defun ipe-edit--mode-check ()
  "Check `ipe-edit-mode' is active for `ipe-edit--*' commands."
  (if ipe-edit-mode
      (if (ipe--pos-count)
	  t
	(ipe-edit-mode -1)
	nil)
    (message "This command should only be run from within\
 `ipe-edit-mode'.")
    nil))

(defun ipe-edit--redisplay ()
  "Safely call `ipe--pair-pos-redisplay'.

Call `ipe--pair-pos-redisplay', if this results in no PAIRs being
defined within `ipe--pair-pos-list', exit `ipe-edit-mode'."

  (ipe--pair-pos-redisplay)
  (if (zerop (ipe--pos-count))
      (ipe-edit-mode -1)
    (when (not ipe-edit-mode)
      (ipe-edit-mode 1))))

(defun ipe-edit--defn-update ()
  "Run after updating an `ipe' PAIR Definition."

  (when ipe-edit-mode
    (ipe-edit--redisplay)))

(defun ipe-edit--read-mnemonic (prompt &optional force)
  "Safely prompt the user for an `ipe' PAIR MNEMONIC.

If already in `ipe-edit-mode', return `ipe--mnemonic', otherwise
prompt the user for a MNEMONIC.  The list of possible MNEMONICs that
can be entered is obtained from the `car's of the `ipe-mode-pairs' and
`ipe-pair' variables.

PROMPT is passed as the first parameter to `completing-read'.
If FORCE is non-nil, prompt even if already in `ipe-edit-mode'.

Returns the MNEMONIC entered at the prompt."

  (if (and ipe-edit-mode (not force))
      ipe--mnemonic
    (ipe-read--mnemonic prompt)))

;; *******************************************************************
;;;; 'Insert Pair Edit' Mode Commands.
;; *******************************************************************

;; -------------------------------------------------------------------
;;;;; 'Insert PAIR' Command.
;; -------------------------------------------------------------------

(defun ipe-edit--insert-pair ()
  "Insert the `ipe' OPEN & CLOSE strings to the buffer.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to insert the text within the OPEN
\(`ipe--open-overlays') and CLOSE (`ipe--close-overlays') overlays
into the current buffer and to delete the overlays.

\(If the ipe PAIR definition includes :infix or :escapes definitions,
this command will also insert the INFIX (`ipe--infix-overlays')
strings and/or ESCAPE (`ipe--escape-overlays') sequences.)"

  (interactive)
  (when (ipe-edit--mode-check)
    (if (ipe--pair-property (ipe--pair) :auto-insert)
	(progn (ipe--pair-pos-insert 0)
	       (ipe--set-point 0)
	       (run-hooks 'ipe-edit-insert-hook)
	       (ipe--undo-accept))
      (ipe--safecall 'mc/remove-fake-cursors)
      (dotimes (n (ipe--pos-count))
	(ipe--pair-pos-insert n)
	(if (equal (ipe--pos-property n :initial-n) 0)
	    (ipe--set-point n)
	  (save-excursion
	    (ipe--set-point n)
	    (ipe--safecall 'mc/create-fake-cursor-at-point))))
      (run-hooks 'ipe-edit-insert-hook)
      (ipe--undo-accept))
    (when (> (ipe--pos-count) 1)
      (ipe--safecall 'multiple-cursors-mode))
    (ipe-edit--abort)))

;; -------------------------------------------------------------------
;;;;; 'Insert And...' Commands.
;; -------------------------------------------------------------------

(defun ipe-edit--ia-goto-open (arg)
  "Insert the `ipe' OPEN & CLOSE and move to the end of OPEN.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to:

- Insert the text within the OPEN and CLOSE overlays of the current
  `ipe' PAIR into the current buffer (as per `ipe-edit--insert-pair'),
- Delete the OPEN and CLOSE overlays, and;
- Move POINT to the end of the OPEN string.

With universal ARG, move POINT to the start of the OPEN string.
With numeric ARG, move POINT to a position ARG characters forwards
from the start of the OPEN string."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (let ((ipe-move-point-on-insert (if (and arg (listp arg))
					'open-beg
				      (if (numberp arg)
					  arg
					'open-end))))
      (ipe-edit--insert-pair))))

(defun ipe-edit--ia-goto-close (arg)
  "Insert the `ipe' OPEN & CLOSE and move to the end of CLOSE.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to:

- Insert the text within the OPEN and CLOSE overlays of the current
  `ipe' PAIR into the current buffer (as per `ipe-edit--insert-pair'),
- Delete the OPEN and CLOSE overlays, and;
- Move POINT to the end of the CLOSE string.

With universal ARG, move POINT to the start of the CLOSE string.
With numeric ARG, move POINT to a position ARG characters forwards
from the start of the CLOSE string."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (let ((ipe-move-point-on-insert (if (and arg (listp arg))
					'close-beg
				      (if (numberp arg)
					  (- arg)
					'close-end))))
      (ipe-edit--insert-pair))))

(defun ipe-edit--ia-resume ()
  "Insert the `ipe' OPEN & CLOSE and do not move POINT.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to:

- Insert the text within the OPEN and CLOSE overlays of the current
  `ipe' PAIR into the current buffer (as per `ipe-edit--insert-pair'),
  and;
- Delete the OPEN and CLOSE overlays.  (Without moving POINT.)"

  (interactive)
  (when (ipe-edit--mode-check)
    (let ((ipe-move-point-on-insert 'resume))
      (ipe-edit--insert-pair))))

(defun ipe-edit--ia-copy-text ()
  "Insert the `ipe' OPEN & CLOSE and copy the enclosed text.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to:

- Insert the text within the OPEN and CLOSE overlays of the current
  `ipe' PAIR into the current buffer (as per `ipe-edit--insert-pair'),
- Delete the OPEN and CLOSE overlays, and;
- Copy the text that was between the OPEN and CLOSE overlays to the
  kill ring."

  (interactive)
  (when (ipe-edit--mode-check)
    (let ((text ""))
      (dotimes (n (ipe--pos-count))
	(setq text (concat text
			   (buffer-substring (ipe--pos-open n)
					     (ipe--pos-close n)))))
      (kill-new text)
      (ipe-edit--insert-pair))))

(defun ipe-edit--ia-kill-text ()
  "Insert the `ipe' OPEN & CLOSE and kill the enclosed text.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to:

- Insert the text within the OPEN and CLOSE overlays of the current
  `ipe' PAIR into the current buffer (as per `ipe-edit--insert-pair'),
- Delete the OPEN and CLOSE overlays, and;
- Kill the text that was between the OPEN and CLOSE overlays."

  (interactive)
  (when (ipe-edit--mode-check)
    (let ((text ""))
      (dotimes (n (ipe--pos-count))
	(setq text (concat text
			   (buffer-substring (ipe--pos-open n)
					     (ipe--pos-close n)))))
      (ipe-edit--contents-kill)
      (kill-new text)
      (ipe-edit--insert-pair))))

(defun ipe-edit--ia-update-forward (arg)
  "Insert the `ipe' OPEN & CLOSE and update another `ipe' PAIR.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to:

- Insert the text within the OPEN and CLOSE overlays of the current
  `ipe' PAIR into the current buffer (as per `ipe-edit--insert-pair'),
- Prompt the user for another MNEMONIC,
- Search forward for the `ARG'th new PAIR identified by MNEMONIC, and;
- If found, create a new `ipe' PAIR from the found PAIR."

  (interactive "P")
  (when (ipe-edit--mode-check)

    (let ((old-mnemonic ipe--mnemonic)
	  (new-mnemonic))

      (dotimes (i (ipe--pos-count))
	(setq ipe--mnemonic old-mnemonic)

	(ipe--pair-pos-insert (- (ipe--pos-count) i 1))
	(ipe--set-point       (- (ipe--pos-count) i 1))

	(when (not new-mnemonic)
	  (setq new-mnemonic (ipe-edit--read-mnemonic "Update PAIR: " t)))

	(setq ipe--mnemonic new-mnemonic)

	(let* ((n         (- (ipe--pos-count) i 1))
	       (units     (ipe--arg-units arg))
	       (pair      (ipe--pair))
	       (open      (ipe--pair-open-string  pair))
	       (infix     (ipe--pair-infix-string pair))
	       (escapes   (ipe--pair-escapes      pair))
	       (close     (ipe--pair-close-string pair))
	       (len-open  (length (ipe--pos-open-insert n)))
	       (len-close (length (ipe--pos-close-insert n)))
	       (pair-pos  (ipe-updt--next-pair (+ (ipe--pos-open n) len-open)
					       open
					       infix
					       close
					       escapes
					       (point-max)
					       units))
	       (pos-open  (car pair-pos))
	       (end-close (cdr pair-pos)))

	  ;; Check if we have matched an existing PAIR, and try again.
	  (when (and pos-open
		     end-close
		     (or (= pos-open (ipe--pos-open n))
			 (= pos-open (ipe--pos-close n))
			 (= len-close 0)
			 (= end-close (+ (ipe--pos-close n) len-close))))
	    (setq pair-pos (ipe-updt--next-pair (+ (ipe--pos-close n)
						   len-close)
						open
						infix
						close
						escapes
						(point-max)
						units))
	    (setq pos-open (car pair-pos)
		  end-close (cdr pair-pos)))

	  (if (and pos-open end-close)
	      (progn
		(ipe--pos-open-set  n pos-open)
		(ipe--pos-close-set n (- end-close len-close)))
	    (setq ipe--mnemonic old-mnemonic)
	    (setq pair (ipe--pair))
	    (setq len-close (length (ipe--pos-close-insert n)))
	    (message "Could not find PAIR %s" (ipe--mnemonic-describe
					       new-mnemonic)))

	  (ipe-updt--delete-at-pos pair
				   n
				   (ipe--pos-open n)
				   (+ (ipe--pos-close n) len-close))))

      (ipe-edit--redisplay)
      (ipe--pos-recenter (1- (ipe--pos-count)) t))))

(defun ipe-edit--ia-update-backward (arg)
  "Insert the `ipe' OPEN & CLOSE and update another `ipe' PAIR.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to:

- Insert the text within the OPEN and CLOSE overlays of the current
  `ipe' PAIR into the current buffer (as per `ipe-edit--insert-pair'),
- Prompt the user for another MNEMONIC,
- Search backward for the `ARG'th new PAIR identified by MNEMONIC,
  and;
- If found, create a new `ipe' PAIR from the found PAIR."

  (interactive "P")
  (when (ipe-edit--mode-check)

    (let ((old-mnemonic ipe--mnemonic)
	  (new-mnemonic))

      (dotimes (n (ipe--pos-count))
	(setq ipe--mnemonic old-mnemonic)

	(ipe--pair-pos-insert n)
	(ipe--set-point       n)

	(when (not new-mnemonic)
	  (setq new-mnemonic (ipe-edit--read-mnemonic "Update PAIR: " t)))

	(setq ipe--mnemonic new-mnemonic)

	(let* ((units     (ipe--arg-units arg))
	       (pair      (ipe--pair))
	       (open      (ipe--pair-open-string  pair))
	       (infix     (ipe--pair-infix-string pair))
	       (escapes   (ipe--pair-escapes      pair))
	       (close     (ipe--pair-close-string pair))
	       (len-open  (length (ipe--pos-open-insert n)))
	       (len-close (length (ipe--pos-close-insert n)))
	       (pair-pos  (ipe-updt--previous-pair (1- (ipe--pos-open n))
						   open
						   infix
						   close
						   escapes
						   (point-min)
						   (if (or (zerop len-open)
							   (zerop len-close)
							   (string= open close))
						       (ipe--pos-open n)
						     nil)
						   units))
	       (pos-open  (car pair-pos))
	       (end-close (cdr pair-pos)))

	  (if (and pos-open end-close)
	      (progn
		(ipe--pos-open-set  n pos-open)
		(ipe--pos-close-set n (- end-close len-close)))
	    (setq ipe--mnemonic old-mnemonic)
	    (setq pair (ipe--pair))
	    (setq len-close (length (ipe--pos-close-insert n)))
	    (message "Could not find PAIR %s" (ipe--mnemonic-describe
					       new-mnemonic)))

	  (ipe-updt--delete-at-pos pair
				   n
				   (ipe--pos-open n)
				   (+ (ipe--pos-close n) len-close))))

      (ipe-edit--redisplay)
      (ipe--pos-recenter 0))))

;; -------------------------------------------------------------------
;;;;; 'OPEN Movement' Commands.
;; -------------------------------------------------------------------

(defun ipe-edit--open-beg (arg)
  "Move the `ipe' OPEN overlay to the `beginning'.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the OPEN overlay of a PAIR.

The OPEN overlay is moved to the `ARG'th beginning.

Movement units are determined by the current value of
`ipe--movement'."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (let ((units (ipe--arg-units arg)))
      (ipe--open-move units 'beg)
      (ipe-edit--redisplay))))

(defun ipe-edit--open-up (arg)
  "Move the `ipe' OPEN overlay up ARG units.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the OPEN overlay of a PAIR ARG
units `up'.

Movement units are determined by the current value of `ipe--movement'.

With a negative ARG, behave as per `ipe-edit--open-down'."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (let ((units (ipe--arg-units arg)))
      (if (> units 0)
	  (ipe--open-move units 'up)
	(ipe--open-move (- units) 'down))
      (ipe-edit--redisplay))))

(defun ipe-edit--open-backward (arg)
  "Move the `ipe' OPEN overlay back ARG units.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the OPEN overlay of a PAIR ARG
units `backward'.

Movement units are determined by the current value of `ipe--movement'.

With a negative ARG, behave as per `ipe-edit--open-forward'."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (let ((units (ipe--arg-units arg)))
      (if (> units 0)
	  (ipe--open-move units 'backward)
	(ipe--open-move (- units) 'forward))
      (ipe-edit--redisplay))))

(defun ipe-edit--open-forward (arg)
  "Move the `ipe' OPEN overlay forward ARG units.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the OPEN overlay of a PAIR ARG
units `forward'.

Movement units are determined by the current value of `ipe--movement'.

With a negative ARG, behave as per `ipe-edit--open-backward'."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (let ((units (ipe--arg-units arg)))
      (if (> units 0)
	  (ipe--open-move units 'forward)
	(ipe--open-move (- units) 'backward))
      (ipe-edit--redisplay))))

(defun ipe-edit--open-down (arg)
  "Move the `ipe' OPEN overlay down ARG units.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the OPEN overlay of a PAIR ARG
units `down'.

Movement units are determined by the current value of `ipe--movement'.

With a negative ARG, behave as per `ipe-edit--open-up'."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (let ((units (ipe--arg-units arg)))
      (if (> units 0)
	  (ipe--open-move units 'down)
	(ipe--open-move (- units) 'up))
      (ipe-edit--redisplay))))

(defun ipe-edit--open-end (arg)
  "Move the `ipe' OPEN overlay to the `end'.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the OPEN overlay of a PAIR.

The OPEN overlay is moved to the `ARG'th end.

Movement units are determined by the current value of
`ipe--movement'."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (let ((units (ipe--arg-units arg)))
      (ipe--open-move units 'end)
      (ipe-edit--redisplay))))

;; -------------------------------------------------------------------
;;;;; 'CLOSE Movement' Commands.
;; -------------------------------------------------------------------

(defun ipe-edit--close-beg (arg)
  "Move the `ipe' CLOSE overlay to the `beginning'.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the CLOSE overlay of a PAIR.

The CLOSE overlay is moved to the `ARG'th beginning.

Movement units are determined by the current value of
`ipe--movement'."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (let ((units (ipe--arg-units arg)))
      (ipe--close-move units 'beg)
      (ipe-edit--redisplay))))

(defun ipe-edit--close-up (arg)
  "Move the `ipe' CLOSE overlay up ARG units.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the CLOSE overlay of a PAIR ARG
units `up'.

Movement units are determined by the current value of
`ipe--movement'.

With a negative ARG, behave as per `ipe-edit--close-down'."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (let ((units (ipe--arg-units arg)))
      (if (> units 0)
	  (ipe--close-move units 'up)
	(ipe--close-move (- units) 'down))
      (ipe-edit--redisplay))))

(defun ipe-edit--close-backward (arg)
  "Move the `ipe' CLOSE overlay back ARG units.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the CLOSE overlay of a PAIR ARG
units `backward'.

Movement units are determined by the current value of `ipe--movement'.

With a negative ARG, behave as per `ipe-edit--close-forward'."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (let ((units (ipe--arg-units arg)))
      (if (> units 0)
	  (ipe--close-move units 'backward)
	(ipe--close-move (- units) 'forward))
      (ipe-edit--redisplay))))

(defun ipe-edit--close-forward (arg)
  "Move the `ipe' CLOSE overlay forward ARG units.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the CLOSE overlay of a PAIR ARG
units `forward'.

Movement units are determined by the current value of `ipe--movement'.

With a negative ARG, behave as per `ipe-edit--close-backward'."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (let ((units (ipe--arg-units arg)))
      (if (> units 0)
	  (ipe--close-move units 'forward)
	(ipe--close-move (- units) 'backward))
      (ipe-edit--redisplay))))

(defun ipe-edit--close-down (arg)
  "Move the `ipe' CLOSE overlay down ARG units.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the CLOSE overlay of a PAIR ARG
units `down'.

Movement units are determined by the current value of `ipe--movement'.

With a negative ARG, behave as per `ipe-edit--close-up'."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (let ((units (ipe--arg-units arg)))
      (if (> units 0)
	  (ipe--close-move units 'down)
	(ipe--close-move (- units) 'up))
      (ipe-edit--redisplay))))

(defun ipe-edit--close-end (arg)
  "Move the `ipe' CLOSE overlay to the `end'.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the CLOSE overlay of a PAIR.

The CLOSE overlay is moved to the `ARG'th end.

Movement units are determined by the current value of
`ipe--movement'."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (let ((units (ipe--arg-units arg)))
      (ipe--close-move units 'end)
      (ipe-edit--redisplay))))

;; -------------------------------------------------------------------
;;;;; 'Change PAIR' Command.
;; -------------------------------------------------------------------

(defun ipe-edit--change-pair (replace)
  "Replace the current `ipe' PAIR with the one specified by REPLACE.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to replace the current OPEN and CLOSE
overlays of a PAIR with a different OPEN and CLOSE overlay.

REPLACE is the MNEMONIC of an `ipe' PAIR within either
`ipe-mode-pairs' or `ipe-pairs' which is used to specify the new PAIR.

If run interactively, this command will prompt the user for REPLACE
using a `completing-read' via `ipe-edit--read-mnemonic'."

  (interactive (list (ipe-edit--read-mnemonic "Replace PAIR: " t)))
  (when (ipe-edit--mode-check)
    (setq ipe--mnemonic replace)
    (dotimes (n (ipe--pos-count))
      (ipe--pair-pos-init n))
    (ipe-edit--redisplay)))

;; -------------------------------------------------------------------
;;;;; 'Change Movement' Commands.
;; -------------------------------------------------------------------

(defun ipe-edit--movement-by-char ()
  "Set the movements made by `ipe' to be `by char'.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command `ipe-edit-mode') to change the movements made by the
minor-mode commands so that each lexical unit moved is `by character'.

It sets the value of `ipe--movement' to `'char'."

  (interactive)
  (when (ipe-edit--mode-check)
    (ipe--pair-pos-movement-reset 'char)))

(defun ipe-edit--movement-by-word ()
  "Set the movements made by `ipe' to be `by word'.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to change the movements made by the
minor-mode commands so that each lexical unit moved is `by word'.

It sets the value of `ipe--movement' to `'word'."

  (interactive)
  (when (ipe-edit--mode-check)
    (ipe--pair-pos-movement-reset 'word)))

(defun ipe-edit--movement-by-line ()
  "Set the movements made by `ipe' to be `by line'.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to change the movements made by the
minor-mode commands so that each lexical unit moved is `by line'.
\(This will cause the `forward' and `backward' commands to act in the
according to the :side-function property of the PAIR.)

It sets the value of `ipe--movement' to `'line'."

  (interactive)
  (when (ipe-edit--mode-check)
    (ipe--pair-pos-movement-reset 'line)))

(defun ipe-edit--movement-by-list ()
  "Set the movements made by `ipe' to be `by list'.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to change the movements made by the
minor-mode commands so that each lexical unit moved is `by list'
\(s-expressions).  (This will cause the `up' commands to move to the
enclosing S-expression, and `down' commands to move to enclosed
S-expressions.)

It sets the value of `ipe--movement' to `'list'."

  (interactive)
  (when (ipe-edit--mode-check)
    (ipe--pair-pos-movement-reset 'list)))

;; -------------------------------------------------------------------
;;;;; 'Edit CONTENTS' Commands.
;; -------------------------------------------------------------------

(defun ipe-edit--contents-kill ()
  "Kill the text enclosed by `ipe' OPEN & CLOSE.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to kill the text between the OPEN and
CLOSE overlays.

The enclosed text is copied to the `kill-ring'."

  (interactive)
  (when (ipe-edit--mode-check)
    (dotimes (n (ipe--pos-count))
      (let* ((beg (ipe--pos-open n))
	     (end (ipe--pos-close n)))
	(kill-new (buffer-substring beg end))
	(ipe--pos-delete beg end)))
    (ipe-edit--redisplay)))

(defun ipe-edit--contents-copy ()
  "Copy the text enclosed by `ipe' OPEN & CLOSE.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to copy the text between the OPEN and
CLOSE overlays to the `kill-ring'."

  (interactive)
  (when (ipe-edit--mode-check)
    (dotimes (n (ipe--pos-count))
      (let* ((beg      (ipe--pos-open n))
	     (end      (ipe--pos-close n))
	     (contents (buffer-substring-no-properties beg end)))
	(kill-new contents)))
    (ipe-edit--redisplay)))

(defun ipe-edit--contents-yank ()
  "Replace the text enclosed by `ipe' OPEN & CLOSE with killed text.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to replace the text between the OPEN and
CLOSE overlays with the last killed text."

  (interactive)
  (when (ipe-edit--mode-check)
    (dotimes (n (ipe--pos-count))
      (let* ((beg      (ipe--pos-open  n))
	     (end      (ipe--pos-close n))
	     (contents (current-kill (- (ipe--pos-count) n 1) t)))
	(ipe--pos-replace beg end contents)))
    (ipe-edit--redisplay)))

(defun ipe-edit--contents-replace (arg)
  "Replace the text enclosed by `ipe' OPEN & CLOSE.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to replace the text between the OPEN and
CLOSE overlays with a text string entered using the minibuffer.

With prefix ARG, use a different same REPLACE string for all PAIRs."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (let ((replace (unless arg (read-from-minibuffer "Replace: "))))
      (dotimes (n (ipe--pos-count))
	(let* ((beg      (ipe--pos-open n))
	       (end      (ipe--pos-close n))
	       (contents (buffer-substring-no-properties beg end)))
	  (when arg
	    (setq replace (read-from-minibuffer "replace: " contents)))
	  (ipe--pos-replace beg end replace))
	(ipe-edit--redisplay)))))

(defun ipe-edit--contents-trim ()
  "Trim the whitespace from text enclosed by the `ipe' OPEN & CLOSE.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to remove the whitespace immediately after
the OPEN overlay, and immediately before the CLOSE overlay."

  (interactive)
  (when (ipe-edit--mode-check)
    (dotimes (n (ipe--pos-count))
      (let* ((beg      (ipe--pos-open n))
	     (end      (ipe--pos-close n))
	     (contents (buffer-substring-no-properties beg end)))
	(ipe--pos-replace beg end
			  (replace-regexp-in-string
			   (concat "\\(\\`[[:space:]\n]*"
				   "\\|[[:space:]\n]*\\'"
				   "\\|^[[:space:]\n]*"
				   "\\)")
			   ""
			   contents))))
    (ipe-edit--redisplay)))

(defun ipe-edit--contents-upcase ()
  "Convert the text enclosed by `ipe' OPEN & CLOSE to Upper-Case.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to convert the text between the OPEN and
CLOSE overlays to Upper-Case.  (Using `upcase'.)"

  (interactive)
  (when (ipe-edit--mode-check)
    (dotimes (n (ipe--pos-count))
      (let* ((beg      (ipe--pos-open n))
	     (end      (ipe--pos-close n))
	     (contents (buffer-substring-no-properties beg end)))
	(ipe--pos-replace beg end (upcase contents))))
    (ipe-edit--redisplay)))

(defun ipe-edit--contents-capitalize ()
  "Convert the text enclosed by `ipe' OPEN & CLOSE to Capital-Case.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to convert the text between the OPEN and
CLOSE overlays to Capital-Case.  (Using `capitalize'.)"

  (interactive)
  (when (ipe-edit--mode-check)
    (dotimes (n (ipe--pos-count))
      (let* ((beg      (ipe--pos-open n))
	     (end      (ipe--pos-close n))
	     (contents (buffer-substring-no-properties beg end)))
	(ipe--pos-replace beg end (capitalize contents))))
    (ipe-edit--redisplay)))

(defun ipe-edit--contents-downcase ()
  "Convert the text enclosed by `ipe' OPEN & CLOSE to Lower-Case.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to convert the text between the OPEN and
CLOSE overlays to Lower-Case.  (Using `downcase'.)"

  (interactive)
  (when (ipe-edit--mode-check)
    (dotimes (n (ipe--pos-count))
      (let* ((beg      (ipe--pos-open n))
	     (end      (ipe--pos-close n))
	     (contents (buffer-substring-no-properties beg end)))
	(ipe--pos-replace beg end (downcase contents))))
    (ipe-edit--redisplay)))

;; -------------------------------------------------------------------
;;;;; 'Next / Previous' Commands.
;; -------------------------------------------------------------------

(defun ipe-edit--update-next-pair (arg)
  "Insert `ipe' OPEN & CLOSE and edit the next `ipe' PAIR.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to:

- Insert the OPEN and CLOSE overlays of the current `ipe' PAIR into
  the current buffer (as per `ipe-edit--insert-pair'),
- Search forward from the end of the CLOSE string for a matching set
  of OPEN and CLOSE strings, and, if found;
- Re-activate the Insert Pair Edit (ipe) minor mode (command:
  `ipe-edit-mode') with the new OPEN and CLOSE.

If ARG is nil, it will simply search forward for the next OPEN and
CLOSE, otherwise, it assumes ARG is a numeric prefix, and will search
forward for the `ARG'th OPEN and CLOSE.

This will delete the new OPEN and CLOSE string from the buffer, and
replace them with overlays that can be moved by the `ipe-edit-mode'
commands."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (dotimes (i (ipe--pos-count))
      (ipe--pair-pos-insert (- (ipe--pos-count) i 1))
      (ipe--set-point       (- (ipe--pos-count) i 1))

      (let* ((n         (- (ipe--pos-count) i 1))
	     (units     (ipe--arg-units arg))
	     (pair      (ipe--pair))
	     (open      (ipe--pair-open-string  pair))
	     (infix     (ipe--pair-infix-string pair))
	     (escapes   (ipe--pair-escapes      pair))
	     (close     (ipe--pair-close-string pair))
	     (len-open  (length (ipe--pos-open-insert n)))
	     (len-close (length (ipe--pos-close-insert n)))
	     (pair-pos  (ipe-updt--next-pair (+ (ipe--pos-open n) len-open)
					     open
					     infix
					     close
					     escapes
					     (point-max)
					     units))
	     (pos-open  (car pair-pos))
	     (end-close (cdr pair-pos)))

	;; Check if we have matched an existing PAIR, and try again.
	(when (and pos-open
		   end-close
		   (or (= pos-open (ipe--pos-open n))
		       (= pos-open (ipe--pos-close n))
		       (= len-close 0)
		       (= end-close (+ (ipe--pos-close n) len-close))))
	  (setq pair-pos (ipe-updt--next-pair (+ (ipe--pos-close n)
						 len-close)
					      open
					      infix
					      close
					      escapes
					      (point-max)
					      units))
	  (setq pos-open (car pair-pos)
		end-close (cdr pair-pos)))

	(when (and pos-open end-close)
	  (ipe--pos-open-set  n pos-open)
	  (ipe--pos-close-set n (- end-close len-close)))

	(ipe-updt--delete-at-pos pair
				 n
				 (ipe--pos-open n)
				 (+ (ipe--pos-close n) len-close))))

    (ipe-edit--redisplay)
    (ipe--pos-recenter (1- (ipe--pos-count)) t)))

(defun ipe-edit--update-next-contents ()
  "Insert `ipe' OPEN & CLOSE and search forward for enclosed text.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to:

- Insert the OPEN and CLOSE overlays of the current `ipe' PAIR into
  the current buffer (as per `ipe-edit--insert-pair'),
- Search forward from the end of the CLOSE string for the text that
  was between the newly inserted OPEN and CLOSE overlays, and, if
  found;
- Re-activate the Insert Pair Edit (ipe) minor mode (command:
  `ipe-edit-mode') with the new OPEN and CLOSE positions."

  (interactive)
  (when (ipe-edit--mode-check)
    (dotimes (n (ipe--pos-count))

      (let* ((inserted  (+ (length (ipe--pos-open-insert n))
			   (length (ipe--pos-close-insert n))))
	     (substring (buffer-substring (ipe--pos-open n)
					  (ipe--pos-close n)))
	     (end       (+ (ipe--pos-close n) inserted))
	     (next))

	;; Search for a match to the wrapped sub-string.
	(when (> (length substring) 0)
	  (save-excursion
	    (goto-char end)
	    (when (search-forward substring nil t)
	      (setq next t))))

	(when next
	  (ipe--pair-pos-insert n)
	  (ipe--set-point       n)

	  (save-excursion
	    (goto-char end)
	    (search-forward substring nil t)

	    (ipe--pos-open-set     n (match-beginning 0))
	    (ipe--pos-close-set    n (match-end 0))
	    (ipe--pos-property-set n :initial-n n)))))

    (ipe-edit--redisplay)
    (ipe--pos-recenter (1- (ipe--pos-count)) t)))

(defun ipe-edit--update-next-open (arg)
  "Insert `ipe' OPEN overlay and edit the next OPEN string.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to:

- Insert the OPEN overlay of the current `ipe' PAIR into the current
  buffer,
- Search forward from the end of the OPEN for another matching OPEN
  string, and, if found;
- Re-activate the Insert Pair Edit (ipe) minor mode (command:
  `ipe-edit-mode') with the new OPEN and CLOSE.

If ARG is nil, it will simply search forward for the next OPEN,
otherwise, it assumes ARG is a numeric prefix, and will search forward
for the `ARG'th OPEN.

This will delete the new OPEN string from the buffer, and replace it
with an overlay that can be moved by the `ipe-edit-mode' commands."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (dotimes (n (ipe--pos-count))
      (ipe--pair-pos-insert n)
      (ipe--set-point n)

      (let* ((units     (ipe--arg-units arg))
	     (pair      (ipe--pair))
	     (open      (ipe--pair-open-string  pair))
	     (infix     (ipe--pair-infix-string pair))
	     (escapes   (ipe--pair-escapes      pair))
	     (close     (ipe--pos-close-insert  n))
	     (len-open  (length (ipe--pos-open-insert n)))
	     (len-close (length close))
	     (pos-close (ipe--pos-close n))
	     (pos-open  (ipe-updt--next-open (+ (ipe--pos-open n) len-open)
					     open
					     infix
					     close
					     escapes
					     pos-close
					     units)))

	(when (and pos-open pos-close)
	  (ipe--pos-open-set  n pos-open)
	  (ipe--pos-close-set n pos-close))

	(ipe-updt--delete-at-pos pair
				 n
				 (ipe--pos-open n)
				 (+ (ipe--pos-close n) len-close))))

    (ipe-edit--redisplay)
    (ipe--pos-recenter 0)))

(defun ipe-edit--update-next-close (arg)
  "Insert `ipe' CLOSE overlay and edit the next CLOSE string.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to:

- Insert the CLOSE overlay of the current `ipe' PAIR into the current
  buffer,
- Search forward from the end of the CLOSE string for another matching
  CLOSE string, and, if found;
- Re-activate the Insert Pair Edit (ipe) minor mode (command:
  `ipe-edit-mode') with the new OPEN and CLOSE.

If ARG is nil, it will simply search forward for the next CLOSE,
otherwise, it assumes ARG is a numeric prefix, and will search forward
for the `ARG'th CLOSE.

This will delete the new CLOSE string from the buffer, and replace it
with an overlay that can be moved by the `ipe-edit-mode' commands."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (dotimes (n (ipe--pos-count))
      (ipe--pair-pos-insert n)
      (ipe--set-point n)

      (let* ((units     (ipe--arg-units arg))
	     (pair      (ipe--pair))
	     (escapes   (ipe--pair-escapes      pair))
	     (close     (ipe--pair-close-string pair))
	     (len-close (length (ipe--pos-close-insert n)))
	     (pos-open  (ipe--pos-open n))
	     (bound     (when (< (1+ n) (ipe--pos-count))
			  (ipe--pos-open (1+ n))))
	     (pos-close (ipe-updt--next-close
			 (+ (ipe--pos-close n) len-close)
			 close
			 escapes
			 bound
			 units)))

	(if (and pos-open pos-close)
	    (progn
	      (ipe--pos-open-set  n pos-open)
	      (ipe--pos-close-set n pos-close))
	  (ipe--pos-close-set n (+ (ipe--pos-close n) len-close)))

	(ipe-updt--delete-at-pos pair
				 n
				 (ipe--pos-open n)
				 (ipe--pos-close n))))

    (ipe-edit--redisplay)
    (ipe--pos-recenter (1- (ipe--pos-count)) t)))

(defun ipe-edit--update-previous-pair (arg)
  "Insert `ipe' OPEN & CLOSE and edit the previous `ipe' PAIR.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to:

- Insert the OPEN and CLOSE overlays of the current `ipe' PAIR into
  the current buffer (as per `ipe-edit--insert-pair'),
- Search backward from the start of the OPEN string for a matching set
  of OPEN and CLOSE strings, and, if found;
- Re-activate the Insert Pair Edit (ipe) minor mode (command:
  `ipe-edit-mode') with the new OPEN and CLOSE.

If ARG is nil, it will simply search backward for the previous OPEN
and CLOSE, otherwise, it assumes ARG is a numeric prefix, and will
search backward for the `ARG'th OPEN and CLOSE.

This will delete the new OPEN and CLOSE string from the buffer, and
replace them with overlays that can be moved by the `ipe-edit-mode'
commands."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (dotimes (n (ipe--pos-count))
      (ipe--pair-pos-insert n)
      (ipe--set-point n)

      (let* ((units     (ipe--arg-units arg))
	     (pair      (ipe--pair))
	     (open      (ipe--pair-open-string  pair))
	     (infix     (ipe--pair-infix-string pair))
	     (escapes   (ipe--pair-escapes      pair))
	     (close     (ipe--pair-close-string pair))
	     (len-open  (length (ipe--pos-open-insert n)))
	     (len-close (length (ipe--pos-close-insert n)))
	     (pair-pos  (ipe-updt--previous-pair (1- (ipe--pos-open n))
						 open
						 infix
						 close
						 escapes
						 (point-min)
						 (if (or (zerop len-open)
							 (zerop len-close)
							 (string= open close))
						     (ipe--pos-open n)
						   nil)
						 units))
	     (pos-open  (car pair-pos))
	     (end-close (cdr pair-pos)))

	(when (and pos-open end-close)
	  (ipe--pos-open-set  n pos-open)
	  (ipe--pos-close-set n (- end-close len-close)))

	(ipe-updt--delete-at-pos pair
				 n
				 (ipe--pos-open n)
				 (+ (ipe--pos-close n) len-close))))

    (ipe-edit--redisplay)
    (ipe--pos-recenter 0)))

(defun ipe-edit--update-previous-contents ()
  "Insert `ipe' OPEN & CLOSE and search backward for enclosed text.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to:

- Insert the OPEN and CLOSE overlays of the current `ipe' PAIR into
  the current buffer (as per `ipe-edit--insert-pair'),
- Search backward from the start of the OPEN string for the text that
  was between the newly inserted OPEN and CLOSE overlays, and, if
  found;
- Re-activate the Insert Pair Edit (ipe) minor mode (command:
  `ipe-edit-mode') with the new OPEN and CLOSE positions."

  (interactive)
  (when (ipe-edit--mode-check)
    (dotimes (n (ipe--pos-count))

      (let* ((substring (buffer-substring (ipe--pos-open n)
					  (ipe--pos-close n)))
	     (beg       (ipe--pos-open n)))

	;; Search for a match to the wrapped sub-string.
	(unless (or (bobp) (= (length substring) 0))
	  (save-excursion
	    (goto-char (- beg 1))
	    (when (search-backward substring nil t)
	      (progn
		(ipe--pair-pos-insert n)
		(ipe--set-point n)

		(ipe--pos-open-set     n (match-beginning 0))
		(ipe--pos-close-set    n (match-end 0))
		(ipe--pos-property-set n :initial-n n))))))

      (ipe-edit--redisplay)
      (ipe--pos-recenter 0))))

(defun ipe-edit--update-previous-open (arg)
  "Insert `ipe' OPEN overlay and edit the previous OPEN string.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to:

- Insert the OPEN overlay of the current `ipe' PAIR into the current
  buffer,
- Search backward from the beginning of the OPEN for another matching
  OPEN string, and if found;
- Re-activate the Insert Pair Edit (ipe) minor mode (command:
  `ipe-edit-mode') with the new OPEN and CLOSE.

If ARG is nil, it will simply search backward for the previous OPEN,
otherwise, it assumes ARG is a numeric prefix, and will search
backward for the `ARG'th OPEN.

This will delete the new OPEN string from the buffer, and replace it
with an overlay that can be moved by the `ipe-edit-mode' commands."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (dotimes (n (ipe--pos-count))
      (ipe--pair-pos-insert n)
      (ipe--set-point n)

      (let* ((units     (ipe--arg-units arg))
	     (pair      (ipe--pair))
	     (open      (ipe--pair-open-string  pair))
	     (infix     (ipe--pair-infix-string pair))
	     (escapes   (ipe--pair-escapes      pair))
	     (close     (ipe--pair-close-string pair))
	     (len-close (length (ipe--pos-close-insert n)))
	     (bound     (when (> n 0)
			  (ipe--pos-close (1- n))))
	     (pos-open  (ipe-updt--previous-open (ipe--pos-open n)
						 open
						 infix
						 close
						 escapes
						 bound
						 units))
	     (pos-close (ipe--pos-close n)))

	(when (and pos-open pos-close)
	  (ipe--pos-open-set  n pos-open)
	  (ipe--pos-close-set n pos-close))

	(ipe-updt--delete-at-pos pair
				 n
				 (ipe--pos-open n)
				 (+ (ipe--pos-close n) len-close))))

    (ipe-edit--redisplay)
    (ipe--pos-recenter 0)))

(defun ipe-edit--update-previous-close (arg)
  "Insert `ipe' CLOSE overlay and edit the previous CLOSE string.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to:

- Insert the CLOSE overlay of the current `ipe' PAIR into the current
  buffer,
- Search backward from the beginning of the CLOSE string for another
  matching CLOSE string, and, if found;
- Re-activate the Insert Pair Edit (ipe) minor mode (command:
  `ipe-edit-mode') with the new OPEN and CLOSE.

If ARG is nil, it will simply search backward for the previous CLOSE,
otherwise, it assumes ARG is a numeric prefix, and will search
backward for the `ARG'th CLOSE.

This will delete the new CLOSE string from the buffer, and replace it
with an overlay that can be moved by the `ipe-edit-mode' commands."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (dotimes (n (ipe--pos-count))
      (ipe--pair-pos-insert n)
      (ipe--set-point n)

      (let* ((units      (ipe--arg-units arg))
	     (pair       (ipe--pair))
	     (escapes    (ipe--pair-escapes      pair))
	     (close      (ipe--pair-close-string pair))
	     (len-open   (length (ipe--pos-open-insert n)))
	     (pos-open   (ipe--pos-open n))
	     (len-close  (length (ipe--pos-close-insert n)))
	     (close-prev (if (= (ipe--pos-close n) (point-max))
			     (1- (ipe--pos-close n))
			   (ipe--pos-close n)))
	     (pos-close  (ipe-updt--previous-close close-prev
						   close
						   escapes
						   (+ pos-open len-open)
						   units)))

	(if (and pos-open pos-close)
	    (progn
	      (ipe--pos-open-set  n pos-open)
	      (ipe--pos-close-set n pos-close))
	  (ipe--pos-close-set n (+ (ipe--pos-close n) len-close)))

	(ipe-updt--delete-at-pos pair
				 n
				 (ipe--pos-open n)
				 (ipe--pos-close n))))

    (ipe-edit--redisplay)
    (ipe--pos-recenter (1- (ipe--pos-count)) t)))

;; -------------------------------------------------------------------
;;;;; 'Multiple' Commands.
;; -------------------------------------------------------------------

(defun ipe-edit--add-pair ()
  "Add a new `ipe' PAIR at POINT.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to:

- Add a new `ipe' PAIR at POINT."

  (interactive)

  (ipe--pair-pos-init (ipe--pos-count) (point) 1)
  (ipe--pair-pos-redisplay))

(defun ipe-edit--add-next-pair (arg)
  "Create a new `ipe' PAIR by searching forward for OPEN & CLOSE.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to:

- Search forward from the end of the CLOSE overlay for the next OPEN
  and CLOSE, and;
- If they exists, delete that next OPEN and CLOSE strings from the
  buffer and add them as a new `ipe' PAIR.

With a prefix ARG, search for the `ARG'th OPEN and CLOSE."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (let* ((units       (ipe--arg-units arg))
	   (n           (ipe--pos-count))
	   (beg         (ipe--pos-close (1- n)))
	   (pair        (ipe--pair))
	   (open        (ipe--pair-open-string  pair))
	   (infix       (ipe--pair-infix-string pair))
	   (escapes     (ipe--pair-escapes      pair))
	   (close       (ipe--pair-close-string pair))
	   (pos-pair    (ipe-updt--next-pair beg
					     open
					     infix
					     close
					     escapes
					     (point-max)
					     units))
	   (point-open  (ipe--pos-property (1- n) :point-open))
	   (point-close (ipe--pos-property (1- n) :point-close)))

      (if pos-pair
	  (save-excursion
	    (goto-char (car pos-pair))

	    (ipe-updt--delete-pair pair nil
				   (car pos-pair) (cdr pos-pair) t)

	    (when (and (not (equal (ipe--pos-point (1- n)) 'eob))
		       (ipe--pos-contains-p (1- n) (ipe--pos-point (1- n))))
	      (ipe--pos-point n
			      (+ (ipe--pos-open n)
				 (- (ipe--pos-point (1- n))
				    (ipe--pos-open (1- n))))))

	    (ipe--pos-property-set n :point-open point-open)

	    (when (numberp point-close)
	      (ipe--pos-point        n (ipe--pos-close n))
	      (ipe--pos-property-set n :point-close point-close))

	    (ipe-edit--redisplay)
	    (ipe--pos-recenter n t))

	(message "Could not find %s"
		 (ipe--mnemonic-describe ipe--mnemonic))))))

(defun ipe-edit--add-previous-pair (arg)
  "Create a new `ipe' PAIR by searching backward for OPEN & CLOSE.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to:

- Search backward from the start of the OPEN overlay for the previous
  OPEN and CLOSE, and;
- If they exist, delete that previous OPEN and CLOSE from the buffer
  and add them as new `ipe' PAIR.

With a prefix ARG, search for the `ARG'th OPEN and CLOSE backward."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (let* ((units       (ipe--arg-units arg))
	   (n           (ipe--pos-count))
	   (beg         (ipe--pos-open 0))
	   (end         (ipe--pos-close 0))
	   (pair        (ipe--pair))
	   (open        (ipe--pair-open-string  pair))
	   (infix       (ipe--pair-infix-string pair))
	   (escapes     (ipe--pair-escapes      pair))
	   (close       (ipe--pair-close-string pair))
	   (pos-pair    (ipe-updt--previous-pair beg
						 open
						 infix
						 close
						 escapes
						 (point-min)
						 (if (string= open close) end nil)
						 units))
	   (point-open  (ipe--pos-property 0 :point-open))
	   (point-close (ipe--pos-property 0 :point-close)))

      (if pos-pair
	  (save-excursion
	    (goto-char (car pos-pair))

	    (ipe-updt--delete-pair pair nil
				   (car pos-pair) (cdr pos-pair) t)

	    (when (and (not (equal (ipe--pos-point (1- n)) 'eob))
		       (ipe--pos-contains-p (1- n) (ipe--pos-point (1- n))))
	      (ipe--pos-point n
			      (+ (ipe--pos-open n)
				 (- (ipe--pos-point (1- n))
				    (ipe--pos-open (1- n))))))

	    (ipe--pos-property-set n :point-open point-open)

	    (when (numberp point-close)
	      (ipe--pos-point        n (ipe--pos-close n))
	      (ipe--pos-property-set n :point-close point-close))

	    (ipe-edit--redisplay)
	    (ipe--pos-recenter 0))

	(message "Could not find %s"
		 (ipe--mnemonic-describe ipe--mnemonic))))))

(defun ipe-edit--add-next-contents (arg)
  "Create a new `ipe' PAIR by searching forward for enclosed text.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to:

- Search forward from the end of the CLOSE overlay for the text
  currently between the OPEN and CLOSE overlays, and;
- If the text exists, add a new `ipe' PAIR around the text.

With a prefix ARG, search for the `ARG'th matching contents."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (let* ((units       (ipe--arg-units arg))
	   (n           (1- (ipe--pos-count)))
	   (new         (ipe--pos-count))
	   (substring   (buffer-substring (ipe--pos-open n)
					  (ipe--pos-close n)))
	   (point-open  (ipe--pos-property n :point-open))
	   (point-close (ipe--pos-property n :point-close)))

      (save-excursion
	(goto-char (ipe--pos-close n))
	(if (search-forward substring nil t units)
	    (progn
	      (ipe--pos-open-set     new (match-beginning 0))
	      (ipe--pos-close-set    new (match-end 0))
	      (ipe--pos-point        new (match-beginning 0))
	      (ipe--pos-property-set new
				     :initial-n   new
				     :point-open  point-open
				     :point-close point-close)
	      (ipe-edit--redisplay)
	      (ipe--pos-recenter new t))

	  (message "Could not find string '%s'" substring))))))

(defun ipe-edit--add-previous-contents (arg)
  "Create a new `ipe' PAIR by searching backward for enclosed text.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to:

- Search backward from the start of the OPEN overlay for the text
  currently between the OPEN and CLOSE overlays, and;
- If the text exists, add a new `ipe' PAIR around the text.

With a prefix ARG, search for the `ARG'th matching contents."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (let* ((units       (ipe--arg-units arg))
	   (n           0)
	   (new         (ipe--pos-count))
	   (substring   (buffer-substring (ipe--pos-open n)
					  (ipe--pos-close n)))
	   (point-open  (ipe--pos-property n :point-open))
	   (point-close (ipe--pos-property n :point-close)))

      (save-excursion
	(goto-char (ipe--pos-open 0))
	(if (search-backward substring nil t units)
	    (progn
	      (ipe--pos-open-set     new (match-beginning 0))
	      (ipe--pos-close-set    new (match-end 0))
	      (ipe--pos-point        new (match-beginning 0))
	      (ipe--pos-property-set new
				     :initial-n   new
				     :point-open  point-open
				     :point-close point-close)
	      (ipe-edit--redisplay)
	      (ipe--pos-recenter 0))

	  (message "Could not find string '%s'" substring))))))

(defun ipe-edit--insert-first-pair (arg)
  "Insert the first `ipe' OPEN & CLOSE strings into the buffer.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to insert the text within the first OPEN
\(`ipe--open-overlays') overlay and CLOSE (`ipe--close-overlays')
overlay into the current buffer, and to delete the overlays.

If there are no remaining `ipe' PAIRS after the first PAIR has been
inserted, this command behaves exactly as `ipe-edit--insert-pair' and
will exit (command: `ipe-edit-mode'.)

With a `universal-argument' prefix ARG, insert all OPEN and CLOSE
overlays.
With a positive numeric prefix ARG, insert the ARG'th OPEN and CLOSE
overlays.
With a negative numeric prefix ARG, remove the last - ARG'th OPEN and
CLOSE overlays."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (if (or (<= (ipe--pos-count) 1)
	    (and arg (listp arg)))
	(ipe-edit--insert-pair)
      (let ((n (if (integerp arg)
		   (if (> 0 arg)
		       (- (ipe--pos-count) arg)
		     (1- arg))
		 0)))
	(when (and (<= 0 n) (< n (ipe--pos-count)))
	  (ipe--pair-pos-insert n)
	  (ipe--pos-recenter n)
	  (ipe--pair-pos-hide n)
	  (run-hooks 'ipe-edit-insert-hook)
	  (ipe-edit--redisplay))))))

(defun ipe-edit--insert-last-pair (arg)
  "Insert the last `ipe' OPEN & CLOSE strings into the buffer.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to insert the text within the last OPEN
\(`ipe--open-overlays') overlay and CLOSE (`ipe--close-overlays')
overlay into the current buffer, and to delete the overlays.

If there are no remaining `ipe' PAIRS after the last PAIR has been
inserted, this command behaves exactly as `ipe-edit--insert-last-pair'
and will exit (command: `ipe-edit-mode'.)

With a `universal-argument' prefix ARG, insert all OPEN and CLOSE
overlays.
With a positive numeric prefix ARG, insert the ARG'th OPEN and CLOSE
overlays.
With a negative numeric prefix ARG, insert the last - ARG'th OPEN and
CLOSE overlays."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (if (or (<= (ipe--pos-count) 1)
	    (and arg (listp arg)))
	(ipe-edit--insert-pair)
      (let ((n (if (integerp arg)
		   (if (> 0 arg)
		       arg
		     (- (ipe--pos-count) arg))
		 (1- (ipe--pos-count)))))
	(when (and (<= 0 n) (< n (ipe--pos-count)))
	  (ipe--pair-pos-insert n)
	  (ipe--pos-recenter n)
	  (ipe--pair-pos-hide n)
	  (run-hooks 'ipe-edit-insert-hook)
	  (ipe-edit--redisplay))))))

(defun ipe-edit--delete-first-pair (arg)
  "Remove the first `ipe' OPEN & CLOSE overlay.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to remove the OPEN and CLOSE overlays of
the first `ipe' PAIR within the buffer, without changing the buffer.

If there are no remaining `ipe' PAIRS after the first PAIR has been
removed, it will exit (command: `ipe-edit-mode'.)

With a `universal-argument' prefix ARG, remove all OPEN and CLOSE.
With a positive numeric prefix ARG, remove the ARG'th OPEN and CLOSE.
With a negative numeric prefix ARG, remove the last - ARG'th OPEN and
CLOSE."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (if (or (<= (ipe--pos-count) 1)
	    (and arg (listp arg)))
	(progn
	  (ipe--undo-accept)
	  (ipe-edit--abort))
      (let ((n (if (integerp arg)
		   (if (> 0 arg)
		       (- (ipe--pos-count) arg)
		     (1- arg))
		 0)))
	(ipe--pos-recenter n)
	(ipe--pair-pos-hide n)
	(ipe-edit--redisplay)))))

(defun ipe-edit--delete-all-pairs ()
  "Remove all `ipe' OPEN & CLOSE overlays.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to remove all of the OPEN and CLOSE
overlays for `ipe' PAIRs within the buffer, without changing the
buffer.

As there are no remaining `ipe' PAIRS after this command is run, it
will exit (command: `ipe-edit-mode'."

  (interactive)
  (when (ipe-edit--mode-check)
    (ipe--undo-accept)
    (ipe-edit--abort)))

(defun ipe-edit--delete-last-pair (arg)
  "Remove the last `ipe' OPEN & CLOSE overlay.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to remove the OPEN and CLOSE overlays of
the last `ipe' PAIR within the buffer, without changing the buffer.

If there are no remaining `ipe' PAIRS after the last PAIR has been
removed, it will exit (command: `ipe-edit-mode'.)

With a `universal-argument' prefix ARG, remove all OPEN and CLOSE.
With a positive numeric prefix ARG, remove the ARG'th OPEN and CLOSE.
With a negative numeric prefix ARG, remove the first - ARG'th OPEN and
CLOSE."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (if (or (<= (ipe--pos-count) 1)
	    (and arg (listp arg)))
	(progn
	  (ipe--undo-accept)
	  (ipe-edit--abort))
      (let ((n (if (integerp arg)
		   (if (> 0 arg)
		       arg
		     (- (ipe--pos-count) arg))
		 (1- (ipe--pos-count)))))
	(ipe--pos-recenter n)
	(ipe--pair-pos-hide n)
	(ipe-edit--redisplay)))))

(defun ipe-edit--recenter-pair (arg)
  "Center the display on the ARG'th `ipe' OPEN & CLOSE overlay.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to position the current window such that
the OPEN and CLOSE overlays of the ARG'th PAIR are visible.

If both the OPEN & CLOSE cannot be displayed at once, then, if the
OPEN is visible, recenter on CLOSE, if the CLOSE is visible,
recenter on OPEN."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (let ((units (1- (ipe--arg-units arg))))
      (when (and (<= 0 units) (< units (ipe--pos-count)))
	(if (and (ipe--visible-p      (ipe--pos-open  units))
		 (not (ipe--visible-p (ipe--pos-close units))))
	    (ipe--pos-recenter units t)
	  (ipe--pos-recenter units))))))

(defun ipe-edit--edit-current-pair (arg)
  "Edit the current `ipe' PAIR Definition.

This function will act as per `ipe-defn--edit-pair', but will use the
currently active Insert Pair Edit PAIR rather than prompting for a
MNEMONIC.

With prefix ARG, call either `ipe-defn--ui-edit-pair' or
`ipe-defn--ui-edit-mode-pair'."

  (interactive "P")
  (when (ipe-edit--mode-check)
    (ipe-defn--edit-current-pair arg)))

;; -------------------------------------------------------------------
;;;;; 'Other' Commands.
;; -------------------------------------------------------------------

(defun ipe-edit--toggle-escapes (arg)
  "Toggle the replacement of ESCAPES within `ipe' PAIRs.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to toggle the display of ESCAPES overlays
between `ipe' OPEN and CLOSE overlays.

By default it will toggle the display of ESCAPES between on and off.
With prefix ARG, it will toggle the display of ESCAPES on if ARG is
> 0, otherwise it will toggle them off."

  (interactive "P")
  (setq ipe--escapes-show-p (if arg
				(and (numberp arg) (> arg 0))
			      (not ipe--escapes-show-p)))
  (message (concat "Insert Pair Edit ESCAPE processing is now "
		   (if ipe--escapes-show-p "On" "Off")))
  (ipe-edit--redisplay))

(defun ipe-edit--abort ()
  "Remove the `ipe' OPEN & CLOSE overlays.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to remove the `ipe' OPEN and
CLOSE overlays without inserting them into the buffer.

\(NOTE: If the `ipe' PAIR definition includes :infix or :escapes
definitions, this command will also remove the overlays for the INFIX
\(`ipe--infix-overlays') overlays and/or ESCAPE
\(`ipe--escape-overlays') overlays.)"

  (interactive)
  (dotimes (n (ipe--pos-count))
    (when (equal (ipe--pos-property n :initial-n) 0)
      (ipe--set-point n))
    (ipe--pair-pos-hide n))
  (setq ipe--pair-pos-list nil)
  (ipe--undo-abort)
  (ipe--recenter (point))
  (ipe-edit-mode -1))

;; -------------------------------------------------------------------
;;;; Debugging Commands.
;; -------------------------------------------------------------------

(defun ipe-edit--show-pair-pos-list-other-window ()
  "Display the value of `ipe--pair-pos-list' in the other window.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to display debbugging information about
the `ipe--pair-pos-list' in a separator *ipe-pair-pos-list* buffer in
another window."

  (interactive)
  (when (ipe-edit--mode-check)
    (let ((pos-list-buffer (get-buffer-create "*ipe-pair-pos-list*"))
	  (buffer          (current-buffer))
	  (pair-pos-list   ipe--pair-pos-list)
	  (open-overlays   ipe--open-overlays)
	  (close-overlays  ipe--close-overlays)
	  (blank             "\n")
	  (sep             (concat (make-string 70 ?-) "\n")))

      (switch-to-buffer-other-window pos-list-buffer)
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))

      (insert
       "`Insert Pair Edit - Debug'\n"
       blank
       sep
       "PAIR Positions:\n"
       sep
       "\n")

      (insert (format "%s" pair-pos-list))

      (insert
       blank
       blank
       sep
       "OPEN Overlays:\n"
       sep
       blank)

      (dolist (o open-overlays)
	(let ((start  (overlay-start o))
	      (end    (overlay-end   o))
	      (before (overlay-get   o 'before-string))
	      (after  (overlay-get   o 'after-string)))

	  (when (and start end)
	    (insert
	     (message
	      (concat
	       "  Overlay %d to %d:\n"
	       "    Text   [%s]\n"
	       "    Hides  [%s]\n")
	      start
	      end
	      (overlay-get o 'display)
	      (with-current-buffer buffer (buffer-substring start end))))

	    (when (and before (> (length before) 0))
	      (insert (message "    Before [%s]\n" before)))

	    (when (and after (> (length after) 0))
	      (insert (message "    After  [%s]\n" after))))))

      (insert
       blank
       sep
       "CLOSE Overlays:\n"
       sep
       blank)

      (dolist (o close-overlays)
	(let ((start  (overlay-start o))
	      (end    (overlay-end   o))
	      (before (overlay-get   o 'before-string))
	      (after  (overlay-get   o 'after-string)))

	  (when (and start end)
	    (insert
	     (message
	      (concat
	       "  Overlay %d to %d:\n"
	       "    Text   [%s]\n"
	       "    Hides  [%s]\n")
	      start
	      end
	      (overlay-get o 'display)
	      (with-current-buffer buffer (buffer-substring start end))))

	    (when (and before (> (length before) 0))
	      (insert (message "    Before [%s]\n" before)))

	    (when (and after (> (length after) 0))
	      (insert (message "    After  [%s]\n" after))))))

      (insert blank)
      (insert sep)

      (setq buffer-read-only t)
      (goto-char (point-min))
      (other-window 1))))

;; -------------------------------------------------------------------
;;;; Minor Mode Keymap.
;; -------------------------------------------------------------------

(defun ipe-edit--key (pos fn &optional keymap)
  "Define a key within `ipe-edit-mode-map' via `ipe-edit-mode-keys'.

Uses the key definition defined at POS within the `ipe-edit-mode-keys'
customizable list, as the key definition for the interactive function
FN, within either the `ipe-edit-mode-map', or, if KEYMAP is non-nil,
within KEYMAP."

  (let ((key (nth pos ipe-edit-mode-keys))
	(km  (or keymap ipe-edit-mode-map)))
    (when (and key (> (length key) 0))
      (define-key km key fn))))

(defun ipe-edit--keymap-init ()
  "Initialize the `ipe-edit-mode-map' keymap.

This function is called on start-up of the Insert Pair Edit (ipe)
minor-mode (command: `ipe-edit-mode') to define the keymap bindings
for the minor mode.

This function configures both the standard key-bindings, and, derives
the key-bindings for the for basic movement commands from the
`ipe-edit--movement-keysets' / `ipe-edit--custom-movement-keyset'
variables.

This function will also be called by `customize' when the
`ipe-edit-movement-keysets' user variable is changed."

  ;; Remove all of the 'self-insert-command's.
  (define-key ipe-edit-mode-map [remap self-insert-command] 'ignore)
  (define-key ipe-edit-mode-map (kbd "C-!")
	      'ipe-edit--show-pair-pos-list-other-window)

  ;; Re-map numbers to `digit-argument'.
  (define-key ipe-edit-mode-map [?-] 'negative-argument)
  (define-key ipe-edit-mode-map [?0] 'digit-argument)
  (define-key ipe-edit-mode-map [?1] 'digit-argument)
  (define-key ipe-edit-mode-map [?2] 'digit-argument)
  (define-key ipe-edit-mode-map [?3] 'digit-argument)
  (define-key ipe-edit-mode-map [?4] 'digit-argument)
  (define-key ipe-edit-mode-map [?5] 'digit-argument)
  (define-key ipe-edit-mode-map [?6] 'digit-argument)
  (define-key ipe-edit-mode-map [?7] 'digit-argument)
  (define-key ipe-edit-mode-map [?8] 'digit-argument)
  (define-key ipe-edit-mode-map [?9] 'digit-argument)

  (when (not (= (length ipe-edit-mode-keys)
		(length ipe-edit--mode-keys-default)))
    (setq ipe-edit-mode-keys ipe-edit--mode-keys-default))

  ;; Insert PAIR
  (ipe-edit--key 0 'ipe-edit--insert-pair)

  ;; Insert And... >
  (ipe-edit--key 1 'ipe-edit--ia-goto-open)
  (ipe-edit--key 2 'ipe-edit--ia-goto-close)
  (ipe-edit--key 3 'ipe-edit--ia-resume)
  (ipe-edit--key 4 'ipe-edit--ia-copy-text)
  (ipe-edit--key 5 'ipe-edit--ia-kill-text)
  (ipe-edit--key 6 'ipe-edit--ia-update-forward)
  (ipe-edit--key 7 'ipe-edit--ia-update-backward)

  ;; Change PAIR >
  (ipe-edit--key 8 'ipe-edit--change-pair)

  ;; Check if we have a Change Movement prefix.
  (let ((change-movement (nth 9 ipe-edit-mode-keys))
	(map))
    (if (or (not change-movement)
	    (= (length change-movement) 0))
	(setq map ipe-edit-mode-map)
      (ipe-edit--key 9 ipe-edit-mode-movement-map)
      (setq map ipe-edit-mode-movement-map))

    ;; Movement >
    (ipe-edit--key 10 'ipe-edit--movement-by-char map)
    (ipe-edit--key 11 'ipe-edit--movement-by-word map)
    (ipe-edit--key 12 'ipe-edit--movement-by-line map)
    (ipe-edit--key 13 'ipe-edit--movement-by-list map))

  ;; Edit CONTENTS >
  (ipe-edit--key 14 'ipe-edit--contents-kill)
  (ipe-edit--key 15 'ipe-edit--contents-copy)
  (ipe-edit--key 16 'ipe-edit--contents-yank)
  (ipe-edit--key 17 'ipe-edit--contents-replace)
  (ipe-edit--key 18 'ipe-edit--contents-trim)
  (ipe-edit--key 19 'ipe-edit--contents-upcase)
  (ipe-edit--key 20 'ipe-edit--contents-capitalize)
  (ipe-edit--key 21 'ipe-edit--contents-downcase)

  ;; Next / Previous >
  (ipe-edit--key 22 'ipe-edit--update-next-pair)
  (ipe-edit--key 23 'ipe-edit--update-next-contents)
  (ipe-edit--key 24 'ipe-edit--update-next-open)
  (ipe-edit--key 25 'ipe-edit--update-next-close)
  (ipe-edit--key 26 'ipe-edit--update-previous-pair)
  (ipe-edit--key 27 'ipe-edit--update-previous-contents)
  (ipe-edit--key 28 'ipe-edit--update-previous-open)
  (ipe-edit--key 29 'ipe-edit--update-previous-close)

  ;; Multiple >
  (ipe-edit--key 30 'ipe-edit--add-pair)
  (ipe-edit--key 31 'ipe-edit--add-next-pair)
  (ipe-edit--key 32 'ipe-edit--add-previous-pair)
  (ipe-edit--key 33 'ipe-edit--add-next-contents)
  (ipe-edit--key 34 'ipe-edit--add-previous-contents)
  (ipe-edit--key 35 'ipe-edit--insert-first-pair)
  (ipe-edit--key 36 'ipe-edit--insert-last-pair)
  (ipe-edit--key 37 'ipe-edit--delete-first-pair)
  (ipe-edit--key 38 'ipe-edit--delete-all-pairs)
  (ipe-edit--key 39 'ipe-edit--delete-last-pair)
  (ipe-edit--key 40 'ipe-edit--recenter-pair)

  ;; Edit PAIR Definitions >
  (ipe-edit--key 41 'ipe-defn--edit-pair)
  (ipe-edit--key 42 'ipe-defn--edit-mode-pair)
  (ipe-edit--key 43 'ipe-edit--edit-current-pair)
  (ipe-edit--key 44 'ipe-defn--change-pair-mnemonic)
  (ipe-edit--key 45 'ipe-defn--change-mode-pair-mnemonic)
  (ipe-edit--key 46 'ipe-defn--delete-pair)
  (ipe-edit--key 47 'ipe-defn--delete-mode-pair)

  ;; Other commands.
  (ipe-edit--key 48 'ipe-edit--toggle-escapes)

  (ipe-edit--key 49 'ipe-edit--abort)
  (ipe-edit--key 50 'ipe-options)
  (ipe-edit--key 51 'ipe-help-info)
  (ipe-edit--key 52 'ipe-help-edit-mode)

  ;; Default bindings.
  (define-key ipe-edit-mode-map (kbd "C-g")
	      'ipe-edit--abort)
  (define-key ipe-edit-mode-map (kbd "<ESC> <ESC>")
	      'ipe-edit--abort)
  (define-key ipe-edit-mode-map (kbd "<escape> <escape>")
	      'ipe-edit--abort)
  (define-key ipe-edit-mode-map (kbd "<return>")
	      'ipe-edit--insert-pair)

  ;; Set up ipe minibuffer bindings.
  (ipe-edit--key 41 'ipe-defn--edit-pair
		 ipe-read--minibuffer-keymap)
  (ipe-edit--key 42 'ipe-defn--edit-mode-pair
		 ipe-read--minibuffer-keymap)
  (ipe-edit--key 44 'ipe-defn--change-pair-mnemonic
		 ipe-read--minibuffer-keymap)
  (ipe-edit--key 45 'ipe-defn--change-mode-pair-mnemonic
		 ipe-read--minibuffer-keymap)
  (ipe-edit--key 46 'ipe-defn--delete-pair
		 ipe-read--minibuffer-keymap)
  (ipe-edit--key 47 'ipe-defn--delete-mode-pair
		 ipe-read--minibuffer-keymap)
  (ipe-edit--key 50 'ipe-options
		 ipe-read--minibuffer-keymap)
  (ipe-edit--key 51 'ipe-help-info
		 ipe-read--minibuffer-keymap)
  (ipe-edit--key 52 'ipe-help-prompt
		 ipe-read--minibuffer-keymap)

  ;; Remove existing basic movement keymappings.
  (map-keymap
   (lambda (event defn)
     (if (and (or (not (symbolp event))
		  (not (or (string-match ".*mouse.*" (symbol-name event))
			   (string-match ".*wheel.*" (symbol-name event)))))
	      (member defn
		      '(ipe-edit--open-beg
			ipe-edit--open-up
			ipe-edit--open-backward
			ipe-edit--open-forward
			ipe-edit--open-down
			ipe-edit--open-end
			ipe-edit--close-beg
			ipe-edit--close-up
			ipe-edit--close-backward
			ipe-edit--close-forward
			ipe-edit--close-down
			ipe-edit--close-end)))
	 (define-key ipe-edit-mode-map (vector event) nil)))
   ipe-edit-mode-map)

  ;; Set basic movement based on `ipe-edit--movement-keysets variable.

  ;; 'Modifiers Basic Movement Key Bindings.
  (when (member 'modifiers ipe-edit--movement-keysets)
    (define-key ipe-edit-mode-map [?\C-a] 'ipe-edit--open-beg)
    (define-key ipe-edit-mode-map [?\C-p] 'ipe-edit--open-up)
    (define-key ipe-edit-mode-map [?\C-b] 'ipe-edit--open-backward)
    (define-key ipe-edit-mode-map [?\C-f] 'ipe-edit--close-forward)
    (define-key ipe-edit-mode-map [?\C-n] 'ipe-edit--close-down)
    (define-key ipe-edit-mode-map [?\C-e] 'ipe-edit--close-end)

    (define-key ipe-edit-mode-map [?\M-a] 'ipe-edit--close-beg)
    (define-key ipe-edit-mode-map [?\M-p] 'ipe-edit--close-up)
    (define-key ipe-edit-mode-map [?\M-b] 'ipe-edit--close-backward)
    (define-key ipe-edit-mode-map [?\M-f] 'ipe-edit--open-forward)
    (define-key ipe-edit-mode-map [?\M-n] 'ipe-edit--open-down)
    (define-key ipe-edit-mode-map [?\M-e] 'ipe-edit--open-end))

  ;; 'Alpha Basic Movement Key Bindings.
  (when (member 'alpha ipe-edit--movement-keysets)
    (define-key ipe-edit-mode-map [?a] 'ipe-edit--open-beg)
    (define-key ipe-edit-mode-map [?p] 'ipe-edit--open-up)
    (define-key ipe-edit-mode-map [?b] 'ipe-edit--open-backward)
    (define-key ipe-edit-mode-map [?f] 'ipe-edit--close-forward)
    (define-key ipe-edit-mode-map [?n] 'ipe-edit--close-down)
    (define-key ipe-edit-mode-map [?e] 'ipe-edit--close-end)

    (define-key ipe-edit-mode-map [?A] 'ipe-edit--close-beg)
    (define-key ipe-edit-mode-map [?P] 'ipe-edit--close-up)
    (define-key ipe-edit-mode-map [?B] 'ipe-edit--close-backward)
    (define-key ipe-edit-mode-map [?F] 'ipe-edit--open-forward)
    (define-key ipe-edit-mode-map [?N] 'ipe-edit--open-down)
    (define-key ipe-edit-mode-map [?E] 'ipe-edit--open-end))

  ;; 'Arrow Basic Movement Key Bindings.
  (when (member 'arrow ipe-edit--movement-keysets)
    (define-key ipe-edit-mode-map (kbd "<home>")
		'ipe-edit--open-beg)
    (define-key ipe-edit-mode-map (kbd "<up>")
		'ipe-edit--open-up)
    (define-key ipe-edit-mode-map (kbd "<left>")
		'ipe-edit--open-backward)
    (define-key ipe-edit-mode-map (kbd "<right>")
		'ipe-edit--close-forward)
    (define-key ipe-edit-mode-map (kbd "<down>")
		'ipe-edit--close-down)
    (define-key ipe-edit-mode-map (kbd "<end>")
		'ipe-edit--close-end)

    (define-key ipe-edit-mode-map (kbd "<C-home>")
		'ipe-edit--close-beg)
    (define-key ipe-edit-mode-map (kbd "<C-up>")
		'ipe-edit--close-up)
    (define-key ipe-edit-mode-map (kbd "<C-left>")
		'ipe-edit--close-backward)
    (define-key ipe-edit-mode-map (kbd "<C-right>")
		'ipe-edit--open-forward)
    (define-key ipe-edit-mode-map (kbd "<C-down>")
		'ipe-edit--open-down)
    (define-key ipe-edit-mode-map (kbd "<C-end>")
		'ipe-edit--open-end))

  ;; 'wasd Basic Movement Key Bindings.
  (when (member 'wasd ipe-edit--movement-keysets)
    (define-key ipe-edit-mode-map [?q] 'ipe-edit--open-beg)
    (define-key ipe-edit-mode-map [?w] 'ipe-edit--open-up)
    (define-key ipe-edit-mode-map [?a] 'ipe-edit--open-backward)
    (define-key ipe-edit-mode-map [?d] 'ipe-edit--close-forward)
    (define-key ipe-edit-mode-map [?s] 'ipe-edit--close-down)
    (define-key ipe-edit-mode-map [?e] 'ipe-edit--close-end)

    (define-key ipe-edit-mode-map [?Q] 'ipe-edit--close-beg)
    (define-key ipe-edit-mode-map [?W] 'ipe-edit--close-up)
    (define-key ipe-edit-mode-map [?A] 'ipe-edit--close-backward)
    (define-key ipe-edit-mode-map [?D] 'ipe-edit--open-forward)
    (define-key ipe-edit-mode-map [?S] 'ipe-edit--open-down)
    (define-key ipe-edit-mode-map [?E] 'ipe-edit--open-end))

  ;; 'vi Basic Movement Key Bindings.
  (when (member 'vi ipe-edit--movement-keysets)
    (define-key ipe-edit-mode-map [?0] 'ipe-edit--open-beg)
    (define-key ipe-edit-mode-map [?k] 'ipe-edit--open-up)
    (define-key ipe-edit-mode-map [?h] 'ipe-edit--open-backward)
    (define-key ipe-edit-mode-map [?l] 'ipe-edit--close-forward)
    (define-key ipe-edit-mode-map [?j] 'ipe-edit--close-down)
    (define-key ipe-edit-mode-map [?$] 'ipe-edit--close-end)

    (define-key ipe-edit-mode-map [?W] 'ipe-edit--close-beg)
    (define-key ipe-edit-mode-map [?K] 'ipe-edit--close-up)
    (define-key ipe-edit-mode-map [?H] 'ipe-edit--close-backward)
    (define-key ipe-edit-mode-map [?L] 'ipe-edit--open-forward)
    (define-key ipe-edit-mode-map [?J] 'ipe-edit--open-down)
    (define-key ipe-edit-mode-map [?B] 'ipe-edit--open-end))

  ;; When `ipe-edit--movement-keysets' is `custom, read the basic
  ;; movement keymap bindings from the
  ;; `ipe-edit--custom-movement-keyset' variable.
  (when (member 'custom ipe-edit--movement-keysets)
    (define-key ipe-edit-mode-map
		(nth 0 ipe-edit--custom-movement-keyset)
		'ipe-edit--open-beg)
    (define-key ipe-edit-mode-map
		(nth 1 ipe-edit--custom-movement-keyset)
		'ipe-edit--open-up)
    (define-key ipe-edit-mode-map
		(nth 2 ipe-edit--custom-movement-keyset)
		'ipe-edit--open-backward)
    (define-key ipe-edit-mode-map
		(nth 3 ipe-edit--custom-movement-keyset)
		'ipe-edit--open-forward)
    (define-key ipe-edit-mode-map
		(nth 4 ipe-edit--custom-movement-keyset)
		'ipe-edit--open-down)
    (define-key ipe-edit-mode-map
		(nth 5 ipe-edit--custom-movement-keyset)
		'ipe-edit--open-end)
    (define-key ipe-edit-mode-map
		(nth 6 ipe-edit--custom-movement-keyset)
		'ipe-edit--close-beg)
    (define-key ipe-edit-mode-map
		(nth 7 ipe-edit--custom-movement-keyset)
		'ipe-edit--close-up)
    (define-key ipe-edit-mode-map
		(nth 8 ipe-edit--custom-movement-keyset)
		'ipe-edit--close-backward)
    (define-key ipe-edit-mode-map
		(nth 9 ipe-edit--custom-movement-keyset)
		'ipe-edit--close-forward)
    (define-key ipe-edit-mode-map
		(nth 10 ipe-edit--custom-movement-keyset)
		'ipe-edit--close-down)
    (define-key ipe-edit-mode-map
		(nth 11 ipe-edit--custom-movement-keyset)
		'ipe-edit--close-end)))

;; -------------------------------------------------------------------
;;;; Key Customization.
;; -------------------------------------------------------------------

(defun ipe-edit--mode-keys-set (sym defn)
  "`customize' :set function for `ipe-edit-mode-keys-set'.

SYM is the symbol being set.
DEFN is the value to which to set SYM.

This function ensure that the newly changed keys are installed via
`ipe-edit--keymap-init'."

  (custom-set-default sym defn)
  (when (functionp 'ipe-edit--keymap-init)
    (ipe-edit--keymap-init)))

(defun ipe-edit--movement-keysets-set (sym defn)
  "`customize' :set function for `ipe-edit--movement-keysets'.

SYM is the symbol being set.
DEFN is the value to which to set SYM.

This function `:set's the value of the `ipe-edit--movement-keysets' /
`ipe-edit--custom-movement-keyset' internal variables from the
`customize'-able `ipe-edit-movement-keysets' variable.

This function will either: parse the position-dependent list of flags
representing the default pre-configured keymaps, and translate them
into a list of symbols (`'modifiers', `'alpha', `'arrow', `'wasd',
`'vi') that will be stored in the `ipe-edit--movement-keysets'
variable, or; it will take the list of custom key definitions and add
them to the `ipe-edit--custom-movement-keyset' variable."

  (if (= (length defn) 12)
      (progn
	(custom-set-default sym defn)
	(setq ipe-edit--movement-keysets '(custom))
	(setq ipe-edit--custom-movement-keyset defn))

    (setq ipe-edit--movement-keysets nil)

    (when (nth 0 defn)
      (add-to-list 'ipe-edit--movement-keysets 'modifiers))
    (when (nth 1 defn)
      (add-to-list 'ipe-edit--movement-keysets 'alpha))
    (when (nth 2 defn)
      (add-to-list 'ipe-edit--movement-keysets 'arrow))
    (when (nth 3 defn)
      (add-to-list 'ipe-edit--movement-keysets 'wasd))
    (when (nth 4 defn)
      (add-to-list 'ipe-edit--movement-keysets 'vi))

    (if (> (length ipe-edit--movement-keysets) 0)
	(custom-set-default sym defn)
      (add-to-list 'ipe-edit--movement-keysets 'modifiers)
      (custom-set-default sym '(t nil nil nil nil))))
  (ipe-edit--keymap-init))

(defconst ipe-edit--mode-keys-default
  (list
   (kbd "RET")        ; 0. Insert PAIR
   (kbd "O")          ; 1. Insert And Goto OPEN
   (kbd "C")          ; 2. Insert And Goto CLOSE
   (kbd "M")          ; 3. Insert And Resume
   (kbd "Y")          ; 4. Insert And Copy Text
   (kbd "K")          ; 5. Insert And Kill Text
   (kbd "u")          ; 6. Insert And Update Forward
   (kbd "U")          ; 7. Insert And Update Backward
   (kbd "(")          ; 8. Change PAIR
   (kbd "m")          ; 9. Change Movement
   (kbd "c")          ; 10. Change Movement By Characters
   (kbd "w")          ; 11. Change Movement By Words
   (kbd "l")          ; 12. Change Movement By Lines
   (kbd "x")          ; 13. Change Movement By List
   (kbd "C-k")        ; 14. Edit CONTENTS Kill
   (kbd "M-w")        ; 15. Edit CONTENTS Copy
   (kbd "C-y")        ; 16. Edit CONTENTS Yank
   (kbd "%")          ; 17. Edit CONTENTS Replace
   (kbd "C-SPC")      ; 18. Edit CONTENTS Trim
   (kbd "M-u")        ; 19. Edit CONTENTS Upcase
   (kbd "M-c")        ; 20. Edit CONTENTS Capitalize
   (kbd "M-l")        ; 21. Edit CONTENTS Downcase
   (kbd "C-s")        ; 22. Next PAIR
   (kbd "M-s")        ; 23. Next CONTENTS
   (kbd "M->")        ; 24. Next OPEN
   (kbd "C->")        ; 25. Next CLOSE
   (kbd "C-r")        ; 26. Previous PAIR
   (kbd "M-r")        ; 27. Previous CONTENTS
   (kbd "C-<")        ; 28. Previous OPEN
   (kbd "M-<")        ; 29. Previous CLOSE
   (kbd "M-(")        ; 30. Add PAIR (At Point)
   (kbd "s")          ; 31. Add PAIR (Search Forward)
   (kbd "r")          ; 32. Add PAIR (Search Backward)
   (kbd "S")          ; 33. Add CONTENTS (Search Forward)
   (kbd "R")          ; 34. Add CONTENTS (Search Backward)
   (kbd "M-j")        ; 35. Insert PAIR (First)
   (kbd "C-j")        ; 36. Insert PAIR (Last)
   (kbd "M-d")        ; 37. Delete PAIR (First)
   (kbd "DEL")        ; 38. Delete PAIR (All)
   (kbd "C-d")        ; 39. Delete PAIR (Last)
   (kbd "C-l")        ; 40. Recenter PAIR
   (kbd "C-+")        ; 41. Add PAIR Definition...
   (kbd "M-+")        ; 42. Add Mode-Specific PAIR Definition...
   (kbd "=")          ; 43. Edit Current PAIR Definition...
   (kbd "C-%")        ; 44. Edit MNEMONIC Definition...
   (kbd "M-%")        ; 45. Edit Mode-Specific MNEMONIC Definition...
   (kbd "C-*")        ; 46. Delete PAIR Definition...
   (kbd "M-*")        ; 47. Delete Mode-Specific PAIR Definition...
   (kbd "\\")         ; 48. Toggle ESCAPEs
   (kbd "q")          ; 49. Abort
   (kbd "C-o")        ; 50. Options
   (kbd "M-h")        ; 51. Info
   (kbd "?")          ; 52. Help
   )
  "Default key bindings for `ipe-edit-mode-keys'.")

(defcustom ipe-edit-mode-keys ipe-edit--mode-keys-default
  "Key bindings for interactive functions within `ipe-edit-mode'.

This list (of 53 strings) contains the key bindings for the
interactive functions available within `ipe-edit-mode'.  These key
bindings are enabled when the `ipe-edit-mode' minor mode is activated
by one of the 'Insert Pair Edit' functions.

Each entry in this list is a string which will be parsed by the `kbd'
function to return the sequence of keys that will be used within
`ipe-edit-mode' to invoke a `ipe-edit-mode' function."
  :group 'ipe-keys
  :tag   "Insert Pair Edit - `ipe-edit-mode' key bindings."
  :link  '(function-link ipe-insert-pair-edit)
  :link  '(emacs-commentary-link "ipe-edit.el")
  :set   'ipe-edit--mode-keys-set
  :type
  '(list
    :tag "`ipe-edit-mode' Key Bindings"
    (key-sequence :tag "Insert PAIR                               ")
    (key-sequence :tag "Insert And Goto OPEN                      ")
    (key-sequence :tag "Insert And Goto CLOSE                     ")
    (key-sequence :tag "Insert And Resume                         ")
    (key-sequence :tag "Insert And Copy Text                      ")
    (key-sequence :tag "Insert And Kill Text                      ")
    (key-sequence :tag "Insert And Update Forward                 ")
    (key-sequence :tag "Insert And Update Backward                ")
    (key-sequence :tag "Change PAIR                               ")
    (key-sequence :tag "Change Movement                           ")
    (key-sequence :tag "Change Movement By Characters             ")
    (key-sequence :tag "Change Movement By Words                  ")
    (key-sequence :tag "Change Movement By Lines                  ")
    (key-sequence :tag "Change Movement By List                   ")
    (key-sequence :tag "Edit CONTENTS Kill                        ")
    (key-sequence :tag "Edit CONTENTS Copy                        ")
    (key-sequence :tag "Edit CONTENTS Yank                        ")
    (key-sequence :tag "Edit CONTENTS Replace                     ")
    (key-sequence :tag "Edit CONTENTS Upcase                      ")
    (key-sequence :tag "Edit CONTENTS Trim                        ")
    (key-sequence :tag "Edit CONTENTS Capitalize                  ")
    (key-sequence :tag "Edit CONTENTS Downcase                    ")
    (key-sequence :tag "Next PAIR                                 ")
    (key-sequence :tag "Next CONTENTS                             ")
    (key-sequence :tag "Next OPEN                                 ")
    (key-sequence :tag "Next CLOSE                                ")
    (key-sequence :tag "Previous PAIR                             ")
    (key-sequence :tag "Previous CONTENTS                         ")
    (key-sequence :tag "Previous OPEN                             ")
    (key-sequence :tag "Previous CLOSE                            ")
    (key-sequence :tag "Add PAIR (At Point)                       ")
    (key-sequence :tag "Add PAIR (Search Forward)                 ")
    (key-sequence :tag "Add PAIR (Search Backward)                ")
    (key-sequence :tag "Add CONTENTS (Search Forward)             ")
    (key-sequence :tag "Add CONTENTS (Search Backward)            ")
    (key-sequence :tag "Insert PAIR (First)                       ")
    (key-sequence :tag "Insert PAIR (Last)                        ")
    (key-sequence :tag "Delete PAIR (First)                       ")
    (key-sequence :tag "Delete PAIR (All)                         ")
    (key-sequence :tag "Delete PAIR (Last)                        ")
    (key-sequence :tag "Recenter PAIR                             ")
    (key-sequence :tag "Add PAIR Definition...                    ")
    (key-sequence :tag "Add Mode-Specific PAIR Definition...      ")
    (key-sequence :tag "Edit Current PAIR Definition...           ")
    (key-sequence :tag "Edit MNEMONIC Definition...               ")
    (key-sequence :tag "Edit Mode-Specific MNEMONIC Definition... ")
    (key-sequence :tag "Delete PAIR Definition...                 ")
    (key-sequence :tag "Delete Mode-Specific PAIR Definition...   ")
    (key-sequence :tag "Toggle ESCAPEs                            ")
    (key-sequence :tag "Abort                                     ")
    (key-sequence :tag "Options                                   ")
    (key-sequence :tag "Info                                      ")
    (key-sequence :tag "Help                                      ")))

(defcustom ipe-edit-movement-keysets '(nil t t nil nil)
  "Movement key sets for `ipe-edit-mode' minor mode.

Defines keymappings for the basic Insert Pair Edit (ipe) minor-mode
movement (up, down, forward, backward) of the OPEN and CLOSE
overlays.

The following 'Predefined Key Sets' are supplied:

- Modifier Keys (`modifiers')

   <Ctrl+a> - OPEN  beginning, <Ctrl+e>  - CLOSE end,
   <Ctrl+p> - OPEN  up,        <Ctrl+n>  - CLOSE down,
   <Ctrl+b> - OPEN  backward,  <Ctrl+f>  - CLOSE forward,
   <Meta+a> - CLOSE beginning, <Meta+e>  - OPEN  end,
   <Meta+p> - CLOSE up,        <Meta+n>  - OPEN  down,
   <Meta+b> - CLOSE backward,  <Meta+f>  - OPEN  forward.

- Alphabetic (`alpha')

   <a>      - OPEN  beginning, <e>       - CLOSE end,
   <p>      - OPEN  up,        <n>       - CLOSE down,
   <b>      - OPEN  backward,  <f>       - CLOSE forward,
   <A>      - CLOSE beginning, <E>       - OPEN  end,
   <P>      - CLOSE up,        <N>       - OPEN  down,
   <B>      - CLOSE backward,  <F>       - OPEN  forward.

- Arrows (`arrow')

   <home>   - OPEN  beginning, <end>     - CLOSE end,
   <up>     - OPEN  up,        <down>    - CLOSE down,
   <left>   - OPEN  backward,  <right>   - CLOSE forward,
   <C-home> - CLOSE beginning, <C-end>   - OPEN  end,
   <C-up>   - CLOSE up,        <C-down>  - OPEN  down,
   <C-left> - CLOSE backward,  <C-right> - OPEN  forward.

- WASD (`wasd')

   <q>      - OPEN  beginning, <e>       - CLOSE end,
   <w>      - OPEN  up,        <s>       - CLOSE down,
   <a>      - OPEN  backward,  <d>       - CLOSE forward,
   <Q>      - CLOSE beginning, <E>       - OPEN  end,
   <W>      - CLOSE up,        <S>       - OPEN  down,
   <A>      - CLOSE backward,  <D>       - OPEN  forward.

- VI (`vi')

   <0>     - OPEN  beginning, <$>        - CLOSE end,
   <k>     - OPEN  up,        <j>        - CLOSE down,
   <h>     - OPEN  backward,  <l>        - CLOSE forward,
   <B>     - CLOSE beginning, <W>        - OPEN  end,
   <K>     - CLOSE up,        <J>        - OPEN  down,
   <H>     - CLOSE backward,  <L>        - OPEN  forward.

Or a Custom Key Set can be defined with user defined mappings for
each of the movement commands:

   OPEN  beginning, OPEN end,
   OPEN  up,        OPEN down,
   OPEN  backward,  OPEN forward,
   CLOSE beginning, CLOSE end,
   CLOSE up,        CLOSE down,
   CLOSE backward,  CLOSE forward.

Custom Key Set bindings will be loaded after the standard
`ipe-edit-mode' minor-mode key-bindings, and as such, may override
existing key-bindings for other `ipe-edit-mode' commands.

----------------------------------------------------------------------

The internal format of this variable is either:

- A 5-tuple containing five position dependent boolean flags:

  1 - If non-nil, turn on Modifiers
  2 - If non-nil, turn on Alphabetic
  3 - If non-nil, turn on Arrow
  4 - If non-nil, turn on WASD
  5 - If non-nil, turn on VI

or

- A 12-tuple containing 12 strings representing custom key sequences
  for the following Insert Pair Edit (ipe) movement functions:

  1  - `ipe-edit--open-beg'
  2  - `ipe-edit--open-up'
  3  - `ipe-edit--open-backward'
  4  - `ipe-edit--open-forward'
  5  - `ipe-edit--open-down'
  6  - `ipe-edit--open-up'
  7  - `ipe-edit--close-beg'
  8  - `ipe-edit--close-up'
  9  - `ipe-edit--close-backward'
  10 - `ipe-edit--close-forward'
  11 - `ipe-edit--close-down'
  12 - `ipe-edit--close-end'

This variable is expected to be set by
  `ipe-edit--movement-keysets-set'."
  :group 'ipe-keys
  :tag   "Insert Pair Edit - `ipe-edit-mode' movement keysets."
  :link  '(function-link ipe-insert-pair-edit)
  :link  '(emacs-commentary-link "ipe-edit.el")
  :type  '(choice
	   (list :tag "Predefined Key Sets"
		 (boolean :tag "Modifiers  ")
		 (boolean :tag "Alphabetic ")
		 (boolean :tag "Arrows     ")
		 (boolean :tag "WASD       ")
		 (boolean :tag "VI         "))
	   (list :tag "Custom Key Set"
		 (key-sequence :tag "Move OPEN Start Key     ")
		 (key-sequence :tag "Move OPEN Up Key        ")
		 (key-sequence :tag "Move OPEN Backward Key  ")
		 (key-sequence :tag "Move OPEN Forward Key   ")
		 (key-sequence :tag "Move OPEN Down Key      ")
		 (key-sequence :tag "Move OPEN End Key       ")
		 (key-sequence :tag "Move CLOSE Start Key    ")
		 (key-sequence :tag "Move CLOSE Up Key       ")
		 (key-sequence :tag "Move CLOSE Backward Key ")
		 (key-sequence :tag "Move CLOSE Forward Key  ")
		 (key-sequence :tag "Move CLOSE Down Key     ")
		 (key-sequence :tag "Move CLOSE End Key      ")))
  :set 'ipe-edit--movement-keysets-set)

(ipe-edit--keymap-init)

(provide 'ipe-edit)

;;; ipe-edit.el ends here
