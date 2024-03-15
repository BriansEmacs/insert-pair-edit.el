;;; insert-pair-edit.el --- M-( with interactive editing and modal pairs
;; Copyright (C) 2023 Brian Kavanagh

;; Author: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Maintainer: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Created: 28 June, 2020
;; Version: 2023.12.30
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
;; This file defines a command, `insert-pair-edit', which is targeted
;; as being a more feature rich alternative to the standard 'M-('
;; Emacs keybinding, `insert-parentheses'.
;;
;; `insert-pair-edit' will prompt the user to enter a `customize'-able
;; MNEMONIC (See: `ipe-pairs' / `ipe-mode-pairs'), that selects a
;; 'major-mode dependent' PAIR to be inserted around point.  The PAIR
;; consists of OPEN and CLOSE strings which delimit text in some
;; fashion.
;;
;; First, the OPEN and CLOSE strings are inserted as overlays, then
;; `insert-pair-edit' runs the command `ipe-edit-mode' to enter the
;; 'Insert Pair Edit' (ipe) minor mode.
;;
;; The `ipe-edit-mode' supplies commands to interactively and
;; independently move the overlays representing the OPEN and CLOSE
;;
;; strings for the inserted PAIR about the buffer, and to either
;; insert (`ipe-edit--insert-pair'), or discard (`ipe-edit--abort')
;; them once they have been correctly positioned.
;;
;; Movement of the OPEN and CLOSE overlays is based upon 'movement
;; units'.  The 'lexical units' are either: characters, words, lines,
;; or lists (S-expressions).  (For the full list of movement / editing
;; commands, see the documentation for `ipe-edit-mode'.)
;;
;; By default, movement will be by 'words', but this can be changed
;; interactively via the: `ipe-edit--movement-by-*' commands or, by
;; `ipe-pairs' / `ipe-mode-pairs' `customize'-ations.
;;
;; Additional commands are supplied to operate on the CONTENTS of the
;; PAIR (i.e. the text between the OPEN and CLOSE overlays.)  Text can
;; be copied, deleted, replaced and case converted.
;;
;; Certain characters between the OPEN and CLOSE overlays can also be
;; ESCAPE-d.  These characters will be replaced by overlays, which
;; will be updated by the `ipe-edit-mode' movement commands, and
;; inserted when the `ipe-edit--insert-pair' command is invoked.
;;
;; Customizations for the mode can be found under the `ipe' group.

;; -------------------------------------------------------------------
;;; Installation:
;;
;; Add the following to your `.emacs' file:
;;
;;  (require 'insert-pair-edit)
;;  (global-set-key (kbd "M-(") 'insert-pair-edit)

;; -------------------------------------------------------------------
;;; Code:

(require 'ipe)
(require 'ipe-updt)
(require 'ipe-edit)

(when ipe-mouse-support-p (require 'ipe-mouse))
(when ipe-menu-support-p  (require 'ipe-menu))

(defun insert-pair-edit-delete (mnemonic)
  "Delete the OPEN and CLOSE strings of an Insert Pair Edit PAIR.

This command deletes the OPEN and CLOSE strings of an Insert Pair
Edit (ipe) PAIR.  The OPEN and CLOSE strings to be deleted are
determined by: prompting for, and looking up, a MNEMONIC in the
`customize'-able `ipe-pairs' / `ipe-mode-pairs' variables.

Once selected, this command will:

- Search around POINT for the nearest matching OPEN or CLOSE string
  for the PAIR to be deleted,
- Find its matching counterpart (OPEN and CLOSE strings may be
  nested), and;
- Delete both the OPEN and CLOSE string from the buffer."
  (interactive (list (ipe-edit--read-mnemonic "Delete PAIR: ")))

  (save-excursion
    (if ipe-edit-mode
	(ipe-edit--delete-first-pair nil)
      (ipe-updt--delete-find-pairs mnemonic))))

(defun insert-pair-edit-replace (mnemonic replace)
  "Replace the OPEN and CLOSE strings of an Insert Pair Edit PAIR.

This command replaces the OPEN and CLOSE strings of an Insert Pair
Edit (ipe) PAIR.  The OPEN and CLOSE strings of the PAIR to be
replaced are determined by: prompting for, and looking up, a MNEMONIC
in the `customize'-able `ipe-pairs' / `ipe-mode-pairs' variables.

The PAIR to be replaced is identified by MNEMONIC.
The replacement PAIR is identified by the REPLACE mnemonic.

Once selected, this command will:

- Search around POINT for the nearest matching OPEN or CLOSE string
  for the PAIR to be replaced,
- Find its matching counterpart (OPEN and CLOSE strings may be
  nested),
- Delete both the OPEN and CLOSE strings from the buffer,
- Replace them with OPEN and CLOSE overlays of the new REPLACE-ment
  PAIR, and then;
- Run the command `ipe-edit-mode' to enter the Insert Pair Edit
  minor mode.

Once activated, the command `ipe-edit-mode' allows these newly created
REPLACE-ment OPEN and CLOSE overlays be moved around the buffer using
the commands defined within `ipe-edit-mode-map'.

The OPEN and CLOSE overlays may then be either inserted
\(`ipe-edit--insert-pair') or discarded (`ipe-edit--abort').

Movement of the OPEN and CLOSE overlays is independent, and made by
different lexical units (char, word, line, list),
depending initially on the settings for the given REPLACE-ment PAIR,
and then, on the current movement mode (`ipe--movement'.)  (For the
full list of movement / editing commands, see the documentation for
`ipe-edit-mode'.)

By default, the initial movement of the OPEN and CLOSE overlays is by
words, however, alternate lexical units (char, word, line,
list) can be associated with the movement of each PAIR within
`ipe-pairs' / `ipe-mode-pairs'.

If the selected REPLACE-ment PAIR has a different lexical unit, the
initial movement of OPEN and CLOSE parts of the REPLACE-ment PAIR will
also be by characters, lines, or lists (depending on the
`customize'-ation for the given PAIR.)"
  (interactive (list (ipe-edit--read-mnemonic "Replace PAIR: ")
		     (let ((ipe--mnemonic nil))
		       (ipe-edit--read-mnemonic "With PAIR: "))))

  (ipe--undo-start)

  (setq ipe--mnemonic replace)
  (ipe--movement-set (ipe--pair-movement-initial (ipe--pair)))
  (ipe-updt--delete-find-pairs mnemonic t)
  (ipe--pair-pos-redisplay)

  (if (= (ipe--pos-count) 0)
      (ipe--undo-abort)

    (ipe-edit-mode t)

    (when (< (ipe--pos-open 0) (window-start))
      (goto-char (ipe--pos-open 0))
      (recenter -1))

    (when (> (ipe--pos-open 0) (window-end))
      (goto-char (ipe--pos-open 0))
      (recenter 1))))

(defun insert-pair-edit-update (mnemonic)
  "Update the position of an Insert Pair Edit PAIR.

This command updates the position of the OPEN and CLOSE strings of an
Insert Pair Edit (ipe) PAIR.  The OPEN and CLOSE strings of the PAIR
are determined by: prompting for, and looking up, a MNEMONIC in the
`customize'-able `ipe-pairs' / `ipe-mode-pairs' variables.

Once selected, this command will:

- Search around POINT for the nearest matching OPEN or CLOSE string
  for the PAIR,
- Find its matching counterpart (OPEN and CLOSE strings can be
  nested),
- Delete both the OPEN and CLOSE strings from the buffer,
- Replace them with overlays, and then;
- Run the command `ipe-edit-mode' to enter the Insert Pair Edit
  minor mode.

Once activated, the command `ipe-edit-mode' allows these newly created
OPEN and CLOSE overlays to be moved around the buffer using the
commands defined within `ipe-edit-mode-map'.

The OPEN and CLOSE overlays may then be either inserted
\(`ipe-edit--insert-pair') or discarded (`ipe-edit--abort').

Movement of the OPEN and CLOSE overlays is independent, and made by
different lexical units (char, word, line, list), depending initially
on the settings for the given PAIR, and then, on the current movement
mode (`ipe--movement'.)  (For the full list of movement / editing
commands, see the documentation for `ipe-edit-mode'.)

By default, the initial movement of the OPEN and CLOSE overlays is by
words, however, alternate lexical units (char, word, line,
list) can be associated with the movement of each PAIR within
`ipe-pairs' / `ipe-mode-pairs' variables.

If the selected PAIR has a different lexical unit, the initial
movement of OPEN and CLOSE parts of the PAIR is by characters, lines,
or lists (S-expressions) (depending on the `customize'-ation for the
given PAIR.)"
  (interactive (list (ipe-edit--read-mnemonic "Update PAIR: ")))
  (insert-pair-edit-replace mnemonic mnemonic))

(defun insert-pair-edit (arg mnemonic &optional replace)
  "Surround the current ARG lexical units with an `ipe' PAIR.

An Insert Pair Edit (ipe) PAIR consists of an OPEN and CLOSE string
that are determined by: prompting for, and looking up, a MNEMONIC in
the `customize'-able `ipe-pairs' / `ipe-mode-pairs' variables.

Once selected, this command will:

- Create overlays for both the OPEN and CLOSE strings of the PAIR,
- Insert the OPEN and CLOSE overlays into the buffer around the
  current ARG lexical units, and;
- Run the command `ipe-edit-mode' to enter the Insert Pair Edit
  minor mode.

Once activated, the command `ipe-edit-mode' allows these OPEN and
CLOSE overlays to be moved around the buffer using the commands
defined within `ipe-edit-mode-map'.

The OPEN and CLOSE overlays may then be either inserted
\(`ipe-edit--insert-pair') or discarded (`ipe-edit--abort').

Movement of the OPEN and CLOSE overlays is independent, and made by
different lexical units (char, word, line, list), depending initially
on the settings for the given PAIR, and then, on the current movement
mode (`ipe--movement'.)  (For the full list of movement / editing
commands, see the documentation for `ipe-edit-mode'.)

By default, the initial position of the OPEN and CLOSE overlays is
around the current ARG words, however, alternate lexical units (char,
word, line, list) can be associated with each PAIR by customizations
within `ipe-pairs' / `ipe-mode-pairs'.

If the selected PAIR has a different lexical unit, the OPEN and CLOSE
overlays are inserted either surrounding ARG characters, ARG lines, or
ARG lists (S-expressions) and, initial movement for that PAIR will
also be by characters, lines or lists (S-expressions) (depending on
the `customize'-ation for the given PAIR.)

If passed a:

  '\\[universal-argument]'

prefix ARG, (instead of a numeric ARG) this will call:

  `insert-pair-edit-update'.

With MNEMONIC specifying the PAIR to be updated.

If passed two:

  '\\[universal-argument] \\[universal-argument]'

prefix ARGs, this will call:

  `insert-pair-edit-delete'

With MNEMONIC specifying the PAIR to be deleted.

If passed three:

  '\\[universal-argument] \\[universal-argument] \
\\[universal-argument]'

prefix ARGs, this will call:

  `insert-pair-edit-replace'

With REPLACE specifying the MNEMONIC of the replacement PAIR."
  (interactive
   (list
    current-prefix-arg
    (ipe-edit--read-mnemonic
     (if (or (not current-prefix-arg)
	     (integerp current-prefix-arg))
	 "Insert PAIR: "
       (if (and (listp current-prefix-arg)
		(= 4 (car current-prefix-arg)))
	   "Update PAIR: "
	 (if (and (listp current-prefix-arg)
		  (= 16 (car current-prefix-arg)))
	     "Delete PAIR: "
	   "Replace PAIR: "))))
    (if (and current-prefix-arg
	     (listp current-prefix-arg)
	     (< 16 (car current-prefix-arg)))
	(ipe-edit--read-mnemonic "With PAIR: ")
      nil)))

  (cond
   ;; Insert
   ((or (not current-prefix-arg)
	(integerp current-prefix-arg))

    (setq ipe--mnemonic mnemonic)
    (ipe--pair-pos-init (ipe--pos-count) (point) arg)
    (ipe--pair-pos-redisplay)

    (when (> (ipe--pos-count) 0)
      (ipe--undo-start)
      (ipe-edit-mode t)

      (when (or (not ipe--mnemonic)
		(string= ipe--mnemonic "")
		(ipe--pair-property (ipe--pair) :auto-insert))

	(ipe-edit--insert-pair))))

   ;; Update
   ((and current-prefix-arg
	 (listp current-prefix-arg)
	 (= 4 (car current-prefix-arg)))
    (insert-pair-edit-update mnemonic))

   ;; Delete
   ((and current-prefix-arg
	 (listp current-prefix-arg)
	 (= 16 (car current-prefix-arg)))
    (insert-pair-edit-delete mnemonic))

   ;; Replace
   (t
    (if (> (ipe--pos-count) 0)
	(setq mnemonic ipe--replace-mnemonic)
      (setq ipe--replace-mnemonic mnemonic))
    (insert-pair-edit-replace mnemonic replace))))

(defun insert-pair-edit-cmd (arg)
  "Surround the current ARG lexical units with an `ipe' PAIR.

This command is expected to be bound to a key sequence which matches a
Insert Pair Edit (ipe) PAIR MNEMONIC.  (To be prompted for the
MNEMONIC, use the standard `insert-pair-edit' function.)

An Insert Pair Edit (ipe) PAIR consists of an OPEN and CLOSE string
that are determined by: prompting for, and looking up, a MNEMONIC in
the `customize'-able `ipe-pairs' / `ipe-mode-pairs' variables.

Once selected, this command will:

- Create overlays for both the OPEN and CLOSE strings of the PAIR,
- Insert the OPEN and CLOSE overlays into the buffer around the
  current ARG lexical units, and;
- Run the command `ipe-edit-mode' to enter the Insert Pair Edit
  minor mode.

Once activated, the command `ipe-edit-mode' allows these newly created
OPEN and CLOSE overlays to be moved around the buffer using the
commands defined within `ipe-edit-mode-map'.

The OPEN and CLOSE overlays may then be either inserted
\(`ipe-edit--insert-pair') or discarded (`ipe-edit--abort').

Movement of the OPEN and CLOSE overlays is independent, and made by
different lexical units (char, word, line, list), depending initially
on the settings for the given PAIR, and then, on the current movement
mode (`ipe--movement'.)  (For the full list of movement / editing
commands, see the documentation for `ipe-edit-mode'.)

By default, the initial position of the OPEN and CLOSE overlays is
around the current ARG words, however, alternate lexical units (char,
word, line, list) can be associated with each PAIR by customizations
within `ipe-pairs' / `ipe-mode-pairs'.

If the selected PAIR has a different lexical unit, the OPEN and CLOSE
overlays are inserted either surrounding ARG characters, ARG lines, or
ARG lists (S-expressions) and, initial movement for that PAIR will
also be by characters, lines or lists (S-expressions) (depending on
the `customize'-ation for the given PAIR.)

If passed a:

  '\\[universal-argument]'

prefix ARG, (instead of a numeric ARG) this will call:

  `insert-pair-edit-update'.

With MNEMONIC specifying the PAIR to be updated.

If passed two:

  '\\[universal-argument] \\[universal-argument]'

prefix ARGs, this will call:

  `insert-pair-edit-delete'

With MNEMONIC specifying the PAIR to be deleted."
  (interactive "P")
  (let ((cmd (substring-no-properties (this-command-keys))))
    (setq ipe--mnemonic
	  (if (member cmd (ipe--mnemonic-list))
	      cmd
	    (ipe-edit--read-mnemonic "Insert PAIR: ")))
    (insert-pair-edit arg ipe--mnemonic)))

(provide 'insert-pair-edit)

;;; insert-pair-edit.el ends here
