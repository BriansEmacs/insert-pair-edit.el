;;; ipe-read.el --- Insert Pair Edit - read ipe mnemonics -*- lexical-binding: t; -*-
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
;; This file defines a function, `ipe-read--mnemonic', used to
;; prompt the user for an 'Insert Pair Edit' MNEMONIC using the Emacs
;; minibuffer.
;;
;; This function uses `completing-read' to offer completions for the
;; `ipe' MNEMONICs from the `ipe-pairs' / `ipe-mode-pairs' variables.
;;
;; When the `ipe-mnemonic-prompt-shortcut-p' variable is set to `t',
;; special shortcut behaviour is added to `completing-read', whereby
;; entering a complete unique MNEMONIC (for the current major-mode)
;; into the minibuffer will immediately exit the minibuffer without
;; requiring the user to enter a <RET>.

;; -------------------------------------------------------------------
;;; Code:

(require 'ipe-core)
(require 'ipe-help)

(defvar ipe-read--mnemonic-history '()
  "History list for `ipe-read--mnemonic-read' / \
`ipe-read--mnemonic-read-by-char'.")

(defvar ipe-read--minibuffer-depth ()
  "The depth at which `ipe-read--mnemonic-read-by-char' is installed.

This variable defines a list of the `minibuffer-depth's at which the
`ipe-read--mnemonic-read-by-char' is expected to be active.  This is
to stop recursive minibuffer invocations from shortcutting exit from
the minibuffer when not reading MNEMONICs.")

;; -------------------------------------------------------------------
;;;;; Minibuffer MNEMONIC Read Commands.
;; -------------------------------------------------------------------

(defun ipe-read--pop-minibuffer-depth ()
  "Remove `minibuffer-depth' from `ipe-read--pop-minibuffer-depth'.

This function is installed as a `minibuffer-exit-hook' to remove the
current `minibuffer-depth' from the list of depths
\(`ipe-read--pop-minibuffer-depth') at which the
`ipe-read--mnemonic-read-by-char' function will shortcut exit from the
minibuffer."

  (setq ipe-read--minibuffer-depth
	(remove (minibuffer-depth) ipe-read--minibuffer-depth))

  (when (= (minibuffer-depth) 0)
    (setq ipe-read--minibuffer-depth nil)
    (remove-hook 'minibuffer-exit-hook
		 #'ipe-read--pop-minibuffer-depth)))

(defun ipe-read--mnemonic-insert-and-exit-on-match (arg)
  "`self-insert-command' - but will exit minibuffer on `ipe' match.

Insert the character you type, ARG, then check the input within the
minibuffer to see if it forms a unique match to any of the MNEMONICs
within `ipe-mode-pair' / `ipe-pair'.  If it does, call
`exit-minibuffer'."

  (interactive "p")
  (self-insert-command arg)
  (let* ((mnemonics (ipe--mnemonic-list ipe--major-mode))
	 (input     (buffer-substring (field-end 1) (point-max)))
	 (matches   (test-completion input mnemonics))
	 (prefixes  (length (all-completions input mnemonics))))
    (when (and matches
	       (= prefixes 1)
	       (member (minibuffer-depth) ipe-read--minibuffer-depth))
      (exit-minibuffer))))

(defvar ipe-read--minibuffer-keymap
  (let ((keymap (copy-keymap minibuffer-local-must-match-map)))
    (define-key keymap [remap self-insert-command]
		'ipe-read--mnemonic-insert-and-exit-on-match)
    keymap)
  "Keymap used by `ipe-read--mnemonic-read-by-char'.

This keymap is a copy of `minibuffer-local-must-match-map', but
replaces `self-insert-command' with
`ipe-read--mnemonic-insert-and-exit-on-match' so as to exit
immediately when the user has entered a complete `ipe' MNEMONIC.")

(defun ipe-read--mnemonic-read-by-char (prompt)
  "Prompt for a matching Insert Pair Edit PAIR MNEMONIC.

PROMPT is passed as the first parameter to `completing-read'.  The
list of possible MNEMONICs is obtained from the `car's of the
`ipe-mode-pairs' and `ipe-pairs' variables.

This command alters `minibuffer-local-must-match-map' so as to exit
the minibuffer immediately on typing a unique MNEMONIC."

  ;; Set completing-read default input.
  (unless (memq ipe--mnemonic (ipe--mnemonic-list))
    (setq ipe--mnemonic (car (ipe--mnemonic-list))))

  (setq ipe-read--minibuffer-depth
	(cons (1+ (minibuffer-depth)) ipe-read--minibuffer-depth))

  (add-hook 'minibuffer-exit-hook #'ipe-read--pop-minibuffer-depth)

  (let* ((ipe--major-mode major-mode)
	 (completion-extra-properties
	  '(:annotation-function ipe--mnemonic-annotate))
	 (enable-recursive-minibuffers t)
	 (minibuffer-local-must-match-map ipe-read--minibuffer-keymap)
	 (mnemonic (completing-read prompt
				    (ipe--mnemonic-list ipe--major-mode)
				    nil
				    t
				    nil
				    ipe-read--mnemonic-history
				    ipe--mnemonic)))
    mnemonic))

(defun ipe-read--mnemonic (prompt)
  "Prompt for a matching Insert Pair Edit PAIR MNEMONIC.

PROMPT is passed as the first parameter to `completing-read'.  The
list of possible MNEMONICs is obtained from the `car's of the
`ipe-mode-pairs' and `ipe-pair' variables."

  ;; Set default for change of mode.
  (unless (memq ipe--mnemonic (ipe--mnemonic-list))
    (setq ipe--mnemonic (car (ipe--mnemonic-list))))

  (let* ((ipe--major-mode major-mode)
	 (completion-extra-properties
	  '(:annotation-function ipe--mnemonic-annotate))
	 (mnemonics       (ipe--mnemonic-list))
	 (zero-p          (zerop (length mnemonics)))
	 (one-p           (= (length mnemonics) 1))
	 (help
	  (propertize
	   (substitute-command-keys
	    "(\\<ipe-read--minibuffer-keymap>\\[ipe-help-prompt]\
 - Help) - ")
	   'face 'font-lock-doc-face))
	 (read-prompt (concat help prompt)))
    (if zero-p
	(message "No PAIRs defined")
      (if one-p
	  (car mnemonics)
	(if ipe-mnemonic-prompt-shortcut-p
	    (ipe-read--mnemonic-read-by-char read-prompt)
	  (completing-read prompt
			   mnemonics
			   nil t nil ipe-read--mnemonic-history
			   ipe--mnemonic))))))

(provide 'ipe-read)

;;; ipe-read.el ends here
