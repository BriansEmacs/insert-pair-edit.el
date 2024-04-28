;;; ipe-mc.el --- Insert Pair Edit - multiple-cursors compatibility -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Brian Kavanagh

;; Author: Brian Kavanagh <brians.emacs@gmail.com>
;; Maintainer: Brian Kavanagh <brians.emacs@gmail.com>
;; Created: 18 March, 2024
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
;; This file sets up the `multiple-cursors' `mc/cmds-to-run-once'
;; for the `ipe' (Insert Pair Edit) package.
;;

;; -------------------------------------------------------------------
;;; Code:
;; -------------------------------------------------------------------

(defvar mc/cmds-to-run-once)
(defvar mc/cmds-to-run-for-all)

(declare-function mc/save-lists "ext:multiple-cursors-core.el")
(declare-function mc/load-lists "ext:multiple-cursors-core.el")

;; Ensure the ipe-edit commands are only run once by multiple-cursors.
(defun ipe-mc--update-run-cmds ()
  "Update the \"mc/cmds-to-run-*\" for the `ipe' package.

This function will add `ipe' functions to the appropriate:

   `mc/cmds-to-run-once'
   `mc/cmds-to-run-for-all'

`multiple-cursors' variables depending upon whether or not they will
work with multiple cursors."

  (when (featurep 'multiple-cursors)
    (mc/load-lists)
    (let ((update))
      (mapc
       (lambda (command)
	 (when (not (memq command mc/cmds-to-run-once))
	   (add-to-list 'mc/cmds-to-run-once command)
	   (setq update t))
	 (when (memq command mc/cmds-to-run-for-all)
	   (setq mc/cmds-to-run-for-all
		 (remove command mc/cmds-to-run-for-all)
		 update t)))
       '(ipe-defn--change-mode-pair-mnemonic
	 ipe-defn--change-pair-mnemonic
	 ipe-defn--delete-mode-pair
	 ipe-defn--delete-pair
	 ipe-defn--edit-mode-pair
	 ipe-defn--edit-pair
	 ipe-edit--abort
	 ipe-edit--add-next-contents
	 ipe-edit--add-next-pair
	 ipe-edit--add-previous-contents
	 ipe-edit--add-previous-pair
	 ipe-edit--change-pair
	 ipe-edit--close-backward
	 ipe-edit--close-beg
	 ipe-edit--close-down
	 ipe-edit--close-end
	 ipe-edit--close-forward
	 ipe-edit--close-up
	 ipe-edit--contents-capitalize
	 ipe-edit--contents-copy
	 ipe-edit--contents-downcase
	 ipe-edit--contents-kill
	 ipe-edit--contents-replace
	 ipe-edit--contents-trim
	 ipe-edit--contents-upcase
	 ipe-edit--contents-yank
	 ipe-edit--delete-first-pair
	 ipe-edit--delete-all-pairs
	 ipe-edit--delete-last-pair
	 ipe-edit--edit-current-pair
	 ipe-edit--ia-copy-text
	 ipe-edit--ia-goto-close
	 ipe-edit--ia-goto-open
	 ipe-edit--ia-kill-text
	 ipe-edit--ia-resume
	 ipe-edit--ia-update-backward
	 ipe-edit--ia-update-forward
	 ipe-edit--insert-first-pair
	 ipe-edit--insert-pair
	 ipe-edit--insert-last-pair
	 ipe-edit--movement-by-char
	 ipe-edit--movement-by-line
	 ipe-edit--movement-by-list
	 ipe-edit--movement-by-word
	 ipe-edit--open-backward
	 ipe-edit--open-beg
	 ipe-edit--open-down
	 ipe-edit--open-end
	 ipe-edit--open-forward
	 ipe-edit--open-up
	 ipe-edit--recenter-pair
	 ipe-edit--toggle-escapes
	 ipe-edit--update-next-close
	 ipe-edit--update-next-contents
	 ipe-edit--update-next-open
	 ipe-edit--update-next-pair
	 ipe-edit--update-previous-close
	 ipe-edit--update-previous-contents
	 ipe-edit--update-previous-open
	 ipe-edit--update-previous-pair
	 ipe-help
	 ipe-help-info
	 ipe-help-edit-mode
	 ipe-options
	 ipe-test-run))

      (mapc
       (lambda (command)
	 (when (not (memq command mc/cmds-to-run-for-all))
	   (add-to-list 'mc/cmds-to-run-for-all command)
	   (setq update t))
	 (when (memq command mc/cmds-to-run-once)
	   (setq mc/cmds-to-run-once
		 (remove command mc/cmds-to-run-once)
		 update t)))
       '(ipe-insert-pair-edit
	 ipe-insert-pair-edit-delete
	 ipe-insert-pair-edit-update
	 ipe-insert-pair-edit-replace))

      (when update
	(mc/save-lists)))))

(provide 'ipe-mc)

;;; ipe-mc.el ends here
