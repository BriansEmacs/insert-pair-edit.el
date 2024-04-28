;;; ipe-char.el --- Insert Pair Edit - char pair movement functions -*- lexical-binding: t; -*-
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
;; This file defines the 'move-by' functionality used by the 'Insert
;; Pair Edit' package for character movement.
;;
;; The 'move-by' functionality is supplied by a set of 'pluggable'
;; functions of the form:
;;
;;   (MOVE-BY-FUNCTION DEFN N SIDE ACTION POS OTHER UNITS PUSH)
;;
;; Where:
;;
;;   DEFN is the definition of the PAIR from `ipe-pairs'.
;;   N is the position of the PAIR in `ipe--pair-pos-list'.
;;   SIDE is either 'open or 'close.
;;   ACTION is either 'init, 'beg, 'up, 'backward, 'forward, 'down,
;;   'end or 'reset.
;;   POS is the position from which the movement begins.
;;   OTHER is the position of the other member of the PAIR.
;;   UNITS are the number of units to move.
;;   PUSH indicates that this is a push move from the other PAIR.
;;
;; And which returns the position to which to set one of strings of a
;; PAIR when the given movement command ('beg, 'up, 'down, 'backward,
;; 'forward, 'end) is entered by the user.
;;
;; This module supplies a function `ipe-char--move-by' which performs
;; this movement for 'characters'.

;; -------------------------------------------------------------------
;;; Code:

(require 'ipe-core)

;; -------------------------------------------------------------------
;;;; Utility Functions.
;; -------------------------------------------------------------------

(defun ipe-char-line-forward (arg)
  "Move forward ARG lines maintaining current column position.

If ARG is negative, move backward ARG lines."
  (let ((column (current-column)))
    (forward-line arg)
    (move-to-column column)))

;; -------------------------------------------------------------------
;;;; move-by Functions.
;; -------------------------------------------------------------------

(defun ipe-char--move-by (_defn _n side action pos _other units
				&optional push)
  "Calculate movement `by char' for Insert Pair Edit.

- DEFN is the definition of the PAIR from `ipe-pairs'.
- N is the position of the PAIR in `ipe--pair-pos-list'.
- SIDE is either `'open' or `'close'.
- ACTION is either `'init', `'beg', `'up', `'backward', `'forward',
  `'down', `'end', or `'reset'.
- POS is the position from which the movement begins.
- OTHER is the position of the other member of the PAIR.
- UNITS are the number of units to move.
- PUSH indicates that this is a push move from the other PAIR.

Movement is calculated from POINT."

  (save-excursion
    (goto-char (or pos (point)))
    (cond
     ((identity push))

     ((equal side 'open)
      (cond
       ((equal action 'init)
	(condition-case nil (if (> units 0) (backward-char units))
	  ('beginning-of-buffer nil)
	  ('end-of-buffer nil)))

       ((equal action 'beg)
	(beginning-of-line (- (if (looking-at "^")
				  1
				(if (> units 0) 2 0))
			      units)))

       ((equal action 'up)
	(ipe-char-line-forward (- units)))

       ((equal action 'backward)
	(condition-case nil (backward-char units)
	  ('beginning-of-buffer nil)
	  ('end-of-buffer nil)))

       ((equal action 'forward)
	(condition-case nil (forward-char units)
	  ('beginning-of-buffer nil)
	  ('end-of-buffer nil)))

       ((equal action 'down)
	(ipe-char-line-forward units))

       ((equal action 'end)
	(end-of-line (+ units
			(if (looking-at "$")
			    (if (> units 0) 1 2)
			  (if (> units 0) 0 1)))))

       ((equal action 'reset)))) ;; No Action

     ((equal side 'close)
      (cond
       ((equal action 'init)
	(condition-case nil (if (> units 0) (forward-char units))
	  ('beginning-of-buffer nil)
	  ('end-of-buffer nil)))

       ((equal action 'beg)
	(beginning-of-line (- (if (looking-at "^")
				  (if (> units 0) 1 0)
				(if (> units 0) 2 1))
			      units)))

       ((equal action 'up)
	(ipe-char-line-forward (- units)))

       ((equal action 'backward)
	(condition-case nil (backward-char units)
	  ('beginning-of-buffer nil)
	  ('end-of-buffer nil)))

       ((equal action 'forward)
	(condition-case nil (forward-char units)
	  ('beginning-of-buffer nil)
	  ('end-of-buffer nil)))

       ((equal action 'down)
	(ipe-char-line-forward units))

       ((equal action 'end)
	(end-of-line (+ units
			(if (> units 0)
			    (if (looking-at "$") 1 0)
			  1)))
	(when (bobp)
	  (end-of-line)))

       ((equal action 'reset)))))

    (point)))

(ipe-move-by-install 'char
		     'ipe-char--move-by
		     nil
		     "characters")

(provide 'ipe-char)

;;; ipe-char.el ends here
