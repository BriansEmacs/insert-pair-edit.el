;;; ipe-word.el --- Insert Pair Edit - word pair movement functions -*- lexical-binding: t; -*-
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
;; Pair Edit' package for word movement.
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
;; This module supplies a function `ipe-word--move-by' which performs
;; this movement for 'words'.

;; -------------------------------------------------------------------
;;; Code:

(require 'ipe-core)

;; -------------------------------------------------------------------
;;;; Utility Functions.
;; -------------------------------------------------------------------

(defun ipe-word-line-forward (arg)
  "Move forward ARG lines maintaining current column position.

If ARG is negative, move backward ARG lines."
  (let ((column (current-column)))
    (forward-line arg)
    (move-to-column column)))

;; -------------------------------------------------------------------
;;;; move-by Functions.
;; -------------------------------------------------------------------

(defun ipe-word--move-by (_defn _n side action pos _other units
				&optional push)
  "Calculate movement `by word' for Insert Pair Edit.

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
	(or (looking-at "\\<")
	    (re-search-backward "\\<" (ipe--bol) t)
	    (re-search-forward "\\<" (ipe--eol) t))

	(when (> units 1)
	  (backward-word (1- units))))

       ((equal action 'beg)
	(beginning-of-line (- (if (looking-at "^")
				  1
				(if (> units 0) 2 0))
			      units)))

       ((equal action 'up)
	(ipe-word-line-forward (- units))
	(or (looking-at "\\<")
	    (re-search-backward "\\<" (ipe--bol) t)
	    (re-search-forward "\\<" (ipe--eol) t)))

       ((equal action 'backward)
	(backward-word units))

       ((equal action 'forward)
	(unless (eobp)
	  (forward-char 1)
	  (ipe-dotimes units
	    (if (re-search-forward "\\<" nil t)
		(forward-char 1)
	      (goto-char (point-max))))
	  (unless (eobp)
	    (backward-char 1))))

       ((equal action 'down)
	(ipe-word-line-forward units)
	(unless (looking-at "\\<")
	  (re-search-forward "\\<" (ipe--eol) t)))

       ((equal action 'end)
	(let ((beg (point)))
	  (end-of-line (+ units (if (> units 0) 0 1)))
	  (re-search-backward "\\<" (ipe--bol) t)
	  (when (= beg (point))
	    (end-of-line 2)
	    (re-search-backward "\\<" (ipe--bol) t))))

       ((equal action 'reset)
	(re-search-backward "\\<" (ipe--bol) t))))

     ((equal side 'close)
      (cond
       ((equal action 'init)
	(or (looking-at "\\>")
	    (re-search-forward "\\>" (ipe--eol) t)
	    (re-search-backward "\\>" (ipe--bol) t))

	(when (> units 1)
	  (forward-word (1- units)))

	(unless (looking-at "\\>")
	  (re-search-backward "\\>" (ipe--bol) t)))

       ((equal action 'beg)
	(let ((beg (point)))
	  (beginning-of-line (- (if (> units 0) 2 1) units))
	  (re-search-forward "\\>" (ipe--eol) t)
	  (when (= beg (point))
	    (beginning-of-line 0)
	    (re-search-forward "\\>" (ipe--eol) t))))

       ((equal action 'up)
	(ipe-word-line-forward (- units))
	(or (looking-at "\\>")
	    (re-search-backward "\\>" (ipe--bol) t)
	    (re-search-forward "\\>" (ipe--eol) t)))

       ((equal action 'backward)
	(unless (bobp)
	  (backward-char 1)
	  (ipe-dotimes units
	    (if (re-search-backward "\\>" nil t)
		(backward-char 1)
	      (goto-char (point-min))))
	  (unless (bobp)
	    (forward-char 1))))

       ((equal action 'forward)
	(forward-word units)
	(unless (looking-at "\\>")
	  (re-search-backward "\\>" (ipe--bol) t)))

       ((equal action 'down)
	(ipe-word-line-forward units)
	(unless (looking-at "\\>")
	  (re-search-forward "\\>" (ipe--eol) t)))

       ((equal action 'end)
	(end-of-line (+ units
			(if (> units 0)
			    (if (looking-at "$") 1 0)
			  1)))

	(when (bobp)
	  (end-of-line 1)))

       ((equal action 'reset)
	(re-search-forward "\\>" (ipe--eol) t)))))

    (point)))

(ipe-move-by-install 'word
		     'ipe-word--move-by
		     nil
		     "words")

(provide 'ipe-word)

;;; ipe-word.el ends here
