;;; ipe-list.el --- Insert Pair Edit - list pair movement functions -*- lexical-binding: t; -*-
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
;; Pair Edit' package for list movement.
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
;; This module supplies a function `ipe-list--move-by' which performs
;; this movement for 'lists' (S-expression)
;;
;; The ipe concept of 'up and 'down are changed to map to (`up-list' /
;; `backward-up-list') and (`down-list').

;; -------------------------------------------------------------------
;;; Code:

(require 'ipe-core)

;; -------------------------------------------------------------------
;;;; Utility Functions.
;; -------------------------------------------------------------------

(defun ipe-list--beg-list (units)
  "Safe `beginning-of-defun' that will not error.

Move backward to the beginning of a defun.  With UNITS, do it that
many times."

  (let ((done t))
    (if (< units 0)
	(ipe-dotimes (- units)
	  (condition-case nil (end-of-defun 1)
	    (error (setq done nil))))
      (ipe-dotimes units
	(condition-case nil (beginning-of-defun 1)
	  (error (setq done nil)))))
    done))

(defun ipe-list--forward-list (units)
  "Safe `forward-list' that will not error.

Move forward UNITS number of balanced groups of parentheses.  If it is
impossible to go the full number of UNITS, got as far as possible."

  (if (< units 0)
      (ipe-list--backward-list (- units))
    (let ((done t))
      (ipe-dotimes units
	(condition-case nil (forward-list 1)
	  (error (setq done nil))))
      (unless done
	(condition-case nil
	    (progn (up-list 1) (down-list -1))
	  (error (goto-char (point-max)))))
      done)))

(defun ipe-list--backward-list (units)
  "Safe `backward-list' that will not error.

Move backward UNITS number of balanced groups of parentheses.  If it
is impossible to go the full number of UNITS, got as far as possible."

  (if (< units 0)
      (ipe-list--forward-list (- units))
    (let ((done t))
      (ipe-dotimes units
	(condition-case nil (backward-list 1)
	  (error (setq done nil))))
      (unless done
	(condition-case nil
	    (progn (up-list -1) (down-list 1))
	  (error (goto-char (point-min)))))
      done)))

(defun ipe-list--up-list (units)
  "Safe `up-list' that will not error.

Move up UNITS number of balanced groups of parentheses.  If it is
impossible to go the full number of UNITS, got as far as possible."

  (let ((done t))
    (if (< units 0)
	(progn
	  (ipe-dotimes (- units)
	    (condition-case nil (backward-up-list 1)
	      (error (setq done nil))))
	  (unless done (goto-char (point-min))))
      (ipe-dotimes units
	(condition-case nil (up-list 1)
	  (error (setq done nil))))
      (unless done (goto-char (point-max))))
    done))

(defun ipe-list--down-list (units)
  "Safe `down-list' that will not error.

Move down UNITS number of balanced groups of parentheses.  If it is
impossible to go the full number of UNITS, got as far as possible."

  (let ((done t))
    (if (< units 0)
	(progn
	  (ipe-dotimes (- units)
	    (condition-case nil (down-list -1)
	      (error (setq done nil))))
	  (condition-case nil
	      (if done
		  (progn (down-list -1) (up-list 1))
		(ipe-list--forward-list 1))
	    (error nil)))
      (ipe-dotimes units
	(condition-case nil (down-list 1)
	  (error (setq done nil))))
      (condition-case nil
	  (if done
	      (progn (down-list 1) (up-list -1))
	    (ipe-list--backward-list 1))
	(error nil)))
    done))

(defun ipe-list--end-list (units)
  "Safe `end-of-defun' that will not error.

Move forwards to the end of a defun.  With UNITS, do it that
many times."

  (let ((done t))
    (if (< units 0)
	(ipe-dotimes (- units)
	  (condition-case nil (beginning-of-defun 1)
	    (error (setq done nil))))
      (ipe-dotimes units
	(condition-case nil (end-of-defun 1)
	  (error (setq done nil)))))
    done))

(defun ipe-list--push (side action other units)
  "Calculate movement by list for a push move.

Moves POINT to a position corresponding to a movement initiated by the
movement of the OTHER member of the PAIR.

- SIDE is either `'open' or `'close'.
- ACTION is either `'beg', `'up', `'backward', `'forward', `'down', or
  `'end', indicating the movement performed by the other member of the
  PAIR.
- OTHER is the position of the other member of the PAIR.
- UNITS are the number of units to move.

Movement is calculated from POINT."

  (cond
   ((equal side 'open)
    (cond
     ((equal action 'beg)
      (goto-char other))

     ((equal action 'down)
      (goto-char other)
      (when (ipe-list--up-list 1)
	(if (ipe-list--backward-list 1)
	    (ipe-list--down-list 1)
	  (ipe-list--down-list 1))))

     ((equal action 'up)
      (ipe-list--up-list (- units)))

     ((equal action 'end)
      (goto-char other))))

   ((equal side 'close)
    (cond
     ((equal action 'beg)
      (goto-char other))

     ((equal action 'down)
      (goto-char other)
      (when (ipe-list--up-list -1)
	(if (ipe-list--forward-list 1)
	    (ipe-list--down-list -1)
	  (ipe-list--down-list -1))))

     ((equal action 'up)
      (ipe-list--up-list units))

     ((equal action 'end)
      (goto-char other))))))

;; -------------------------------------------------------------------
;;;; move-by Functions.
;; -------------------------------------------------------------------

(defun ipe-list--move-by (_defn _n side action pos other units
				&optional push)
  "Calculate movement `by list' for Insert Pair Edit.

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
     ((identity push)
      (ipe-list--push side action other units))

     ((equal side 'open)
      (cond
       ((equal action 'init)     (ipe-list--backward-list units))
       ((equal action 'beg)      (ipe-list--beg-list units))
       ((equal action 'up)       (ipe-list--up-list (- units)))
       ((equal action 'backward) (ipe-list--backward-list units))
       ((equal action 'forward)  (ipe-list--forward-list units))
       ((equal action 'down)     (ipe-list--down-list units))
       ((equal action 'end)      (ipe-list--end-list units))
       ((equal action 'reset)    (goto-char pos))))

     ((equal side 'close)
      (cond
       ((equal action 'init)     (ipe-list--forward-list units))
       ((equal action 'beg)      (ipe-list--beg-list units))
       ((equal action 'up)       (ipe-list--up-list units))
       ((equal action 'backward) (ipe-list--backward-list units))
       ((equal action 'forward)  (ipe-list--forward-list units))
       ((equal action 'down)     (ipe-list--down-list (- units)))
       ((equal action 'end)      (ipe-list--end-list units))
       ((equal action 'reset)    (goto-char pos)))))

    (point)))

(ipe-move-by-install 'list
		     'ipe-list--move-by
		     nil
		     "lists (S-expressions)")

(provide 'ipe-list)

;;; ipe-list.el ends here
