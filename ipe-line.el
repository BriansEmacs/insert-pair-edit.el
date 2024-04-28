;;; ipe-line.el --- Insert Pair Edit - line pair movement functions -*- lexical-binding: t; -*-
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
;; Pair Edit' package for line movement.
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
;; This module supplies a function `ipe-line--move-by' which performs
;; this movement for 'lines'.
;;
;; The ipe concept of 'beg, 'forward, 'backward and 'end are changed
;; to map to:
;;
;; * 'beg and 'end toggle newlines before and after the OPEN and
;;   CLOSE.
;; * 'forward and 'backward indent the lines between OPEN and CLOSE,

;; -------------------------------------------------------------------
;;; Code:

(require 'ipe-core)

;; -------------------------------------------------------------------
;;;; Utility Functions.
;; -------------------------------------------------------------------

(defun ipe-line--open-set (n)
  "Set the `ipe' :open pos property for the `N'th PAIR.

Updates the :open string used to display the `N'th PAIR when
`ipe-movement' eq `'line'.

The :open string is modified from the standard `ipe--pair-open-string'
based upon the current values of the following positional properties:

  * :indent-1 - A number which specifies the amount of whitespace to
    add before the :infix.
  * :indent-2 - A number which specifies the amount of whitespace to
    add after the :infix.
  * :open-toggle - If eq t, adds a newlines after the :open string."

  (let* ((pair        (ipe--pair))
	 (indent-1    (ipe--pos-property n :indent-1))
	 (open        (ipe--pair-open-string pair))
	 (open-toggle (ipe--pos-property n :open-toggle))
	 (infix       (ipe--pair-infix-string pair))
	 (indent-2    (ipe--pos-property n :indent-2)))

    (setq open (concat (if indent-1 (make-string indent-1 ? ) "")
		       open
		       (if open-toggle
			   (concat "\n"
				   (if indent-1 (make-string indent-1 ? ) "")
				   infix)
			 "")
		       (if indent-2 (make-string indent-2 ? ) "")))

    (ipe--pos-property-set n :open open)))

(defun ipe-line--infix-set (n)
  "Set the `ipe' :infix pos property for the `N'th PAIR.

Updates the :infix string used to display the `N'th PAIR when
`ipe-movement' eq `'line'.

The :infix string is modified from the standard
`ipe--pair-infix-string' based upon the current values of the
following positional properties:

  * :indent-1 - A number which specifies the amount of whitespace to
    add before the :infix.
  * :indent-2 - A number which specifies the amount of whitespace to
    add after the :infix."

  (let* ((pair     (ipe--pair))
	 (infix    (ipe--pair-infix-string pair))
	 (indent-1 (ipe--pos-property n :indent-1))
	 (indent-2 (ipe--pos-property n :indent-2)))

    (setq infix (concat (if indent-1 (make-string indent-1 ? ) "")
			infix
			(if indent-2 (make-string indent-2 ? ) "")))

    (ipe--pos-property-set n :infix infix)))

(defun ipe-line--close-set (n)
  "Set the `ipe' :close pos property for the `N'th PAIR.

Updates the :close string used to display the `N'th PAIR when
`ipe-movement' eq `'line'.

The :close string is modified from the standard
`ipe--pair-close-string' based upon the current values of the
following positional properties:

  * :indent-1 - A number which specifies the amount of whitespace to
    add before the :infix.
  * :close-toggle - If eq t, adds a newline before the :close
    string."

  (let* ((pair         (ipe--pair))
	 (close        (ipe--pair-close-string pair))
	 (indent-1     (ipe--pos-property n :indent-1))
	 (close-toggle (ipe--pos-property n :close-toggle)))

    (setq close (concat (if close-toggle
			    (concat "\n" (if indent-1 (make-string indent-1 ? ) ""))
			  "")
			close))

    (ipe--pos-property-set n :close close)))

(defun ipe-line--inserted (n)
  "Adjusts :point when the `N'th line PAIR is inserted.

This function updates the :point property for the `N'th
`ipe--pair-pos-list' entry upon insert of a `'line' PAIR.

:point needs to be adjusted for `'line' PAIRs to account for the
offset created by the :indent-1 property."

  (let ((pos-open   (ipe--pos-open n))
	(indent-1   (ipe--pos-property n :indent-1))
	(point      (ipe--pos-property n :point))
	(point-open (ipe--pos-property n :point-open)))

    (when (and indent-1 (> indent-1 0))
      (ipe--pos-open-set n (+ pos-open indent-1))
      (ipe--pos-property-set n :open nil)

      (when (equal pos-open point)
	(when (numberp point-open)
	  (ipe--pos-property-set n :point (+ point indent-1)))
	(when (equal point-open 'after)
	  (ipe--pos-property-set n :point (+ point indent-1)))))))

(defun ipe-line--unset (n)
  "Remove the `ipe' position properties for the `N'th PAIR.

Removes the position properties used bin the `'line' `move-by'
function.

The following positional properties are removed:

  * :indent-1
  * :indent-2
  * :open-toggle
  * :close-toggle"

  (ipe--pos-property-set n
			 :open         nil
			 :infix        nil
			 :close        nil
			 :indent-1     nil
			 :indent-2     nil
			 :open-toggle  nil
			 :close-toggle nil))

(defun ipe-line--set (n &optional pname _value)
  "Set the `ipe' position properties for the `N'th PAIR.

This function acts as the `movement callback' for the `'line'
movement.

- N is the current entry within `ipe--pair-pos-list' for which the
  property is being set.
- PNAME is the name of the property.
- VALUE is the value to which the property is set.

This function will update the position properties for the :open,
:infix, and :close strings that are used to display the `N'th PAIR
when `ipe-movement' eq `'line'.

The :open, :infix, and :close properties are updated based upon the
current values of the following positional properties:

  * :indent-1 - A number which specifies the amount of whitespace to
    add before :infix.
  * :indent-2 - A number which specifies the amount of whitespace to
    add after :infix.
  * :open-toggle - If eq t, adds a newline after the :open string.
  * :close-toggle - If eq t, adds a newline before the :close string."

  (let ((ipe--pos-property-set-callback nil))

    (cond ((or (equal pname :open)
	       (equal pname :open-toggle))
	   (ipe-line--open-set n))

	  ((equal pname :infix)
	   (ipe-line--infix-set n))

	  ((or (equal pname :close)
	       (equal pname :close-toggle))
	   (ipe-line--close-set n))

	  ((equal pname :indent-1)
	   (ipe-line--open-set n)
	   (ipe-line--infix-set n)
	   (ipe-line--close-set n))

	  ((equal pname :indent-2)
	   (ipe-line--open-set  n)
	   (ipe-line--infix-set n))

	  ((equal pname :inserted-p)
	   (ipe-line--inserted n))

	  ((not pname)
	   (ipe-line--unset n)))))

;; -------------------------------------------------------------------
;;;; Line Movement Functions.
;; -------------------------------------------------------------------

(defun ipe-line--open-beg (n)
  "Start action for `ipe' OPEN string in `'line' movement.

The action performed by a `'beg' key for the OPEN string of an
Insert Pair Edit (ipe) PAIR when `ipe--movement' eq `'line' is
to add a newline to the end of the OPEN string.

- N is the position of the PAIR in the `ipe--pair-pos-list'."

  (when (not (ipe--pos-property n :open-toggle))
    (ipe--pos-property-set n :open-toggle t)))

(defun ipe-line--open-forward (n units)
  "Forward action for `ipe' OPEN string in `'line' movement.

The action performed by a `'forward' key for the OPEN string of an
Insert Pair Edit (ipe) PAIR when `ipe--movement' eq `'line' is
to remove whitespace before the :infix string.

- N is the position of the PAIR in the `ipe--pair-pos-list'.
- UNITS are the number of units to move."

  (let ((indent-1 (ipe--pos-property n :indent-1)))
    (if indent-1
	(ipe--pos-property-set n
			       :indent-1
			       (if (> (- indent-1 units) 0)
				   (- indent-1 units)
				 0)))))

(defun ipe-line--open-backward (n units)
  "Backward action for `ipe' OPEN string in `'line' movement.

The action performed by a `'backward' key for the OPEN string of an
Insert Pair Edit (ipe) PAIR when `ipe--movement' eq `'line' is to add
whitespace before the :infix string.

- N is the position of the PAIR in the `ipe--pair-pos-list'.
- UNITS are the number of units to move."

  (let ((indent-1 (ipe--pos-property n :indent-1)))
    (ipe--pos-property-set n
			   :indent-1
			   (if indent-1
			       (+ indent-1 units)
			     units))))

(defun ipe-line--open-end (n)
  "Backward action for `ipe' OPEN string in `'line' movement.

The action performed by a `'end' key for the OPEN string of an
Insert Pair Edit (ipe) PAIR when `ipe--movement' eq `'line' is
to remove a newline from the start of the CLOSE string.

- N is the position of the PAIR in the `ipe--pair-pos-list'."

  (when (ipe--pos-property n :close-toggle)
    (ipe--pos-property-set n :close-toggle nil)))

(defun ipe-line--close-beg (n)
  "Start action for `ipe' CLOSE string in `'line' movement.

The action performed by a `'beg' key for the CLOSE string of an Insert
Pair Edit (ipe) PAIR when `ipe--movement' eq `'line' is to remove a
newline from the end of the OPEN string.

- N is the position of the PAIR in the `ipe--pair-pos-list'."

  (when (ipe--pos-property n :open-toggle)
    (ipe--pos-property-set n :open-toggle nil)))

(defun ipe-line--close-forward (n units)
  "Forward action for `ipe' CLOSE string in `'line' movement.

The action performed by a `'forward' key for the CLOSE string of an
Insert Pair Edit (ipe) PAIR when `ipe--movement' eq `'line' is to add
whitespace after the :infix string.

- N is the position of the PAIR in the `ipe--pair-pos-list'.
- UNITS are the number of units to move."

  (let ((indent-2 (ipe--pos-property n :indent-2)))
    (ipe--pos-property-set n
			   :indent-2
			   (if indent-2
			       (+ indent-2 units)
			     units))))

(defun ipe-line--close-backward (n units)
  "Backward action for `ipe' CLOSE string for `'line' movement.

The action performed by a `'backward' key for the CLOSE string of an
Insert Pair Edit (ipe) PAIR when `ipe--movement' eq `'line' is
to remove whitespace after the :infix string.

- N is the position of the PAIR in the `ipe--pair-pos-list'.
- UNITS are the number of units to move."

  (let ((indent-2 (ipe--pos-property n :indent-2)))
    (if indent-2
	(ipe--pos-property-set n
			       :indent-2
			       (if (> (- indent-2 units) 0)
				   (- indent-2 units)
				 0)))))

(defun ipe-line--close-end (n)
  "Backward action for `ipe' CLOSE string for `'line' movement.

The action performed by a `'end' key for the CLOSE string of an
Insert Pair Edit (ipe) PAIR when `ipe--movement' eq `'line' is
to add a newline to the start of the CLOSE string.

- N is the position of the PAIR in the `ipe--pair-pos-list'."

  (when (not (ipe--pos-property n :close-toggle))
    (ipe--pos-property-set n :close-toggle t)))

;; -------------------------------------------------------------------
;;;; move-by Function.
;; -------------------------------------------------------------------

(defun ipe-line--move-by (_defn n side action pos _other units
				&optional push)
  "Calculate movement `by line' for Insert Pair Edit.

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
	(unless (and (= units 0) (looking-at "^"))
	  (beginning-of-line (- 1 units))))

       ((equal action 'beg)
	(ipe-line--open-beg n))

       ((equal action 'up)
	(beginning-of-line (- 1 units)))

       ((equal action 'backward)
	(ipe-line--open-backward n units))

       ((equal action 'forward)
	(ipe-line--open-forward n units))

       ((equal action 'down)
	(beginning-of-line (1+ units))

	(when (eobp)
	  (beginning-of-line)))

       ((equal action 'end)
	(ipe-line--open-end n))

       ((equal action 'reset)
	(beginning-of-line)
	(ipe-line--unset n))))

     ((equal side 'close)
      (cond
       ((equal action 'init)
	(end-of-line (if (> units 0) units 1)))

       ((equal action 'beg)
	(ipe-line--close-beg n))

       ((equal action 'up)
	(end-of-line (- 1 units))
	(when (bobp) (end-of-line)))

       ((equal action 'backward)
	(ipe-line--close-backward n units))

       ((equal action 'forward)
	(ipe-line--close-forward n units))

       ((equal action 'down)
	(end-of-line (1+ units)))

       ((equal action 'end)
	(ipe-line--close-end n))

       ((equal action 'reset)
	(end-of-line)
	(ipe-line--unset n)))))

    (point)))

(ipe-move-by-install 'line
		     'ipe-line--move-by
		     'ipe-line--set
		     "lines")

(provide 'ipe-line)

;;; ipe-line.el ends here
