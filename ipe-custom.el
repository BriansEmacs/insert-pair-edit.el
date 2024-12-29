;;; ipe-custom.el --- Insert Pair Edit - customize widgets + functions -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Brian Kavanagh

;; Author: Brian Kavanagh <brians.emacs@gmail.com>
;; Maintainer: Brian Kavanagh <brians.emacs@gmail.com>
;; Created: 18 July, 2020
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
;; Widgets and `helper functions' for the customization of the
;; `ipe' package.
;;
;; Most of the helper functions within this package are used to
;; convert between the representations of the 'Insert Pair Edit'
;; variables used to 'nicely' format the `customize' widgets, and, the
;; internal format required by the `ipe-pairs' / `ipe-mode-pairs'
;; variables.  They are expected to be passed as :get and :set options
;; to `defcustom' when defining new variables via 'customize' widgets
;; defined below:
;;
;;   `ipe-custom-pair'
;;   `ipe-custom-pair-list'
;;   `ipe-custom-mode-pairs'

;; -------------------------------------------------------------------
;;; Code:

(require 'wid-edit)

;; -------------------------------------------------------------------
;;;; :get predicates.
;; -------------------------------------------------------------------

(defun ipe-custom--basic-defn-get-p (defn)
  "Non-nil if DEFN is an `ipe' Basic PAIR :get form.

This Insert Pair Edit predicate is used by the customization
`widget' :get functions to determine whether or not a given DEFN
matches an `ipe' Basic PAIR definition.

A Basic PAIR definition will be of the form:

  (MNEMONIC OPEN CLOSE)

Where:

- MNEMONIC: is a string input into the minibuffer by the user after
  calling `ipe-insert-pair-edit' to indicate which OPEN and CLOSE
  strings are to be inserted.
- OPEN: is a string placed at the beginning of a lexical unit, and
  then moved by the Insert Pair Edit commands.
- CLOSE: is a string placed at the end of a lexical unit, and then
  moved by the Insert Pair Edit commands."

  (and (listp defn)
       (= (length defn) 3)
       (stringp (nth 0 defn))
       (stringp (nth 1 defn))
       (stringp (nth 2 defn))))

(defun ipe-custom--plist-defn-get-p (defn properties)
  "Non-nil if DEFN is an `ipe' PAIR :get form with a PLIST.

This Insert Pair Edit predicate is used by the customization
`widget' :get functions to determine whether or not a given DEFN
matches an `ipe' PLIST PAIR definition.

An PLIST PAIR definition will be of the form:

  (MNEMONIC OPEN CLOSE (PLIST))

Where:

- MNEMONIC: is a string input into the minibuffer by the user after
  calling `ipe-insert-pair-edit' to indicate which OPEN and CLOSE
  strings are to be inserted.
- OPEN: is a string placed at the beginning of a lexical unit, and
  then moved by the Insert Pair Edit commands.
- CLOSE: is a string placed at the end of a lexical unit, and then
  moved by the Insert Pair Edit commands.
- PLIST: is an property list defining extra properties used by the
  `ipe-insert-pair-edit' movement functions to determine the movement
  of the OPEN and CLOSE strings in `ipe-edit-mode'.

The PROPERTIES parameter passed to this function is a list of the
properties which must be present within the PLIST for this predicate
to return non-nil.  This allows the calling function to be more
specific about the set of extra properties required by a particular
type of Advanced PAIR definition."

  (and (listp defn)
       (= (length defn) 4)
       (stringp (nth 0 defn))
       (stringp (nth 1 defn))
       (stringp (nth 2 defn))
       (listp   (nth 3 defn))
       (not (member nil
		    (mapcar (lambda (x) (plist-get (nth 3 defn) x))
			    properties)))))

(defun ipe-custom--intermediate-defn-get-p (defn)
  "Non-nil if DEFN is an `ipe' Intermediate :get form.

\(see: `ipe-custom--plist-defn-get-p')

This will check that PLIST contains a `'char', `'word', `'line' or
`'list' :movement property, but none of the `advanced' properties."
  (and (ipe-custom--plist-defn-get-p defn '(:movement))
       (member t (mapcar (lambda (_x)
			   (ipe-custom--plist-defn-get-p defn '(:movement)))
			 '(char line word list)))
       (not (member t (mapcar (lambda (x)
				(when (plist-get (nth 3 defn) x)
				  t))
			      '(:move-point
				:indent-function
				;; TODO: Add regexp update matching.
				;; :open-regexp
				;; :close-regexp
				:menu))))))

(defun ipe-custom--advanced-defn-get-p (defn)
  "Non-nil if DEFN is an `ipe' Advanced :get form.

\(see: `ipe-custom--plist-defn-get-p')

This will check that PLIST contains a `'char', `'word', `'line' or
`'list' :movement property."
  (and (ipe-custom--plist-defn-get-p defn '(:movement))
       (member (plist-get (nth 3 defn) :movement)
	       '(char word line list))
       t))

(defun ipe-custom--custom-movement-defn-get-p (defn)
  "Non-nil if DEFN is an `ipe' Custom Movement :get form.

\(see: `ipe-custom--plist-defn-get-p')

This will check that PLIST contains a :movement property that is not
covered by `ipe-custom--advanced-defn-get-p'.  This is the most
generic of the `ipe-custom--*-get-p' predicates, and should be called
last."

  (and (ipe-custom--plist-defn-get-p defn '(:movement))
       (not (member (plist-get (nth 3 defn) :movement)
		    '(char word line list)))
       t))

(defun ipe-custom--pair-list-get-p (defn)
  "Non-nil if DEFN is an `ipe' PAIR Definition List :get form.

This Insert Pair Edit predicate is used by the customization
`widget' :get functions to determine whether or not a given DEFN
matches an `ipe' PAIR Definition List.

An `ipe' PAIR Definition List will be a list containing elements of
the form:

  (MNEMONIC OPEN CLOSE)
or
  (MNEMONIC OPEN CLOSE (PLIST))

Where:

- MNEMONIC: is a string input into the minibuffer by the user after
  calling `ipe-insert-pair-edit' to indicate which OPEN and CLOSE
  strings are to be inserted.
- OPEN: is a string placed at the beginning of a lexical unit, and
  then moved by the Insert Pair Edit commands.
- CLOSE: is a string placed at the end of a lexical unit, and then
  moved by the Insert Pair Edit commands.
- PLIST: is a property list defining extra properties used by the
  `ipe-insert-pair-edit' movement functions to determine the movement
  / display of the OPEN and CLOSE strings in `ipe-edit-mode'."

  (and (listp defn)
       (not (member
	     nil
	     (mapcar
	      (lambda (x)
		(or (ipe-custom--basic-defn-get-p x)
		    (ipe-custom--intermediate-defn-get-p x)
		    (ipe-custom--advanced-defn-get-p x)
		    (ipe-custom--custom-movement-defn-get-p x)))
	      defn)))))

(defun ipe-custom--mode-pair-get-p (defn)
  "Non-nil if DEFN is an `ipe' Mode-Specific PAIR Definition List \
:get form.

This Insert Pair Edit predicate is used by the customization
`widget' :get functions to determine whether or not a given DEFN an
matches an `ipe' Mode-Specific PAIR Definition List.

An `ipe' Mode-Specific PAIR Definition List will be a list
containing elements of the form:

  (MAJOR-MODE SYMBOL)
or
  (MAJOR-MODE PAIR-DEFINITION-LIST)

Where:

- MAJOR:-MODE is a symbol defining the major mode for which the
  PAIR-DEFINITION-LIST will be defined.
- SYMBOL: is a symbol for a variable which will match
  `ipe-custom--pair-list-get-p'.
- PAIR-DEFINITION-LIST: is a form which will match
  `ipe-custom--pair-list-get-p'"

  (and (listp defn)
       (= (length defn) 2)
       (symbolp (nth 0 defn))
       (or (symbolp (nth 1 defn))
	   (ipe-custom--pair-list-get-p (nth 1 defn)))))

;; -------------------------------------------------------------------
;;;; :set predicates.
;; -------------------------------------------------------------------

(defun ipe-custom--basic-defn-set-p (defn)
  "Non-nil if DEFN is an `ipe' Basic PAIR :set form.

This Insert Pair Edit predicate is used by the customization
`widget' :set functions to determine whether or not a given DEFN
matches an `ipe' Basic PAIR definition.

A Basic PAIR definition retrieved from an `ipe-custom--basic-defn'
widget will be of the form:

  (MNEMONIC OPEN CLOSE)

Where:

- MNEMONIC: is a string input into the minibuffer by the user after
  calling `ipe-insert-pair-edit' to indicate which OPEN and CLOSE
  strings are to be inserted.
- OPEN: is a string placed at the beginning of a lexical unit, and
  then moved by the Insert Pair Edit commands.
- CLOSE: is a string placed at the end of a lexical unit, and then
  moved by the Insert Pair Edit commands."

  (and (listp defn)
       (= (length defn) 3)
       (and (stringp (nth 0 defn))
	    (not (zerop (length (nth 0 defn)))))
       (stringp (nth 1 defn))
       (stringp (nth 2 defn))))

(defun ipe-custom--intermediate-defn-set-p (defn)
  "Non-nil if DEFN is an `ipe' Intermediate :set form.

This Insert Pair Edit predicate is used by the customization
`widget' :set functions to determine whether or not a given DEFN
matches an `ipe' Intermediate PAIR definition.

A Intermediate PAIR definition retrieved from a
`ipe-custom--intermediate-defn' widget will be of the form:

  (MNEMONIC OPEN INFIX CLOSE MOVEMENT ESCAPES AUTO-INSERT)

Where:

- MNEMONIC: is a string input into the minibuffer by the user after
  calling `ipe-insert-pair-edit' to indicate which OPEN and CLOSE
  strings are to be inserted.
- OPEN: is a string placed at the beginning of a lexical unit, and
  then moved by the Insert Pair Edit commands.
- INFIX: is a string added to the front of lines between the OPEN and
  CLOSE PAIR.
- CLOSE: is a string placed at the end of a lexical unit, and then
  moved by the Insert Pair Edit commands.
- MOVEMENT: is a symbol defined within the car of one of the elements
  within `ipe-move-by-movements'.
- ESCAPES: is a list of pairs of strings.  Each pair of strings (MATCH
  REPLACE) represents an escape sequence.  Each MATCH between OPEN and
  CLOSE will be replaced by REPLACE.
- AUTO-INSERT: is a boolean which, if non-nil, will cause the
  `ipe-insert-pair-edit' function to automatically insert the PAIR
  without entering `ipe-edit-mode'."

  (and (listp defn)
       (= (length defn) 7)
       (and (stringp (nth 0 defn))
	    (not (zerop (length (nth 0 defn)))))
       (stringp (nth 1 defn))
       (stringp (nth 2 defn))
       (stringp (nth 3 defn))
       (or (not (nth 4 defn))
	   (symbolp (nth 4 defn)))
       (or (not (nth 5 defn))
	   (listp (nth 5 defn)))
       (or (not (nth 6 defn))
	   (booleanp (nth 6 defn)))))

(defun ipe-custom--advanced-defn-set-p (defn)
  "Non-nil if DEFN is an `ipe' Advanced :set form.

This Insert Pair Edit predicate is used by the customization
`widget' :set functions to determine whether or not a given DEFN
matches an `ipe' Advanced PAIR definition.

An Advanced PAIR definition retrieved from a
`ipe-custom--advanced-defn' widget will be of the form:

  (MNEMONIC OPEN INFIX CLOSE MOVEMENT ESCAPES AUTO-INSERT
   MOVE-POINT INDENT-FUNCTION OPEN-REGEXP CLOSE-REGEXP MENU)

Where:

- MNEMONIC: is a string input into the minibuffer by the user after
  calling `ipe-insert-pair-edit' to indicate which OPEN and CLOSE
  strings are to be inserted.
- OPEN: is a string placed at the beginning of a lexical unit, and
  then moved by the Insert Pair Edit commands.
- INFIX: is a string added to the front of lines between the OPEN and
  CLOSE PAIR.
- CLOSE: is a string placed at the end of a lexical unit, and then
  moved by the Insert Pair Edit commands.
- MOVEMENT: is the initial lexical unit indicating the type movements
  made by the Insert Pair Edit commands.  (Values: {`'char', `'word',
  `'line', `'list'}).  Optional.  Default: `'word'.
- ESCAPES: is a list of pairs of strings.  Each pair of strings (MATCH
  REPLACE) represents an escape sequence.  Each MATCH between OPEN and
  CLOSE will be replaced by REPLACE.
- AUTO-INSERT: is a boolean which, if non-nil, will cause the
  `ipe-insert-pair-edit' function to automatically insert the PAIR
  without entering `ipe-edit-mode'.
- MOVE-POINT: indicates where to move POINT when a new PAIR is
  inserted.  This is a PAIR specific override to the global
  `ipe-move-point-on-insert' setting, and takes the same values
  (`'resume', `'open-beg', `'open-end', `'close-beg', `'close-end')
- INDENT-FUNCTION: is either nil, or a function used to indent the
  OPEN and CLOSE strings.
- OPEN-REGEXP: is a regular expression used to match OPEN strings when
  updating.
- CLOSE-REGEXP: is a regular expression used to match CLOSE strings
  when updating.
- MENU: is a forward slash (`/') separated string of the names of the
  menus under which the PAIR is displayed within the Emacs `ipe'
  menus."
  (and (listp defn)
       (and (>= (length defn) 4)
	    (<= (length defn) 12))
       (and (stringp (nth 0 defn))
	    (not (zerop (length (nth 0 defn)))))
       (stringp (nth 1 defn))
       (or (not (nth 2 defn))
	   (stringp (nth 2 defn)))
       (stringp (nth 3 defn))
       (or (< (length defn) 4)
	   (symbolp (nth 4 defn)))
       (or (< (length defn) 5)
	   (listp (nth 5 defn)))
       (or (< (length defn) 6)
	   (booleanp (nth 6 defn)))
       (or (< (length defn) 7)
	   (not (nth 7 defn))
	   (consp (nth 7 defn))
	   (symbolp (nth 7 defn)))
       (or (< (length defn) 8)
	   (not (nth 8 defn))
	   (symbolp (nth 8 defn)))
       ;; TODO: Add regexp update matching.
       ;; (or (< (length defn) 9)
       ;;     (not (nth 9 defn))
       ;;     (stringp (nth 9 defn)))
       ;; (or (< (length defn) 10)
       ;;     (not (nth 10 defn))
       ;;     (stringp (nth 10 defn)))
       (or (< (length defn) 9)
	   (not (nth 9 defn))
	   (stringp (nth 9 defn)))))

(defun ipe-custom--custom-movement-defn-set-p (defn)
  "Non-nil if DEFN is an `ipe' Custom Movement :set form.

This Insert Pair Edit predicate is used by the customization
`widget' :set functions to determine whether or not a given DEFN
matches an `ipe' Custom Movement PAIR definition.

A Custom Movement PAIR definition retrieved from a
`ipe-custom--custom-movement-defn' widget will be of the form:

  (MNEMONIC OPEN CLOSE MOVEMENT (MOVEMENT-PROPERTIES) MENU)

Where:

- MNEMONIC: is a string input into the minibuffer by the user after
  calling `ipe-insert-pair-edit' to indicate which OPEN and CLOSE
  strings are to be inserted.
- OPEN: is a string placed at the beginning of a lexical unit, and
  then moved by the Insert Pair Edit commands.
- CLOSE: is a string placed at the end of a lexical unit, and then
  moved by the Insert Pair Edit commands.
- MOVEMENT: is the initial lexical unit indicating the type movements
  made by the Insert Pair Edit commands.  (For a custom movement
  this should be a symbol _other_ than one of the standard values:
  {`'char', `'word', `'line', `'list'})s.
- MOVEMENT-PROPERTIES: is a list of `cons' cells representing the
  abstract properties that will be passed to the custom movement
  function associated with MOVEMENT (see: `ipe-move-by-movements').
  Optional.
- MENU: is a forward slash (`/') separated string of the names of the
  menus under which the PAIR is displayed within the Emacs `ipe'
  menus."
  (and (listp defn)
       (= (length defn) 6)
       (and (stringp (nth 0 defn))
	    (not (zerop (length (nth 0 defn)))))
       (stringp (nth 1 defn))
       (stringp (nth 2 defn))
       (symbolp (nth 3 defn))
       (listp (nth 4 defn))
       (not (member nil
		    (mapcar (lambda (x) (and (consp x) (symbolp (car x))
					     (stringp (cdr x))))
			    (nth 4 defn))))
       (or (not (nth 5 defn))
	   (stringp (nth 5 defn)))))

(defun ipe-custom--pair-list-set-p (defn)
  "Non-nil if DEFN is an `ipe' PAIR Definition List :set form.

This Insert Pair Edit predicate is used by the customization
`widget' :set functions to determine which or not a given DEFN matches
an `ipe' PAIR Definition List.

A PAIR Definition List retrieved from an `ipe-custom-pair-list'
widget will be a list with elements which are either:

- an `ipe' Basic PAIR.
    (see: `ipe-custom--basic-defn-set-p')
- an `ipe' Intermediate PAIR, or
    (see: `ipe-custom--intermediate-defn-set-p')
- an `ipe' Advanced PAIR.
    (see: `ipe-custom--advanced-defn-set-p')
- an `ipe' Custom Movement PAIR.
    (see: `ipe-custom--custom-movement-defn-set-p')"
  (and (listp defn)
       (not (member
	     nil
	     (mapcar
	      (lambda (x)
		(or (ipe-custom--basic-defn-set-p x)
		    (ipe-custom--intermediate-defn-set-p x)
		    (ipe-custom--advanced-defn-set-p x)
		    (ipe-custom--custom-movement-defn-set-p x)))
	      defn)))))

(defun ipe-custom--mode-pair-set-p (defn)
  "Non-nil if DEFN is an `ipe' Mode-Specific PAIR Definition List \
:set form.

This Insert Pair Edit predicate is used by the customization
`widget' :set functions to determine which or not a given DEFN matches
an `ipe' Mode-Specific PAIR Definition List.

A Mode-Specific PAIR Definition List retrieved from an
`ipe-custom-mode-pairs' widget will be a list with elements which are
either:

   (MAJOR-MODE SYMBOL)
or
   (MAJOR-MODE CUSTOMIZE-PAIR-DEFINITION-LIST)

Where:

- MAJOR-MODE: is a symbol defining the major mode for which the
  CUSTOMIZE-PAIR-DEFINITION-LIST will be defined.
- SYMBOL: is a symbol for a variable which will match
  `ipe-custom--pair-list-set-p'.
- CUSTOMIZE-PAIR-DEFINITION-LIST: is a form which will match
  `ipe-custom--pair-list-set-p'"

  (and (listp defn)
       (= (length defn) 2)
       (symbolp (nth 0 defn))
       (or (symbolp (nth 1 defn))
	   (ipe-custom--pair-list-set-p (nth 1 defn)))))

;; -------------------------------------------------------------------
;;;; Internal :get functions.
;; -------------------------------------------------------------------

(defun ipe-custom--plist-get (defn symbol)
  "Return the PLIST property SYMBOL from `ipe' PAIR definition DEFN.

This will return a default of the empty string (\"\") if SYMBOL is not
defined within DEFN."

  (let ((value (plist-get (nth 3 defn) symbol)))
    (or value "")))

(defun ipe-custom--intermediate-defn-get (defn)
  "`customize' :get function for `ipe' Intermediate PAIR.

DEFN is the external representation of an `ipe' Intermediate PAIR.

DEFN:    (MNEMONIC OPEN CLOSE (PLIST)).
Returns: (MNEMONIC OPEN INFIX CLOSE MOVEMENT ESCAPES AUTO-INSERT)"
  (list (nth 0 defn)
	(nth 1 defn)
	(ipe-custom--plist-get defn :infix)
	(nth 2 defn)
	(plist-get (nth 3 defn) :movement)
	(plist-get (nth 3 defn) :escapes)
	(plist-get (nth 3 defn) :auto-insert)))

(defun ipe-custom--advanced-defn-get (defn)
  "`customize' :get function for `ipe' Advanced PAIR.

DEFN is the external representation of an `ipe' Advanced PAIR.

DEFN:    (MNEMONIC OPEN CLOSE (PLIST)).
Returns: (MNEMONIC OPEN INFIX CLOSE MOVEMENT ESCAPES AUTO-INSERT
	  MOVE-POINT INDENT-FUNCTION OPEN-REGEXP CLOSE-REGEXP MENU)"

  (list (nth 0 defn)
	(nth 1 defn)
	(ipe-custom--plist-get defn :infix)
	(nth 2 defn)
	(plist-get (nth 3 defn) :movement)
	(plist-get (nth 3 defn) :escapes)
	(plist-get (nth 3 defn) :auto-insert)
	(let ((move-point (plist-get (nth 3 defn) :move-point)))
	  (if (numberp move-point)
	      (if (> move-point 0)
		  (cons 'open move-point)
		(cons 'close (- move-point)))
	    move-point))
	(plist-get (nth 3 defn) :indent-function)
	;; TODO: Add regexp update matching.
	;; (ipe-custom--plist-get defn :open-regexp)
	;; (ipe-custom--plist-get defn :close-regexp)
	(ipe-custom--plist-get defn :menu)))

(defun ipe-custom--custom-movement-defn-get (defn)
  "`customize' :get function for `ipe' Custom Movement PAIR.

DEFN is the external representation of an `ipe' Custom Movement
PAIR.

DEFN:    (MNEMONIC OPEN CLOSE (PLIST)).
Returns: (MNEMONIC OPEN CLOSE MOVEMENT MOVEMENT-PROPERTIES)."
  (list (nth 0 defn)
	(nth 1 defn)
	(nth 2 defn)
	(plist-get (nth 3 defn) :movement)
	(let ((plist (nth 3 defn))
	      (list))
	  (while plist
	    (when (not (equal (car plist) :movement))
	      (push (cons (car plist) (cadr plist)) list))
	    (setq plist (cddr plist)))
	  list)
	(ipe-custom--plist-get defn :menu)))

(defun ipe-custom--pair-get (defn)
  "`customize' :get function for `ipe' PAIR Definition.

DEFN is the external representation of an `ipe' PAIR Definition

DEFN:    (PAIR-DEFINITION)
Returns: (CUSTOMIZE-PAIR-DEFINITION)"
  (when (listp defn)
    (cond ((ipe-custom--basic-defn-get-p defn)
	   defn)
	  ((ipe-custom--intermediate-defn-get-p defn)
	   (ipe-custom--intermediate-defn-get defn))
	  ((ipe-custom--advanced-defn-get-p defn)
	   (ipe-custom--advanced-defn-get defn))
	  ((ipe-custom--custom-movement-defn-get-p defn)
	   (ipe-custom--custom-movement-defn-get defn)))))

(defun ipe-custom--pair-list-get (defn)
  "`customize' :get function for `ipe' PAIR Definition List.

DEFN is the external representation of an `ipe' PAIR Definition
List.

DEFN:    (PAIR-DEFINITION-LIST)
Returns: (CUSTOMIZE-PAIR-DEFINITION-LIST)"
  (when (listp defn)
    (mapcar (lambda (d) (ipe-custom--pair-get d)) defn)))

(defun ipe-custom--mode-pair-get (defn)
  "`customize' :get function for `ipe' Mode-Specific PAIR \
Definition List.

DEFN is the external representation of an `ipe' Mode-Specific PAIR
Definition List.

DEFN:    (MAJOR-MODE (SYMBOL-OR-PAIR-DEFINITION-LIST)
Returns: (MAJOR-MODE (SYMBOL-OR-CUSTOMIZE-PAIR-LIST)"
  (list (nth 0 defn)
	(if (symbolp (nth 1 defn))
	    (nth 1 defn)
	  (ipe-custom--pair-list-get (nth 1 defn)))))

;; -------------------------------------------------------------------
;;;; Internal :set functions.
;; -------------------------------------------------------------------

(defun ipe-custom--plist-put (plist symbol defn i)
  "Conditionally set the PLIST property SYMBOL from definition DEFN.

DEFN is assumed to be a LIST containing properties set by a custom
widget.  If the `I'th member of DEFN is non-nil, this will set the
SYMBOL property within PLIST to its value."

  (let ((value (nth i defn)))
    (when value (plist-put plist symbol value))))

(defun ipe-custom--intermediate-defn-set (defn)
  "`customize' :set function for `ipe' Intermediate PAIR.

DEFN is the internal `customize' widget representation of an `ipe'
Intermediate PAIR.

DEFN: (MNEMONIC OPEN INFIX CLOSE MOVEMENT ESCAPES AUTO-INSERT)
Returns: (MNEMONIC OPEN CLOSE (PLIST))"

  (list (nth 0 defn)
	(nth 1 defn)
	(nth 3 defn)
	(let ((plist (list :movement (nth 4 defn))))
	  (ipe-custom--plist-put plist :infix       defn 2)
	  (ipe-custom--plist-put plist :escapes     defn 5)
	  (ipe-custom--plist-put plist :auto-insert defn 6)
	  plist)))

(defun ipe-custom--advanced-defn-set (defn)
  "`customize' :set function for `ipe' Advanced PAIR.

DEFN is the internal `customize' widget representation of an `ipe'
Advanced PAIR.

DEFN: (MNEMONIC OPEN INFIX CLOSE MOVEMENT ESCAPES AUTO-INSERT
       MOVE-POINT INDENT-FUNCTION OPEN-REGEXP CLOSE-REGEXP MENU)
Returns: (MNEMONIC OPEN CLOSE (PLIST))"

  (list (nth 0 defn)
	(nth 1 defn)
	(nth 3 defn)
	(let ((plist (list :movement (nth 4 defn))))
	  (ipe-custom--plist-put plist :infix           defn 2)
	  (ipe-custom--plist-put plist :escapes         defn 5)
	  (ipe-custom--plist-put plist :auto-insert     defn 6)
	  (let ((move-point (nth 7 defn)))
	    (plist-put plist
		       :move-point
		       (if (consp move-point)
			   (if (eq (car move-point) 'open)
			       (cdr move-point)
			     (- (cdr move-point)))
			 move-point)))
	  (ipe-custom--plist-put plist :indent-function defn 8)
	  ;; TODO: Add regexp update matching.
	  ;; (ipe-custom--plist-put plist :open-regexp     defn 9)
	  ;; (ipe-custom--plist-put plist :close-regexp    defn 10)
	  (ipe-custom--plist-put plist :menu            defn 9)
	  plist)))

(defun ipe-custom--custom-movement-defn-set (defn)
  "`customize' :set function for `ipe' Custom Movement PAIR.

DEFN is the internal `customize' widget representation of an `ipe'
Custom Movement PAIR.

DEFN: (MNEMONIC OPEN CLOSE MOVEMENT MOVEMENT-PROPERTIES MENU).
Returns: (MNEMONIC OPEN CLOSE (PLIST))"

  (list (nth 0 defn)
	(nth 1 defn)
	(nth 2 defn)
	(let ((plist (list :movement (nth 3 defn))))
	  (mapc (lambda (x)
		  (when (and x (cdr x))
		    (plist-put plist (car x) (cdr x))))
		(nth 4 defn))
	  (ipe-custom--plist-put plist :menu defn 5)
	  plist)))

(defun ipe-custom--pair-set (defn)
  "`customize' :set function for `ipe' PAIR Definition.

DEFN is the internal `customize' widget representation of an `ipe'
PAIR Definition.

DEFN:    (CUSTOMIZE-PAIR-DEFINITION).
Returns: (PAIR-DEFINITION)"

  (when (listp defn)
    (cond ((ipe-custom--basic-defn-set-p defn)
	   defn)
	  ((ipe-custom--intermediate-defn-set-p defn)
	   (ipe-custom--intermediate-defn-set defn))
	  ((ipe-custom--advanced-defn-set-p defn)
	   (ipe-custom--advanced-defn-set defn))
	  ((ipe-custom--custom-movement-defn-set-p defn)
	   (ipe-custom--custom-movement-defn-set defn)))))

(defun ipe-custom--pair-list-set (defn)
  "`customize' :set function for `ipe' PAIR Definition List.

DEFN is the internal `customize' widget representation of an `ipe'
PAIR Definition List.

DEFN:    (CUSTOMIZE-PAIR-DEFINITION-LIST).
Returns: (PAIR-DEFINITION-LIST)"

  (when (listp defn)
    (delq nil
	  (mapcar (lambda (d) (ipe-custom--pair-set d)) defn))))

(defun ipe-custom--mode-pair-set (defn)
  "`customize' :set function for `ipe-custom-mode-pairs' entry.

DEFN the internal representation of an `ipe' Mode-Specific PAIR
Definition List.

DEFN:    (MAJOR-MODE (SYMBOL-OR-PAIR-DEFINITION-LIST)
Returns: (MAJOR-MODE (SYMBOL-OR-CUSTOMIZE-PAIR-LIST)"

  (list (nth 0 defn)
	(if (symbolp (nth 1 defn))
	    (nth 1 defn)
	  (ipe-custom--pair-list-set (nth 1 defn)))))

(defun ipe-custom--mode-pairs-set (defn)
  "`customize' :set function for `ipe-custom-mode-pairs'.

DEFN the internal representation of a list of `ipe' Mode-Specific
PAIR Definition List used by the `customize' widgets.

This function returns an external Mode-Specific PAIR Definition List
suitable a value for the `ipe-mode-pairs' variable.

\(see: `ipe-custom-mode-pairs')"
  (when (listp defn)
    (mapcar
     (lambda (e)
       (when (ipe-custom--mode-pair-set-p e)
	 (ipe-custom--mode-pair-set e)))
     defn)))

;; -------------------------------------------------------------------
;;;; Public :get / :set functions.
;; -------------------------------------------------------------------

;; Public :get functions
(defun ipe-custom-pair-get (sym)
  "`customize' :get function for `ipe-custom-pair' widget.

SYM is the name of a symbol that defines a Insert Pair Edit
PAIRS (see: `ipe-pairs').

This function returns a value suitable to be set as the model for an
`ipe-custom-pair' widget."

  (when (listp (eval sym))
    (let ((defn (eval sym)))
      (cond ((ipe-custom--basic-defn-get-p defn)
	     defn)
	    ((ipe-custom--intermediate-defn-get-p defn)
	     (ipe-custom--intermediate-defn-get defn))
	    ((ipe-custom--advanced-defn-get-p defn)
	     (ipe-custom--advanced-defn-get defn))
	    ((ipe-custom--custom-movement-defn-get-p defn)
	     (ipe-custom--custom-movement-defn-get defn))))))

(defun ipe-custom-pair-list-get (sym)
  "`customize' :get function for `ipe-custom-pair-list' widget.

SYM is the name of a symbol that defines a list of Insert Pair Edit
PAIRS (see: `ipe-pairs').

This function returns a value suitable to be set as the model for an
`ipe-custom-pair-list' widget."

  (when (listp (eval sym))
    (let ((defn (eval sym)))
      (mapcar (lambda (d) (ipe-custom--pair-get d)) defn))))

(defun ipe-custom-mode-pairs-get (sym)
  "`customize' :get function for the `ipe-custom-mode-pairs' widget.

SYM is the name of a symbol that defines a list of Insert Pair Edit
Mode-Specific PAIRS (see: `ipe-mode-pairs').

This function returns a value suitable to be set as the model for an
`ipe-custom-mode-pairs' widget."

  (when (listp (eval sym))
    (mapcar
     (lambda (defn)
       (when (ipe-custom--mode-pair-get-p defn)
	 (ipe-custom--mode-pair-get defn)))
     (eval sym))))

;; Public :set functions
(defun ipe-custom-pair-set (sym defn)
  "`customize' :set function for `ipe-custom-pair' widget.

SYM is the symbol being set.
DEFN the value returned by the `ipe-custom-pair' widget.

This function returns a value suitable to be set as an element within
the `ipe-pairs' variable."

  (set sym (ipe-custom--pair-set defn)))

(defun ipe-custom-pair-list-set (sym defn)
  "`customize' :set function for `ipe-custom-pair-list' widget.

SYM is the symbol being set.
DEFN the value returned by the `ipe-custom-pair-list' widget.

This function returns a value suitable to be set as an `ipe-pairs'
variable."

  (set sym (ipe-custom--pair-list-set defn)))

(defun ipe-custom-mode-pairs-set (sym defn)
  "`customize' :set function for `ipe-custom-mode-pairs' widget.

SYM is the symbol being set.
DEFN the value returned by the `ipe-custom-mode-pairs' widget.

This function returns a value suitable to be set as an
`ipe-mode-pairs' variable."

  (set sym (ipe-custom--mode-pairs-set defn)))

(declare-function ipe-mouse--install   "ipe-mouse" ())
(declare-function ipe-mouse--uninstall "ipe-mouse" ())

(defun ipe-custom--mouse-set (sym defn)
  "`customize' :set function for `ipe-mouse-support-p'.

SYM is the symbol being set.
DEFN the value returned by the `ipe-mouse-support-p' widget.

This function calls either `ipe-mouse--install' or
`ipe-mouse--uninstall' depending upon the value set."

  (set sym defn)
  (if defn
      (when (functionp #'ipe-mouse--install)
	(ipe-mouse--install))
    (when (functionp #'ipe-mouse--uninstall)
      (ipe-mouse--uninstall))))

(declare-function ipe-menu--install   "ipe-menu" ())
(declare-function ipe-menu--uninstall "ipe-menu" ())

(defun ipe-custom--menu-set (sym defn)
  "`customize' :set function for `ipe-menu-support-p'.

SYM is the symbol being set.
DEFN the value returned by the `ipe-menu-support-p' widget.

This function calls either `ipe-menu--install' or
`ipe-menu--uninstall' depending upon the value set."

  (set sym defn)
  (if defn
      (when (functionp #'ipe-menu--install)
	(ipe-menu--install))
    (when (functionp #'ipe-menu--uninstall)
      (ipe-menu--uninstall))))

(defun ipe-custom--pair-sort-set (sym defn)
  "`customize' :set function for `ipe-pair-sort'.

SYM is the symbol being set.
DEFN the value returned by the `ipe-pair-sort' widget.

This function flushes the menu cache and re-installs
the menus."

  (set sym defn)
  (when (and ipe-menu-support-p
	     (functionp #'ipe-menu--install))
    (ipe-menu--install)))

(defun ipe-custom--delete-highlight-wait-set (sym defn)
  "`customize' :set function for `ipe-delete-highlight-wait'.

SYM is the symbol being set.
DEFN the value returned by the `ipe-delete-highlight-wait' widget.

This function ensures that the wait is a positive float."

  (if (or (not (numberp defn))
	  (< defn 0))
      (set sym 0.0)
    (set sym (float defn))))

;; -------------------------------------------------------------------
;;;; Key Binding Getters / Setters
;; -------------------------------------------------------------------

;; Helper Predicates

(defun ipe-custom--ipe-key-ignore-p (keyseq)
  "Return t if KEYSEQ should be ignored."

  (and (vectorp keyseq)
       (> (length keyseq) 0)
       (eq (aref keyseq 0) 'ignore)))

(defun ipe-custom--ipe-key-ascii-p (keyseq)
  "Return t if KEYSEQ begins with an ASCII character."

  (and (vectorp keyseq)
       (> (length keyseq) 0)
       (numberp (aref keyseq 0))
       (>= (aref keyseq 0) 32)
       (<= (aref keyseq 0) 128)))

;; Getters

(defun ipe-custom--ipe-key-get (fn)
  "`customize' :get helper function for the`ipe-*-key-binding' widget.

FN is the name of the `ipe-edit-insert-pair*' function for which to
search for a `key-binding'.

This function will return either:

- A vector representing the current key sequence to which the given FN
  is bound, or;
- A list of vectors representing the current set of global key
  sequences to which the given FN is bound."

  (let* ((where   (where-is-internal fn))
	 (keyseqs (if (and (listp where) (> (length where) 0))
		      where
		    (list [ignore])))
	 (filter  (ipe--list-filter keyseqs
				    'ipe-custom--ipe-key-ignore-p)))
    (if (= (length filter) 0)
	[ignore]
      (if (= (length filter) 1)
	  (car filter)
	filter))))

(defun ipe-custom--ipe-insert-key-get (_sym)
  "`customize' :get function for `ipe-insert-key-binding' widget.

_SYM is the name of the `ipe-insert-key-binding' symbol.

This function will return either:

- A vector representing the current key sequence to which the
  `ipe-insert-pair-edit' is bound, or;
- A list of vectors representing the current set of global key
  sequences to which `ipe-insert-pair-edit' is bound."

  (ipe-custom--ipe-key-get 'ipe-insert-pair-edit))

(defun ipe-custom--ipe-update-key-get (_sym)
  "`customize' :get function for `ipe-update-key-binding' widget.

_SYM is the name of the `ipe-update-key-binding' symbol.

This function will return either:

- A vector representing the current key sequence to which the
  `ipe-insert-pair-edit-update' is bound, or;
- A list of vectors representing the current set of global key
  sequences to which `ipe-insert-pair-edit-update' is bound."

  (ipe-custom--ipe-key-get 'ipe-insert-pair-edit-update))

(defun ipe-custom--ipe-delete-key-get (_sym)
  "`customize' :get function for `ipe-delete-key-binding' widget.

_SYM is the name of the `ipe-delete-key-binding' symbol.

This function will return either:

- A vector representing the current key sequence to which the
  `ipe-insert-pair-edit-delete' is bound, or;
- A list of vectors representing the current set of global key
  sequences to which `ipe-insert-pair-edit-delete' is bound."

  (ipe-custom--ipe-key-get 'ipe-insert-pair-edit-delete))

;; Setters

(defun ipe-custom--ipe-key-unset (fn)
  "Unbind FN from any global key-bindings."

  (let ((keyseqs (ipe-custom--ipe-key-get fn)))
    (if (listp keyseqs)
	(dolist (keyseq keyseqs)
	  (global-unset-key keyseq))
      (when (and (vectorp keyseqs) (> (length keyseqs) 0))
	(global-unset-key keyseqs)))))

(defun ipe-custom--ipe-key-set (sym defn fn)
  "`customize' :set function for `ipe-*-key-binding' widgets.

SYM is the symbol being set.
DEFN is the value returned by the `ipe-*-key-binding' widget.
FN is the function to which the key is to be bound.

This function adds global-key bindings for the given FN based upon the
contents of DEFN.

For sanity, it will not bind a key-sequence that starts with just a
\(unmodified) ASCII character, (as this may be confusing)."

  (ipe-custom--ipe-key-unset fn)

  (if defn
      (if (and defn (listp defn))
	  (let ((keyseqs nil))
	    (dolist (i defn)
	      (when (and i
			 (vectorp i)
			 (> (length i) 0)
			 (not (eq (aref i 0) 'ignore)))
		(if (ipe-custom--ipe-key-ascii-p i)
		    (message
		     "Not binding `%s to key-sequence \"%s\" (no leading modifier.)"
		     fn
		     (key-description i))
		  (global-set-key i fn)
		  (setq keyseqs (append keyseqs (list i))))))
	    (set sym keyseqs))
	(if (and defn
		 (vectorp defn)
		 (> (length defn) 0)
		 (not (eq (aref defn 0) 'ignore)))
	    (progn
	      (if (ipe-custom--ipe-key-ascii-p defn)
		  (message
		   "Not binding `%s to key-sequence \"%s\" (no leading modifier.)"
		   fn
		   (key-description defn))
		(global-set-key defn fn)
		(set sym defn)))
	  (set sym [ignore])))
    (set sym [ignore])))

(defun ipe-custom--ipe-insert-key-set (sym defn)
  "`customize' :set function for `ipe-insert-key-binding' widget.

SYM is the symbol being set.
DEFN is the value returned by the `ipe-insert-key-binding' widget.

This function adds global-key bindings for `ipe-insert-pair-edit'."

  (ipe-custom--ipe-key-set sym defn 'ipe-insert-pair-edit))

(defun ipe-custom--ipe-update-key-set (sym defn)
  "`customize' :set function for `ipe-update-key-binding' widget.

SYM is the symbol being set.
DEFN is the value returned by the `ipe-update-key-binding' widget.

This function adds global-key bindings for
`ipe-insert-pair-edit-update'."

  (ipe-custom--ipe-key-set sym defn 'ipe-insert-pair-edit-update))

(defun ipe-custom--ipe-delete-key-set (sym defn)
  "`customize' :set function for `ipe-delete-key-binding' widget.

SYM is the symbol being set.
DEFN is the value returned by the `ipe-delete-key-binding' widget.

This function adds global-key bindings for
`ipe-insert-pair-edit-delete'."

  (ipe-custom--ipe-key-set sym defn 'ipe-insert-pair-edit-delete))

;; -------------------------------------------------------------------
;;;; Widget definitions.
;; -------------------------------------------------------------------

;; Private widgets.
(define-widget 'ipe-custom--basic-pair 'lazy
  "A widget for a Basic Insert Pair Edit PAIR.

This widget is used by `customize' to enter a list:

  (MNEMONIC OPEN CLOSE)

Where:

- MNEMONIC: is a string input into the minibuffer by the user after
  calling `ipe-insert-pair-edit' to indicate which OPEN and CLOSE
  strings are to be inserted.
- OPEN: is a string placed at the beginning of a lexical unit, and
  then moved by the Insert Pair Edit commands.
- CLOSE: is a string placed at the end of a lexical unit, and then
  moved by the Insert Pair Edit commands."
  :type '(group :format "\n%v"
		(string :tag "Mnemonic" :format "%t: %v\n" :size 1)
		(string :tag "  Open  " :format "%t: %v\n" :size 10)
		(string :tag "  Close " :format "%t: %v\n" :size 10)))

(define-widget 'ipe-custom--intermediate-pair 'lazy
  "A widget for an Intermediate Insert Pair Edit PAIR.

This widget is used by `customize' to enter a list:

  (MNEMONIC OPEN INFIX CLOSE MOVEMENT ESCAPES AUTO-INSERT)

Where:

- MNEMONIC: is a string input into the minibuffer by the user after
  calling `ipe-insert-pair-edit' to indicate which OPEN and CLOSE
  strings are to be inserted.
- OPEN: is a string placed at the beginning of a lexical unit, and
  then moved by the Insert Pair Edit commands.
- INFIX: is a string added to the front of lines between the OPEN and
  CLOSE PAIR.
- CLOSE: is a string placed at the end of a lexical unit, and then
  moved by the Insert Pair Edit commands.
- MOVEMENT: is a symbol defined within the car of one of the elements
  within `ipe-move-by-movements'.
- ESCAPES: is a list of pairs of strings. Each pair of strings (MATCH
  REPLACE) represents an escape sequence.  Each MATCH between OPEN and
  CLOSE will be replaced by REPLACE.
- AUTO-INSERT: is a boolean which, if non-nil, will cause the
  `ipe-insert-pair-edit' function to automatically insert the PAIR
  without entering `ipe-edit-mode'."
  :type '(group
	  :format "\n%v"
	  (string :tag "Mnemonic" :format "%t: %v\n" :size 1)
	  (string :tag "  Open  " :format "%t: %v\n" :size 10)
	  (string :tag "  Infix " :format "%t: %v\n" :size 10)
	  (string :tag "  Close " :format "%t: %v\n" :size 10)
	  (radio :tag "    Movement"
		 :indent 8
		 :value word
		 (const :tag "By Character" char)
		 (const :tag "By Word" word)
		 (const :tag "By Line" line)
		 (const :tag "By List (S-expression)" list))
	  (repeat :tag "    Escapes"
		  :indent 8
		  (group :format "%v"
			 (string :tag "Match"   :format "%t: %v " :size 10)
			 (string :tag "Replace" :format "%t: %v\n" :size 10)))
	  (boolean :tag "    Auto Insert"
		   :on  "Automatically Insert PAIR (non-nil)"
		   :off "Enter 'Insert Pair Edit' Mode (nil)")))

(define-widget 'ipe-custom--advanced-pair 'lazy
  "A widget for an Advanced Insert Pair Edit PAIR.

This widget is used by `customize' to enter a list:

  (MNEMONIC OPEN INFIX CLOSE MOVEMENT ESCAPES AUTO-INSERT MOVE-POINT
  INDENT-FUNCTION OPEN-REGEXP CLOSE-REGEXP MENU)

Where:

- MNEMONIC: is a string input into the minibuffer by the user after
  calling `ipe-insert-pair-edit' to indicate which OPEN and CLOSE
  strings are to be inserted.
- OPEN: is a string placed at the beginning of a lexical unit, and
  then moved by the Insert Pair Edit commands.
- INFIX: is a string added to the front of lines between the OPEN and
  CLOSE PAIR.
- CLOSE: is a string placed at the end of a lexical unit, and then
  moved by the Insert Pair Edit commands.
- MOVEMENT: is a symbol defined within the car of one of the elements
  within `ipe-move-by-movements'.
- ESCAPES: is a list of pairs of strings. Each pair of strings (MATCH
  REPLACE) represents an escape sequence.  Each MATCH between OPEN and
  CLOSE will be replaced by REPLACE.
- AUTO-INSERT: is a boolean which, if non-nil, will cause the
  `ipe-insert-pair-edit' function to automatically insert the PAIR
  without entering `ipe-edit-mode'.
- MOVE-POINT: indicates where to move POINT when a new PAIR is
  inserted.  This is a PAIR specific override to the global
  `ipe-move-point-on-insert' setting, and takes the same values
  (`'resume', `'open-beg', `'open-end', `'close-beg', `'close-end')
- INDENT-FUNCTION: is either nil, or a function used to indent the
  OPEN and CLOSE strings.
- OPEN-REGEXP: is a regular expression used to match OPEN strings when
  updating.
- CLOSE-REGEXP: is a regular expression used to match CLOSE strings
  when updating.
- MENU: is a forward slash (`/') separated string of the names of the
  menus under which the PAIR is displayed within the Emacs `ipe'
  menus."
  :type '(group
	  :format "\n%v"
	  (string :tag "Mnemonic" :format "%t: %v\n" :size 1)
	  (string :tag "  Open  " :format "%t: %v\n" :size 10)
	  (string :tag "  Infix " :format "%t: %v\n" :size 10)
	  (string :tag "  Close " :format "%t: %v\n" :size 10)
	  (radio :tag "    Movement"
		 :indent 8
		 :value word
		 (const :tag "By Character" char)
		 (const :tag "By Word" word)
		 (const :tag "By Line" line)
		 (const :tag "By List (S-expression)" list))

	  (repeat :tag "    Escapes"
		  :indent 8
		  (group :format "%v"
			 (string :tag "Match"   :format "%t: %v " :size 10)
			 (string :tag "Replace" :format "%t: %v\n" :size 10)))
	  (boolean :tag "    Auto Insert"
		   :on  "Automatically Insert PAIR (non-nil)"
		   :off "Enter 'Insert Pair Edit' Mode (nil)")
	  (choice
	   :tag "    Move Point on Insert"
	   :indent 8
	   :value nil
	   (radio :tag "Predefined"
		  (const :tag "As per `ipe-move-point-on-insert`"
			 nil)
		  (const :tag "Point will remain at original position."
			 resume)
		  (const :tag "The beginning of the OPEN string."
			 open-beg)
		  (const :tag "The end of the OPEN string."
			 open-end)
		  (const :tag "The beginning of the CLOSE string."
			 close-beg)
		  (const :tag "The end of the CLOSE string."
			 close-end))
	   (cons :tag "OPEN Offset"  :format "\n        %v"
		 (const   :format "%t" :tag "OPEN Offset: " open)
		 (integer :format "%v\n" :size 10))
	   (cons :tag "CLOSE Offset" :format "\n        %v"
		 (const   :format "%t" :tag "CLOSE Offset: " close)
		 (integer :format "%v\n" :size 10)))
	  (choice :tag "    Indent Function"
		  (const :tag "None" nil)
		  (radio :tag "Standard"
			 :entry-format "      %b %v"
			 (const :tag "Relative To OPEN Line"     current)
			 (const :tag "Relative To Previous Line" previous))
		  (symbol :tag "Custom"
			  :value ""
			  :format "%t\n        Function Name: %v\n"
			  :size 20))
	  ;; TODO: Add regexp update matching.
	  ;; (regexp :tag "    Open Regexp " :format "%t: %v\n" :size 15)
	  ;; (regexp :tag "    Close Regexp" :format "%t: %v\n" :size 15)
	  (string :tag "    Menu" :format "%t: %v\n" :size 15)))

(define-widget 'ipe-custom--custom-movement-pair 'lazy
  "A widget for a Custom Movement Insert Pair Edit PAIR.

This widget is used by `customize' to enter a list:

  (MNEMONIC OPEN CLOSE MOVEMENT (PROPERTIES...) MENU)

Where:

- MNEMONIC: is a string input into the minibuffer by the user after
  calling `ipe-insert-pair-edit' to indicate which OPEN and CLOSE
  strings are to be inserted.
- OPEN: is a string placed at the beginning of a lexical unit, and
  then moved by the Insert Pair Edit commands.
- CLOSE: is a string placed at the end of a lexical unit, and then
  moved by the Insert Pair Edit commands.
- MOVEMENT: is the customized movement symbol (see:
  `ipe-move-by-movements')
- PROPERTIES: is a PLIST of properties passed to the
  `ipe-move-by-movements' MOVE-BY-FUNCTION.
- MENU: is a forward slash (`/') separated string of the names of the
  menus under which the PAIR is displayed within the Emacs `ipe'
  menus."
  :type
  '(group
    :format "\n%v"
    (string :tag "Mnemonic" :format "%t: %v\n" :size 1)
    (string :tag "  Open  " :format "%t: %v\n" :size 10)
    (string :tag "  Close " :format "%t: %v\n" :size 10)
    (symbol :tag "    Movement" :format "%t: %v\n" :size 10
	    :value word)
    (repeat :tag "    Options "
	    :format "%t:\n%v        %i\n"
	    :entry-format "      %i %d %v"
	    (cons :format "%v"
		  (symbol :tag "Property "   :value ""
			  :format "%t: %v "  :size 10)
		  (string :tag "Value "      :value ""
			  :format "%t: %v\n" :size 10)))
    (string :tag "    Menu    " :format "%t: %v\n" :size 15)))

;; Public widgets.
(define-widget 'ipe-custom-pair 'lazy
  "Widget to read definitions of a Insert Pair Edit PAIR.

The `ipe-insert-pair-edit' function uses this widget (via `customize')
to display and configure an `ipe' PAIR definitions.

A PAIR definition consists of:

  (MNEMONIC OPEN CLOSE)
or
  (MNEMONIC OPEN CLOSE (PLIST))

Where:

- MNEMONIC: is a string input into the minibuffer by the user after
  calling `ipe-insert-pair-edit' to indicate which OPEN and CLOSE
  strings are to be inserted.
- OPEN: is a string placed at the beginning of a lexical unit, and
  then moved by the Insert Pair Edit commands.
- CLOSE: is a string placed at the end of a lexical unit, and then
  moved by the Insert Pair Edit commands.
- PLIST: is an property list specifying a set of optional Advanced
  options that can be included to specify extra configuration passed
  to specialized movement functions. (see: `ipe-move-by-movements')

  Known PLIST properties are:

    :movement        MOVEMENT
    :infix           INFIX
    :escapes         ESCAPES
    :auto-insert     AUTO-INSERT
    :move-point      MOVE-POINT
    :indent-function INDENT-FUNCTION
    :open-regexp     OPEN-REGEXP
    :close-regexp    CLOSE-REGEXP
    :menu            MENU

  - MOVEMENT: The initial lexical unit indicating the type of
    movements made by the Insert Pair Edit commands. (pre-defined
    values: `'char', `'word', `'line', `'list'; default: `'word').
  - INFIX: A string added to the front of lines between the OPEN and
    CLOSE PAIR when MOVEMENT = `'line'.
  - ESCAPES: Is a list of pairs of strings. Each pair of strings
    (MATCH REPLACE) represents an escape sequence.  Each MATCH between
    OPEN and CLOSE will be replaced by REPLACE.
  - AUTO-INSERT: If non-nil, will cause the `ipe-insert-pair-edit'
    function to automatically insert the PAIR without entering
    `ipe-edit-mode'.
  - MOVE-POINT: indicates where to move POINT when a new PAIR is
    inserted.  This is a PAIR-specific override to the global
    `ipe-move-point-on-insert' setting, and takes the same values
    (`'resume', `'open-beg', `'open-end', `'close-beg', `'close-end')
  - INDENT-FUNCTION: is either nil, or a function used to indent the
    OPEN and CLOSE strings.
  - OPEN-REGEXP: is a regular expression used to match OPEN strings
    when updating.
  - CLOSE-REGEXP: is a regular expression used to match CLOSE strings
    when updating.
  - MENU: is a forward slash (`/') separated string of the names of
    the menus under which the PAIR is displayed within the Emacs `ipe'
    menus.

Pre-defined :set / :get functions for this widget:

  :get `ipe-custom-pair-get'
  :set `ipe-custom-pair-set'"
  :type
  '(choice
    :tag "Pair"
    (ipe-custom--basic-pair           :tag "Basic")
    (ipe-custom--intermediate-pair    :tag "Intermediate")
    (ipe-custom--advanced-pair        :tag "Advanced")
    (ipe-custom--custom-movement-pair :tag "Custom Movement")))

(define-widget 'ipe-custom-pair-list 'lazy
  "Widget to read definitions of a set of Insert Pair Edit PAIRs.

The `ipe-insert-pair-edit' function uses this widget (via `customize')
to display and configure a list of PAIR definitions.

A PAIR definition consists of:

  (MNEMONIC OPEN CLOSE)
or
  (MNEMONIC OPEN CLOSE (PLIST))

Where:

- MNEMONIC: is a string input into the minibuffer by the user after
  calling `ipe-insert-pair-edit' to indicate which OPEN and CLOSE
  strings are to be inserted.
- OPEN: is a string placed at the beginning of a lexical unit, and
  then moved by the Insert Pair Edit commands.
- CLOSE: is a string placed at the end of a lexical unit, and then
  moved by the Insert Pair Edit commands.
- PLIST: is an property list specifying a set of optional Advanced
  options that can be included to specify extra configuration passed
  to specialized movement functions. (see: `ipe-move-by-movements')

  Known PLIST properties are:

    :movement        MOVEMENT
    :infix           INFIX
    :escapes         ESCAPES
    :auto-insert     AUTO-INSERT
    :move-point      MOVE-POINT
    :indent-function INDENT-FUNCTION
    :open-regexp     OPEN-REGEXP
    :close-regexp    CLOSE-REGEXP
    :menu            MENU

  - MOVEMENT: The initial lexical unit indicating the type of
    movements made by the Insert Pair Edit commands. (pre-defined
    values: `'char', `'word', `'line', `'list'; default: `'word').
  - INFIX: A string added to the front of lines between the OPEN and
    CLOSE PAIR when MOVEMENT = 'line.
  - ESCAPES: Is a list of pairs of strings. Each pair of strings
    (MATCH REPLACE) represents an escape sequence.  Each MATCH between
    OPEN and CLOSE will be replaced by REPLACE.
  - AUTO-INSERT: If non-nil, will cause the `ipe-insert-pair-edit'
    function to automatically insert the PAIR without entering
    `ipe-edit-mode'.
  - MOVE-POINT: indicates where to move POINT when a new PAIR is
    inserted.  This is a PAIR-specific override to the global
    `ipe-move-point-on-insert' setting, and takes the same values
    (`'resume', `'open-beg', `'open-end', `'close-beg', `'close-end')
  - INDENT-FUNCTION: is either nil, or a function used to indent the
    OPEN and CLOSE strings.
  - OPEN-REGEXP: is a regular expression used to match OPEN strings
    when updating.
  - CLOSE-REGEXP: is a regular expression used to match CLOSE strings
    when updating.
  - MENU: is a forward slash (`/') separated string of the names of
    the menus under which the PAIR is displayed within the Emacs `ipe'
    menus.

Pre-defined :set / :get functions for this widget:

  :get `ipe-custom-pair-list-get'
  :set `ipe-custom-pair-list-set'"
  :type
  '(repeat
    :tag ""
    :format "\n%v%i\n"
    :entry-format "%i %d %v"
    (choice
     :tag "Pair"
     :offset -10
     (ipe-custom--basic-pair           :tag "Basic")
     (ipe-custom--intermediate-pair    :tag "Intermediate")
     (ipe-custom--advanced-pair        :tag "Advanced")
     (ipe-custom--custom-movement-pair :tag "Custom Movement"))))

(define-widget 'ipe-custom-mode-pairs 'lazy
  "Widget to read definitions for Mode-Specific `ipe' PAIRs.

The `ipe-insert-pair-edit' function uses this widget (via `customize')
to display and configure a list of Mode-Specific PAIR definitions.

Each entry in the list of Mode-Specific PAIR definitions is itself a
list consisting of entries of the form:

  (MAJOR-MODE PAIR-DEFINITION-LIST)

Where:

- MAJOR-MODE: is a symbol defining the major mode for which the
  PAIR-DEFINITION-LIST will be defined.
- PAIR-DEFINITION-LIST: is either: a symbol referring to a variable
  which defines a list of PAIR definitions (as per `ipe-pair'), or, an
  inline definition (as per the format of `ipe-pairs') of a list of
  PAIR definitions for the given MAJOR-MODE.

Pre-defined :set / :get functions for this widget:

  :get `ipe-custom-mode-pairs-get'
  :set `ipe-custom-mode-pairs-set'"
  :tag    ""
  :type
  '(repeat
    :tag "'Mode-Specific' PAIRs"
    (group
     (symbol :tag "Major Mode" :value fundamental-mode)
     (choice :offset 2
	     (symbol :tag "Customization Variable" :value ipe-pairs)
	     (ipe-custom-pair-list :tag "Inline Customization")))))

;; -------------------------------------------------------------------
;;;; Customize-like functions.
;; -------------------------------------------------------------------

(defvar ipe-custom--edit-pair-defn-return nil
  "Widget used by `ipe-custom--edit-pair-defn' function.

This widget will contain the final value of the edited PAIR
definition.")

(defun ipe-custom--edit-pair-defn (mode defn callback)
  "Edit the `ipe' PAIR definition DEFN.

Displays the `ipe' PAIR definition for the given DEFN using an Emacs
widget (`ipe-custom-pair') in a new buffer *ipe-edit-pair-defn*.

The user can edit the PAIR definition and either \"Save\" or
\"Cancel\".

On \"Save\", both the old PAIR definition (ORIG-DEFN) and the new PAIR
definition (DEFN) will be passed to CALLBACK:

  (CALLBACK MODE DEFN ORIG-DEFN)"

  (when (get-buffer "*ipe-edit-pair-defn*")
    (kill-buffer "*ipe-edit-pair-defn*"))

  (let ((buffer (get-buffer-create "*ipe-edit-pair-defn*"))
	(widget-push-button-prefix "")
	(widget-push-button-suffix "")
	(check-save)
	(on-save)
	(on-cancel))

    (with-current-buffer buffer

      (erase-buffer)
      (goto-char (point-min))

      (if (not mode)
	  (widget-insert "Edit 'Insert Pair Edit' (ipe) PAIR Definition\n\n")
	(widget-insert "Edit 'Insert Pair Edit' (ipe) 'Mode-Specific'\
 PAIR Definition\n\n")
	(widget-insert (concat "Mode: " (symbol-name mode) "\n\n")))

      (make-local-variable 'ipe-custom--edit-pair-defn-return)

      (setq ipe-custom--edit-pair-defn-return
	    (widget-create 'ipe-custom-pair
			   :button-face 'custom-button
			   :format "%v\n"
			   :value (ipe-custom--pair-get defn)))

      (setq check-save
	    (lambda (defn orig-defn)
	      (if (or (not defn)
		      (not (stringp (car defn)))
		      (zerop (length (car defn))))
		  (message "Cannot save PAIR with empty MNEMONIC.")
		(when (funcall callback mode defn orig-defn)
		  (when (> (count-windows) 1)
		    (delete-window))
		  (kill-buffer "*ipe-edit-pair-defn*")))))

      (setq on-save
	    (lambda (_widget _event)
	      (funcall check-save
		       (ipe-custom--pair-set
			(widget-value ipe-custom--edit-pair-defn-return))
		       defn)))

      (widget-create 'push-button
		     :tag         "Save"
		     :button-face 'custom-button
		     :action      on-save
		     :help        "Save the 'ipe' PAIR Definition.")

      (widget-insert " ")

      (setq on-cancel
	    (lambda (_widget _event)
	      (when (> (count-windows) 1)
		(delete-window))
	      (kill-buffer "*ipe-edit-pair-defn*")))

      (widget-create 'push-button
		     :tag         "Cancel"
		     :button-face 'custom-button
		     :action      on-cancel
		     :help        "Exit without saving the 'ipe' PAIR Definition.")

      (widget-insert "\n")
      (widget-setup)

      (widget-minor-mode)

      ;; Add local key bindings for Save / Cancel.
      (local-set-key (kbd "C-g")
		     (list 'lambda nil (list 'interactive)
			   (list on-cancel nil nil)))

      (local-set-key (kbd "<ESC> <ESC>")
		     (list 'lambda nil (list 'interactive)
			   (list on-cancel nil nil)))

      (local-set-key (kbd "<escape> <escape>")
		     (list 'lambda nil (list 'interactive)
			   (list on-cancel nil nil)))

      (local-set-key (kbd "C-x C-s")
		     (list 'lambda nil (list 'interactive)
			   (list on-save nil nil)))

      (goto-char (point-min))
      (widget-forward 1))

    (switch-to-buffer-other-window buffer)))

(provide 'ipe-custom)

;;; ipe-custom.el ends here
