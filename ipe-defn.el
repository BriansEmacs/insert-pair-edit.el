;;; ipe-defn.el --- Insert Pair Edit - add / delete pair definitions -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Brian Kavanagh

;; Author: Brian Kavanagh <brians.emacs@gmail.com>
;; Maintainer: Brian Kavanagh <brians.emacs@gmail.com>
;; Created: 19 December, 2020
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
;; This file defines functions to interactively add / delete 'Insert
;; Pair Edit' PAIR Definitions.
;;
;; These functions are used to modify the PAIR Definitions within the
;; `ipe-pairs' / `ipe-mode-pairs' variables.
;;
;; The `ipe-pairs' / `ipe-mode-pairs' variables are used by the
;; `ipe-insert-pair-edit' command to look-up PAIRs.
;;
;; The functions within this file offer an alternative to the
;; `custom'-izations available under the `ipe' group.  (See:
;; `ipe-custom.el')

;; -------------------------------------------------------------------
;;; Code:

(require 'ipe-compat)
(require 'ipe-core)

;; -------------------------------------------------------------------
;;;; Variables
;; -------------------------------------------------------------------

(defvar ipe-defn--indent-function-desc
  '((none     . "Do not indent lines between OPEN and CLOSE.")
    (current  . "Indent lines between OPEN and CLOSE relative to\
 indentation on OPEN line.")
    (previous . "Indent lines between OPEN and CLOSE relative to\
 indentation on line before OPEN line."))
  "Descriptions of the :indent-function `ipe' PAIR property.

This is a list of cons cells:

  (SYMBOL . DESC)

Where:

- SYMBOL is a symbol matching the value to which the :indent-function
  `ipe' PAIR property can be set (`'none', `'current', `'previous'.)
- DESC is a string representing a description of the action taken when
  the :indent-function property is set to the given SYMBOL.  (This is
  used by the `completing-read' function when annotating results.)")

(defvar ipe-defn--update-hook '()
  "This hook is called after updating `ipe-pairs'/`ipe-mode-pairs'.")

;; -------------------------------------------------------------------
;;;; Helper Functions
;; -------------------------------------------------------------------

(defun ipe-defn--string-ends-with (string postfix)
  "Return non-nil if STRING ends within POSTFIX."

  (and (>= (length string) (length postfix))
       (string-equal (substring string (- (length string)
					  (length postfix)))
		     postfix)))

(defun ipe-defn--matching-defn-p (&optional mnemonic)
  "Return a predicate used to match `ipe' PAIR Definitions.

If MNEMONIC is non-nil, return a predicate used to match an `ipe' PAIR
definition with MNEMONIC."

  (if mnemonic
      (lambda (defn)
	(string= (car defn) mnemonic))
    (lambda (defn1 defn2)
      (string= (car defn1) (car defn2)))))

(defun ipe-defn--mode-variables (&optional all)
  "Return the list of modes defined within the `ipe-mode-pairs'.

If ALL is non-nil, simply return all of symbols which end with the
postfix `*-mode'"

  (if all
      (let ((modes nil))
	;; Search for all the symbols which end with `-mode'.
	(mapatoms (lambda (x)
		    (and (symbolp x)
			 (ipe-defn--string-ends-with (symbol-name x) "-mode")
			 (setq modes (cons (symbol-name x) modes)))))
	modes)
    (mapcar (lambda (x) (symbol-name (car x))) ipe-mode-pairs)))

(defun ipe-defn--update-pair-list (pair-list-sym defn &optional save)
  "Update an `ipe' PAIR Definition List with a new PAIR Definition.

- PAIR-LIST-SYM is a symbol name containing a list of `ipe' PAIR
  Definitions.
- DEFN is a definition of an `ipe' PAIR to be added / updated /
  deleted within the list of PAIR Definitions represented by the
  PAIR-LIST-SYM.  DEFN is either a list of the form:

    (MNEMONIC OPEN CLOSE (PLIST))

  Which represents a new or updated PAIR Definition, or a list of the
  form:

    (MNEMONIC)

  Which represents the PAIR Definition to be deleted from the
  PAIR-LIST-SYM.
- SAVE, if non-nil, causes the change to be saved via
  `customize-save-customized'."

  (let ((mnemonic (car defn)))

    (set pair-list-sym
	 (if (cdr defn)
	     ;; Add / Update
	     (ipe--list-update (eval pair-list-sym)
			       defn
			       (ipe-defn--matching-defn-p))
	   ;; Delete
	   (when mnemonic
	     (ipe--list-filter (eval pair-list-sym)
			       (ipe-defn--matching-defn-p mnemonic)))))

    ;; Save the change to `customize'.
    (when save
      (customize-set-variable pair-list-sym
			      (ipe-custom--pair-list-get (eval pair-list-sym)))
      (when (or custom-file user-init-file)
	(customize-save-customized)))))

(defun ipe-defn--update-mode-pair (mode defn &optional save)
  "Update an Mode-Specific `ipe' PAIR Definition List.

- MODE is the mode in which the PAIR Definition will be updated.
- DEFN is the MODE-Specific PAIR Definition that will either be added,
  updated, or deleted from the `ipe-mode-pairs' variable.  This is a
  either a list of the form:

    (MNEMONIC OPEN CLOSE (PLIST)).

  Which represents a new or updated PAIR Definition, or a list of the
  form:

    (MNEMONIC)

  Which represents the PAIR Definition to be deleted from the
  PAIR-LIST-SYM.
- SAVE, if non-nil, causes the change to be saved via
  `customize-save-customized'.

The updated Mode-Specific PAIR Definition List will be added to
`ipe-mode-pairs'."

  (let ((mode-pair-list (cadr (assoc mode ipe-mode-pairs))))

    ;; Are there already definitions for mode?
    (if mode-pair-list

	;; Are the definitions stored in a separate variable?
	(if (symbolp mode-pair-list)
	    (ipe-defn--update-pair-list mode-pair-list defn t)

	  ;; Inline definitions.
	  (let ((updated-pair-list
		 (if (cdr defn)
		     ;; Add / Update.
		     (ipe--list-update mode-pair-list
				       defn
				       (ipe-defn--matching-defn-p))

		   ;; Delete
		   (when (car defn)
		     (ipe--list-filter mode-pair-list
				       (ipe-defn--matching-defn-p (car defn)))))))

	    (setq ipe-mode-pairs
		  (if updated-pair-list
		      (ipe--alist-update ipe-mode-pairs
					 mode
					 updated-pair-list)
		    (assq-delete-all mode ipe-mode-pairs)))))

      ;; New mode definitions
      (when (cdr defn)
	(setq ipe-mode-pairs
	      (append ipe-mode-pairs
		      (list (list mode (list defn)))))))

    ;; Save the change to `customize'.
    (when save
      (customize-set-variable 'ipe-mode-pairs
			      (ipe-custom-mode-pairs-get 'ipe-mode-pairs))
      (when (or custom-file user-init-file)
	(customize-save-customized)))))

;; -------------------------------------------------------------------
;;;; Annotation Functions
;; -------------------------------------------------------------------

(defun ipe-defn--annotate-y-or-n (y-or-n)
  "Return a suitable annotation for an `ipe' Y-OR-N answer.

This function is called as an :annotation-function within
`completing-read'."

  (concat " - " (cdr (assoc y-or-n '(("y" . "Yes") ("n" . "No"))))))

(defun ipe-defn--annotate-movement (movement)
  "Return a suitable annotation for an `ipe' MOVEMENT.

Searches the `ipe-move-by-movements' LIST for a element whose `cadddr'
is MOVEMENT, and returns a description of the MOVEMENT.

This function is called as an :annotation-function within
`completing-read'."

  (format " - (Move 'Insert Pair Edit' OPEN and CLOSE by %s.)"
	  (ipe-compat--cadddr
	   (ipe--list-member
	    ipe-move-by-movements
	    (lambda (x) (string= (ipe-compat--cadddr x) movement))))))

(defun ipe-defn--annotate-indent-function (indent-function)
  "Return a suitable annotation for an `ipe' INDENT-FUNCTION.

Searches the `ipe-defn--indent-function-desc' LIST for an element
whose `car' is a symbol with the `symbol-name' given by
INDENT-FUNCTION, and returns the `cdr' of that element.

This function is called as an :annotation-function within
`completing-read'."

  ;; Look up the given :indent-function as a symbol within the
  ;; `ipe-defn--indent-function-desc' list.
  (concat " - ("
	  (cdr
	   (ipe--list-member ipe-defn--indent-function-desc
			     (lambda (indent-function-desc)
			       (equal (intern indent-function)
				      (car indent-function-desc)))))
	  ")"))

(defun ipe-defn--annotate-move-point (move-point)
  "Return a suitable annotation for an `ipe' MOVE-POINT.

This returns the description of MOVE-POINT (as given by the `cadr' of
the element within the `ipe--move-point-on-insert-desc' LIST which
matches MOVE-POINT.)

This function is called as an :annotation-function within
`completing-read'."

  ;; Look up the given MOVE-POINT in the
  ;; `ipe--move-point-on-insert-desc' list.
  (concat " - ("
	  (cadr
	   (ipe--list-member ipe--move-point-on-insert-desc
			     (lambda (move-point-desc)
			       (equal (intern move-point)
				      (car move-point-desc)))))
	  ".)"))

;; -------------------------------------------------------------------
;;;; Minibuffer Read Functions
;; -------------------------------------------------------------------

(defun ipe-defn--y-or-n-p (prompt &optional yes-default)
  "Prompt the user for a `y' (yes) or `n' (no) response.

PROMPT is the prompt passed to `completing-read'.

On empty input, defaults to `n' if YES-DEFAULT is nil, or `y' if
YES-DEFAULT is non-nil.

Returns t, if a `y' is input, nil otherwise."

  (if (and (display-popup-menus-p)
	   last-input-event
	   (listp last-nonmenu-event)
	   (functionp #'x-popup-dialog))
      (funcall #'x-popup-dialog t (list prompt
					'("Yes" . t)
					'("No" . nil)))
    (let* ((completion-extra-properties
	    '(:annotation-function ipe-defn--annotate-y-or-n))
	   (response (downcase
		      (completing-read (concat prompt
					       "(y or n) ["
					       (if yes-default "y" "n")
					       "]? ")
				       '("y" "n")))))
      (if yes-default
	  (or (string= response "y") (string= response ""))
	(string= response "y")))))

(defun ipe-defn--read-mode (prompt &optional all)
  "Prompt the user for a `major-mode'.

Calls `completing-read' to prompt the user to enter an Emacs
`major-mode', and returns a symbol representing this mode.

- PROMPT is the string used to prompt the user to enter a mode.
- ALL if nil, only offer completions for those modes which already
  have an entry within `ipe-mode-pairs', otherwise, if non-nil, offer
  completions for all of symbols which end with the postfix `*-mode'."

  (let* ((completion-extra-properties '(:annotation-function nil))
	 (mode    (or ipe--major-mode major-mode))
	 (default (if all
		      (symbol-name mode)
		    (when (memq mode (ipe-defn--mode-variables))
		      (symbol-name mode))))
	 (initial (if default (cons default 0) nil)))

    (intern
     (completing-read
      prompt
      (ipe-defn--mode-variables all)
      nil
      t
      initial))))

(defun ipe-defn--read-mnemonic (prompt &optional mode require-match)
  "Prompt the user for a MNEMONIC.

Calls `completing-read' to PROMPT the user to enter an 'Insert Pair
Edit' MNEMONIC.  The set of MNEMONICs used as completions is read from
the `ipe-pairs' / `ipe-mode-pairs' variables.

If MODE is not specified, the set of completions is obtained form the
`ipe-pairs' variable, otherwise, if MODE is specified, the set of
completions is obtained from the entry within `ipe-mode-pairs' which
matches MODE.

If REQUIRE-MATCH is nil, the user can enter MNEMONICs which do not
already exist within `ipe-pairs' / `ipe-mode-pairs', otherwise, they
must enter a MNEMONICs which already exists within these variables."

  (let* ((ipe--major-mode (if mode mode ipe--major-mode))
	 (completion-extra-properties
	  '(:annotation-function ipe--mnemonic-annotate))
	 (mnemonics (if mode
			(mapcar #'car (ipe--mode-pairs mode))
		      (mapcar #'car (ipe--pairs t)))))
    (substring-no-properties
     (completing-read prompt mnemonics nil require-match))))

(defun ipe-defn--read-movement (prompt default)
  "Prompt the user for an `ipe' :movement.

Calls `completing-read' to PROMPT the user to enter an 'Insert Pair
Edit' :movement.  The set of :movements used as completions is read
from the `ipe-move-by-movements' variable.

DEFAULT is used to specify the initial contents of the
`completing-read' prompt."

  (let* ((completion-extra-properties
	  '(:annotation-function ipe-defn--annotate-movement))
	 (movements (mapcar #'ipe-compat--cadddr ipe-move-by-movements))
	 (movement  (ipe-compat--cadddr
		     (assoc default ipe-move-by-movements)))
	 (initial   (cons movement 0))
	 (string))

    ;; Prompt the user to enter a :movement.
    (setq string (completing-read prompt
				  movements
				  nil
				  t
				  initial))

    ;; Convert the :movement string to a symbol.
    (car (ipe--list-member
	  ipe-move-by-movements
	  (lambda (x) (string= (ipe-compat--cadddr x) string))))))

(defun ipe-defn--read-escapes (defaults)
  "Prompt the user for a list of `ipe' PAIR ESCAPE sequences.

Prompts the user (in a loop) to enter:

  \"Escape Match Text (Type RET to exit): \"
  \"Escape Replacement Text: \"

And returns a list of elements:

  (MATCH REPLACE)

Where:

- MATCH is text found between OPEN and CLOSE.
- REPLACE is the text used to replace each MATCH when inserting the
  PAIR.

DEFAULTS is a list of existing ESCAPE sequences that are used as
defaults when prompting."

  (let* ((escapes)
	 (default  (if defaults (car defaults) (list "" "")))
	 (prompt   "Escape Match Text (Type RET to exit): ")
	 (initial  (cons (car default) 0))
	 (match    (substring-no-properties
		    (read-from-minibuffer prompt initial)))
	 (replace))

    (while (> (length match) 0)
      (setq initial  (cons (cadr default) 0)
	    replace  (substring-no-properties
		      (read-from-minibuffer "Escape Replacement Text: "
					    initial))
	    escapes  (nconc escapes (list (list match replace)))
	    defaults (cdr defaults)
	    default  (if defaults (car defaults) (list "" ""))
	    initial  (cons (car default) 0)
	    match    (substring-no-properties
		      (read-from-minibuffer prompt initial))))
    escapes))

(defun ipe-defn--read-property (defn prompt orig-defn property values
				     nil-default annotate)
  "Prompt the user for an `ipe' PAIR PROPERTY.

Calls `completing-read' to prompt the user to enter the given PAIR
PROPERTY, and then sets the property within the PAIR Definition
\(DEFN).

- DEFN is the PAIR Definition in which the property is to be set (This
  has the structure of an element from `ipe-pairs'.)
- PROMPT is the string used to prompt the user to enter data.
- ORIG-DEFN is the PAIR Definition from which the default property is
  to be read (This has the structure of an element from `ipe-pairs'.)
- PROPERTY is a symbol representing one of the `ipe' PAIR Definition
  properties.  (See `ipe-pairs' for a list of valid PROPERTIES.)
- VALUES is an (optional) list of strings representing the names of
  the symbols to which the given PAIR PROPERTY can be set.  (If this
  is non-nil, the property within PAIR is assumed to be a symbol,
  otherwise, it will be read / written as a string.)
- NIL-DEFAULT is initial value to display in the minibuffer if the
  given PROPERTY is not currently set within the ORIG-DEFN.
- ANNOTATE is a `completing-read' :annotation-function that will
  annotate the values within VALUES if VALUES is non-nil."

  (let* ((default (ipe--pair-property orig-defn property))
	 (initial (cons (if default
			    (if values
				(symbol-name default)
			      default)
			  nil-default)
			0))
	 (completion-extra-properties (list :annotation-function annotate))
	 (value   (substring-no-properties
		   (completing-read prompt values nil values initial))))

    (ipe--pair-property-set defn
			    property
			    (if (or (string= value nil-default)
				    (string= value ""))
				nil
			      (if values
				  (intern value)
				value)))))

(defun ipe-defn--read-intermediate (defn)
  "Prompt the user for Intermediate `ipe' PAIR properties for DEFN.

Asks the user a series of questions (via the minibuffer) to set the
Intermediate `ipe' PAIR properties for a given PAIR Definition,
DEFN."

  ;; Read in user responses from the minibuffer.
  (let* ((mnemonic  (car defn))
	 (orig-defn (ipe--pair mnemonic))
	 (default)
	 (initial)
	 (movement)
	 (infix)
	 (escapes)
	 (auto-insert))

    ;; Read in initial movement.
    (setq movement (ipe-defn--read-movement
		    "Movement: "
		    (ipe--pair-movement-initial orig-defn)))

    (ipe--pair-property-set defn :movement movement)

    ;; Read in an infix.
    (setq default (ipe--pair-infix-string orig-defn)
	  initial (if default (cons default 0) "")
	  infix   (substring-no-properties
		   (read-from-minibuffer "INFIX: " initial)))

    (when (> (length infix) 0)
      (ipe--pair-property-set defn :infix infix))

    ;; Read in a list of ESCAPE sequences.
    (setq escapes (ipe--pair-property orig-defn :escapes))

    (when (ipe-defn--y-or-n-p "Include Escapes? " escapes)
      (setq escapes (ipe-defn--read-escapes escapes))
      (ipe--pair-property-set defn :escapes escapes))

    ;; Read in flag indicating whether to insert PAIR or enter
    ;; 'ipe-edit-mode'.
    (setq auto-insert (ipe--pair-property orig-defn :auto-insert))

    (when (ipe-defn--y-or-n-p "Auto-Insert? " auto-insert)
      (ipe--pair-property-set defn :auto-insert t)))

  defn)

(defun ipe-defn--read-advanced (defn)
  "Prompt the user for Advanced `ipe' PAIR properties for DEFN.

Asks the user a series of questions (via the minibuffer) to set the
Advanced `ipe' PAIR properties for a given PAIR Definition, DEFN."

  ;; Read in user responses from the minibuffer.
  (let* ((mnemonic  (car defn))
	 (orig-defn (ipe--pair mnemonic))
	 (default)
	 (initial)
	 (string)
	 (indent-function))

    ;; Read in position to which to move POINT on insert.
    (ipe-defn--read-property
     defn
     "Move Point on Insert: "
     orig-defn
     :move-point
     (mapcar (lambda (x) (symbol-name (car x)))
	     ipe--move-point-on-insert-desc)
     ""
     'ipe-defn--annotate-move-point)

    ;; Read in an indent function symbol / string
    (setq default (ipe--pair-property orig-defn :indent-function))

    (setq initial
	  (cons (cond ((not default)      "")
		      ((symbolp default) (symbol-name default))
		      (t                 default))
		0))

    (setq string
	  (let ((completion-extra-properties
		 '(:annotation-function ipe-defn--annotate-indent-function)))
	    (completing-read "Indent Function: "
			     '("none" "current" "previous")
			     nil
			     nil
			     initial)))

    (setq indent-function
	  (cond ((string= string "none") nil)
		((string= string "current")  'current)
		((string= string "previous") 'previous)
		((> (length string) 0)       string)
		(t                           nil)))

    (when indent-function
      (ipe--pair-property-set defn
			      :indent-function
			      indent-function))

    ;; TODO: Add regexp update matching.
    ;; ;; Read in OPEN Regexp (for updates.)
    ;; (ipe-defn--read-property defn "OPEN Regexp: " orig-defn
    ;;                          :open-regexp nil "" nil)
    ;;
    ;; ;; Read in CLOSE Regexp (for updates.)
    ;; (ipe-defn--read-property defn "CLOSE Regexp: " orig-defn
    ;;                          :close-regexp nil "" nil)

    ;; Read in Menu group.
    (ipe-defn--read-property defn "Menu: " orig-defn
			     :menu nil "" nil))

  defn)

;; -------------------------------------------------------------------
;;;; Interactive Customization Functions
;; -------------------------------------------------------------------

(defun ipe-defn--edit-pair (arg &optional mnemonic)
  "Interactively edit a new Insert Pair Edit PAIR Definition.

Prompts the user to enter the details for an `ipe' PAIR Definition.

If MNEMONIC is nil:
  It will prompt for: a MNEMONIC, an OPEN and a CLOSE string.

If MNEMONIC is non-nil:
  It will assume this is editing an existing PAIR and only prompt for
  an OPEN and a CLOSE string.

MNEMONIC is the mnemonic entered at the `insert-pair-edit' prompt.
OPEN and CLOSE are the strings to be used as an `ipe' PAIR.

If a PAIR Definition does not exist for MNEMONIC it will be created.

The updated / newly defined PAIR Definition will be added to
`ipe-pairs'.

With prefix ARG, call `ipe-defn--ui-edit-pair'."
  (interactive "P")

  (if arg
      (ipe-defn--ui-edit-pair mnemonic)

    ;; Read in the MNEMONIC.
    (unless mnemonic
      (setq mnemonic
	    (ipe-defn--read-mnemonic
	     "Add new 'Insert Pair Edit' PAIR Definition for - MNEMONIC: "))

      (when
	  (or (and (ipe--pair mnemonic t)
		   (not (ipe-defn--y-or-n-p
			 (format
			  "The MNEMONIC '%s' is already defined as the\
 Global PAIR%s Overwrite? "
			  mnemonic
			  (ipe--mnemonic-annotate mnemonic t)))))
	      (and (ipe--mode-pair mnemonic ipe--major-mode)
		   (not (ipe-defn--y-or-n-p
			 (format
			  "The MNEMONIC '%s' is already defined as%s\
 in MODE '%s'.  Define as Global PAIR anyway? "
			  mnemonic
			  (ipe--mnemonic-annotate mnemonic ipe--major-mode)
			  ipe--major-mode)))))

	(setq mnemonic nil)))

    (when mnemonic

      ;; Read in OPEN and CLOSE strings.
      (let ((pair (ipe--pair mnemonic))
	    (open)
	    (close)
	    (open-initial "")
	    (close-initial ""))

	(when pair
	  (setq open  (ipe--pair-open-string  pair)
		close (ipe--pair-close-string pair)
		open-initial (cons open 0)
		close-initial (cons close 0)))

	(setq open  (substring-no-properties
		     (read-from-minibuffer "OPEN: "  open-initial))
	      close (substring-no-properties
		     (read-from-minibuffer "CLOSE: " close-initial)))

	;; Check if the user wants to set any Intermediate options.
	(let ((defn (list mnemonic open close)))
	  (when (ipe-defn--y-or-n-p "Set 'Intermediate' options? "
				    (ipe--pair-intermediate-p pair))
	    (setq defn (ipe-defn--read-intermediate defn))

	    ;; Check if the user wants to set any Advanced options.
	    (when (ipe-defn--y-or-n-p
		   "Set 'Advanced' options? "
		   (ipe--pair-advanced-p pair))
	      (setq defn (ipe-defn--read-advanced defn))))

	  (ipe-defn--update-pair-list 'ipe-pairs defn t)
	  (run-hooks 'ipe-defn--update-hook))))))

(defun ipe-defn--edit-current-pair (arg)
  "Edit the current `ipe' PAIR Definition.

This function will act as per `ipe-defn--edit-pair', but will use the
currently active Insert Pair Edit PAIR rather than prompting for a
MNEMONIC.

With prefix ARG, call either `ipe-defn--ui-edit-pair' or
`ipe-defn--ui-edit-mode-pair'."

  (if (ipe--mode-pair ipe--mnemonic major-mode)
      (ipe-defn--edit-mode-pair arg major-mode ipe--mnemonic)
    (ipe-defn--edit-pair arg ipe--mnemonic)))

(defun ipe-defn--edit-mode-pair (arg &optional mode mnemonic)
  "Interactively edit a new Mode-Specific `ipe' PAIR Definition.

Prompts the user to enter the details for an Mode-Specific `ipe' PAIR
definition.

If MODE and MNEMONIC is nil:
  It will prompt for: a MODE, a MNEMONIC, an OPEN and a CLOSE string.

If MODE is non-nil, and MNEMONIC is nil:
  It will prompt for: a MNEMONIC, an OPEN and a CLOSE string.

If both MODE and MNEMONIC are non-nil:
  It will assume this is editing an existing Mode-Specific PAIR and
  only prompt for an OPEN and a CLOSE string.

MODE is the mode in which the PAIR Definition will be added.
MNEMONIC is the mnemonic entered at the `insert-pair-edit' prompt.
OPEN and CLOSE strings are the strings to be used as an `ipe' PAIR.

If a PAIR Definition does not exist for the given MNEMONIC in the
given MODE it will be created.

The updated / newly defined Mode-Specific PAIR Definition will be
added to `ipe-mode-pairs'.

With prefix ARG, call `ipe-defn--ui-edit-mode-pair'."

  (interactive "P")

  (if arg
      (ipe-defn--ui-edit-mode-pair mode mnemonic)

    ;; Read in the MODE.
    (unless mode
      (setq mode (ipe-defn--read-mode
		  "Add new 'Insert Pair Edit' PAIR Definition for MODE: "
		  t))

      ;; Read in the MNEMONIC.
      (unless mnemonic
	(setq mnemonic
	      (ipe-defn--read-mnemonic
	       "Add new Mode-Specific 'Insert Pair Edit' (ipe) PAIR\
 Definition for - MNEMONIC: "
	       mode)))

      (when
	  (or (and (ipe--mode-pair mnemonic mode)
		   (not (ipe-defn--y-or-n-p
			 (format
			  "The MNEMONIC '%s' is already defined as%s in\
 mode '%s'.  Overwrite? "
			  mnemonic
			  (ipe--mnemonic-annotate mnemonic mode)
			  mode))))
	      (and (ipe--pair mnemonic t)
		   (not (ipe-defn--y-or-n-p
			 (format
			  "The MNEMONIC '%s' is already defined as the\
 Global PAIR%s.  Override? "
			  mnemonic
			  (ipe--mnemonic-annotate mnemonic t))))))

	(setq mnemonic nil)))

    (when mnemonic

      ;; Read in OPEN and CLOSE strings.
      (let ((pair (ipe--pair mnemonic ipe--major-mode))
	    (open)
	    (close)
	    (open-initial "")
	    (close-initial ""))

	(when pair
	  (setq open  (ipe--pair-open-string  pair)
		close (ipe--pair-close-string pair)
		open-initial (cons open 0)
		close-initial (cons close 0)))

	(setq open  (substring-no-properties
		     (read-from-minibuffer "OPEN: "  open-initial))
	      close (substring-no-properties
		     (read-from-minibuffer "CLOSE: " close-initial)))

	;; Check if the user wants to set any Intermediate options.
	(let ((defn (list mnemonic open close)))
	  (when (ipe-defn--y-or-n-p "Set 'Intermediate' options? "
				    (ipe--pair-intermediate-p pair))
	    (setq defn (ipe-defn--read-intermediate defn))

	    ;; Check if the user wants to set any Advanced options.
	    (when (ipe-defn--y-or-n-p
		   "Set 'Advanced' options? "
		   (ipe--pair-advanced-p pair))
	      (setq defn (ipe-defn--read-advanced defn))))

	  (ipe-defn--update-mode-pair mode defn t)
	  (run-hooks 'ipe-defn--update-hook))))))

(defun ipe-defn--delete-pair (&optional mnemonic)
  "Interactively delete an `ipe' PAIR Definition.

MNEMONIC is the mnemonic entered at the `insert-pair-edit' prompt of
the Insert Pair Edit PAIR to be deleted.

The PAIR Definition is deleted from `ipe-pairs'."

  (interactive
   (list (ipe-defn--read-mnemonic
	  "Delete 'Insert Pair Edit' PAIR Definition for - MNEMONIC: "
	  nil
	  t)))

  ;; Read in the MNEMONIC of the PAIR to be deleted.
  (unless mnemonic
    (setq mnemonic
	  (ipe-defn--read-mnemonic
	   "Delete 'Insert Pair Edit' PAIR Definition for - MNEMONIC: "
	   nil
	   t)))

  ;; Confirm the User wants to delete the PAIR.
  (when (and (ipe--pair mnemonic)
	     (ipe-defn--y-or-n-p
	      (format
	       "Delete 'Insert Pair Edit' PAIR Definition - %s? "
	       (ipe--mnemonic-describe mnemonic t))))

    (ipe-defn--update-pair-list 'ipe-pairs (list mnemonic) t)

    (unless (ipe--pair ipe--mnemonic)
      (setq ipe--mnemonic nil))

    (run-hooks 'ipe-defn--update-hook)))

(defun ipe-defn--delete-mode-pair (&optional mode mnemonic)
  "Interactively delete an Mode-Specific `ipe' PAIR Definition.

MODE is the mode in which the Mode-Specific PAIR is defined.
MNEMONIC is the mnemonic entered at the `insert-pair-edit' prompt.

The PAIR Definition is deleted from `ipe-mode-pairs'."

  (interactive
   (let* ((mode
	   (ipe-defn--read-mode
	    "Delete 'Insert Pair Edit' PAIR Definition for MODE: "))
	  (mnemonic
	   (if (or (assoc mode ipe-mode-pairs)
		   (not (cdr (assoc mode ipe-mode-pairs))))
	       (ipe-defn--read-mnemonic "MNEMONIC: " mode t)
	     (message
	      "No Mode-Specific MNEMONICS are defined for MODE '%s'"
	      mode)
	     nil)))
     (list mode mnemonic)))

  (unless mode
    ;; Read in the MODE of the PAIR to be deleted.
    (setq mode
	  (ipe-defn--read-mode
	   "Delete 'Insert Pair Edit' PAIR Definition for MODE: "))

    ;; Read in the MNEMONIC of the PAIR to be deleted.
    (unless mnemonic
      (setq mnemonic
	    (if (or (assoc mode ipe-mode-pairs)
		    (not (cdr (assoc mode ipe-mode-pairs))))
		(ipe-defn--read-mnemonic "MNEMONIC: " mode t)
	      (message
	       "No Mode-Specific MNEMONICS are defined for MODE '%s'"
	       mode)
	      nil))))

  ;; Confirm the User wants to delete the PAIR.
  (when (and (> (length mnemonic) 0)
	     (ipe--mode-pair mnemonic mode)
	     (ipe-defn--y-or-n-p
	      (format "Delete Mode-Specific 'Insert Pair Edit' PAIR\
 Definition - %s? "
		      (ipe--mnemonic-describe mnemonic mode))))

    (ipe-defn--update-mode-pair mode (list mnemonic) t)

    (unless (ipe--pair ipe--mnemonic)
      (setq ipe--mnemonic nil))

    (run-hooks 'ipe-defn--update-hook)))

(defun ipe-defn--change-pair-mnemonic ()
  "Change the MNEMONIC for a given `ipe' PAIR to NEW-MNEMONIC.

The PAIR Definition is updated in `ipe-pairs'."
  (interactive)

  ;; Read in the MNEMONIC of the PAIR to be changed.
  (let* ((mnemonic
	  (ipe-defn--read-mnemonic
	   "Change MNEMONIC for 'Insert Pair Edit' PAIR with MNEMONIC: "
	   nil
	   t))
	 (new-mnemonic
	  (or (< (length mnemonic) 1)
	      (substring-no-properties
	       (read-from-minibuffer "To NEW-MNEMONIC: ")))))

    ;; Check for clashes with existing MNEMONICs.
    (when
	(or (and (ipe--pair new-mnemonic t)
		 (not (ipe-defn--y-or-n-p
		       (format
			"The NEW-MNEMONIC '%s' is already defined as the\
 Global PAIR%s.  Overwrite? "
			new-mnemonic
			(ipe--mnemonic-annotate new-mnemonic t)))))
	    (and (ipe--mode-pair new-mnemonic ipe--major-mode)
		 (not (ipe-defn--y-or-n-p
		       (format
			"The NEW-MNEMONIC '%s' is already defined as%s\
 in mode '%s'.  Change anyway? "
			new-mnemonic
			(ipe--mnemonic-annotate new-mnemonic ipe--major-mode)
			ipe--major-mode)))))

      (setq new-mnemonic nil))

    (when (and (> (length mnemonic) 0)
	       (> (length new-mnemonic) 0))

      (let ((defn (ipe--pair mnemonic t)))
	(ipe-defn--update-pair-list 'ipe-pairs (list mnemonic))
	(setcar defn new-mnemonic)
	(ipe-defn--update-pair-list 'ipe-pairs defn t)
	(run-hooks 'ipe-defn--update-hook)

	(message
	 "Changed MNEMONIC for 'Insert Pair Edit' PAIR %s to '%s'"
	 (ipe--mnemonic-annotate new-mnemonic)
	 new-mnemonic)))))

(defun ipe-defn--change-mode-pair-mnemonic ()
  "Change the MNEMONIC for a given Mode-Specific `ipe' PAIR.

The PAIR Definition is updated in `ipe-mode-pairs'."

  (interactive)

  ;; Read in the MODE + MNEMONIC of the PAIR to be changed.
  (let*
      ((mode
	(ipe-defn--read-mode
	 "Change 'Insert Pair Edit' MNEMONIC for MODE: "))
       (mnemonic
	(ipe-defn--read-mnemonic
	 "Change MNEMONIC for 'Insert Pair Edit' PAIR with MNEMONIC: "
	 mode
	 t))
       (new-mnemonic
	(substring-no-properties
	 (read-from-minibuffer "To NEW-MNEMONIC: "))))

    ;; Check for clashes with existing MNEMONICs.
    (when
	(or (and (ipe--mode-pair new-mnemonic mode)
		 (not (ipe-defn--y-or-n-p
		       (format
			"The NEW-MNEMONIC '%s' is already defined as%s in\
 mode '%s'.  Overwrite? "
			new-mnemonic
			(ipe--mnemonic-annotate new-mnemonic mode)
			mode))))
	    (and (ipe--pair new-mnemonic t)
		 (not (ipe-defn--y-or-n-p
		       (format
			"The NEW-MNEMONIC '%s' is already defined as the\
 Global PAIR%s.  Override? "
			new-mnemonic
			(ipe--mnemonic-annotate new-mnemonic t))))))

      (setq new-mnemonic nil))

    (when (and (> (length mnemonic) 0)
	       (> (length new-mnemonic) 0))

      (let ((defn (ipe--mode-pair mnemonic mode)))
	(ipe-defn--update-mode-pair mode (list mnemonic))
	(setcar defn new-mnemonic)
	(ipe-defn--update-mode-pair mode defn t)
	(run-hooks 'ipe-defn--update-hook)

	(message
	 "Changed MNEMONIC for 'Insert Pair Edit' PAIR %s to '%s'\
 in mode '%s'"
	 (ipe--mnemonic-annotate new-mnemonic mode)
	 new-mnemonic
	 mode)))))

;; -------------------------------------------------------------------
;;;; UI Customization Functions
;; -------------------------------------------------------------------

(defun ipe-defn--ui-add-pair-callback (_mode defn _orig-defn)
  "The callback function used by the UI 'Add Pair'.

This function is passed to `ipe-custom--edit-pair-defn' after calling
`ipe-defn--ui-add-pair' to save the updated PAIR definition.

DEFN is the definition of a PAIR as returned by
`ipe-custom--edit-pair-defn'."

  (when (or (not (ipe--pair (car defn) t))
	    (ipe-defn--y-or-n-p
	     (format
	      "The MNEMONIC '%s' is already defined as the\
 Global PAIR%s Overwrite? "
	      (car defn)
	      (ipe--mnemonic-annotate (car defn) t))))

    (ipe-defn--update-pair-list 'ipe-pairs defn t)
    (run-hooks 'ipe-defn--update-hook)
    t))

(defun ipe-defn--ui-add-pair ()
  "Add a new `ipe' PAIR Definition using custom-like widgets.

Calls `ipe-custom--edit-pair-defn' to create a new definition for an
`ipe' PAIR using the *ipe-edit-pair-defn* buffer to display a
custom-like widget interface.

On \"Save\", the PAIR Definition is written back to `ipe-pairs'."

  (interactive)

  (ipe-custom--edit-pair-defn
   nil
   '("" "" "")
   #'ipe-defn--ui-add-pair-callback))

(defun ipe-defn--ui-add-mode-pair-callback (mode defn _orig-defn)
  "The callback function used by the UI 'Add Mode Pair'.

This function is passed to `ipe-custom--edit-pair-defn' after calling
`ipe-defn--ui-add-mode-pair' to save the updated PAIR definition.

MODE is the `major-mode` for which the PAIR definition is being
created.

DEFN is the definition of a PAIR as returned by
`ipe-custom--edit-pair-defn'."

  (when (or (and (ipe--mode-pair (car defn) mode)
		 (ipe-defn--y-or-n-p
		  (format
		   "The MNEMONIC '%s' is already defined as%s\
 in MODE '%s'.  Overwrite? "
		   (car defn)
		   (ipe--mnemonic-annotate (car defn) mode)
		   mode)))
	    (and (ipe--pair (car defn) t)
		 (ipe-defn--y-or-n-p
		  (format
		   "The MNEMONIC '%s' is already defined as the\
 Global PAIR%s Overwrite? "
		   (car defn)
		   (ipe--mnemonic-annotate (car defn) t))))
	    (and (not (ipe--pair (car defn) t))
		 (not (ipe--mode-pair (car defn) mode))))

    (ipe-defn--update-mode-pair mode defn t)
    (run-hooks 'ipe-defn--update-hook)
    t))

(defun ipe-defn--ui-add-mode-pair (&optional mode)
  "Add a Mode-Specific PAIR Definition using custom-like widgets.

If MODE is nil, prompts the user for a MODE, then calls
`ipe-custom--edit-pair-defn' to create a new definition a
Mode-Specific `ipe' PAIR using the *ipe-edit-pair-defn* buffer to
display a custom-like widget interface.

On \"Save\" the PAIR Definition is written back to `ipe-mode-pairs'."

  (interactive)

  (unless mode
    (setq mode (ipe-defn--read-mode
		"Add new 'Insert Pair Edit' PAIR Definition for MODE: "
		t)))

  (ipe-custom--edit-pair-defn
   mode
   '("" "" "")
   #'ipe-defn--ui-add-mode-pair-callback))

(defun ipe-defn--ui-edit-pair-callback (_mode defn orig-defn)
  "The callback function used by the UI 'Edit Pair'.

This function is passed to `ipe-custom--edit-pair-defn' after calling
`ipe-defn--ui-edit-pair' to save the updated PAIR definition.

ORIG-DEFN is the definition of a PAIR before it was edited by the
widgets in `ipe-custom--edit-pair-defn'.

DEFN is the definition of a PAIR as returned by
`ipe-custom--edit-pair-defn'."

  ;; Delete the old defn if the MNEMONIC changes.
  (if (not (string= (car defn) (car orig-defn)))
      (when (and (ipe--pair (car defn) t)
		 (ipe-defn--y-or-n-p
		  (format
		   "The MNEMONIC '%s' is already defined as the\
 Global PAIR%s Overwrite? "
		   (car defn)
		   (ipe--mnemonic-annotate (car defn) t))))
	(ipe-defn--update-pair-list 'ipe-pairs
				    (list (car orig-defn)))
	(ipe-defn--update-pair-list 'ipe-pairs defn t)
	(run-hooks 'ipe-defn--update-hook)
	t)
    (ipe-defn--update-pair-list 'ipe-pairs defn t)
    (run-hooks 'ipe-defn--update-hook)
    t))

(defun ipe-defn--ui-edit-pair (&optional mnemonic)
  "Edit an `ipe' PAIR Definition using custom-like widgets.

Prompts the user for an Insert Pair Edit (ipe) MNEMONIC, then calls
`ipe-custom--edit-pair-defn' to edit the definition for the specified
`ipe' PAIR using the *ipe-edit-pair-defn* buffer to display a
custom-like widget interface.

On \"Save\" the PAIR Definition is written back to `ipe-pairs'."

  (interactive)

  (unless mnemonic
    (setq mnemonic
	  (ipe-defn--read-mnemonic
	   "Edit 'Insert Pair Edit' PAIR Definition for MNEMONIC: ")))

  (let* ((pair-defn (ipe--pair mnemonic t))
	 (pair-defn (or pair-defn (list mnemonic "" ""))))

    (ipe-custom--edit-pair-defn
     nil
     pair-defn
     #'ipe-defn--ui-edit-pair-callback)))

(defun ipe-defn--ui-edit-mode-pair-callback (mode defn orig-defn)
  "The callback function used by the UI 'Edit Mode Pair'.

This function is passed to `ipe-custom--edit-mode-pair-defn' after
calling `ipe-defn--ui-add-mode-pair' to save the updated PAIR
definition.

MODE is the `major-mode` for which the PAIR definition is being
edited.

ORIG-DEFN is the definition of a PAIR as before it was edited by the
widgets within `ipe-custom--edit-pair-defn'.

DEFN is the definition of a PAIR as returned by
`ipe-custom--edit-pair-defn'."

  ;; Delete the old defn if the MNEMONIC changes.
  (if (not (string= (car defn) (car orig-defn)))
      (when (or (and (ipe--mode-pair (car defn) mode)
		     (ipe-defn--y-or-n-p
		      (format
		       "The MNEMONIC '%s' is already defined as%s\
 in MODE '%s'.  Overwrite? "
		       (car defn)
		       (ipe--mnemonic-annotate (car defn) mode)
		       mode)))
		(and (ipe--pair (car defn) t)
		     (ipe-defn--y-or-n-p
		      (format
		       "The MNEMONIC '%s' is already defined the\
 Global PAIR%s Overwrite? "
		       (car defn)
		       (ipe--mnemonic-annotate (car defn) mode))))
		(and (not (ipe--mode-pair (car defn) mode))
		     (not (ipe--pair (car defn) t))))

	(ipe-defn--update-mode-pair mode
				    (list (car orig-defn)))
	(ipe-defn--update-mode-pair mode defn t)
	(run-hooks 'ipe-defn--update-hook)
	t)
    (ipe-defn--update-mode-pair mode defn t)
    (run-hooks 'ipe-defn--update-hook)
    t))

(defun ipe-defn--ui-edit-mode-pair (&optional mode mnemonic)
  "Edit a Mode-Specific PAIR Definition using custom-like widgets.

Prompts the user for a MODE and an Insert Pair Edit (ipe) MNEMONIC,
then calls `ipe-custom--edit-pair-defn' to edit the definition for the
specified `ipe' PAIR in the given MODE using the
*ipe-edit-pair-defn* buffer to display a custom-like widget
interface.

On \"Save\" the PAIR Definition is written back to `ipe-mode-pairs'."

  (interactive)

  (unless mode
    (setq mode (ipe-defn--read-mode
		"Edit 'Insert Pair Edit' PAIR Definition for MODE: ")))

  (unless mnemonic
    (setq mnemonic
	  (ipe-defn--read-mnemonic
	   "Edit 'Insert Pair Edit' PAIR Definition for MNEMONIC: "
	   mode)))

  (let* ((pair-defn (ipe--pair mnemonic mode))
	 (pair-defn (or pair-defn (list mnemonic "" ""))))

    (ipe-custom--edit-pair-defn
     mode
     pair-defn
     #'ipe-defn--ui-edit-mode-pair-callback)))

(provide 'ipe-defn)

;;; ipe-defn.el ends here
