;;; ipe-core.el --- Insert Pair Edit - core library -*- lexical-binding: t; -*-
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
;; This file defines the core functionality required by the 'Insert
;; Pair Edit' (ipe) package.
;;
;; The 'Insert Pair Edit' package defines a command
;; `ipe-insert-pair-edit' that can be configured as a more feature
;; rich alternative to the standard 'M-(' Emacs keybinding
;; `insert-parentheses'.
;;
;; `ipe-insert-pair-edit' will prompt the user to enter a customizable
;; MNEMONIC that selects a 'major-mode dependent' PAIR to be inserted
;; around point.  The PAIR consists of OPEN and CLOSE strings which
;; delimit text in some fashion.
;;
;; Once inserted, `ipe-insert-pair-edit' will run the command
;; `ipe-edit-mode' to enter the 'Insert Pair Edit' (ipe) minor mode.
;;
;; The `ipe-edit-mode' supplies commands to interactively and
;; independently move overlays representing the OPEN and CLOSE strings
;; for the inserted PAIR about the buffer, and to either insert
;; (`ipe-edit--insert-pair'), or discard (`ipe-edit--abort') them.
;;
;; Movement of the OPEN and CLOSE overlays is based upon 'lexical
;; units'.  The 'lexical units' are either: characters, words, lines,
;; or lists (S-expressions).
;;
;; By default, movement will be by words, but this can be changed
;; interactively via the: `ipe-edit--movement-by-*' commands or by
;; `ipe-pairs' / `ipe-mode-pairs' `customize'-ations.
;;
;; Customizations for the mode can be found under the `ipe' group.

;; -------------------------------------------------------------------
;;; Code:

(require 'ipe-compat)
(require 'ipe-custom)

;; -------------------------------------------------------------------
;;;; Customization:
;; -------------------------------------------------------------------

(defgroup ipe-display nil
  "Insert Pair Edit - Display customizations.

This group defines `customize'-ations for the way that Insert Pair
Edit (ipe) PAIRs are displayed within `ipe-edit-mode' minor mode."
  :group 'ipe
  :tag  "Insert Pair Edit - Display Options"
  :link '(emacs-commentary-link "ipe.el")
  :link '(function-link ipe-insert-pair-edit)
  :link '(function-link ipe-edit-mode))

(defgroup ipe-advanced nil
  "Insert Pair Edit - Advanced customizations.

This group defines advanced `customize'-ations for the Insert Pair
Edit (ipe) package."
  :group 'ipe
  :tag  "Insert Pair Edit - Advanced Options"
  :link '(emacs-commentary-link "ipe.el")
  :link '(function-link ipe-insert-pair-edit)
  :link '(function-link ipe-edit-mode))

(defgroup ipe-mode-pairs nil
  "Insert Pair Edit - Mode specific customizations.

This group defines `customize'-ations for the Mode-Specific PAIRs for
the Insert Pair Edit (ipe) package."
  :group 'ipe
  :link '(emacs-commentary-link "ipe.el")
  :link '(function-link ipe-insert-pair-edit)
  :link '(function-link ipe-edit-mode))

;; -------------------------------------------------------------------
;;;;; Basic customization:
;; -------------------------------------------------------------------

(defcustom ipe-pairs
  (ipe-custom--pair-list-get
   '(("("  "("  ")")
     ("{"  "{"  "}")
     ("["  "["  "]")
     ("<"  "<"  ">")
     ("\"" "\"" "\""
      (
       :movement word
       :infix    ""
       :escapes  (("\"" "\\\"") ("\\" "\\\\"))))
     ("'"  "'"  "'"
      (
       :movement word
       :infix    ""
       :escapes (("'"  "\\'")  ("\\" "\\\\"))))
     ("`"  "`"  "`")))
  "Opening / Closing PAIRS inserted by `ipe-insert-pair-edit'.

A list consisting of entries of the form:

  (MNEMONIC OPEN CLOSE)
  (MNEMONIC OPEN CLOSE (ALIST))

Where:

- MNEMONIC is an input entered into the minibuffer by the user after
  calling `ipe-insert-pair-edit' to indicate which OPEN and CLOSE
  strings are to be inserted into the buffer around the lexical unit
  at POINT.
- OPEN is a string placed at the beginning of a lexical unit, and then
  moved by the Insert Pair Edit commands.
- CLOSE is a string placed at the end of a lexical unit, and then
  moved by the Insert Pair Edit commands.

Optional Advanced configuration can be added by properties within
ALIST.

- :movement - The initial lexical unit indicating the type of
  movements made by the Insert Pair Edit commands.  (Values: `'char',
  `'word', `'line', `'list').  Optional.  Default: `'word'.
- :infix - A string added to the front of lines between the OPEN and
  CLOSE PAIR when :movement = `'line'.  Optional.
- :escapes - A list with elements of the form (MATCH REPLACE).  Each
  MATCH between OPEN and CLOSE will be replaced by REPLACE.  Optional.
- :auto-insert - A boolean which, if non-nil, will cause the
  `ipe-insert-pair-edit' function to automatically insert the PAIR
  without entering `ipe-edit-mode'.  Optional.  Default: nil
- :move-point - Indicates where to position point after the insertion
  of the OPEN and CLOSE strings.  (Values: either: nil - indicating
  POINT should be moved as per `ipe-move-point-on-insert', or; one of
  the following symbols: `'resume', `'open-beg', `'open-end',
  `'close-beg', `'close-end' - which are interpreted as per
  `ipe-move-point-on-insert'.)  Optional.  Default: nil.
- :indent-function - A function used to indent the lines between OPEN
  and CLOSE.
- :menu is a forward slash (`/') separated string of the names of the
  menus under which the PAIR is displayed within the Emacs `ipe'
  menus.

\(See command: `ipe-edit-mode')"
  :group 'ipe
  :tag    "Insert Pair Edit - Global PAIRs."
  :link   '(function-link ipe-insert-pair-edit)
  :link   '(function-link ipe-edit-mode)
  :get    'ipe-custom-pair-list-get
  :set    'ipe-custom-pair-list-set
  :type   '(ipe-custom-pair-list :tag "Global Pairs"))

(defcustom ipe-mode-pairs '()
  "Modal PAIRS inserted by `ipe-insert-pair-edit' (ipe).

This provides specialization of the `ipe-pairs' variable for
individual MAJOR-MODEs.

This variable is a list consisting of entries of the form:

  (MAJOR-MODE PAIR-DEFINITIONS)

Where:

- MAJOR-MODE is a symbol defining the major mode for which the
  PAIR-DEFINITIONS will be defined.
- PAIR-DEFINITIONS is either a symbol referring to a variable which
  defines a list of PAIRs (as per `ipe-pairs'), or, an inline
  definition (as per `ipe-pairs') of a list of PAIRs for the given
  MAJOR-MODE.

\(See command: `ipe-edit-mode')"
  :group 'ipe-mode-pairs
  :tag   "Insert Pair Edit - Mode Specific PAIRs"
  :link  '(function-link ipe-insert-pair-edit)
  :link  '(function-link ipe-edit-mode)
  :get   'ipe-custom-mode-pairs-get
  :set   'ipe-custom-mode-pairs-set
  :type  '(ipe-custom-mode-pairs :tag "Mode-specific PAIRs"))

(defun ipe-mode-pairs-add (mode mode-pairs)
  "Add a set of major MODE-Specific MODE-PAIRS to `ipe-mode-pairs'.

This is run by Insert Pair Edit Mode-Specific packages to add their
own major MODE specific MODE-PAIRs.

i.e. For a fictitious `xxx-mode':

  (defcustom
    ipe-xxx-pairs
    (ipe-custom--pair-list-get
     \\='((\"(\" \"(\" \")\")
       ;; More PAIR definitions
       ;; ...
       )

    \"`ipe-insert-pair-edit' customizations for `xxx-mode'.\"
    :group \\='ipe-mode-pairs
    :tag   \"Insert Pair Edit - xxx-mode PAIRs.\"
    :link  \\='(function-link ipe-insert-pair-edit)
    :get   \\='ipe-custom-pair-list-get
    :set   \\='ipe-custom-pair-list-set
    :type  \\='(ipe-custom-pair-list
    :tag   \"PAIRs used by `ipe-insert-pair-edit' in `xxx-mode'.\"))"

  (if ipe-mode-pairs
      (if (not (assoc mode ipe-mode-pairs))
	  (add-to-list 'ipe-mode-pairs (list mode mode-pairs))
	(unless (equal (cadr (assoc mode ipe-mode-pairs)) mode-pairs)
	  (warn "Attempt to override user customizations in `%s' for\
 `%s' mode"
		'ipe-mode-pairs
		mode)))
    (setq ipe-mode-pairs (list (list mode mode-pairs)))))

;; -------------------------------------------------------------------
;;;;; 'Advanced' customization:
;; -------------------------------------------------------------------

(defcustom ipe-move-point-on-insert 'resume
  "If and where to move POINT when a new `ipe' PAIR is inserted.

This variable is consulted when inserting (`ipe-edit--insert-pair') an
Insert Pair Edit (ipe) PAIR into the buffer.

If nil, POINT will remain at the position it was when the PAIR was
first inserted.
If one of the following values, POINT will be moved to:

- `'resume'    - Point will remain at original position.
- `'open-beg'  - The beginning of the OPEN string.
- `'open-end'  - The end of the OPEN string.
- `'close-beg' - The beginning of the CLOSE string.
- `'close-end' - The end of the CLOSE string."
  :group 'ipe-advanced
  :link  '(function-link ipe-insert-pair-edit)
  :tag   "Insert Pair Edit - Move POINT on insert."
  :type
  '(radio :value resume
	  (const :tag "Point will remain at original position." resume)
	  (const :tag "The beginning of the OPEN string."  open-beg)
	  (const :tag "The end of the OPEN string."        open-end)
	  (const :tag "The beginning of the CLOSE string." close-beg)
	  (const :tag "The end of the CLOSE string."       close-end)))

(defcustom ipe-set-mark-on-insert 'resume
  "If and where to set MARK when a new `ipe' PAIR is inserted.

This variable is consulted when inserting (`ipe-edit--insert-pair') an
Insert Pair Edit (ipe) PAIR into the buffer.

If nil, MARK will not be set.
If one of the following values, MARK will be set to:

- `'open-beg'  - The beginning of the OPEN string.
- `'open-end'  - The end of the OPEN string.
- `'close-beg' - The beginning of the CLOSE string.
- `'close-end' - The end of the CLOSE string."
  :group 'ipe-advanced
  :link  '(function-link ipe-insert-pair-edit)
  :tag   "Insert Pair Edit - Set MARK on insert."
  :type
  '(radio :value resume
	  (const :tag "Mark will not be set."              resume)
	  (const :tag "The beginning of the OPEN string."  open-beg)
	  (const :tag "The end of the OPEN string."        open-end)
	  (const :tag "The beginning of the CLOSE string." close-beg)
	  (const :tag "The end of the CLOSE string."       close-end)))

(defcustom ipe-delete-action 'highlight
  "The action to take on deletion of an `ipe' PAIR.

This variable is consulted when deleting
\(`ipe-insert-pair-edit-delete') an Insert Pair Edit (ipe) PAIR from
the buffer.

- `'delete' - Deletes the `ipe' PAIR without further user interaction.
- `'highlight' - Highlights the `ipe' PAIR OPEN and CLOSE strings with
  the `ipe-delete-highlight' face for `ipe-delete-highlight-wait'
  seconds before deletion.
- `'fade' - Highlights the `ipe' PAIR OPEN and CLOSE strings with
  the `ipe-delete-highlight' face and fades `ipe-delete-highlight' for
  `ipe-delete-highlight-wait' seconds before deletion.
- `'prompt' - Highlights the `ipe' PAIR OPEN and CLOSE strings with
  the `ipe-delete-highlight' face and prompts the user for
  confirmation of the deletion."
  :group 'ipe-advanced
  :link  '(function-link ipe-insert-pair-edit-delete)
  :tag   "Insert Pair Edit - Action on delete."
  :type
  '(radio :value delete
	  (const :tag "Delete without further user interaction."
		 delete)
	  (const :tag "Highlight PAIR and then delete."
		 highlight)
	  (const :tag "Highlight PAIR, fade and then delete."
		 fade)
	  (const :tag "Highlight PAIR and prompt for confirmation."
		 prompt)))

(defcustom ipe-delete-highlight-wait 1.5
  "The time to wait when deleting an Insert Pair Edit (ipe) PAIR.

This variable specifies the time (in seconds) to wait before deleting
a PAIR when the `ipe-delete-action' is either `'highlight' or
`'fade'."
  :group 'ipe-advanced
  :link  '(function-link 'ipe-insert-pair-edit-delete)
  :tag   "Insert Pair Edit - Wait time for delete actions."
  :set   'ipe-custom--delete-highlight-wait-set
  :type  '(float))

(defcustom ipe-mouse-support-p t
  "Whether to include mouse support in `ipe-edit-mode' minor-mode.

If non-nil, the Insert Pair Edit `ipe-edit-mode' minor mode will
include keybindings for mouse actions.

i.e.

  \\<ipe-edit-mode-map>\\[ipe-mouse--open] \
	- Move the `ipe' OPEN string to mouse click POS.
  \\[ipe-mouse--close] \
	- Move the `ipe' CLOSE string to mouse click POS.
  \\[ipe-mouse--init] \
	- Surround the current lexical unit with a PAIR.
  \\[ipe-mouse--region] \
	- Move both the `ipe' OPEN and CLOSE strings.

  \\[ipe-mouse--add-pair] \
	- Add an `ipe' PAIR around mouse click POS.
  \\[ipe-mouse--delete-pair] \
	- Remove `ipe' PAIR closest to mouse click POS.

  \\[ipe-mouse--open-backward] \
	- Move the `ipe' OPEN string backwards.
  \\[ipe-mouse--close-forward] \
	- Move the `ipe' CLOSE string forwards.

  \\[ipe-mouse--open-forward] \
	- Move the `ipe' OPEN string forwards.
  \\[ipe-mouse--close-backward] \
	- Move the `ipe' CLOSE string backwards.

  \\[ipe-mouse--open-forward-alt] \
	- Move the `ipe' OPEN string forwards (by char).
  \\[ipe-mouse--open-backward-alt] \
	- Move the `ipe' OPEN string backwards (by char).

  \\[ipe-mouse--close-forward-alt] \
	- Move the `ipe' CLOSE string forwards (by char).
  \\[ipe-mouse--close-backward-alt] \
	- Move the `ipe' CLOSE string backwards (by char).

  \\[ipe-mouse--next-movement] \
	- Set movement to the next movement.
  \\[ipe-mouse--previous-movement] \
	- Set movement to the previous movement.

See (package: `ipe-mouse') for other bindings."
  :group 'ipe-advanced
  :tag   "Insert Pair Edit - Include mouse support."
  :link  '(function-link ipe-insert-pair-edit)
  :link  '(emacs-commentary-link "ipe-mouse.el")
  :set   'ipe-custom--mouse-set
  :type  '(boolean))

(defcustom ipe-menu-support-p nil
  "Whether to include menu support for the `ipe' package.

If non-nil, this option will cause the addition of both:

1. An extra `Pairs' sub-menu item to the standard Emacs `Edit' menu.

This new menu item will provide a set of sub-menus of the form:

- Edit >
  - Pairs >
    - Insert PAIR >
      - <OPEN> ... <CLOSE>
      - ...
    - Update PAIR >
      - <OPEN> ... <CLOSE>
      - ...
    - Delete PAIR >
      - <OPEN> ... <CLOSE>
      - ...
    - Edit PAIR Definitions >
      - ...
    - Options
    - Info
    - Help

2. Within the `ipe-edit-mode' minor mode, a new menu item ('Insert
Pair Edit') that will provide a set of sub-menus of the form:

- Insert Pair Edit >
  - Insert PAIR
  - Insert And >
  ---
  - Move OPEN Beginning
  - Move OPEN Up
  - Move OPEN Backward
  - Move OPEN Forward
  - Move OPEN Down
  - Move OPEN End
  ---
  - Move CLOSE Beginning
  - Move CLOSE Up
  - Move CLOSE Backward
  - Move CLOSE Forward
  - Move CLOSE Down
  - Move CLOSE End
  ---
  - Change PAIR >
  - Change Movement >
  - Edit CONTENTS >
  - Next / Previous >
  - Multiple >
  ---
  - Edit PAIR Definitions >
  ---
  - Abort
  - Options
  - Info
  - Help"
  :group 'ipe
  :tag   "Insert Pair Edit - Include menus."
  :link  '(function-link ipe-insert-pair-edit)
  :link  '(emacs-commentary-link "ipe-menu.el")
  :set   'ipe-custom--menu-set
  :type  '(boolean))

(defcustom ipe-pair-sort 'sort-by-mnemonic
  "How to sort PAIRs within the Insert Pair Edit (ipe) menus.

If non-nil, the value of this variable will change the order of the
PAIR definitions within the Insert Pair Edit:

- Edit >
  - Pairs >
    ...

and;

- Insert Pair Edit >
  - Change PAIR >
  - Edit PAIR Definitions >

menus.

- `'sort-by-mnemonic' - sort by MNEMONIC.
- `'sort-by-open'     - sort by OPEN.
- `'sort-by-close'    - sort by CLOSE"
  :group 'ipe-advanced
  :tag   "Insert Pair Edit - PAIR sort order."
  :link  '(function-link ipe-insert-pair-edit)
  :set   'ipe-custom--pair-sort-set
  :type  '(radio
	   (const :tag "Sort by MNEMONIC" sort-by-mnemonic)
	   (const :tag "Sort by OPEN"     sort-by-open)
	   (const :tag "Sort by CLOSE"    sort-by-close)))

(defcustom ipe-mnemonic-prompt-shortcut-p t
  "Does the Insert Pair Edit MNEMONIC prompt require a RETURN?

If nil, the `ipe-insert-pair-edit' prompt that reads a MNEMONIC will
require a RETURN to exit the minibuffer.

If non-nil, the prompt reading a MNEMONIC will exit the minibuffer as
soon as the user has entered a unique matching MNEMONIC."
  :group 'ipe-advanced
  :link  '(function-link ipe-insert-pair-edit)
  :tag   "Insert Pair Edit - MNEMONIC prompt shortcut."
  :type  '(boolean))

(defcustom ipe-elide-description 0
  "Truncate long Insert Pair Edit PAIR descriptions.

If set to a positive number (> 0), this indicates the maximum number
of OPEN and CLOSE characters to display in menus and completion
functions.

i.e. A value of `5' would display a PAIR with:

  OPEN:  <insert-pair-edit>
  CLOSE: </insert-pair-edit>

as:

  <inse... ... </ins...

within the Emacs \"Edit\" menu.

- Edit
  - Pairs
    - Insert PAIR
      - <inse... ... </ins...
      ..."
  :group 'ipe-advanced
  :link  '(function-link ipe-insert-pair-edit)
  :tag   "Insert Pair Edit - Elide long PAIR descriptions."
  :type  '(choice  :offset 2
		   (const   :tag "Off" nil)
		   (integer :tag "Characters to display")))

(defcustom ipe-update-forward-first-p nil
  "Whether to search forward or backward when updating.

If nil, `ipe-insert-pair-edit-update' / `ipe-insert-pair-edit-delete'
and `ipe-insert-pair-edit-replace' will first search backward, then
forward for a PAIR.

If non-nil, `ipe-insert-pair-edit-update' /
`ipe-insert-pair-edit-delete' and `ipe-insert-pair-edit-replace' will
first search forward, then backward for a matching PAIR."
  :group 'ipe-advanced
  :link  '(function-link ipe-insert-pair-edit-update)
  :tag   "Insert Pair Edit - Search forward for PAIR on update."
  :type  '(boolean))

(defcustom ipe-prefix-moves-close-p t
  "Whether a numeric prefix ARG moves the OPEN or CLOSE string.

If non-nil, a numeric ARG to `ipe-insert-pair-edit' will move the
CLOSE string forward.

If nil, a numeric ARG to `ipe-insert-pair-edit' will move the OPEN
string backward."
  :group 'ipe-advanced
  :link  '(function-link ipe-insert-pair-edit)
  :tag   "Insert Pair Edit - Numeric prefix ARG moves CLOSE."
  :type  '(boolean))

(defcustom ipe-empty-open-string "("
  "The text displayed for an empty `ipe' OPEN string.

If an Insert Pair Edit (ipe) PAIR definition contains an empty (\"\")
OPEN, the position of the OPEN point is still maintained by the
`ipe-edit-mode' movement commands.  To display the position of the
empty OPEN string within a buffer when in `ipe-edit-mode', this text
\(highlighted with `ipe-empty-pair-highlight') is used (but not
inserted.)

\(See command: `ipe-edit-mode')"
  :group 'ipe-display
  :link  '(function-link ipe-insert-pair-edit)
  :link  '(function-link ipe-edit-mode)
  :tag   "Insert Pair Edit - Text displayed for empty OPEN."
  :type  '(string))

(defcustom ipe-empty-close-string ")"
  "The text displayed for an empty `ipe' CLOSE string.

If an Insert Pair Edit (ipe) PAIR definition contains an empty (\"\")
CLOSE, the position of the CLOSE point is still maintained by the
`ipe-edit-mode' movement commands.  To display the position of the
empty CLOSE string within a buffer when in `ipe-edit-mode', this text
\(highlighted with `ipe-empty-pair-highlight') is used (but not
inserted.)

\(See command: `ipe-edit-mode')"
  :group 'ipe-display
  :link  '(function-link ipe-insert-pair-edit)
  :link  '(function-link ipe-edit-mode)
  :tag   "Insert Pair Edit - Text displayed for empty CLOSE."
  :type  '(string))

;; -------------------------------------------------------------------
;;;;; Faces:
;; -------------------------------------------------------------------

(defface ipe-open-highlight
  '((t (:inherit highlight)))
  "The face that highlights the Insert Pair Edit OPEN overlays.

This face highlights the OPEN string of a PAIR inserted by
`ipe-insert-pair-edit' while editing in Insert Pair Edit minor
mode.

\(See command: `ipe-edit-mode')"
  :group 'ipe-display
  :tag   "Insert Pair Edit (Faces) - OPEN face."
  :link  '(function-link ipe-insert-pair-edit)
  :link  '(function-link ipe-edit-mode))

(defface ipe-close-highlight
  (list (list t (if (facep 'show-paren-match)
		    (list :inherit 'show-paren-match)
		  (list :background "steelblue3"))))
  "The face that highlights the Insert Pair Edit CLOSE overlays.

This face highlights the CLOSE string of a PAIR inserted by
`ipe-insert-pair-edit' while editing in Insert Pair Edit
`ipe-edit-mode' minor mode.

\(See command: `ipe-edit-mode')"
  :group 'ipe-display
  :tag   "Insert Pair Edit (Faces) - CLOSE face."
  :link  '(function-link ipe-insert-pair-edit)
  :link  '(function-link ipe-edit-mode))

(defface ipe-infix-highlight
  '((t (:inherit ipe-open-highlight)))
  "The face that highlights the Insert Pair Edit INFIX overlays.

This face highlights the INFIX strings inserted between the OPEN and
CLOSE overlays by `ipe-insert-pair-edit' while editing in Insert Pair
Edit `ipe-edit-mode' minor mode .

\(See command: `ipe-edit-mode')"
  :group 'ipe-display
  :tag   "Insert Pair Edit (Faces) - INFIX face."
  :link  '(function-link ipe-insert-pair-edit)
  :link  '(function-link ipe-edit-mode))

(defface ipe-escape-highlight
  '((t (:inherit match)))
  "The face that highlights the Insert Pair Edit ESCAPE strings.

This face highlights the ESCAPE strings inserted between the OPEN and
CLOSE overlays by `ipe-insert-pair-edit' while editing in Insert Pair
Edit `ipe-edit-mode' minor mode.

\(See command: `ipe-edit-mode')"
  :group 'ipe-display
  :tag   "Insert Pair Edit (Faces) - ESCAPE face."
  :link  '(function-link ipe-insert-pair-edit)
  :link  '(function-link ipe-edit-mode))

(defface ipe-delete-highlight
  '((t (:inherit       warning
		       :inverse-video t)))
  "The face that highlights Insert Pair Edit PAIRs on deletion.

This face highlights the OPEN and CLOSE strings to be deleted by
`ipe-insert-pair-edit-delete' when `ipe-delete-action' is set to
`'highlight', `'fade' or `'prompt'.

\(See command: `ipe-insert-pair-edit-delete')"
  :group 'ipe-display
  :tag   "Insert Pair Edit (Faces) - DELETE face."
  :link  '(function-link ipe-insert-pair-edit-delete))

(defface ipe-empty-pair-highlight
  '((t (:inherit warning)))
  "The face that highlights Insert Pair Edit empty PAIR strings.

This face highlights the positions of OPEN or CLOSE overlays that have
an empty (\"\") definition, while editing in Insert Pair Edit minor
mode.

\(See command: `ipe-edit-mode')"
  :group 'ipe-display
  :tag   "Insert Pair Edit (Faces) - Empty PAIR face."
  :link  '(function-link ipe-insert-pair-edit)
  :link  '(function-link ipe-edit-mode))

;; -------------------------------------------------------------------
;;;; Variables:
;; -------------------------------------------------------------------

(defvar ipe-move-by-movements '()
  "The movement made by the Insert Pair Edit minor mode commands.

This is a list of 4-tuples:

\(SYMBOL MOVE-BY-FUNCTION POS-SET-FUNCTION DESCRIPTION)

- SYMBOL is the identifier defining the type of movement.

- MOVE-BY-FUNCTION

  Is a function that returns the position of the OPEN and CLOSE
  strings for a given `lexical unit' movement mode.  It should have a
  signature of the form:

    (MOVE-BY-FUNCTION DEFN N SIDE ACTION POS OTHER UNITS PUSH)

  Where:

  - DEFN is the definition of the PAIR from `ipe-pairs'.
  - N is the position of the PAIR in `ipe--pair-pos-list'.
  - SIDE is either `'open' or `'close'.
  - ACTION is either `'init', `'beg', `'up', `'backward', `'forward',
    `'down', `'end', or `'reset'.
  - POS is the position from which the movement begins.
  - OTHER is the position of the other member of the PAIR.
  - UNITS are the number of units to move.
  - PUSH indicates that this is a push move from the other member of
    the PAIR.

  It should return the position to which the given part of the PAIR
  (`'open' or `'close') is set.

  If POS is nil, movement should be calculated from POINT.

- POS-SET-FUNCTION is an (optional) function that is called whenever
  `ipe--pos-property-set' is called to perform movement specific
  updates to the `ipe--pair-pos-list' state variable.  It should have
  a signature of the form:

    (POS-SET-FUNCTION N)

  Where:

  - N is the position of the PAIR in `ipe--pair-pos-list' for which a
    `ipe--pos-property-set' call has just been made.

- DESCRIPTION is the text output to the echo area to describe the
  movement.")

;; -------------------------------------------------------------------
;;;; Utility macros:
;; -------------------------------------------------------------------

(defmacro ipe-dotimes (count &rest body)
  "Run BODY COUNT times.

Like `dotimes', but avoids unused VAR error if the iteration variable
is not used within BODY."

  (declare (indent 1))
  (let ((end         count)
	(upper-bound (make-symbol "upper-bound"))
	(counter     (make-symbol "counter")))
    `(let ((,upper-bound ,end)
	   (,counter     0))
       (while (< ,counter ,upper-bound)
	 ,@body
	 (setq ,counter (1+ ,counter))))))

;; -------------------------------------------------------------------
;;;; Local variables:
;; -------------------------------------------------------------------

(defvar-local ipe--mnemonic nil
  "The MNEMONIC for the last active Insert Pair Edit PAIR.

This corresponds to one of the MNEMONICs in the `ipe-mode-pairs' /
`ipe-pairs' variables.")

(defvar-local ipe--replace-mnemonic nil
  "The MNEMONIC for the last active Insert Pair Edit Replace PAIR.

This corresponds to one of the MNEMONICs in the `ipe-mode-pairs' /
`ipe-pairs' variables.")

(defvar-local ipe--major-mode major-mode
  "The `major-mode' of the buffer in Insert Pair Edit minor-mode.

This is a separate variable to allow `ipe--mnemonic-annotate' access
to the original `major-mode' when running in `help-mode'.")

(defvar-local ipe--undo-handle nil
  "The undo handle used to group changes made in `ipe-edit-mode'.")

(defvar-local ipe--movement 'word
  "The unit of movement made by the Insert Pair Edit (ipe) commands.

This variable defines what lexical units are used when moving the ipe
OPEN and CLOSE strings around the current buffer when `ipe-edit-mode'
is active.

This variable is set to one of the constants within the `cadr' of the
elements within the `ipe-move-by-movements' list.  (By default, this
should include at least: `'char', `'word', `'line' and `'list'.)

This variable is set by either `ipe--pair-pos-init' or
`ipe--pair-pos-movement-reset'.")

(defvar-local ipe--pair-pos-list '()
  "A list of the current `ipe' OPEN / CLOSE string positions.

A list of elements of the form:

  (POS-OPEN POS-CLOSE (ALIST))

Representing the positions of Insert Pair Edit OPEN and CLOSE
overlays within the current buffer for the current `ipe--mnemonic'.

Where:

- POS-OPEN - is a position within the current buffer at which an
  overlay displaying the current `ipe' OPEN string is located.
- POS-CLOSE - is a position within the current buffer at which an
  overlay displaying the current `ipe' CLOSE string is located.
- ALIST - is an association list of properties used by the ipe
  movement functions (see: `ipe-move-by-movements') to determine the
  current state of the PAIR.

  Current properties for ALIST are:

  * :close - The value to be displayed by the CLOSE overlay.  (This
    may contain multiple concatenated OPEN / CLOSE strings, if one
    overlay is being used to display multiple parts of one or more
    PAIRS.)
  * :close-list - The offsets within the :close string at which
    different parts of PAIRS are located when more than one PAIR is
    display by the one CLOSE overlay.)
  * :infix - The current infix string (This may differ from the PAIR
    definition, as it may include leading / trailing whitespace added
    by movement commands.)
  * :initial-n - A 0-based index indicating the original position of
    of the PAIR within the list.  (Used when editing multiple PAIRs at
    once to work out which PAIR was created first even after the
    `ipe--pair-pos-list' is normalized.)
  * :inserted-p - A flag indicating whether or not the current PAIR
    has been inserted into the buffer.  (Used by `ipe--set-point'.)
  * :open - The value to be displayed by the OPEN overlay.  (This may
    contain multiple concatenated OPEN / CLOSE strings if one overlay
    is being used to display multiple parts of one or more PAIRS.)
  * :open-list - The offsets within the :open string at which
    different parts of PAIRS are located when more than one PAIR is
    displayed by the one OPEN overlay.
  * :point - The position of POINT for this PAIR.  (This is
    updated by `ipe--pos-insert' / `ipe--pos-delete' functions to
    maintain the position of POINT even when characters are inserted
    before the current PAIR.)
  * :point-close - The position of POINT within a PAIR when
    POINT = POS-CLOSE.
    . `'before' means that POINT is before the CLOSE string.
    . A number, N, means that POINT is within the CLOSE string, offset
      N characters from the starting character of the CLOSE string.
    . `'after' means that POINT is after the CLOSE string.
  * :point-open - The position of POINT within a PAIR when
    POINT = POS-OPEN.
    . `'before' means that POINT is before the OPEN string.
    . A number, N, means that POINT is within the OPEN string, offset
      N characters from the starting character of the OPEN string.
    . `'after' means that POINT is after the OPEN string.")

(defvar-local ipe--open-overlays '()
  "Overlays displaying the OPEN strings of an `ipe' PAIR.

A list of overlays used to display the OPEN strings for the Insert
Pair Edit PAIRs within the current buffer.

These overlays are moved by the commands available within
`ipe-edit-mode'.")

(defvar-local ipe--close-overlays '()
  "Overlays displaying the CLOSE strings of an `ipe' PAIR.

A list of overlays used to display the CLOSE strings for the Insert
Pair Edit PAIRs within the current buffer.

These overlays are moved by the commands available within
`ipe-edit-mode'.")

(defvar-local ipe--infix-overlays '()
  "Overlays displaying the INFIX strings of an `ipe' PAIR.

A list of lists of overlays.  Each sub-list of overlays in this list
is used to display the `INFIX'es at the beginning of the lines
between a single `ipe' OPEN and CLOSE string.  These `INFIX'es are
displayed when the lexical unit of movement specified by
`ipe--movement' is `'line', and the PAIR definition includes an :infix
property.")

(defvar-local ipe--escapes-show-p t
  "Whether to process escapes within `ipe' PAIRs.

If non-nil, the :escapes property within the `ipe' PAIRs defined
within `ipe-mode-pairs' / `ipe-pairs' will be used to replace strings
between the OPEN and CLOSE strings.

These strings will be highlighted with `ipe-escape-highlight' in
`ipe-edit-mode' and `insert'ed into the buffer when the PAIR is
inserted.")

(defvar-local ipe--escape-overlays '()
  "Overlays displaying the ESCAPE strings of an `ipe' PAIR.

A list of lists of overlays.  Each sub-list of overlays in this list
is used to display the ESCAPE strings between a single OPEN and CLOSE
string when the `ipe' PAIR definition includes an :escapes property.")

(defvar-local ipe--pos-property-set-callback nil
  "A function called by the `ipe--pos-property-set' function.

This function is used by the ipe-move-by-* functions to update
dependent properties within the `ipe--pair-pos-list' when
`ipe--pos-property-set' is called.

It is expected to be of the form:

   (lambda (n &optional pname value))

Where:

- N is the current entry within `ipe--pair-pos-list' for which the
  property is being set.
- PNAME is the name of the property.
- VALUE is the value to which the property is set.")

;; -------------------------------------------------------------------
;;;; Internal constants:
;; -------------------------------------------------------------------

(defconst ipe--move-point-on-insert-desc
  '((nil
     "As per global `ipe-move-point-on-insert'")
    (resume
     "Leave POINT where it was on PAIR insert")
    (open-beg
     "Move POINT to the beginning of the OPEN string on PAIR insert")
    (open-end
     "Move POINT to the end of the OPEN string on PAIR insert")
    (close-beg
     "Move POINT to the beginning of the CLOSE string on PAIR insert")
    (close-end
     "Move POINT to the end of the CLOSE string on PAIR insert"))

  "The values to which `ipe-move-point-on-insert' can be set.

This is a list of the form:

  (SYMBOL STRING)

Where:

- SYMBOL is a value to which `ipe--move-point-on-insert' can be set.
- STRING is a human readable description of SYMBOL.")

(defconst ipe--set-mark-on-insert-desc
  '((nil
     "Do not set mark")
    (open-beg
     "Set mark to the beginning of the OPEN string on PAIR insert")
    (open-end
     "Set mark to the end of the OPEN string on PAIR insert")
    (close-beg
     "Set mark to the beginning of the CLOSE string on PAIR insert")
    (close-end
     "Set mark to the end of the CLOSE string on PAIR insert"))

  "The values to which `ipe-set-mark-on-insert' can be set.

This is a list of the form:

  (SYMBOL STRING)

Where:

- SYMBOL is a value to which `ipe-set-mark-on-insert' can be set.
- STRING is a human readable description of SYMBOL.")

;; -------------------------------------------------------------------
;;;; Utility functions:
;; -------------------------------------------------------------------

(defun ipe--string-starts-with-p (string prefix)
  "Return non-nil if STRING begins with PREFIX."

  (when string
    (string= (substring string
			0
			(min (length string) (length prefix)))
	     prefix)))

(defun ipe--list-pad (list n)
  "Pad a LIST out to `N' elements by adding nil entries."

  (if (< (length list) n)
      (append list (make-list (- n (length list)) nil))
    list))

(defun ipe--list-remove (list n)
  "Return a copy of LIST with the `N'th element removed."

  (when list
    (let ((copy (copy-sequence list)))
      (when (< n (length copy))
	(setf (nthcdr n copy) (nthcdr (1+ n) copy)))
      copy)))

(defun ipe--list-member (list pred)
  "Return the first element of list LIST to match PRED.

PRED is a single parameter function which will return non-nil for an
item of interest within LIST.

If no element within LIST matches PRED, return nil."

  (when list
    (if (funcall pred (car list))
	(car list)
      (ipe--list-member (cdr list) pred))))

(defun ipe--list-element (list element pred &optional beg)
  "Return the position of ELEMENT in LIST.

PRED is a two parameter function which will return non-nil for a
match between ELEMENT and the element within LIST.

If BEG is non-nil, it specifies the position in LIST at which to begin
the search."

  (when list
    (or (and (funcall pred element (car list))
	     (or beg 0))
	(ipe--list-element (cdr list)
			   element
			   pred
			   (1+ (or beg 0))))))

(defun ipe--list-filter (list pred)
  "Delete all elements in LIST which match PRED and return a copy.

PRED is a single parameter function which will return non-nil if an
item is to be deleted from LIST.

Returns a copy of LIST with all of the items which matched PRED
removed."

  (delq nil
	(mapcar (lambda (x) (and (not (funcall pred x)) x)) list)))

(defun ipe--list-update (list value pred)
  "Replace first match to PRED in LIST with VALUE and return a copy.

PRED is a two parameter function that will take both VALUE and an
element of the list, and return non-nil for the item within LIST which
is to be replaced with VALUE.

Returns a copy of LIST with the element which matched PRED replaced by
VALUE."

  (if (not list)
      (list value)
    (if (funcall pred value (car list))
	(cons value (cdr list))
      (cons (car list)
	    (ipe--list-update (cdr list) value pred)))))

(defun ipe--alist-update (alist key value)
  "Return new ALIST containing an association between KEY and VALUE.

- ALIST is the ALIST being updated.
- KEY is the name of the ALIST property to add / update.
- VALUE is the new value to associate with KEY within the new ALIST.

Returns a new ALIST with either a new VALUE associated with an
existing KEY, or a new KEY + VALUE association."

  (if (not alist)
      (list (cons key (list value)))
    (let ((copy (copy-alist alist)))
      (if (not (assoc key copy))
	  (cons (cons key (list value)) copy)
	(setf (cdr (assoc key copy)) (list value))
	copy))))

(defun ipe--safecall (symbol &rest arguments)
  "Check if a given SYMBOL is function before calling it.

ARGUMENTS, if specified are the arguments to be passed
to the function SYMBOL."

  (when (functionp symbol)
    (if arguments
	(apply symbol arguments)
      (funcall symbol))))

(defun ipe--arg-units (arg)
  "Convert a prefix ARG to a positive INTEGER.

This assumes ARG is a value passed to `interactive' as a PREFIX
command.

Return either:

- 1, if ARG is nil, 0, or cannot be converted to a INTEGER.
- ARG, if ARG is an INTEGER.
- (car ARG), if ARG is a list containing a INTEGER (i.e. has been set
  via `universal-argument'.)"

  (if (integerp arg)
      (if (zerop arg) 1 arg)
    (if (and arg (listp arg) (integerp (car arg)))
	(if (zerop (car arg)) 1 (car arg))
      1)))

(defun ipe--bol (&optional pos)
  "Return the `beginning-of-line' for the line containing POS.

If POS is nil, return the `beginning-of-line' of the line containing
POINT."

  (save-excursion
    (goto-char (or pos (point)))
    (beginning-of-line)
    (point)))

(defun ipe--eol (&optional pos)
  "Return the `end-of-line' for the line containing POS.

If POS is nil, return the `end-of-line' of the line containing POINT."

  (save-excursion
    (goto-char (or pos (point)))
    (end-of-line)
    (point)))

(defun ipe--to-eol-contains-p (pos string)
  "Return t if the text between POS and EOL contain STRING.

If text between POS and the `end-of-line' does not contain STRING,
return nil."

  (unless (zerop (length string))
    (save-excursion
      (goto-char pos)
      (when (re-search-forward (regexp-quote string) (ipe--eol pos) t)
	(match-beginning 0)))))

(defun ipe--hvisible-p (pos)
  "Return t, if POS is horizontally visible."

  (let* ((window (get-buffer-window))
	 (w      (window-width window)))

    (if (or truncate-lines
	    (and truncate-partial-width-windows
		 (< w truncate-partial-width-windows)))
	(save-excursion
	  (goto-char pos)
	  (if (or (<  (- (current-column) (window-hscroll window)) 0)
		  (>= (- (current-column) (window-hscroll window)) w))
	      nil
	    t))
      t)))

(defun ipe--hrecenter (pos)
  "Return a hscroll value that will display POS."

  (let* ((window  (get-buffer-window))
	 (w       (if window (window-width window) 0))
	 (page    0))

    (when (and window
	       (or truncate-lines
		   (and truncate-partial-width-windows
			(< w truncate-partial-width-windows))))
      (save-excursion
	(goto-char pos)
	(when
	    (or (<= (- (1- (current-column)) (window-hscroll window)) 0)
		(>= (- (1- (current-column)) (window-hscroll window)) w))
	  (setq page (/ (1- (current-column)) w)))))

    (* page w)))

(defun ipe--visible-p (pos)
  "Return t, if POS is visible."

  (cond
   ((or (<  pos     (window-start))
	(>= (point) (window-end)))
    nil)

   ((or (>  pos     (window-end))
	(<  (point) (window-start)))
    nil)

   ((not (ipe--hvisible-p pos))
    nil)

   (t t)))

(defun ipe--recenter (pos)
  "Recenter the window so that POS is visible."

  (let* ((window (get-buffer-window (current-buffer)))
	 (w      (if window (window-width) 0))
	 (start)
	 (hscroll))

    (when (and window
	       (not (ipe--visible-p pos)))

      (save-excursion
	(goto-char pos)
	(recenter)
	(set-window-hscroll window 0)
	(setq start   (window-start window))
	(setq hscroll (ipe--hrecenter pos)))

      (set-window-start window start)
      (redisplay t)
      (when (or truncate-lines
		(and truncate-partial-width-windows
		     (< w truncate-partial-width-windows)))
	(set-window-hscroll window hscroll)))))

(defun ipe--property-match-delete (string property)
  "Delete substrings within STRING with text PROPERTY."

  (let* ((beg (if (get-char-property 0 property string)
		  0
		(next-single-property-change 0 property string)))
	 (end))
    (while beg
      (setq end    (next-single-property-change beg property string)
	    string (concat (substring string 0 beg)
			   (if end (substring string end) ""))
	    beg    (and end
			(next-single-property-change 0 property string)))))
  string)

(defun ipe--point-overlay-create (pos priority)
  "Create a Point Overlay at POS.

This creates an overlay that hides a single character in the buffer
and displays that character as the `'after-string' of the overlay,
giving the impression of inserting the overlay between two existing
characters at the given POS.

- POS is the position at which the overlay is to be displayed.
- PRIORITY is the priority to assign to the overlay.

\(NOTE: Insertion of a Point Overlay becomes a problem at the `'end'
of the buffer (`point-max'), so we have some special processing for
this edge case, whereby we do not display an `'after-string' at the
end of the buffer.)"

  ;; Take special care at 'point-max'.
  (let* ((beg     (if (>= pos (point-max)) (1- (point-max)) pos))
	 (end     (if (>= pos (point-max)) (point-max) (1+ pos)))
	 (overlay (make-overlay beg end))
	 (before  (if (or (< pos (point-max))
			  (< beg (point-min)))
		      ""
		    (buffer-substring beg end)))
	 (after   (if (or (>= pos (point-max))
			  (<  beg (point-min)))
		      ""
		    (buffer-substring beg end))))

    (overlay-put overlay 'category      'ipe)
    (overlay-put overlay 'before-string before)
    (overlay-put overlay 'display       "")
    (overlay-put overlay 'after-string  after)
    (overlay-put overlay 'priority      priority)

    overlay))

(defun ipe--point-overlay-move (o-name pos display
				       &optional no-after-p)
  "Move a Point Overlay at POS.

This moves an overlay that hides a single character in the buffer and
displays that character as the `'after-string' of the overlay, giving
the impression of inserting the overlay between two existing
characters at the given POS.

- O-NAME is a symbol name for the overlay to be inserted.
- POS is the position at which the overlay is to be displayed.
- DISPLAY is the string to be displayed by the overlay.
- NO-AFTER-P specifies that this should not display an
  `'after-string'.

\(NOTE: Insertion of a Point Overlay becomes a problem at the end
of the buffer (`point-max'), so we have some special processing for
this edge case, whereby we do not display an `'after-string' at end
the of the buffer.)"

  ;; Take special care at 'point-max'.
  (let* ((beg      (if (>= pos (point-max)) (1- (point-max)) pos))
	 (end      (if (>= pos (point-max)) (point-max) (1+ pos)))
	 (overlay  (eval o-name))
	 (before   (if (or (< pos (point-max))
			   (< beg (point-min)))
		       ""
		     (buffer-substring beg end)))
	 (odisplay (propertize display 'line-height t))
	 (after    (if (or (>= pos (point-max))
			   (<  beg (point-min))
			   no-after-p)
		       ""
		     (buffer-substring beg end))))

    (move-overlay overlay beg end)

    (overlay-put overlay 'category      'ipe)
    (overlay-put overlay 'before-string before)
    (overlay-put overlay 'display       odisplay)
    (overlay-put overlay 'after-string  after)))

;; -------------------------------------------------------------------
;;;; PAIR List sort functions:
;; -------------------------------------------------------------------

(defun ipe--pair-sort-by-mnemonic (pair1 pair2)
  "Order Insert Pair Edit (ipe) PAIRs by MNEMONIC.

This predicate is passed to `sort' to order `ipe' PAIRs by MNEMONIC.

- Return < 0 if the MNEMONIC of PAIR1 < the MNEMONIC of PAIR2.
- Return 0   if the MNEMONIC of PAIR1 = the MNEMONIC of PAIR2.
- Return > 0 if the MNEMONIC of PAIR1 > the MNEMONIC of PAIR2."

  (string< (car pair1) (car pair2)))

(defun ipe--pair-sort-by-open (pair1 pair2)
  "Order Insert Pair Edit (ipe) PAIRs by OPEN string.

This predicate is passed to `sort' to order `ipe' PAIRs by OPEN.

- Return < 0 if the OPEN of PAIR1 < the OPEN of PAIR2.
- Return 0   if the OPEN of PAIR1 = the OPEN of PAIR2.
- Return > 0 if the OPEN of PAIR1 > the OPEN of PAIR2."

  (string< (cadr pair1) (cadr pair2)))

(defun ipe--pair-sort-by-close (pair1 pair2)
  "Order Insert Pair Edit (ipe) PAIRs by CLOSE string.

This predicate is passed to `sort' to order `ipe' PAIRs by CLOSE.

- Return < 0 if the CLOSE of PAIR1 < the CLOSE of PAIR2.
- Return 0   if the CLOSE of PAIR1 = the CLOSE of PAIR2.
- Return > 0 if the CLOSE of PAIR1 > the CLOSE of PAIR2."

  (string< (ipe-compat--caddr pair1) (ipe-compat--caddr pair2)))

(defun ipe--pair-sort-by ()
  "Return the function used to sort `ipe' PAIR definitions."

  (cond ((equal ipe-pair-sort 'sort-by-mnemonic)
	 (lambda (pair1 pair2)
	   (ipe--pair-sort-by-mnemonic pair1 pair2)))
	((equal ipe-pair-sort 'sort-by-open)
	 (lambda (pair1 pair2)
	   (ipe--pair-sort-by-open pair1 pair2)))
	((equal ipe-pair-sort 'sort-by-close)
	 (lambda (pair1 pair2)
	   (ipe--pair-sort-by-close pair1 pair2)))
	(t
	 (lambda (pair1 pair2)
	   (ipe--pair-sort-by-mnemonic pair1 pair2)))))

;; -------------------------------------------------------------------
;;;; PAIR Definition structure accessor functions:
;; -------------------------------------------------------------------

(defun ipe--mode-pairs (mode)
  "Return the Insert Pair Edit (ipe) PAIRs for the given MODE.

Return the entry within `ipe-mode-pairs' associated with MODE.  If
MODE is not defined within `ipe-mode-pairs', return nil."

  (when (assoc mode ipe-mode-pairs)
    (let* ((mode-pairs-defn (cadr (assoc mode ipe-mode-pairs))))
      (if (symbolp mode-pairs-defn)
	  (eval mode-pairs-defn)
	mode-pairs-defn))))

(defun ipe--mode-pair (mnemonic mode)
  "Return the `ipe' PAIR for the given MNEMONIC in the given MODE.

If MNEMONIC is nil, use the global `ipe--mnemonic'.
If MODE is nil, use `major-mode'.
If MODE is a mode within `ipe-mode-pairs', return the PAIR for the
given MNEMONIC within the given MODE (if defined), else return nil."

  (let ((default-mnemonic (or mnemonic ipe--mnemonic))
	(default-mode     (or mode major-mode)))
    (assoc default-mnemonic (ipe--mode-pairs default-mode))))

(defun ipe--pair-merge (mode-pairs pairs)
  "Return the merge of MODE-PAIRS and PAIRS.

This will return an `ipe' PAIRs list that contains all of MODE-PAIRS,
and, any global PAIRS which do not already have a MNEMONIC defined in
MODE-PAIRS.

\(See variables: `ipe-mode-pairs' / `ipe-pairs'.)"

  (append
   mode-pairs
   (append (ipe-compat--mapcan
	    (lambda (e)
	      (unless (member (car e) (mapcar #'car mode-pairs))
		(list e)))
	    pairs))))

(defun ipe--pairs (mode)
  "Return the Insert Pair Edit (ipe) PAIRs for the given MODE.

This will return a combination of all of the Mode-Specific entries
within the `ipe-mode-pairs' for the given MODE, and the Global PAIRs
defined within the `ipe-pairs' variable."

  (let ((mode-pairs (ipe--mode-pairs mode)))
    (if mode-pairs
	(ipe--pair-merge mode-pairs ipe-pairs)
      ipe-pairs)))

(defun ipe--pair (&optional mnemonic mode)
  "Return the `ipe' PAIR for the given MNEMONIC in the given MODE.

If MNEMONIC is nil, use the global `ipe--mnemonic'.
If MODE is nil, use `major-mode'.
If MODE is t, only search for MNEMONIC in only the global PAIR list,
`ipe-pairs'.
If MODE is a mode within `ipe-mode-pairs', then, if a PAIR for the
given MNEMONIC within the given MODE is defined, return it, else
return the PAIR for the given MNEMONIC from the global PAIR list
`ipe-pairs'."

  (let ((default-mnemonic (or mnemonic ipe--mnemonic))
	(default-mode     (or mode major-mode)))
    (assoc default-mnemonic (ipe--pairs default-mode))))

(defun ipe--pair-empty-display (open-p)
  "Return the Insert Pair Edit (ipe) Empty PAIR string.

The Empty PAIR string is used to show the position of empty OPEN /
CLOSE strings without inserting text into the buffer on insert.

If OPEN-P is t, return an Empty PAIR OPEN string:

  `ipe-empty-open-string'

Else, return an Empty PAIR CLOSE string:

  `ipe-empty-close-string'."

  (let ((empty (if open-p
		   ipe-empty-open-string
		 ipe-empty-close-string)))

    (propertize empty
		'face      'ipe-empty-pair-highlight
		'ipe-empty t)))

(defun ipe--pair-open-string (pair)
  "Return the Insert Pair Edit (ipe) OPEN string for PAIR."

  (if pair (cadr pair) ""))

(defun ipe--pair-close-string (pair)
  "Return the Insert Pair Edit (ipe) CLOSE string for PAIR."

  (if pair (ipe-compat--caddr pair) ""))

(defun ipe--pair-intermediate-p (pair)
  "Return non-nil if an `ipe' PAIR has `Intermediate' properties."

  (let ((plist (and pair (nth 3 pair))))
    (if plist
	t
      nil)))

(defun ipe--pair-advanced-p (pair)
  "Return non-nil if an `ipe' PAIR has `Advanced' properties."

  (let ((plist (and pair (nth 3 pair))))
    (and plist
	 (member t (mapcar (lambda (x)
			     (when (plist-get plist x)
			       t))
			   '(:move-point
			     :indent-function
			     :menu
			     ;; TODO: Add regexp update matching.
			     ;; :open-regexp
			     ;; :close-regexp
			     ))))))

(defun ipe--pair-property-p (pair pname)
  "Return t if the `ipe' PAIR property PNAME is defined for PAIR."

  (if (and pair (nth 3 pair) (plist-get (nth 3 pair) pname))
      t
    nil))

(defun ipe--pair-property (pair pname)
  "Return the Insert Pair Edit (ipe) PAIR property named PNAME."

  (when (and pair (nth 3 pair))
    (plist-get (nth 3 pair) pname)))

(defun ipe--pair-property-set (pair pname value)
  "Add a property to an Insert Pair Edit (ipe) PAIR.

- PAIR is the `ipe' PAIR to which the property is to be added.
- PNAME is the name of the property.
- VALUE is the value to which the property is set.

The PAIR will be modified using `nconc'."

  (if (nth 3 pair)
      (plist-put (nth 3 pair) pname value)
    (nconc pair (list (list pname value))))
  pair)

(defun ipe--pair-movement-initial (pair)
  "Return the Insert Pair Edit (ipe) Initial Movement for a PAIR.

The Initial Movement is retrieved from the :movement property of the
`ipe' PAIR.

If this is not defined, return `'word'."

  (or (ipe--pair-property pair :movement)
      'word))

(defun ipe--pair-infix-string (pair)
  "Return the Insert Pair Edit (ipe) INFIX string for a PAIR.

The INFIX string is retrieved from the :infix property of the `ipe'
PAIR.

If this is not defined, return an empty string."

  (or (ipe--pair-property pair :infix)
      ""))

(defun ipe--pair-escapes (pair)
  "Return the Insert Pair Edit (ipe) ESCAPES list for a PAIR.

The ESCAPES list is retrieved from the :escapes property of the `ipe'
PAIR.

If this is not defined, return nil."

  (ipe--pair-property pair :escapes))

(defun ipe--pair-indent-function (pair)
  "Return the Insert Pair Edit (ipe) indent function for a PAIR.

The indent function is retrieved from the :indent-function property of
the `ipe' PAIR.

If this is not defined, return nil."

  (let ((indent-fn (ipe--pair-property pair :indent-function)))
    (cond ((not indent-fn)          nil)
	  ((eq indent-fn 'current)  'ipe--indent-current)
	  ((eq indent-fn 'previous) 'ipe--indent-previous)
	  ((and (stringp   indent-fn)
		(functionp (intern-soft indent-fn)))
	   (intern-soft indent-fn))
	  (t nil))))

;; -------------------------------------------------------------------
;;;; MNEMONICS:
;; -------------------------------------------------------------------

(defun ipe--mode-mnemonic-list (mode)
  "Return the list of `ipe' PAIR MNEMONICS for the given MODE."

  (mapcar (lambda (pair) (car pair))
	  (if ipe-pair-sort
	      (sort (copy-sequence (ipe--mode-pairs mode))
		    (ipe--pair-sort-by))
	    (ipe--mode-pairs mode))))

(defun ipe--mnemonic-list (&optional mode)
  "Return the merged list of `ipe' PAIR MNEMONICS for the given MODE.

This will return a list of Insert Pair Edit (ipe) MNEMONICS that can
be used to look-up PAIR definitions from both the `ipe-pairs' and
`ipe-mode-pairs' variables for the given MODE."

  (mapcar (lambda (pair) (car pair))
	  (if ipe-pair-sort
	      (sort (copy-sequence (ipe--pairs mode))
		    (ipe--pair-sort-by))
	    (ipe--pairs mode))))

(defun ipe--mnemonic-describe (mnemonic &optional mode)
  "Return a description of the `ipe' PAIR for MNEMONIC in MODE.

This will return a string of the form:

  \"Opening \\='OPEN' and Closing \\='CLOSE'\"

The returned description highlights the OPEN and CLOSE strings for the
Insert Pair Edit (ipe) PAIR using `ipe-open-highlight' and
`ipe-close-highlight'.

To avoid excessive description lengths, if either the OPEN or CLOSE
string is greater than `ipe-elide-description' characters long, the
string will be truncated at `ipe-elide-description' characters, and
the post-fix \"...\" will be added to the string."

  (let* ((pair  (ipe--pair mnemonic mode))
	 (open  (replace-regexp-in-string "[\r\n]+" ""
					  (ipe--pair-open-string pair)))
	 (close (replace-regexp-in-string "[\r\n]+" ""
					  (ipe--pair-close-string pair))))
    (when (and ipe-elide-description
	       (integerp ipe-elide-description)
	       (> ipe-elide-description 0))
      (when (> (length open) ipe-elide-description)
	(setq open (concat
		    (substring open 0 ipe-elide-description)
		    "...")))
      (when (> (length close) ipe-elide-description)
	(setq close (concat
		     (substring close 0 ipe-elide-description)
		     "..."))))
    (concat "Opening '" open "' and Closing '" close "'")))

(defun ipe--mnemonic-annotate (mnemonic &optional mode)
  "Return a suitable annotation for the given `ipe' MNEMONIC.

This is called as an :annotation-function within `completing-read'.

MODE, if non-nil, specifies the `major-mode' of the Mode-Specific PAIR
for which the annotation is to be returned."

  ;; We use `ipe--major-mode' rather than `major-mode', because we
  ;; want the major mode of the actual buffer in which the
  ;; `ipe-insert-pair-edit' is being run, but the annotate function
  ;; itself will be called within `help-mode', and, as such will set
  ;; the `major-mode' to `help-mode'.
  (concat " - Insert "
	  (ipe--mnemonic-describe mnemonic (or mode ipe--major-mode))))

(defun ipe-move-by-install (symbol move-by-fn pos-set-fn desc)
  "Add an entry to `ipe-move-by-movements'.

- SYMBOL is the identifier defining the type of movement.

- MOVE-BY-FN is a function that returns the position of the OPEN and
  CLOSE strings for a given `lexical unit' movement mode.  (See:
  `ipe-move-by-movements')

- POS-SET-FN is an (optional) function that is called whenever
  `ipe--pos-property-set' is called to perform movement specific
  updates to the `ipe--pair-pos-list' state variable.  (See:
  `ipe-move-by-movements')

- DESC is the text output to the echo area to describe the
  movement."

  (setq ipe-move-by-movements
	(append
	 (ipe--list-filter
	  ipe-move-by-movements
	  (lambda (x) (equal (car x) symbol)))
	 (list (list symbol move-by-fn pos-set-fn desc)))))

(defun ipe--move-by-function ()
  "Return the function to calculate `ipe' OPEN and CLOSE positions.

This looks up the move-by function for the current `ipe--movement'
within the `ipe--move-by-function' list.

If no move-by function is found, it will return the default movement
function for the `ipe--movement' equal to `'word'."

  (let ((move-by (cadr (assoc ipe--movement ipe-move-by-movements))))
    (unless move-by
      (message "Unrecognised movement '%s' for PAIR '%s'.  \
Defaulting to movement by 'words'."
	       ipe--movement
	       (ipe--mnemonic-describe ipe--mnemonic))
      (setq ipe--movement 'word
	    move-by (ipe-compat--caddr
		     (assoc ipe--movement ipe-move-by-movements))))

    move-by))

(defun ipe--movement-set (movement)
  "Set the value of `ipe--movement' to MOVEMENT."

  (setq ipe--movement
	movement
	ipe--pos-property-set-callback
	(ipe-compat--caddr (assoc movement ipe-move-by-movements))))

;; -------------------------------------------------------------------
;;;; Undo processing:
;; -------------------------------------------------------------------

(defun ipe--undo-start ()
  "Set the point at which to undo a `ipe' change.

If not already set, saves the current `buffer-undo-list' position in
`ipe--undo-handle'."

  (unless ipe--undo-handle
    (setq ipe--undo-handle (prepare-change-group))
    (activate-change-group ipe--undo-handle)))

(defun ipe--undo-accept ()
  "Accept the current change set made by `ipe'.

Amalgamates the changes made while in `ipe-edit-mode' into a single
`undo'."

  (when ipe--undo-handle
    (ipe--safecall 'undo-amalgamate-change-group ipe--undo-handle)
    (condition-case nil
	(accept-change-group ipe--undo-handle)
      (error nil))
    (setq ipe--undo-handle nil)))

(defun ipe--undo-abort ()
  "Abort the current change set made by `ipe'.

Reverts the buffer back to the undo state saved by `ipe--undo-start'."

  (when ipe--undo-handle
    (condition-case nil
	(cancel-change-group ipe--undo-handle)
      (error nil))
    (setq ipe--undo-handle nil)))

;; -------------------------------------------------------------------
;;;; PAIR Position structure accessor functions:
;; -------------------------------------------------------------------

(defun ipe--pos-location (pos beg end)
  "Return the position of POS between BEG and END.

If POINT is less than or equal to BEG return `'before'.
If POINT is greater than or equal to END return `'after'.
Else if POINT is between BEG and END, return its offset from BEG."

  (if (< pos beg)
      'before
    (if (>= pos end)
	'after
      (- pos beg))))

(defun ipe--pos-count ()
  "Return a count of the number `ipe' PAIR Positions."

  (length ipe--pair-pos-list))

(defun ipe--pos-property (n pname)
  "Return the PNAME property of the `N'th `ipe' PAIR Position.

If `N' is nil, return the value of the PNAME property of the ALIST
attribute of the first entry within the `ipe--pair-pos-list'.
Otherwise, return the value of the PNAME property of the ALIST
attribute of the `N'th entry within the `ipe--pair-pos-list'.

If there are less than `N' `ipe' PAIR Positions currently defined,
return nil."

  (cadr (assoc pname (ipe-compat--caddr (nth n ipe--pair-pos-list)))))

(defun ipe--pos-property-set (n &rest args)
  "Set PNAME = VALUE for the `N'th `ipe' PAIR Position.

ARGS is a list (PNAME VALUE PNAME VALUE PNAME VALUE ...)

If `N' is nil, for each PNAME, set the VALUE of the PNAME property
within the ALIST of the first entry within the `ipe--pair-pos-list'.
Otherwise, for each PNAME, set the VALUE of the PNAME property within
the ALIST of the `N'th entry within the `ipe--pair-pos-list'.

`N' counts from 0.

If there are less than `N'+1 `ipe' PAIR Positions currently defined
within `ipe--pair-pos-list', add empty PAIR Positions to
`ipe--pair-pos-list' to pad it out to `N'+1 entries."

  (let* ((pair-pos-list (ipe--list-pad ipe--pair-pos-list (1+ n)))
	 (pair-pos      (nth n pair-pos-list))
	 (properties    args)
	 (alist         (ipe-compat--caddr pair-pos))
	 (new-alist     alist)
	 (pname)
	 (value))

    (while properties
      (setq pname      (car  properties)
	    value      (cadr properties)
	    new-alist  (ipe--alist-update new-alist pname value)
	    properties (cddr properties)))

    (setcar (nthcdr n pair-pos-list)
	    (list (car pair-pos) (cadr pair-pos) new-alist))

    (setq ipe--pair-pos-list pair-pos-list)

    ;; Call the property call back for each property.
    (setq properties args)

    (while properties
      (setq pname      (car  properties)
	    value      (cadr properties)
	    properties (cddr properties))

      (when (functionp ipe--pos-property-set-callback)
	(funcall ipe--pos-property-set-callback
		 n
		 pname
		 value)))))

(defun ipe--pos-open (n)
  "Return the `N'th Insert Pair Edit (ipe) OPEN Position.

Return the POS-OPEN attribute of `N'th entry within the
`ipe--pair-pos-list'.

If there are less than `N' `ipe' PAIR Positions currently defined,
return nil."

  (car (nth n ipe--pair-pos-list)))

(defun ipe--pos-open-set (n pos)
  "Set the `N'th Insert Pair Edit (ipe) OPEN Position to POS.

Set the POS-OPEN attribute within the `N'th entry within the
`ipe--pair-pos-list'.

`N' counts from 0.

If there are less than `N'+1 `ipe' PAIR Positions currently defined
within `ipe--pair-pos-list', add empty PAIR Positions to
`ipe--pair-pos-list' to pad it out to `N'+1 entries."

  (let* ((pair-pos-list (ipe--list-pad ipe--pair-pos-list (1+ n)))
	 (pair-pos      (nth n pair-pos-list)))
    (setcar (nthcdr n pair-pos-list)
	    (list pos (cadr pair-pos) (ipe-compat--caddr pair-pos)))
    (setq ipe--pair-pos-list pair-pos-list)))

(defun ipe--pos-open-insert (n)
  "Return the `N'th Insert Pair Edit (ipe) OPEN insert string.

The OPEN string is retrieved from either:

* The :open property of the `N'th entry within `ipe--pair-pos-list',
  or, if no :open property is defined;
* The OPEN definition within the `ipe-mode-pairs' / `ipe-pairs' for
  the given `ipe--mnemonic' MNEMONIC in the current `major-mode'."

  (or (ipe--pos-property n :open)
      (ipe--pair-open-string (ipe--pair))))

(defun ipe--pos-open-display (n)
  "Return the string to be displayed as the `N'th `ipe' OPEN overlay.

The OPEN string is retrieved from either:

* The :open property of the `N'th entry within `ipe--pair-pos-list',
  or, if no :open property is defined;
* The OPEN definition within the `ipe-mode-pairs' / `ipe-pairs' for
  the given `ipe--mnemonic' MNEMONIC in the current `major-mode'.

The returned string is highlighted with `ipe-open-highlight'.

If the returned string is either nil, or the empty string,
`ipe--pair-empty-display' is returned instead."

  (let* ((open (ipe--pos-open-insert n)))
    (if (and open (> (length open) 0))
	(propertize open 'face 'ipe-open-highlight)
      (ipe--pair-empty-display t))))

(defun ipe--pos-close (n)
  "Return the `N'th Insert Pair Edit (ipe) CLOSE Position.

Return the POS-CLOSE attribute of `N'th entry within the
`ipe--pair-pos-list'.

If there are less than `N' `ipe' PAIRs currently defined, return nil."

  (cadr (nth n ipe--pair-pos-list)))

(defun ipe--pos-close-set (n pos)
  "Set the `N'th Insert Pair Edit (ipe) CLOSE Position to POS.

Set the POS-CLOSE attribute within the `N'th entry within the
`ipe--pair-pos-list'.

`N' counts from 0.

If there are less than `N'+1 `ipe' PAIR Positions currently defined
within `ipe--pair-pos-list', add empty PAIR Positions to
`ipe--pair-pos-list' to pad it out to `N'+1 entries."

  (let* ((pair-pos-list (ipe--list-pad ipe--pair-pos-list (1+ n)))
	 (pair-pos      (nth n pair-pos-list)))
    (setcar (nthcdr n pair-pos-list)
	    (list (car pair-pos) pos (ipe-compat--caddr pair-pos)))
    (setq ipe--pair-pos-list pair-pos-list)))

(defun ipe--pos-close-insert (n)
  "Return the `N'th Insert Pair Edit CLOSE insert string.

The CLOSE string is retrieved from either:

* The :close property of the `N'th entry within `ipe--pair-pos-list',
  or, if no :close property is defined;
* The CLOSE definition within the `ipe-mode-pairs' / `ipe-pairs' for
  the given `ipe--mnemonic' MNEMONIC in the current `major-mode'."

  (or (ipe--pos-property n :close)
      (ipe--pair-close-string (ipe--pair))))

(defun ipe--pos-close-display (n)
  "Return the string to be displayed as the `N'th `ipe' CLOSE overlay.

The CLOSE string is retrieved from either:

* The :close property of the `N'th entry within `ipe--pair-pos-list',
  or, if no :close property is defined;
* The CLOSE definition within the `ipe-mode-pairs' / `ipe-pairs' for
  the given `ipe--mnemonic' MNEMONIC in the current `major-mode'.

The returned string is highlighted with `ipe-close-highlight'.

If the returned string is either nil, or the empty string,
`ipe--pair-empty-display' is returned instead."

  (let* ((close (ipe--pos-close-insert n)))
    (if (and close (> (length close) 0))
	(propertize close 'face 'ipe-close-highlight)
      (ipe--pair-empty-display nil))))

(defun ipe--pos-infix-insert (n)
  "Return the `N'th Insert Pair Edit (ipe) INFIX insert string.

The INFIX string is retrieved from either:

* The :infix property of the `N'th entry within `ipe--pair-pos-list',
  or, if no :infix property is defined;
* The INFIX definition within the `ipe-mode-pairs' / `ipe-pairs' for
  the given `ipe--mnemonic' MNEMONIC in the current `major-mode'."

  (or (ipe--pos-property n :infix)
      (ipe--pair-infix-string (ipe--pair))))

(defun ipe--pos-infix-display (n)
  "Return the string to be displayed as the `N'th `ipe' INFIX overlay.

The INFIX string is retrieved from either:

* The :infix property of the `N'th entry within `ipe--pair-pos-list',
  or, if no :infix property is defined;
* The INFIX definition within the `ipe-mode-pairs' / `ipe-pairs' for
  the given `ipe--mnemonic' MNEMONIC in the current `major-mode'.

The returned string is highlighted with `ipe-infix-highlight'."

  (propertize (ipe--pos-infix-insert n)
	      'face 'ipe-infix-highlight))

(defun ipe--pos-point (n &optional pos)
  "Return :point property for the `N'th PAIR Position.

If POS, return the original value, and set :point to POS."

  (let ((ipe-point (ipe--pos-property n :point)))
    (when pos
      (ipe--pos-property-set n :point pos))
    ipe-point))

(defun ipe--pos-contains-p (n pos)
  "Return t if POS is within the `N'th `ipe' OPEN and CLOSE Position."

  (and (<= (ipe--pos-open n) pos)
       (<= pos (ipe--pos-close n))))

(defun ipe--pos-adjacent-p (n)
  "Return t if the `N'th `ipe' OPEN and CLOSE are adjacent."

  (= (ipe--pos-open n) (ipe--pos-close n)))

(defun ipe--pos-adjacent-2-p (i j)
  "Return t if two `ipe' PAIRs are adjacent.

This will return t, if the CLOSE string for the `I'th `ipe' PAIR is at
the some position as the OPEN string for the `J'th `ipe' PAIR."

  (= (ipe--pos-close i) (ipe--pos-open j)))

(defun ipe--pos-eob1-p (n)
  "Return t if the `N'th `ipe' PAIR surrounds the char at the eob.

This will return t, if the OPEN string for the `N'th PAIR and the
CLOSE string for the `N'th PAIR surround the last character in the
buffer.  This predicate is needed so that we can concatenate the OPEN
and CLOSE overlays used to display PAIRs at the end of the buffer, as
we need special processing to display around the final character."

  (and (=  (1- (point-max))   (ipe--pos-open n))
       (>= (ipe--pos-close n) (point-max))))

(defun ipe--pos-eob1-2-p (i j)
  "Return t if two `ipe' PAIRs surround the char at the eob.

This will return t, if the CLOSE string for the `I'th `ipe' PAIR and
the OPEN string for the `J'th `ipe' PAIR surround the last character
in the buffer.  This predicate is needed so that we can concatenate
the OPEN and CLOSE overlays used to display PAIRs at the end of the
buffer, as we need special processing to display around the final
character."

  (and (>= i 0) (>= j 0)
       (= (ipe--pos-close i) (1- (point-max)))
       (= (ipe--pos-open j)  (point-max))))

(defun ipe--pos-list-nearest (pos)
  "Return the index of the `ipe' PAIR closest to POS.

This searches the `ipe--pair-pos-list' for the nearest PAIR to POS."

  (let ((distance (point-max))
	(nearest  0))
    (dotimes (n (ipe--pos-count))
      (if (and (<= (ipe--pos-open n) pos)
	       (<  pos (ipe--pos-close n)))
	  (setq nearest  n
		distance 0)
	(if (and (<= pos (ipe--pos-open n))
		 (<  (- (ipe--pos-open n) pos) distance))
	    (setq nearest n
		  distance (- (ipe--pos-open n) pos))
	  (if (and (<= (ipe--pos-close n) pos)
		   (< (- pos (ipe--pos-close n) distance)))
	      (setq nearest n
		    distance (- pos (ipe--pos-close n)))))))
    nearest))

(defun ipe--pos-list-normalize ()
  "Adjust the `ipe' PAIR Positions so there are no overlaps.

This adjusts the `ipe--pair-pos-list' so that entries within the list
are ordered by POS-OPEN, and so that for any two consecutive entries
within `ipe--pair-pos-list', the POS-CLOSE of the first entry will be
<= the POS-OPEN of the second."

  (setq ipe--pair-pos-list
	(sort
	 (let ((result))
	   (dolist (x ipe--pair-pos-list)
	     (when (and (car x) (cadr x))
	       (setq result (append result (list x)))))
	   result)
	 (lambda (x y) (or (< (car x) (car y))
			   (and (= (car x)  (car y))
				(< (cadr x) (cadr y)))))))

  (dotimes (i (1- (ipe--pos-count)))
    (dotimes (j (- (ipe--pos-count) i 1))
      (let ((n (+ i j 1)))
	(when (> (ipe--pos-close i) (ipe--pos-open n))
	  (if (> (ipe--pos-close i) (ipe--pos-close n))
	      (ipe--pos-close-set i (ipe--pos-open n))
	    (ipe--pos-open-set n (ipe--pos-close i))))))))

(defun ipe--pos-list-singular ()
  "Ensure there is only one `ipe' PAIR Position defined.

This adjusts the `ipe--pair-pos-list' so that it contains, at most,
one entry."

  (when ipe--pair-pos-list
    (let ((count (1- (ipe--pos-count))))
      (dotimes (n count)
	(ipe--pair-pos-hide (- count n))))))

;; -------------------------------------------------------------------
;;;; PAIR Position aware buffer editing functions:
;; -------------------------------------------------------------------

(defun ipe--pos-insert (string pos-point)
  "Insert STRING at POINT, modifying the `ipe--pair-pos-list'.

This inserts string as per `insert', but updates the positions stored
within the `ipe--pair-pos-list' to account for the shift in positions
caused by inserting string at POINT.

If POS-POINT is non-nil, it is expected to indicate how to adjust the
:point position when POINT = IPE-POINT.

- `'before' means that the string is to be inserted before the
  IPE-POINT and the value of :point should be changed.
- a number N means that the string to be inserted will contain
  IPE-POINT, and that the value of the :point property should be
  offset by N from the insertion point.
- `'after' means that the string is to be inserted after the IPE-POINT
  and the value of :point should NOT be changed.

Return the number of characters inserted."

  (let ((point    (point))
	(inserted (length string)))

    (dotimes (n (ipe--pos-count))
      (let ((pos-open  (ipe--pos-open n))
	    (pos-close (ipe--pos-close n))
	    (ipe-point (ipe--pos-point n)))

	;; If inserting before OPEN, adjust POS-OPEN.
	(when (< point pos-open)
	  (ipe--pos-open-set n (+ pos-open inserted)))

	;; If inserting before CLOSE, adjust POS-CLOSE.
	(when (< point pos-close)
	  (ipe--pos-close-set n (+ pos-close inserted)))

	;; If inserting before :point, adjust :point.
	(when (and (integerp ipe-point) (< point ipe-point))
	  (ipe--pos-point n (+ ipe-point inserted)))

	;; If inserting at :point, special behaviour.
	(when (and (integerp ipe-point) (equal point ipe-point))
	  (cond
	   ;; Insertion is before point, adjust.
	   ((equal pos-point 'before)
	    (ipe--pos-point n (+ ipe-point inserted)))

	   ;; Insertion contains point, adjust by offset.
	   ((integerp pos-point)
	    (ipe--pos-point n (+ ipe-point pos-point)))

	   ;; Insertion is after point, do not adjust.
	   ((equal pos-point 'after))))))

    ;; Insert the string.
    (insert string)
    inserted))

(defun ipe--pos-delete (beg end)
  "Delete text from BEG to END, modifying the `ipe--pair-pos-list'.

This deletes text as per `delete-region', but also updates the
positions stored within the `ipe--pair-pos-list' to account for the
shift in positions caused by deleting the text between BEG and END.

Return the number of characters deleted."

  (let ((deleted (- end beg)))
    (dotimes (n (ipe--pos-count))
      (let ((pos-open  (ipe--pos-open n))
	    (pos-close (ipe--pos-close n))
	    (ipe-point (ipe--pos-point n)))

	;; If deleting before OPEN, adjust POS-OPEN.
	(when (and pos-open (> pos-open beg))
	  (ipe--pos-open-set n
			     (if (< pos-open end)
				 beg
			       (- pos-open deleted))))

	;; If deleting before CLOSE, adjust POS-CLOSE.
	(when (and pos-close (>= pos-close beg))
	  (ipe--pos-close-set n
			      (if (< pos-close end)
				  beg
				(- pos-close deleted))))

	;; If deleting before :point, adjust the :point.
	(when (and (integerp ipe-point) (> ipe-point beg))
	  (ipe--pos-point n (if (< ipe-point end)
				beg
			      (- ipe-point deleted))))))

    ;; Delete the text.
    (delete-region beg end)
    deleted))

(defun ipe--pos-delete-l (pos length)
  "Delete text at POS, modifying the `ipe--pair-pos-list'.

Delete LENGTH characters from the buffer (starting at POS.)  The
positions stored within the `ipe--pair-pos-list' are updated to
account for the shift in positions caused by deleting the text.

Return the number of characters deleted."

  (ipe--pos-delete pos (+ pos length)))

(defun ipe--pos-replace (beg end string)
  "Replace the text from BEG to END with STRING.

This replaces the given region with STRING, but updates the positions
stored within the `ipe--pair-pos-list' to account for the shift in
positions caused by replacing the text at the given position.

Return the number of characters inserted.  (This will be a negative
number if the replacement string was smaller than the region
replaced.)"

  (save-excursion
    (goto-char beg)

    (let* ((len        (- end beg))
	   (len-string (length string))
	   (inserted   (- len-string len)))

      (cond
       ((= len len-string)
	(delete-region beg end)
	(insert string))

       ((> len len-string)
	(delete-region beg (+ beg len-string))
	(insert string)
	(ipe--pos-delete (+ beg len-string) end))

       ((< len len-string)
	(delete-region beg end)
	(insert (substring string 0 len))

	(dotimes (n (ipe--pos-count))
	  (let ((pos-open  (ipe--pos-open n))
		(pos-close (ipe--pos-close n))
		(ipe-point (ipe--pos-point n)))

	    ;; If inserting before OPEN, adjust POS-OPEN.
	    (when (< (point) pos-open)
	      (ipe--pos-open-set n (+ pos-open inserted)))

	    ;; If inserting before CLOSE, adjust POS-CLOSE.
	    (when (<= (point) pos-close)
	      (ipe--pos-close-set n (+ pos-close inserted)))

	    ;; If inserting at :point, special behaviour.
	    (when (and (integerp ipe-point) (<= (point) ipe-point))
	      (ipe--pos-point n (+ ipe-point inserted)))))

	(insert (substring string len len-string))))
      inserted)))

(defun ipe--pos-recenter (n &optional close)
  "Recenter the window so that the `N'th `ipe' PAIR is visible.

If CLOSE is non-nil, ensure that the `N'th CLOSE overlay is visible.
If CLOSE is nil, ensure that the `N'th OPEN overlay is visible."

  (let ((pos (if close (ipe--pos-close n) (ipe--pos-open n))))
    (ipe--recenter pos)))

(defun ipe--insert-overlay (overlay pos-point)
  "Insert OVERLAY into the buffer.

This inserts the text displayed by an OVERLAY into the buffer.

Insertions / deletions take into account the current
`ipe--pair-pos-list' positions.

POS-POINT is expected to indicate how to adjust the :point
position when POINT = IPE-POINT.

- `'before' means that the text is to be inserted before the IPE-POINT
  and the value of :point should be changed.
- a number N means that the text to be inserted will contain
  IPE-POINT, and that the value of :point property should be offset by
  N from the insertion point.
- `'after' means that the text is to be inserted after the IPE-POINT
  and the value of :point should NOT be changed.

Return the number of characters inserted."

  (save-excursion
    (let* ((beg           (overlay-start overlay))
	   (end           (overlay-end   overlay))
	   (before-string (overlay-get   overlay 'before-string))
	   (display       (overlay-get   overlay 'display))
	   (after-string  (overlay-get   overlay 'after-string))
	   (inserted      0))

      (when beg
	(goto-char beg)

	(when before-string
	  (setq before-string (substring-no-properties before-string)
		inserted (+ inserted (ipe--pos-insert before-string pos-point))))

	(when display
	  (setq display (substring-no-properties
			 (ipe--property-match-delete display 'ipe-empty))
		inserted (+ inserted (ipe--pos-insert display pos-point))))

	(when after-string
	  (setq after-string (substring-no-properties after-string)
		inserted (+ inserted (ipe--pos-insert after-string 'after))))

	(setq inserted (- inserted
			  (ipe--pos-delete (+ beg inserted) (+ end inserted)))))
      inserted)))

;; -------------------------------------------------------------------
;;;; Indentation:
;; -------------------------------------------------------------------

(defun ipe--indent-current (pos-open _pos-close _location)
  "Return a string representing the indentation for an `ipe' PAIR.

- POS-OPEN is the location of the PAIR OPEN string.
- POS-CLOSE is the location of the PAIR CLOSE string.
- LOCATION should be either:

  * `'open'
  * `'close'
  * `'infix'

  indicating what type of indentation is to be returned.

Indentation is calculated from the leading whitespace of first current
line prior to POS-OPEN."

  (save-excursion
    (goto-char (or pos-open 1))
    (beginning-of-line)
    (while (and (not (bobp))
		(looking-at "^[\\ ]*$"))
      (forward-line -1))
    (re-search-forward "^\\([\\ ]*\\)" (point-max) t)
    (match-string 1)))

(defun ipe--indent-previous (pos-open _pos-close _location)
  "Return a string representing the indentation for an `ipe' PAIR.

- POS-OPEN is the location of the PAIR OPEN string.
- POS-CLOSE is the location of the PAIR CLOSE string.
- LOCATION should be either:

  * `'open'
  * `'close'
  * `'infix'

  indicating what type of indentation is to be returned.

Indentation is calculated from the leading whitespace of first
non-whitespace-only line prior to POS-OPEN."

  (save-excursion
    (goto-char (or pos-open 1))
    (forward-line -1)
    (while (and (not (bobp))
		(looking-at "^[\\ ]*$"))
      (forward-line -1))
    (re-search-forward "^\\([\\ ]*\\)" (point-max) t)
    (match-string 1)))

(defun ipe--indent-at (n location)
  "Return the indentation for the `N'th `ipe' PAIR.

- LOCATION should be either:

  * `'open'
  * `'close'
  * `'infix'

  indicating what type of indentation is to be returned.

The indentation function to be used is retrieved from the
:indent-function property of the current `ipe' PAIR definition."

  (let* ((pair      (ipe--pair))
	 (indent-fn (ipe--pair-indent-function pair)))
    (if (or (not (equal ipe--movement 'line))
	    (not indent-fn))
	""
      (let* ((indent (funcall indent-fn
			      (ipe--pos-open n)
			      (ipe--pos-close n)
			      location)))
	(cond
	 ((equal location 'open)
	  (propertize indent 'face 'ipe-open-highlight))
	 ((equal location 'close)
	  (propertize indent 'face 'ipe-close-highlight))
	 ((equal location 'infix)
	  (propertize indent 'face 'ipe-infix-highlight))
	 (t
	  indent))))))

(defun ipe--indent-string (string indent &optional close-p)
  "Return a STRING indented by INDENT.

Each line within the STRING will be prefixed with the INDENT string.
If CLOSE-P is non-nil, the first line is not indented."

  (concat (if close-p "" indent)
	  ;; TODO: Check for other eol sequences.
	  (replace-regexp-in-string "\n"
				    (concat "\n" indent)
				    string)))

(defun ipe--indent-escape (string)
  "Return STRING with any embedded newlines escaped for indentation."

  (replace-regexp-in-string "\n" "\n *" (regexp-quote string)))

;; -------------------------------------------------------------------
;;;; OPEN strings:
;; -------------------------------------------------------------------

(defun ipe--open-overlay (n)
  "Return the `N'th overlay of `ipe--open-overlays'.

If there are less than `N'+1 `ipe--open-overlays' currently defined,
pad it out to `N'+1 entries, and return a newly created OPEN
overlay."

  (when (>= n (length ipe--open-overlays))
    (ipe-dotimes (1+ (- n (length ipe--open-overlays)))
      (let ((open-overlay (ipe--point-overlay-create (point) 5)))
	(overlay-put open-overlay
		     'help-echo
		     "OPEN string for 'ipe' PAIR")
	(if ipe--open-overlays
	    (nconc ipe--open-overlays (list open-overlay))
	  (setq ipe--open-overlays (list open-overlay))))))
  (nth n ipe--open-overlays))

(defun ipe--open-init-pos (n pos &optional arg)
  "Return the position where the `N'th `ipe' OPEN is displayed.

Return the value of the `N'th `ipe--pos-open' by searching around POS
for a starting OPEN position using the lexical units defined by
`ipe--movement'.

The number of lexical units searched is determined by both the
`ipe-prefix-moves-close-p' flag, and, the ARG parameter.

If `ipe-prefix-moves-close-p' is non-nil, ARG is ignored, and OPEN
initialization is as if ARG was one.

If `ipe-prefix-moves-close-p' is nil, and, ARG is nil or one, and, POS
is already looking at the beginning of the lexical unit, sets the
value of the current `ipe--pos-open' to the current POS.

Otherwise, if `ipe-prefix-moves-close-p' is nil, and ARG is greater
than one, sets the value of the current `ipe--pos-open' by searching
backwards ARG lexical units from POS."

  (let* ((move-by  (ipe--move-by-function))
	 (pair     (ipe--pair))
	 (pos-open (or pos (ipe--pos-open n)))
	 (units    (if arg
		       (if ipe-prefix-moves-close-p
			   (if (< arg 0) (- arg) 0)
			 (if (listp arg)
			     (car arg)
			   arg))
		     0)))

    (funcall move-by
	     pair
	     n
	     'open
	     'init
	     pos-open
	     (ipe--pos-close n)
	     units)))

(defun ipe--open-init (n pos arg)
  "Initialize the position where the `N'th `ipe' OPEN is displayed.

Sets the value of the `N'th `ipe--pos-open' by searching around POS
for a starting OPEN position using the lexical units defined by
`ipe--movement'.

The number of lexical units searched is determined by both the
`ipe-prefix-moves-close-p' flag, and, the ARG parameter.

If `ipe-prefix-moves-close-p' is non-nil, ARG is ignored, and OPEN
initialization is as if ARG was one.

If `ipe-prefix-moves-close-p' is nil, and, ARG is nil or one, and, POS
is already looking at the beginning of the lexical unit, this function
sets the value of the current `ipe--pos-open' to the current POS.

Otherwise, if `ipe-prefix-moves-close-p' is nil, and ARG is greater
than one, this function sets the value of the current `ipe--pos-open'
by searching backwards ARG lexical units from POS."

  (let ((pair (ipe--pair)))
    (ipe--pos-open-set n (ipe--open-init-pos n pos arg))
    (unless (ipe--pos-property n :point-open)
      (ipe--pos-property-set n
			     :point-open
			     (length (ipe--pair-open-string pair))))))

(defun ipe--open-hide (n)
  "Delete the `N'th Insert Pair Edit (ipe) OPEN overlay.

Remove the overlay (`ipe--open-overlays') displaying the `N'th OPEN
string from the current buffer."

  (let ((overlay (ipe--open-overlay n)))
    (when (overlayp overlay)
      (overlay-put overlay 'display "")
      (delete-overlay overlay))))

(defun ipe--open-show (n open &optional no-after-p)
  "Display the `N'th Insert Pair Edit OPEN string as an overlay.

Display the OPEN string as an overlay (`ipe--open-overlays') in
the current buffer at `ipe--pos-open'.

If NO-AFTER-P is non-nil, the OPEN string is assumed to be part of a
PAIR surrounding the final character in the buffer, and the generated
overlay will not display an `'after-string'."

  ;; Check for an empty buffer.  We can't insert an overlay in an
  ;; empty buffer, so insert the PAIR.
  (if (>= (point-min) (point-max))
      ;; Call Ahead.  Defined below.
      (ipe--pair-pos-insert-empty)
    (let ((indent (ipe--indent-at n 'open)))
      (ipe--point-overlay-move (ipe--open-overlay n)
			       (ipe--pos-open n)
			       (ipe--indent-string open indent)
			       no-after-p))))

(defun ipe--open-set-face (n face)
  "Set the FACE for the `N'th Insert Pair Edit OPEN string."

  (let* ((overlay   (ipe--open-overlay n))
	 (display   (overlay-get overlay 'display))
	 (string    (substring-no-properties display))
	 (redisplay (propertize string 'face face)))
    (overlay-put overlay 'display redisplay)))

(defun ipe--open-move (units action)
  "Move the Insert Pair Edit (ipe) OPEN position.

- UNITS If non-nil, the number of movement units to move the OPEN
  overlay.  Movement units are determined by the current value of
  `ipe--movement'.
- ACTION is either `'beg', `'up', `'backward', `'forward', `'down',
  `'end', or `'reset'."

  (let ((move-by (ipe--move-by-function))
	(pair    (ipe--pair)))

    ;; Reset OPEN / CLOSE using the appropriate 'move-by'.
    (dotimes (n (ipe--pos-count))

      ;; Delete the overlays.
      (ipe--open-hide n)
      (ipe--close-hide n)

      (ipe--pos-open-set n (funcall move-by
				    pair
				    n
				    'open
				    action
				    (ipe--pos-open n)
				    (ipe--pos-close n)
				    units))

      (ipe--pos-close-set n (funcall move-by
				     pair
				     n
				     'close
				     action
				     (ipe--pos-close n)
				     (ipe--pos-open n)
				     units t))

      (when (> (ipe--pos-open n) (ipe--pos-close n))
	(ipe--pos-close-set n (funcall move-by
				       pair
				       n
				       'close
				       'reset
				       (ipe--pos-open n)
				       (ipe--pos-close n)
				       1))))

    ;; Re-display the overlays in the new positions.
    (ipe--pos-recenter 0)))

;; -------------------------------------------------------------------
;;;; CLOSE strings:
;; -------------------------------------------------------------------

(defun ipe--close-overlay (n)
  "Return the `N'th overlay of `ipe--close-overlays'.

If there are less than `N'+1 `ipe--close-overlays' currently defined,
pad it out to `N'+1 entries, and return a newly created CLOSE
overlay."

  (when (>= n (length ipe--close-overlays))
    (ipe-dotimes (1+ (- n (length ipe--close-overlays)))
      (let ((close-overlay (ipe--point-overlay-create (point) 5)))
	(overlay-put close-overlay
		     'help-echo
		     "CLOSE string for 'ipe' PAIR")
	(if ipe--close-overlays
	    (nconc ipe--close-overlays (list close-overlay))
	  (setq ipe--close-overlays (list close-overlay))))))
  (nth n ipe--close-overlays))

(defun ipe--close-init-pos (n pos &optional arg)
  "Return the position where the `N'th `ipe' CLOSE is displayed.

Return the value of the `N'th `ipe--pos-close' by searching around
POS for a starting CLOSE position using the lexical units defined by
`ipe--movement'.

The number of lexical units searched is determined by both the
`ipe-prefix-moves-close-p' flag, and, the ARG parameter.

If `ipe-prefix-moves-close-p' is nil, ARG is ignored, and CLOSE
initialization is as if ARG was one.

If `ipe-prefix-moves-close-p' is non-nil, and, ARG is nil or one, and,
POS is already looking at the end of the lexical unit, sets the value
of the current `ipe--pos-close' to the current POS.

Otherwise, if `ipe-prefix-moves-close-p' is non-nil, and ARG is
greater than one, sets the value of the current `ipe--pos-close' by
searching forward ARG lexical units from POS."

  (let* ((move-by   (ipe--move-by-function))
	 (pair      (ipe--pair))
	 (pos-close (or pos (ipe--pos-close n)))
	 (units     (if arg
			(if ipe-prefix-moves-close-p
			    (if (listp arg)
				(car arg)
			      arg)
			  (if (< arg 0) (- arg) 0))
		      0)))

    (funcall move-by
	     pair
	     n
	     'close
	     'init
	     pos-close
	     (ipe--pos-open n)
	     units)))

(defun ipe--close-init (n pos arg)
  "Initialize the position where the `N'th `ipe' CLOSE is displayed.

Sets the value of the `N'th `ipe--pos-close' by searching around POS
for a starting CLOSE position using the lexical units defined by
`ipe--movement'.

The number of lexical units searched is determined by both the
`ipe-prefix-moves-close-p' flag and the ARG parameter.

If `ipe-prefix-moves-close-p' is nil, ARG is ignored, and CLOSE
initialization is as if ARG was one.

If `ipe-prefix-moves-close-p' is non-nil, and, ARG is nil or one, and
POS is already looking at the end of the lexical unit, sets the value
of the current `ipe--pos-close' to the current POS.

Otherwise, if `ipe-prefix-moves-close-p' is non-nil, and ARG is
greater than one, sets the value of the current `ipe--pos-close' by
searching forward ARG lexical units from POS."

  (ipe--pos-close-set n (ipe--close-init-pos n pos arg)))

(defun ipe--close-hide (n)
  "Delete the `N'th Insert Pair Edit (ipe) CLOSE overlay.

Remove the overlay (`ipe--close-overlays') displaying the `N'th CLOSE
string from the current buffer."

  (let ((overlay (ipe--close-overlay n)))
    (when (overlayp overlay)
      (overlay-put overlay 'display "")
      (delete-overlay overlay))))

(defun ipe--close-show (n close &optional no-after-p)
  "Display the `N'th Insert Pair Edit CLOSE string as an overlay.

Display the CLOSE string as an overlay (`ipe--close-overlay') in the
current buffer at `ipe--pos-close'.

When NO-AFTER-P is non-nil, assume that CLOSE is at the end of buffer,
and do not display an `'after-string' as part of the CLOSE overlay."

  (let ((indent (ipe--indent-at n 'close)))
    (ipe--point-overlay-move (ipe--close-overlay n)
			     (ipe--pos-close n)
			     (ipe--indent-string close indent t)
			     no-after-p)))

(defun ipe--close-set-face (n face)
  "Set the FACE for the `N'th Insert Pair Edit CLOSE string."

  (let* ((overlay   (ipe--close-overlay n))
	 (display   (overlay-get overlay 'display))
	 (string    (substring-no-properties display))
	 (redisplay (propertize string 'face face)))
    (overlay-put overlay 'display redisplay)))

(defun ipe--close-move (units action)
  "Move the Insert Pair Edit (ipe) CLOSE position.

- UNITS If non-nil, the number of movement units to move the CLOSE
  overlay.  Movement units are determined by the current value of
  `ipe--movement'.
- ACTION is either `'beg', `'up', `'backward', `'forward', `'down',
  `'end', or `'reset'."

  (let ((move-by (ipe--move-by-function))
	(pair    (ipe--pair)))

    ;; Reset OPEN / CLOSE using the appropriate 'move-by'.
    (dotimes (n (ipe--pos-count))

      ;; Delete the overlays
      (ipe--open-hide n)
      (ipe--close-hide n)

      (ipe--pos-close-set n (funcall move-by
				     pair
				     n
				     'close
				     action
				     (ipe--pos-close n)
				     (ipe--pos-open n)
				     units))

      (ipe--pos-open-set n (funcall move-by
				    pair
				    n
				    'open
				    action
				    (ipe--pos-open n)
				    (ipe--pos-close n)
				    units t))

      (when (< (ipe--pos-close n) (ipe--pos-open n))
	(ipe--pos-open-set n (funcall move-by
				      pair
				      n
				      'open
				      'reset
				      (ipe--pos-close n)
				      (ipe--pos-open n)
				      1))))

    ;; Re-display the overlays in the new positions.
    (ipe--pos-recenter 0 t)))

;; -------------------------------------------------------------------
;;;; Infixes:
;; -------------------------------------------------------------------

(defun ipe--infix-pos (pos infix)
  "Return the position of INFIX within the line containing POS.

If the line containing POS does not contain the INFIX, return nil."

  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (when (re-search-forward (concat "^ *" (regexp-quote infix))
			     (ipe--eol (point))
			     t)
      (- (point) (length infix)))))

(defun ipe--infix-show-p (n)
  "Return non-nil if `ipe' should display `INFIX'es for the `N'th\
PAIR.

If MNEMONIC is nil, derive INFIX from the current `ipe--mnemonic', and
current `ipe--movement'.
Otherwise, derive INFIX from PAIR with MNEMONIC, and the PAIRs Initial
Movement."

  (let ((pair (ipe--pair)))
    (and (equal ipe--movement 'line)
	 (or (> (length (ipe--pair-infix-string pair)) 0)
	     (> (length (ipe--pos-infix-insert n)) 0)))))

(defun ipe--infix-overlay (n i)
  "Return the `I'th INFIX for the `N'th Insert Pair Edit (ipe) PAIR.

If the `I'th INFIX overlay does not exist, create it and add it to
`ipe--infix-overlays'."

  (let* ((infix-overlay-list (ipe--list-pad ipe--infix-overlays
					    (1+ n)))
	 (infix-overlays     (nth n ipe--infix-overlays)))
    (when (>= i (length infix-overlays))
      (ipe-dotimes (1+ (- i (length infix-overlays)))
	(let ((infix-overlay (ipe--point-overlay-create (point) 4)))
	  (overlay-put infix-overlay 'ipe-infix t)
	  (overlay-put infix-overlay
		       'help-echo
		       "INFIX string for 'ipe' PAIR")
	  (if infix-overlays
	      (nconc infix-overlays (list infix-overlay))
	    (setq infix-overlays (list infix-overlay))))))

    (setcar (nthcdr n infix-overlay-list) infix-overlays)
    (setq ipe--infix-overlays infix-overlay-list)
    (nth i infix-overlays)))

(defun ipe--infixes-hide (n)
  "Delete the `N'th Insert Pair Edit (ipe) INFIX overlays.

Remove all of the overlays (`ipe--infix-overlays') that display the
`ipe' INFIX strings between the `N'th `ipe' OPEN and CLOSE strings."

  (when (nth n ipe--infix-overlays)
    (dolist (infix-overlay (nth n ipe--infix-overlays))
      (when (overlayp infix-overlay)
	(overlay-put infix-overlay 'display "")
	(delete-overlay infix-overlay)))
    (setcar (nthcdr n ipe--infix-overlays) nil)))

(defun ipe--infixes-set-face (n face)
  "Set the FACE for the `N'th Insert Pair Edit INFIX overlays."

  (when (nth n ipe--infix-overlays)
    (dolist (infix-overlay (nth n ipe--infix-overlays))
      (let* ((display   (overlay-get infix-overlay 'display))
	     (string    (substring-no-properties display))
	     (redisplay (propertize string 'face face)))
	(overlay-put infix-overlay 'display redisplay)))))

(defun ipe--infixes-update (n)
  "Update the `N'th Insert Pair Edit (ipe) INFIX strings.

If the `ipe--movement' is `'line' and the current `ipe' PAIR has an
:infix defined, create INFIX overlays between the `N'th Insert Pair
Edit (ipe) OPEN and CLOSE strings."

  (ipe--infixes-hide n)
  (when (ipe--infix-show-p n)
    (let* ((open-line   (line-number-at-pos (ipe--pos-open n)))
	   (close-line  (line-number-at-pos (ipe--pos-close n)))
	   (infix-lines (- close-line open-line))
	   (indent      (concat (ipe--indent-at n 'infix)
				(ipe--pos-infix-display n))))

      (save-excursion
	(goto-char (point-min))
	(forward-line open-line)
	(dotimes (i infix-lines)
	  (unless (equal (point) (ipe--pos-close n))
	    (ipe--point-overlay-move (ipe--infix-overlay n i)
				     (point)
				     indent))
	  (forward-line))))))

(defun ipe--infixes-insert (n)
  "Insert the `N'th Insert Pair Edit (ipe) INFIX strings.

Add the text within the `ipe' INFIX overlays (`ipe--infix-overlays')
to the buffer between the `N'th Insert Pair Edit (ipe) OPEN and CLOSE
strings.

Return the number of characters inserted."

  (let ((inserted 0))
    (dotimes (i (length (nth n ipe--infix-overlays)))
      (let* ((overlay  (ipe--infix-overlay n i)))
	(setq inserted (+ inserted
			  (ipe--insert-overlay overlay 'before)))))
    inserted))

;; -------------------------------------------------------------------
;;;; Escapes:
;; -------------------------------------------------------------------

(defun ipe--escaped-region-p (beg end)
  "Return t if the region between BEG and END has `ipe' `ESCAPE's."

  (if (memq t
	    (ipe-compat--mapcan
	     (lambda (overlays)
	       (mapcar (lambda (overlay)
			 (if (and (overlayp overlay)
				  (overlay-buffer overlay)
				  (overlay-get    overlay 'ipe-escape)
				  (< (overlay-start overlay) end)
				  (> (overlay-end   overlay) beg))
			     t
			   nil))
		       (append ipe--open-overlays overlays)))
	     ipe--escape-overlays))
      t nil))

(defun ipe--escape-overlay (n i)
  "Return the `I'th ESCAPE overlay for the `N'th `ipe' PAIR.

If the `I'th ESCAPE overlay does not exist, create it and add it to
`ipe--escape-overlays'."

  (let* ((escape-overlay-list (ipe--list-pad ipe--escape-overlays
					     (1+ n)))
	 (escape-overlays     (nth n ipe--escape-overlays)))
    (when (>= i (length escape-overlays))
      (ipe-dotimes (1+ (- i (length escape-overlays)))
	(let ((escape-overlay (ipe--point-overlay-create (point) 3)))
	  (overlay-put escape-overlay 'after-string "")
	  (overlay-put escape-overlay 'ipe-escape t)
	  (overlay-put escape-overlay
		       'help-echo
		       "ESCAPE string for 'ipe' PAIR")
	  (if escape-overlays
	      (nconc escape-overlays (list escape-overlay))
	    (setq escape-overlays (list escape-overlay))))))

    (setcar (nthcdr n escape-overlay-list) escape-overlays)
    (setq ipe--escape-overlays escape-overlay-list)
    (nth i escape-overlays)))

(defun ipe--escape-open (n escape)
  "Display an ESCAPE overlay next to the `N'th `ipe' OPEN overlay.

Update the `N'th `ipe' OPEN overlay to include the REPLACE string of
the given ESCAPE.

\(NOTE: This is done with the OPEN overlay because the first character
between the OPEN and CLOSE strings of an `ipe' PAIR is actually the
`'after-string' for the OPEN overlay.  To display an :escape which
matches this first character, the REPLACE string needs to be added to
the OPEN overlay, not the `ipe--escape-overlays'.)

ESCAPE is expected to be a list of the form:

  (MATCH REPLACE)

Where:

- MATCH is the replacement string that has been matched, and
- REPLACE is the replacement for the given MATCH."

  (let* ((open-overlay  (ipe--open-overlay n))
	 (close-overlay (ipe--close-overlay n))
	 (display       (overlay-get open-overlay 'display))
	 (match         (car escape))
	 (replace       (propertize (cadr escape)
				    'face
				    'ipe-escape-highlight)))

    (overlay-put open-overlay 'ipe-escape t)

    ;; Special processing at end-of-buffer
    (if (>= (+ (overlay-start open-overlay) (length match))
	    (point-max))
	(progn
	  (overlay-put close-overlay 'before-string "")
	  (overlay-put close-overlay 'display "")
	  (overlay-put close-overlay 'after-string "")

	  (move-overlay close-overlay (point-max) (point-max))

	  (overlay-put open-overlay  'before-string "")
	  (overlay-put open-overlay  'display
		       (concat (ipe--pos-open-display n)
			       replace
			       (ipe--pos-close-display n)))
	  (overlay-put open-overlay  'after-string ""))

      (overlay-put open-overlay 'display (concat display replace))
      (overlay-put open-overlay 'after-string ""))

    ;; Add the concatenated ESCAPE position to the :open-list.
    (ipe--pos-property-set
     n
     :open-list
     (append (ipe--pos-property n :open-list)
	     (list (length (concat (ipe--pos-open-insert n)
				   replace)))))

    ;; Extend the OPEN overlay over the ESCAPE.
    (move-overlay open-overlay
		  (overlay-start open-overlay)
		  (+ (overlay-end open-overlay)
		     (1- (length match))))))

(defun ipe--escape-close (n escape)
  "Display an ESCAPE overlay next to the `N'th `ipe' CLOSE overlay.

Update the `N'th `ipe' CLOSE overlay to include the REPLACE string of
the given ESCAPE.

\(NOTE: This is done with the CLOSE overlay because the last character
between the OPEN and CLOSE strings of an `ipe' PAIR at the
`end-of-buffer' is actually the `'before-string' for the CLOSE
overlay.  To display an :escape which matches this last character, the
REPLACE string needs to be added to the CLOSE overlay, not the
`ipe--escape-overlays'.)

ESCAPE is expected to be a list of the form:

  (MATCH REPLACE)

Where:

- MATCH is the replacement string that has been matched, and
- REPLACE is the replacement for the given MATCH."

  (let* ((overlay (ipe--close-overlay n))
	 (display (overlay-get overlay 'display))
	 (replace (propertize (cadr escape) 'face 'ipe-escape-highlight)))

    (overlay-put overlay 'before-string "")
    (overlay-put overlay 'display (concat replace display))))

(defun ipe--escapes-hide (n)
  "Delete the `N'th Insert Pair Edit (ipe) ESCAPE overlays.

Remove all of the overlays (`ipe--escape-overlays') that display the
`ipe' ESCAPE strings between the `N'th `ipe' OPEN and CLOSE strings."

  (when (nth n ipe--escape-overlays)
    (dolist (escape-overlay (nth n ipe--escape-overlays))
      (when (overlayp escape-overlay)
	(overlay-put escape-overlay 'display "")
	(delete-overlay escape-overlay)))
    (setcar (nthcdr n ipe--escape-overlays) nil)))

(defun ipe--escapes-show (n)
  "Update the `N'th Insert Pair Edit (ipe) ESCAPE strings.

Create ESCAPE overlays (`ipe--escape-overlays') between the `N'th
`ipe' OPEN and CLOSE strings."

  (ipe--escapes-hide n)
  (let* ((pair    (ipe--pair))
	 (escapes (ipe--pair-escapes pair))
	 (i 0))
    (when (and (ipe--pos-open n)
	       (ipe--pos-close n)
	       escapes)
      (save-excursion
	(overlay-put (ipe--open-overlay n) 'ipe-escape nil)
	(mapc
	 (lambda (escape)
	   (goto-char (ipe--pos-open n))
	   (while (search-forward (car escape) (ipe--pos-close n) t)
	     (unless (ipe--escaped-region-p (match-beginning 0)
					    (match-end 0))
	       (cond
		;; Escape next to OPEN Overlay.
		((equal (match-beginning 0) (ipe--pos-open n))
		 (ipe--escape-open n escape))

		;; Escape next to End Of Buffer.
		((eobp)
		 (ipe--escape-close n escape))

		;; Escape between OPEN and CLOSE Overlays.
		(t
		 (let ((overlay (ipe--escape-overlay n i)))
		   (when (overlayp overlay)
		     (overlay-put overlay 'display
				  (propertize (cadr escape) 'face 'ipe-escape-highlight))
		     (move-overlay overlay (match-beginning 0)
				   (match-end 0))))
		 (setq i (1+ i)))))))
	 escapes)))))

(defun ipe--escapes-set-face (n face)
  "Set the FACE for the `N'th Insert Pair Edit ESCAPE strings."

  (when (nth n ipe--escape-overlays)
    (dolist (escape-overlay (nth n ipe--escape-overlays))
      (let* ((display   (overlay-get escape-overlay 'display))
	     (string    (substring-no-properties display))
	     (redisplay (propertize string 'face face)))
	(overlay-put escape-overlay 'display redisplay)))))

(defun ipe--escapes-insert (n)
  "Insert the `N'th Insert Pair Edit (ipe) ESCAPE strings.

Add the text within the `ipe' ESCAPE overlays (`ipe--escape-overlays')
to the buffer between the `N'th Insert Pair Edit (ipe) OPEN and CLOSE
strings when the `ipe--escapes-show-p' is non-nil.

Return the number of characters inserted.  (This is can be negative
if the ESCAPE contains fewer characters than the MATCH.)"

  (save-excursion
    (let ((inserted 0)
	  (point    (ipe--pos-point n)))
      (dotimes (i (length (nth n ipe--escape-overlays)))
	(when (integerp point)
	  (goto-char point))
	(let* ((overlay   (nth i (nth n ipe--escape-overlays)))
	       (beg       (overlay-start overlay))
	       (end       (overlay-end   overlay))
	       (pos-point (ipe--pos-location (point) beg end)))
	  (setq inserted (+ inserted
			    (ipe--insert-overlay overlay pos-point))
		point    (ipe--pos-point n))))
      inserted)))

;; -------------------------------------------------------------------
;;;; POINT & MARK functions:
;; -------------------------------------------------------------------

(defun ipe--set-mark (n)
  "Set the position of MARK based on the `N'th `ipe' PAIR Position.

The position is determined by the variable `ipe-set-mark-on-insert'.

- A value equal nil, will not set MARK.
- A value equal `'open-beg', will cause MARK to be set to the
  beginning of the OPEN string for the `N'th `ipe' PAIR Position..
- A value equal `'open-end', will cause MARK to be set to the end of
  the OPEN string for the `N'th `ipe' PAIR Position.
- A value equal `'close-beg', will cause MARK to be set to the
  beginning of the CLOSE string for the `N'th `ipe' PAIR Position.
- A value equal `'close-end', will cause MARK to be set to the end of
  the CLOSE string for the `N'th `ipe' PAIR Position.
- A positive numeric value `X', will cause MARK to be set to `X'
  characters after the beginning of the OPEN string for the `N'th ipe
  PAIR Position.
- A negative numeric value `Y', will cause MARK to be set to `Y'
  characters before the end of the CLOSE string for the `N'th ipe
  PAIR Position."

  (when (ipe--pos-property n :inserted-p)
    (cond
     ((equal ipe-set-mark-on-insert 'open-beg)
      (push-mark (ipe--pos-open n)))

     ((equal ipe-set-mark-on-insert 'open-end)
      (push-mark (+ (ipe--pos-open n)
		    (length (ipe--pos-open-insert n)))))

     ((equal ipe-set-mark-on-insert 'close-beg)
      (push-mark (ipe--pos-close n)))

     ((equal ipe-set-mark-on-insert 'close-end)
      (push-mark (+ (ipe--pos-close n)
		    (length (ipe--pos-close-insert n)))))

     ((integerp ipe-set-mark-on-insert)
      (if (> ipe-set-mark-on-insert 0)
	  (push-mark (+ (ipe--pos-open n)
			ipe-set-mark-on-insert))
	(push-mark (+ (ipe--pos-close n)
		      (- ipe-set-mark-on-insert))))))))

(defun ipe--set-point (n)
  "Set the current position of POINT based on the `N'th `ipe' PAIR.

The position of POINT is determined by the value of either:

- The PAIR property :move-point;
- The variable `ipe-move-point-on-insert';
- The previous position of POINT (stored within the `N'th PAIR
  positional property :point.)

The possible values of :move-point and `ipe-move-point-on-insert'
are:

- `'open-beg' - which will cause POINT to be set to the beginning of
  the OPEN string for the `N'th `ipe' PAIR.
- `'open-end' - which will cause POINT to be set to the end of the
  OPEN string for the `N'th `ipe' PAIR.
- `'close-beg' - which will cause POINT to be set to the beginning of
  the CLOSE string for the `N'th `ipe' PAIR.
- `'close-end' - which will cause POINT to be set to the end of the
  CLOSE string for the `N'th `ipe' PAIR.

- A positive numeric value `X', will cause POINT to be set to `X'
  characters after the beginning of the OPEN string for the `N'th ipe
  PAIR.
- A negative numeric value `Y', will cause POINT to be set to `Y'
  characters before the end of the CLOSE string for the `N'th `ipe'
  PAIR.

If both :move-point and `ipe-move-point-on-insert' are nil, the
original position of POINT is used by consulting the `N'th PAIR's
positional properties:

- :point - The position of POINT when command: `ipe-edit-mode' was
  activated.
- :inserted-p - A flag indicating whether or not the OPEN and CLOSE
  strings have already been inserted.
- :point-open - A numeric value, indicating that POINT was originally
  *within* the OPEN string at the given position.
- :point-close - A numeric value, indicating that POINT was
  originally *within* the CLOSE string at the given position."

  (let* ((len-open    (length (ipe--pos-open-insert n)))
	 (len-close   (length (ipe--pos-close-insert n)))
	 (pair        (ipe--pair))
	 (move-point  (if (ipe--pair-property-p pair :move-point)
			  (ipe--pair-property pair :move-point)
			ipe-move-point-on-insert))
	 (pos-open    (ipe--pos-open  n))
	 (pos-close   (ipe--pos-close n))
	 (point       (ipe--pos-point n))
	 (inserted-p  (ipe--pos-property n :inserted-p))
	 (point-open  (ipe--pos-property n :point-open))
	 (point-close (ipe--pos-property n :point-close))
	 (open-list   (ipe--pos-property n :open-list)))

    (unless move-point
      (setq move-point ipe-move-point-on-insert))

    (setq point
	  (cond

	   ;; Property specified location.
	   ((and inserted-p (equal move-point 'open-beg))
	    pos-open)

	   ((and inserted-p (equal move-point 'open-end) pos-open)
	    (+ pos-open len-open))

	   ((and inserted-p (equal move-point 'close-beg))
	    pos-close)

	   ((and inserted-p (equal move-point 'close-end) pos-close)
	    (+ pos-close len-close))

	   ;; Property specified position inside inserted OPEN / CLOSE.
	   ((and inserted-p (integerp move-point) (> move-point 0) pos-open)
	    (+ pos-open move-point))

	   ((and inserted-p (integerp move-point) (<= move-point 0) pos-open)
	    (+ pos-close (- move-point)))

	   ;; Position when :point = POS-OPEN.
	   ((and inserted-p
		 (equal point pos-open)
		 (equal point-open 'before))

	    point)

	   ((and inserted-p
		 (equal point pos-open)
		 (equal point-open 'after))
	    (+ point (or (car open-list) len-open)))

	   ((and inserted-p
		 (equal point pos-open)
		 (integerp point-open))
	    (+ point (min point-open len-open)))

	   ((equal point pos-open)
	    point)

	   ;; Position when :point = POS-CLOSE.
	   ((and inserted-p
		 (equal point pos-close)
		 (equal point-close 'before))

	    point)

	   ((and inserted-p
		 (equal point pos-close)
		 (equal point-close 'after))

	    (+ point len-close))

	   ((and inserted-p (equal point pos-close) (integerp point-close))
	    (+ point (min point-close len-close)))

	   ((and (equal point pos-close))
	    point)

	   ;; Default, :point.
	   (t point)))

    (when (integerp point)
      (goto-char point)
      (ipe--pos-point n point))

    (when (equal point 'eob)
      (goto-char (point-max)))

    (ipe--set-mark n)))

;; -------------------------------------------------------------------
;;;; Initialization and insertion:
;; -------------------------------------------------------------------

(defun ipe--pair-pos-init (n &optional pos arg)
  "Surround the current ARG lexical units by the `ipe' OPEN and CLOSE.

`N' specifies the index (0-based) of the `ipe' PAIR Position used to
record the OPEN and CLOSE positions within the `ipe--pair-pos-list'.

OPEN and CLOSE are determined by looking up the current OPEN and CLOSE
strings for `ipe--mnemonic' in `ipe-mode-pairs' / `ipe-pairs'.

The initial position is determined lexical units derived from POS and
`ipe--pair-movement-initial'."

  ;; Set the current movement from the PAIR definition.
  (let ((pair (ipe--pair)))

    (ipe--movement-set (ipe--pair-movement-initial pair))

    ;; Record the initial position within the `ipe--pair-pos-list'.
    (unless (ipe--pos-property n :initial-n)
      (ipe--pos-property-set n :initial-n n))

    ;; Record where POINT is.
    (unless (ipe--pos-point n)
      (ipe--pos-point n (point)))

    ;; Clear all PAIR Position properties.
    (ipe--pos-property-set n
			   :open  nil
			   :infix nil
			   :close nil)

    ;; Set up the initial OPEN / CLOSE Positions.
    (if (region-active-p)
	(progn (ipe--open-init  n (region-beginning) 1)
	       (ipe--close-init n (region-end) 1))
      (ipe--open-init n pos arg)
      (ipe--close-init n pos arg))))

(defun ipe--pair-pos-insert-empty ()
  "Insert the current `ipe' PAIR into an empty buffer.

When the buffer is empty, the `ipe' overlays cannot be used as there
are no characters to overlay, so we insert the OPEN and CLOSE strings
at the start of the buffer without the use of `ipe--insert-overlay'."

  (goto-char (point-min))

  (dotimes (n (ipe--pos-count))
    (let ((open  (ipe--pos-open-insert n))
	  (close (ipe--pos-close-insert n)))
      (ipe--pos-insert open (ipe--pos-property n :point-open))
      (ipe--pos-close-set n (+ (ipe--pos-close n) (length open)))
      (ipe--pos-insert close (ipe--pos-property n :point-close))
      (ipe--pos-property-set n :inserted-p t)))

  (dotimes (n (ipe--pos-count))
    (ipe--set-point n)
    (ipe--pair-pos-hide n))

  (ipe--undo-accept)
  (setq ipe--pair-pos-list nil)

  (message
   (concat "Cannot edit 'ipe' PAIR in empty buffer.  "
	   "Inserting " (ipe--mnemonic-describe ipe--mnemonic))))

(defun ipe--pair-pos-insert (n)
  "Insert the `N'th `ipe' OPEN & CLOSE strings into the buffer.

Inserts the text within the OPEN (`ipe--open-overlay') and CLOSE
\(`ipe--close-overlay') overlays into the current buffer and deletes
the overlays.

If the PAIR for the current `ipe--mnemonic' has :infix or :escapes
properties, the text within any associated INFIX overlays
\(`ipe--infix-overlay') and ESCAPE overlays (`ipe--escape-overlays')
is also inserted and the overlays deleted.

Return the number of characters inserted into the buffer."

  (save-excursion
    (let* ((inserted 0) (pos-open) (len-open) (pos-close) (len-close))

      ;; Insert OPEN Overlay.
      (setq pos-open (ipe--pos-open n)
	    len-open (ipe--insert-overlay (ipe--open-overlay n) 'after)
	    inserted (+ inserted len-open))

      (ipe--pos-open-set n pos-open)

      ;; Adjust concatenated OPEN overlay positions.
      (let ((open-list (ipe--pos-property n :open-list))
	    (insert-n  n)
	    (offset    0))
	(while (and open-list (< insert-n (ipe--pos-count)))
	  (setq offset    (+ offset (car open-list))
		open-list (cdr open-list))
	  (ipe--pos-close-set insert-n (+ (ipe--pos-open n) offset))
	  (when (and open-list (< (1+ insert-n) (ipe--pos-count)))
	    (setq offset    (+ offset (car open-list))
		  open-list (cdr open-list)
		  insert-n  (1+ insert-n))
	    (ipe--pos-open-set insert-n (+ (ipe--pos-open n) offset)))))

      (ipe--pos-property-set n :open-list nil)

      ;; Insert INFIXES.
      (when (ipe--infix-show-p n)
	(setq inserted
	      (+ inserted (ipe--infixes-insert n))))

      ;; Insert ESCAPES.
      (when ipe--escapes-show-p
	(setq inserted
	      (+ inserted (ipe--escapes-insert n))))

      ;; Insert CLOSE overlay.
      (setq pos-close (ipe--pos-close n)
	    len-close (ipe--insert-overlay (ipe--close-overlay n) 'after)
	    inserted  (+ inserted len-close))

      (ipe--pos-close-set n pos-close)

      ;; Adjust concatenated CLOSE overlay positions.
      (let ((close-list (ipe--pos-property n :close-list))
	    (insert-n  n)
	    (offset    0))
	(while (and close-list (< (1+ insert-n) (ipe--pos-count)))
	  (setq offset     (+ offset (car close-list))
		close-list (cdr close-list)
		insert-n   (1+ insert-n))
	  (ipe--pos-open-set insert-n (+ (ipe--pos-close n) offset))
	  (when close-list
	    (setq offset     (+ offset (car close-list))
		  close-list (cdr close-list))
	    (ipe--pos-close-set insert-n (+ (ipe--pos-close n) offset)))))

      (ipe--pos-property-set n :close-list nil)

      ;; If :point at the eob, more special behaviour.
      (when (and (equal (ipe--pos-point n) (point-max))
		 (>= (ipe--pos-close n)
		     (- (point-max) (length (ipe--pos-close-insert n)))))
	(let* ((ipe-point    (ipe--pos-point n))
	       (point-open   (ipe--pos-property n :point-open))
	       (point-close  (ipe--pos-property n :point-close))
	       (len-open     (length (ipe--pos-open-insert n)))
	       (len-close    (length (ipe--pos-close-insert n)))
	       (offset       (if (integerp point-open)
				 (+ (- ipe-point len-close len-open)
				    point-open)
			       (if (integerp point-close)
				   (+ (- ipe-point len-close)
				      point-close)
				 (- ipe-point len-close)))))
	  (ipe--pos-point n offset)))

      (ipe--pos-property-set n :inserted-p t)
      inserted)))

(defun ipe--pair-pos-hide (n)
  "Delete the `N'th Insert Pair Edit (ipe) PAIR.

Remove the overlays (`ipe--open-overlays' / `ipe--close-overlays' /
`ipe--infix-overlays' / `ipe--escape-overlays') displaying the `N'th
`ipe' PAIR Position from the current buffer."

  (ipe--open-hide n)
  (ipe--infixes-hide n)
  (ipe--escapes-hide n)
  (ipe--close-hide n)

  (setq ipe--pair-pos-list (ipe--list-remove ipe--pair-pos-list n)))

(defun ipe--pair-pos-set-face (n face)
  "Set the FACE for `N' Insert Pair Edit (ipe) PAIR."

  (ipe--open-set-face n face)
  (ipe--infixes-set-face n face)
  (ipe--escapes-set-face n face)
  (ipe--close-set-face n face))

(defun ipe--pair-pos-redisplay ()
  "Redisplay the Insert Pair Edit (ipe) OPEN and CLOSE overlays.

This will reconfigure the `ipe' overlays (`ipe--open-overlays' /
`ipe--close-overlays' / `ipe--infix-overlays' /
`ipe--escape-overlays') to match the positions currently stored within
the `ipe--pair-pos-list'.  If there are multiple PAIRs, this may
require some concatenation of adjacent overlays."

  (ipe--pos-list-normalize)

  (let ((move-by   (ipe--move-by-function))
	(pair      (ipe--pair))
	(n         0)
	(display-n 0)
	(open)
	(open-infix)
	(close)
	(open-list)
	(close-list)
	(eobc)
	(no-after-p))

    (while (< n (ipe--pos-count))

      (funcall move-by pair n 'open  'redisplay 0 0 0)
      (funcall move-by pair n 'close 'redisplay 0 0 0)

      ;; An empty OPEN within an INFIX causes us problems.
      (setq open-infix
	    (if (and (= (length (ipe--pair-open-string pair)) 0)
		     (> (length (ipe--pos-infix-display n)) 0))
		(ipe--pos-infix-display n)
	      (ipe--pos-open-display n)))

      ;; A PAIR at the end-of-buffer causes us problems.
      (setq eobc (if (or (ipe--pos-eob1-p n)
			 (ipe--pos-eob1-2-p (1- n) n))
		     (buffer-substring (1- (point-max)) (point-max))
		   nil)
	    no-after-p (>= (ipe--pos-close n) (point-max)))

      (cond
       ;; LAST_OPEN = OPEN = CLOSE (concatenate OPEN overlay)
       ((and (not (= display-n n))
	     (or (ipe--pos-adjacent-2-p display-n n)
		 (ipe--pos-eob1-2-p     display-n n))
	     (or (ipe--pos-adjacent-p n)
		 (ipe--pos-eob1-p     n))
	     (not close))
	(setq open-list (append (ipe--pos-property display-n :open-list)
				(list (length open-infix)
				      (+ (length eobc)
					 (length (ipe--pos-close-display n)))))
	      open      (concat open
				open-infix
				eobc
				(ipe--pos-close-display n))
	      close     nil))

       ;; LAST_CLOSE = OPEN = CLOSE (concatenate CLOSE overlay)
       ((and (not (= display-n n))
	     (or (ipe--pos-adjacent-2-p display-n n)
		 (ipe--pos-eob1-2-p     display-n n))
	     (or (ipe--pos-adjacent-p n)
		 (ipe--pos-eob1-p     n))
	     close)
	(setq close-list (append (ipe--pos-property display-n :close-list)
				 (list (length open-infix)
				       (+ (length eobc)
					  (length (ipe--pos-close-display n)))))
	      open       nil
	      close      (concat close
				 (if (ipe--pos-adjacent-2-p display-n n)
				     (concat open-infix eobc)
				   (concat eobc open-infix))
				 (ipe--pos-close-display n))))

       ;; LAST_OPEN = OPEN (concatenate just the OPEN overlay)
       ((and (not (= display-n n))
	     (ipe--pos-adjacent-p   display-n)
	     (ipe--pos-adjacent-2-p display-n n))
	(setq open-list (append (ipe--pos-property display-n :open-list)
				(list (length open-infix)))
	      open      (concat open open-infix)
	      close     (ipe--pos-close-display n))
	(ipe--pos-property-set display-n :open-list open-list)
	(ipe--open-show display-n open)
	(setq open       nil
	      open-list  nil
	      display-n  n))

       ;; LAST_CLOSE = OPEN (concatenate just the CLOSE overlay)
       ((and (not (= display-n n))
	     (ipe--pos-adjacent-2-p display-n n))
	(setq open-list  nil
	      close-list (append (ipe--pos-property display-n :close-list)
				 (list (length open-infix)))
	      close      (concat close open-infix))
	(ipe--pos-property-set display-n :close-list close-list)
	(ipe--close-show display-n close)
	(setq open-list  nil
	      close-list nil
	      open       nil
	      close      (ipe--pos-close-display n)
	      display-n  n))

       ;; OPEN = CLOSE (concatenate OPEN overlay)
       ((or (ipe--pos-adjacent-p n)
	    (ipe--pos-eob1-p     n))
	(setq open-list  (list (+ (length eobc)
				  (length (ipe--pos-close-display n))))
	      open       (concat open-infix
				 eobc
				 (ipe--pos-close-display n))
	      close      nil
	      display-n  n))

       ;; Special processing for "blank" lines with infix.
       ((and (save-excursion (goto-char (ipe--pos-close n))
			     (looking-at "^$"))
	     (ipe--infix-show-p n))
	(setq open       open-infix
	      close      (concat (ipe--pos-infix-display n)
				 (ipe--pos-close-display n))
	      display-n  n))

       ;; Normal case.
       (t
	(setq open       open-infix
	      close      (ipe--pos-close-display n)
	      display-n  n)))

      (ipe--pos-property-set display-n :open-list  open-list)
      (ipe--pos-property-set display-n :close-list close-list)

      (setq open-list nil
	    close-list nil)

      (if open
	  (ipe--open-show display-n open (and (not close) no-after-p))
	(ipe--open-hide n))

      (ipe--infixes-update n)

      (if close
	  (ipe--close-show display-n close no-after-p)
	(ipe--close-hide n))

      (if ipe--escapes-show-p
	  (ipe--escapes-show n)
	(ipe--escapes-hide n))

      (setq n (1+ n)))

    (when (> (length ipe--open-overlays) (ipe--pos-count))
      (dotimes (n (- (length ipe--open-overlays) (ipe--pos-count)))
	(ipe--open-hide (+ n (ipe--pos-count)))))

    (when (> (length ipe--infix-overlays) (ipe--pos-count))
      (dotimes (n (- (length ipe--infix-overlays) (ipe--pos-count)))
	(ipe--infixes-hide (+ n (ipe--pos-count)))))

    (when (> (length ipe--escape-overlays) (ipe--pos-count))
      (dotimes (n (- (length ipe--escape-overlays) (ipe--pos-count)))
	(ipe--escapes-hide (+ n (ipe--pos-count)))))

    (when (> (length ipe--close-overlays) (ipe--pos-count))
      (dotimes (n (- (length ipe--close-overlays) (ipe--pos-count)))
	(ipe--close-hide (+ n (ipe--pos-count)))))))

(defun ipe--pair-pos-movement-reset (movement)
  "Set the unit of MOVEMENT for the Insert Pair Edit (ipe) commands.

MOVEMENT is one of the constants within the `car' of the elements
within the `ipe-move-by-movements' list.

This will reset the position of both the `ipe' OPEN and CLOSE overlays
so that they correctly align with the current lexical unit boundaries.

This will also output a notification describing the new movement to
the echo area for the user."

  (when (not (equal ipe--movement movement))

    ;; Reset using the old movement function.
    (let ((move-by (ipe--move-by-function))
	  (pair    (ipe--pair)))
      (dotimes (n (ipe--pos-count))
	(funcall move-by pair n 'open  'reset 0 0 0)
	(funcall move-by pair n 'close 'reset 0 0 0)))

    (setq ipe--movement movement)

    ;; Reset using the new movement function.
    (let ((move-by (ipe--move-by-function))
	  (pair    (ipe--pair)))
      (dotimes (n (ipe--pos-count))
	(funcall move-by pair n 'open  'reset 0 0 0)
	(funcall move-by pair n 'close 'reset 0 0 0)))

    (setq ipe--pos-property-set-callback
	  (ipe-compat--caddr (assoc ipe--movement ipe-move-by-movements)))

    (dotimes (n (ipe--pos-count))
      (ipe--open-init  n (ipe--pos-open n) 0)
      (ipe--close-init n (ipe--pos-close n) 0)
      (when (functionp ipe--pos-property-set-callback)
	(funcall ipe--pos-property-set-callback n))))

  (ipe--pair-pos-redisplay)
  (message (concat "'Insert Pair Edit' movement is now by '"
		   (ipe-compat--cadddr
		    (assoc ipe--movement ipe-move-by-movements))
		   "'.")))

(provide 'ipe-core)

;;; ipe-core.el ends here
