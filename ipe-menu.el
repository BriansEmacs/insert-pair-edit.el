;;; ipe-menu.el --- Insert Pair Edit - menu support -*- lexical-binding: t; -*-
;; Copyright (C) 2023 Brian Kavanagh

;; Author: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Maintainer: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Created: 28 June, 2020
;; Version: 1.1
;; Package: ipe
;; Package-Requires: ((emacs "24.3"))
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
;; This file adds menu support for the `ipe' (Insert Pair Edit)
;; package.
;;
;; It defines an "Insert Pair Edit" minor mode menu, "Mouse" Menu, and
;; a "Pairs" menu item (that can be added to the standard Emacs "Edit"
;; menu) via setting the `customize'-able variable
;; `ipe-menu-display-in-edit-p'.
;;
;; Customizations for the mode can be found under the `ipe' group.

;; -------------------------------------------------------------------
;;; Installation:
;;
;; Add the following to your `.emacs' file:
;;
;;  (require 'ipe-menu)

;; -------------------------------------------------------------------
;;; Code:

(require 'ipe-edit)
(require 'ipe-defn)
(require 'ipe-help)

;; -------------------------------------------------------------------
;;;; Customizations:
;; -------------------------------------------------------------------

(defcustom ipe-menu-display-in-edit-p t
  "Display a `Pairs' sub-menu within standard Emacs `Edit' menu.

If non-nil, this option will cause the addition of an extra `Pairs'
sub-menu item to the standard Emacs `Edit' menu.

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

Which can be used to call `ipe-insert-pair-edit' /
`ipe-insert-pair-edit-update' / `ipe-insert-pair-edit-delete' /
etc... with one of the current set of `ipe-pairs' / `ipe-mode-pairs'
PAIRs."
  :group 'ipe-advanced
  :tag   "Insert Pair Edit - Display 'Pairs' sub-menu in Emacs\
 'Edit' Menu."
  :link  '(function-link ipe-insert-pair-edit)
  :link  '(emacs-commentary-link "ipe-menu.el")
  :type  'boolean)

;; -------------------------------------------------------------------
;;;; Utility functions for :visible:
;; -------------------------------------------------------------------

(defun ipe-menu--pairs-p ()
  "Return non-nil if there are Insert Pair Edit PAIRs defined."
  (> (length ipe-pairs) 0))

(defun ipe-menu--mode-pairs-p (&optional mode)
  "Return non-nil if there are `ipe' Mode-Specific PAIRs defined.

If MODE is nil, return non-nil if there are ANY `ipe' Mode-Specific
PAIRS defined.
If MODE is non nil, return non-nil if there are `ipe' Mode-Specific
PAIRs defined for the given MODE within `ipe-mode-pairs'."

  (if mode
      (> (length (ipe--mode-pairs mode)) 0)
    (> (length ipe-mode-pairs) 0)))

(defun ipe-menu--escapes-p ()
  "Return non-nil if the current Insert Pair Edit PAIR has ESCAPES."
  (ipe--pair-escapes (ipe--pair)))

(defun ipe-menu--text (mnemonic &optional mode)
  "Return the text displayed in an Insert Pair Edit menu.

MNEMONIC is the mnemonic for the PAIR for which the text is to be
returned.

MODE is either:
  nil - The current `major-mode' PAIR for MNEMONIC.
  t - The global PAIR for MNEMONIC from `ipe-pairs'.
  symbol - The Mode-Specific PAIR for MNEMONIC from `ipe-mode-pairs'."

  (let* ((pair (ipe--pair mnemonic mode))
	 (open (replace-regexp-in-string "[\r\n]+" ""
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
    (concat open " ... " close)))

;; -------------------------------------------------------------------
;;;; Utility functions for sub-menus:
;; -------------------------------------------------------------------

(defun ipe-menu--get-menu-pairs (pairs &optional menu-name)
  "Retrieve the `ipe' PAIR definitions with a given MENU-NAME.

Returns all of the `ipe' PAIRS within the `ipe' PAIR definition list,
PAIRS, which have a :menu PAIR property which matches MENU-NAME.

If MENU-NAME is nil, return all of the `ipe' PAIR definitions within
the PAIRS list which either:

- Do not have a :menu PAIR property;
- Have a nil :menu PAIR property, or;
- Have an empty (\"\") :menu PAIR property."

  (delq nil
	(mapcar
	 (lambda (pair)
	   (let ((menu (ipe--pair-property pair :menu)))
	     (when (or (and (or (not menu-name) (= (length menu-name) 0))
			    (or (not menu)      (= (length menu)      0)))
		       (and menu-name
			    menu
			    (string-match (concat menu-name "/?$") menu)))
	       pair)))
	 pairs)))

(defun ipe-menu--get-sub-menu (pair &optional menu)
  "Return the SUB-MENU name under MENU for an `ipe' PAIR.

Queries the :menu property of an Insert Pair Edit (ipe) PAIR
definition and returns either:

- nil, if the :menu property is nil, or does not start with the prefix
  MENU, or;
- The next SUB-MENU within the :menu property after removing any MENU
  prefix.  If MENU is nil, this is simply all of the characters up to
  the first `/' within the :menu property.  If MENU is not nil, this
  is all of the characters between the MENU prefix (+ a trailing `/'
  character) and the next `/' character within the :menu property.

i.e.

  (ipe-menu--get-sub-menu (\"(\" \"(\" \")\"
			  (:menu \"menu-1/sub-menu-1/sub-menu-2\")
			  nil)
  => \"menu-1\"

  (ipe-menu--get-sub-menu (\"(\" \"(\" \")\"
			  (:menu \"menu-1/sub-menu-1/sub-menu-2\")
			  \"menu-1\")
  => \"sub-menu-1\""

  (let* ((pair-menu (ipe--pair-property pair :menu))
	 (prefix    (when (and menu (> (length menu) 0))
		      (concat menu "/")))
	 (path      (if prefix
			(if (string-prefix-p prefix pair-menu)
			    (substring pair-menu (length prefix))
			  nil)
		      pair-menu))
	 (sub-menu   (if path
			 (replace-regexp-in-string " */.*" ""
						   path))))
    (if (> (length sub-menu) 0)
	sub-menu
      nil)))

(defun ipe-menu--get-sub-menus (pairs &optional menu)
  "Return all of the sub-menus under MENU for a list of `ipe' PAIRS.

Queries the :menu PAIR property for each `ipe' PAIR definition within
the definition list, PAIRS, and returns a unique list of the sub-menu
names for all the :menu properties which start with the prefix, MENU.
Sub-Menus within the :menu properties are separated by `/'
characters."

  (delq nil
	(delete-dups
	 (mapcar
	  (lambda (pair)
	    (ipe-menu--get-sub-menu pair menu))
	  pairs))))

;; -------------------------------------------------------------------
;;;; Sub-menu maps:
;; -------------------------------------------------------------------

;; -------------------------------------------------------------------
;;;;; 'Insert And' sub-menu:
;; -------------------------------------------------------------------

(defun ipe-menu--insert-and-map-init ()
  "Create the Keymap for the ipe \"Insert And >\" sub-menu.

This sub-menu is displayed under the Insert Pair Edit mode menu.

It defines menu-items to call `ipe-edit--ia-*' functions to insert a
PAIR into the buffer and perform some sort of additional action.

\(See function: `ipe-edit-mode')."

  (let ((km (make-sparse-keymap "Insert Pair Edit - Insert And")))

    (define-key-after km [goto-open]
      '(menu-item
	"Goto OPEN" ipe-edit--ia-goto-open
	:help "Insert 'Insert Pair Edit' (ipe) OPEN and CLOSE, exit\
 'ipe-edit-mode', and goto the end of OPEN."))

    (define-key-after km [goto-close]
      '(menu-item
	"Goto CLOSE" ipe-edit--ia-goto-close
	:help "Insert 'Insert Pair Edit' (ipe) OPEN and CLOSE, exit\
 'ipe-edit-mode', and goto the end of CLOSE.")
      'goto-open)

    (define-key-after km [resume]
      '(menu-item
	"Resume" ipe-edit--ia-resume
	:help "Insert 'Insert Pair Edit' (ipe) OPEN and CLOSE, exit\
 'ipe-edit-mode', and leave POINT unchanged.")
      'goto-close)

    (define-key-after km [sep-1]
      '(menu-item "--" nil)
      'resume)

    (define-key-after km [copy-text]
      '(menu-item
	"Copy Text" ipe-edit--ia-copy-text
	:help "Insert 'Insert Pair Edit' (ipe) OPEN and CLOSE, exit\
 'ipe-edit-mode', and copy the text between OPEN and CLOSE to the\
 kill ring.")
      'sep-1)

    (define-key-after km [kill-text]
      '(menu-item
	"Kill Text" ipe-edit--ia-kill-text
	:help "Insert 'Insert Pair Edit' (ipe) OPEN and CLOSE, exit\
 'ipe-edit-mode', and kill the text between OPEN and CLOSE.")
      'copy-text)

    ;; Return the keymap.
    km))

(defvar ipe-menu--insert-and-map (ipe-menu--insert-and-map-init)
  "The Keymap for the Insert Pair Edit \"Insert And >\" sub-menu.

This sub-menu is displayed under the Insert Pair Edit mode menu.

It defines menu-items to call `ipe-edit--ia-*' functions to insert a
PAIR into the buffer and perform some sort of additional action.

\(See function: `ipe-edit-mode').")

;; -------------------------------------------------------------------
;;;;; 'Change PAIR' sub-menu:
;; -------------------------------------------------------------------

(defun ipe-menu--sub-menu-keymap (pairs keymap menu-path menu-item-fn)
  "Populate a KEYMAP with sub-menus for a set of `ipe' PAIRS.

Where:

- PAIRS is a list of Insert Pair Edit (ipe) PAIR definitions (see:
  `ipe-pairs')
- KEYMAP is the keymap to which the sub-menu `'menu-items' are to be
  added.
- MENU-PATH is a `/'-separated string, representing the path to the
  sub-menu being generated from the PAIRS list.
- MENU-ITEM-FN is a function used to generate the `'menu-items' within
  the returned SUB-MENU for each eligible PAIR within PAIRS.

Eligible definitions within PAIRS are determined by querying the :menu
property of the PAIR.  If the value of the :menu property for the
given PAIR matches MENU-PATH, a `'menu-item' for the given PAIR is
added to the returned KEYMAP by calling MENU-ITEM-FN to generate a
suitable Emacs `'menu-item' structure.

- MENU-ITEM-FN is expected to be a function which takes two arguments:

  (MENU-ITEM-FN KEYMAP MNEMONIC)

  and which adds a `'menu-item' suitable for use within an Emacs menu
  to the given KEYMAP."

  (let ((sub-menus   (ipe-menu--get-sub-menus   pairs menu-path))
	(menu-pairs  (ipe-menu--get-menu-pairs pairs menu-path))
	(i           1))

    ;; Add next-level sub-menus.
    (mapc (lambda (sub-menu)
	    (define-key-after keymap
	      (vector (intern (concat "custom-menu-" (number-to-string i))))
	      (ipe-menu--create-sub-menu pairs
					 (if menu-path
					     (concat menu-path "/" sub-menu)
					   sub-menu)
					 menu-item-fn))
	    (setq i (1+ i)))
	  sub-menus)

    ;; If there are sub-menus and menu-items, add a separator.
    (when (and sub-menus menu-pairs)
      (define-key-after keymap [sep-1] '(menu-item "--" nil)))

    ;; Add this-level menu-items.
    (mapc (lambda (pair)
	    (funcall menu-item-fn keymap (car pair)))
	  menu-pairs)))

(defun ipe-menu--change-pair-menu-item (keymap mnemonic)
  "Add a \"Change PAIR\" `'menu-item' for an `ipe' PAIR to KEYMAP.

Add a `'menu-item' that calls `ipe-edit--change-pair' for the `ipe'
PAIR with the given MNEMONIC to KEYMAP."

  (define-key-after keymap
    (vector (intern (concat "mnemonic-" mnemonic)))
    (list 'menu-item
	  (ipe-menu--text mnemonic major-mode)
	  (list 'lambda nil (list 'interactive)
		(list 'ipe-edit--change-pair mnemonic))
	  :keys mnemonic
	  :help (concat "Replace the OPEN and CLOSE of "
			"the existing 'Insert Pair Edit' (ipe) PAIR with an "
			(ipe--mnemonic-describe mnemonic
						major-mode)))))

(defun ipe-menu--change-pair ()
  "Generate the 'Change PAIR' mode menu.

This returns a new menu item which provides a sub-menu of items of the
form:

- Insert Pair Edit >
  - Change PAIR >
    - <custom-menu-1>
      - '<OPEN>' ... '<CLOSE>'
      - ...
    - '<OPEN>' ... '<CLOSE>'
    - ...

Which will provide menu items which call `ipe-edit--change-pair' with
one of the current set of `ipe-pairs' / `ipe-mode-pairs' PAIRs.

This contents of this sub-menu changes based upon the current buffers
MAJOR-MODE."

  (interactive)

  (let ((km    (make-sparse-keymap "Insert Pair Edit - Change PAIR"))
	(pairs (ipe--pairs major-mode)))
    (ipe-menu--sub-menu-keymap pairs
			       km
			       nil
			       'ipe-menu--change-pair-menu-item)
    km))

;; -------------------------------------------------------------------
;;;;; 'Change Movement' sub-menu:
;; -------------------------------------------------------------------

(defun ipe-menu--movement-by-map-init ()
  "Create the Keymap for the ipe \"Change Movement >\" sub-menu.

This sub-menu is displayed under the Insert Pair Edit mode menu.

It defines menu-items to adjust the default movement of the OPEN and
CLOSE of a PAIR.

\(See function: `ipe-edit-mode')."

  (let ((km
	 (make-sparse-keymap "Insert Pair Edit - Change Movement")))

    (define-key-after km [char]
      '(menu-item
	"By Character" ipe-edit--movement-by-char
	:help "Change movement of the 'Insert Pair Edit' (ipe) OPEN and\
 CLOSE to `by character'."))

    (define-key-after km [word]
      '(menu-item
	"By Word" ipe-edit--movement-by-word
	:help "Change movement of the 'Insert Pair Edit' (ipe) OPEN and\
 CLOSE to `by word'.")
      'char)

    (define-key-after km [line]
      '(menu-item
	"By Line" ipe-edit--movement-by-line
	:help "Change movement of the 'Insert Pair Edit' (ipe) OPEN and\
 CLOSE to `by line'.")
      'word)

    (define-key-after km [list]
      '(menu-item
	"By List" ipe-edit--movement-by-list
	:help "Change movement of the 'Insert Pair Edit' (ipe) OPEN and\
 CLOSE to `by list'.")
      'line)
    km))

(defvar ipe-menu--movement-by-map (ipe-menu--movement-by-map-init)
  "The Keymap for the Insert Pair Edit \"Change Movement >\" sub-menu.

This sub-menu is displayed under the Insert Pair Edit mode menu.

It defines menu-items to adjust the default movement of the OPEN and
CLOSE of a PAIR.

\(See function: `ipe-edit-mode').")

;; -------------------------------------------------------------------
;;;;; 'Edit CONTENTS' sub-menu:
;; -------------------------------------------------------------------

(defun ipe-menu--edit-contents-map-init ()
  "Create the Keymap for the ipe \"Edit CONTENTS >\" sub-menu.

This sub-menu is displayed under the Insert Pair Edit mode menu.

It defines menu-items to edit the text enclosed by the OPEN and CLOSE
of a PAIR.

\(See function: `ipe-edit-mode')."

  (let ((km (make-sparse-keymap "Insert Pair Edit - Edit CONTENTS")))

    (define-key-after km [kill]
      '(menu-item
	"Kill" ipe-edit--contents-kill
	:help "Kill the text enclosed by the 'Insert Pair Edit' (ipe)\
 OPEN and CLOSE."))

    (define-key-after km [copy]
      '(menu-item
	"Copy" ipe-edit--contents-copy
	:help "Copy the text enclosed by the 'Insert Pair Edit' (ipe)\
 OPEN and CLOSE to the kill-ring.")
      'kill)

    (define-key-after km [paste]
      '(menu-item
	"Paste" ipe-edit--contents-yank
	:help "Replace the text enclosed by the 'Insert Pair Edit' (ipe)\
 OPEN and CLOSE with the last killed text.")
      'copy)

    (define-key-after km [replace]
      '(menu-item
	"Replace" ipe-edit--contents-replace
	:help "Replace the text enclosed by the 'Insert Pair Edit' (ipe)\
 OPEN and CLOSE.")
      'paste)

    (define-key-after km [sep-1]
      '(menu-item "--" nil)
      'replace)

    (define-key-after km [trim]
      '(menu-item
	"Trim" ipe-edit--contents-trim
	:help "Trim whitespace from around the text enclosed by\
  the 'Insert Pair Edit' (ipe) OPEN and CLOSE.")
      'sep-1)

    (define-key-after km [upcase]
      '(menu-item
	"Upcase" ipe-edit--contents-upcase
	:help "Convert the text enclosed by the 'Insert Pair Edit' (ipe)\
 OPEN and CLOSE to UPPERCASE.")
      'trim)

    (define-key-after km [capitalize]
      '(menu-item
	"Capitalize" ipe-edit--contents-capitalize
	:help "Convert the text enclosed by the 'Insert Pair Edit' (ipe)\
 OPEN and CLOSE to Capital Case.")
      'upcase)

    (define-key-after km [downcase]
      '(menu-item
	"Downcase" ipe-edit--contents-downcase
	:help "Convert the text enclosed by the 'Insert Pair Edit' (ipe)\
 OPEN and CLOSE to lowercase.")
      'capitalize)
    km))

(defvar ipe-menu--edit-contents-map (ipe-menu--edit-contents-map-init)
  "The Keymap for the Insert Pair Edit \"Edit CONTENTS >\" sub-menu.

This sub-menu is displayed under the Insert Pair Edit mode menu.

It defines menu-items to edit the text enclosed by the OPEN and CLOSE
of a PAIR.

\(See function: `ipe-edit-mode').")

;; -------------------------------------------------------------------
;;;;; 'Next / Previous' sub-menu:
;; -------------------------------------------------------------------

(defun ipe-menu--next-prev-map-init ()
  "Create the Keymap for the ipe \"Next / Previous >\" sub-menu.

This sub-menu is displayed under the Insert Pair Edit mode menu.

It defines menu-items to update existing PAIRs or existing individual
OPEN or CLOSE of a PAIR.

\(See function: `ipe-edit-mode')."

  (let ((km (make-sparse-keymap
	     "Insert Pair Edit - Next / Previous")))

    (define-key-after km [next-pair]
      '(menu-item
	"Next PAIR" ipe-edit--update-next-pair
	:help "Insert current 'Insert Pair Edit' (ipe) OPEN and CLOSE,\
 search forward for the OPEN and CLOSE text, and create a new PAIR\
 from them."))

    (define-key-after km [next-contents]
      '(menu-item
	"Next CONTENTS" ipe-edit--update-next-contents
	:help "Insert current 'Insert Pair Edit' (ipe) OPEN and CLOSE,\
 search forward for the text between OPEN and CLOSE and create\
 another PAIR around the text.")
      'next-pair)

    (define-key-after km [next-open]
      '(menu-item
	"Next OPEN" ipe-edit--update-next-open
	:help "Insert current 'Insert Pair Edit' (ipe) OPEN, then search\
 forward for the OPEN text, and make it the new OPEN.")
      'next-contents)

    (define-key-after km [next-close]
      '(menu-item
	"Next CLOSE" ipe-edit--update-next-close
	:help "Insert current 'Insert Pair Edit' (ipe) CLOSE, then search\
 forward for the CLOSE text, and make it the new CLOSE.")
      'next-open)

    (define-key-after km [sep-1]
      '(menu-item "--" nil)
      'next-close)

    (define-key-after km [previous-pair]
      '(menu-item
	"Previous PAIR" ipe-edit--update-previous-pair
	:help "Insert current 'Insert Pair Edit' (ipe) OPEN and CLOSE,\
 search backward for the OPEN and CLOSE text, and create a new' PAIR\
 from them.")
      'sep-1)

    (define-key-after km [previous-contents]
      '(menu-item
	"Previous CONTENTS" ipe-edit--update-previous-contents
	:help "Insert current 'Insert Pair Edit' (ipe) OPEN and CLOSE,\
 search backward for the text between OPEN and CLOSE and create\
 another PAIR around the text.")
      'previous-pair)

    (define-key-after km [previous-open]
      '(menu-item
	"Previous OPEN" ipe-edit--update-previous-open
	:help "Insert current 'Insert Pair Edit' (ipe) OPEN, then search\
 backward for the OPEN text, and make it the new OPEN.")
      'previous-contents)

    (define-key-after km [previous-close]
      '(menu-item
	"Previous CLOSE" ipe-edit--update-previous-close
	:help "Insert current 'Insert Pair Edit' (ipe) CLOSE, then search\
 backward for the CLOSE text, and make it the new CLOSE.")
      'previous-open)

    km))

(defvar ipe-menu--next-prev-map (ipe-menu--next-prev-map-init)
  "The Keymap for the Insert Pair Edit \"Next / Previous >\" sub-menu.

This sub-menu is displayed under the Insert Pair Edit mode menu.

It defines menu-items to update existing PAIRs or existing individual
OPEN or CLOSE of a PAIR.

\(See function: `ipe-edit-mode').")

;; -------------------------------------------------------------------
;;;;; 'Multiple' sub-menu:
;; -------------------------------------------------------------------

(defun ipe-menu--multiple-map-init ()
  "Create the Keymap for the ipe \"Multiple >\" sub-menu.

This sub-menu is displayed under the Insert Pair Edit mode menu.

It defines menu-items to add delete extra PAIRs.

\(See function: `ipe-edit-mode')."

  (let ((km (make-sparse-keymap "Insert Pair Edit - Multiple")))

    (define-key km [add-pair]
		'(menu-item
		  "Add PAIR (At Point) " ipe-insert-pair-edit
		  :help "Add another 'Insert Pair Edit' (ipe) PAIR to the\
 buffer at POINT."))

    (define-key-after km [add-pair-forward]
      '(menu-item
	"Add PAIR (Search Forward) " ipe-edit--add-next-pair
	:help "Add another 'Insert Pair Edit' (ipe) PAIR to the buffer by\
 searching forward for text which matches OPEN and CLOSE, and\
 creating a new PAIR from it.")
      'add-pair)

    (define-key-after km [add-pair-backward]
      '(menu-item
	"Add PAIR (Search Backward) " ipe-edit--add-previous-pair
	:help "Add another 'Insert Pair Edit' (ipe) PAIR to the buffer by\
 searching backward for text which matches OPEN and CLOSE, and\
 creating a new PAIR from it.")
      'add-pair-forward)

    (define-key-after km [add-contents-forward]
      '(menu-item
	"Add CONTENTS (Search Forward) " ipe-edit--add-next-contents
	:help "Add another 'Insert Pair Edit' (ipe) PAIR to the buffer by\
 searching forward for text between OPEN and CLOSE and creating a new\
 PAIR around the text.")
      'add-pair-backward)

    (define-key-after km [add-contents-backward]
      '(menu-item
	"Add CONTENTS (Search Backward) "
	ipe-edit--add-previous-contents
	:help "Add another 'Insert Pair Edit' (ipe) PAIR to the buffer by\
 searching backward for text between OPEN and CLOSE, and creating a\
 new PAIR around the text.")
      'add-contents-forward)

    (define-key-after km [sep-1]
      '(menu-item "--" nil)
      'add-contents-backward)

    (define-key-after km [delete-first-pair]
      '(menu-item
	"Delete PAIR (First)" ipe-edit--delete-first-pair
	:help "Remove the first 'Insert Pair Edit' (ipe) PAIR from the\
 buffer.")
      'sep-1)

    (define-key-after km [delete-last-pair]
      '(menu-item
	"Delete PAIR (Last)" ipe-edit--delete-last-pair
	:help "Remove the last 'Insert Pair Edit' (ipe) PAIR from the\
 buffer.")
      'delete-first-pair)

    km))

(defvar ipe-menu--multiple-map (ipe-menu--multiple-map-init)
  "The Keymap for the Insert Pair Edit \"Multiple >\" sub-menu.

This sub-menu is displayed under the Insert Pair Edit mode menu.

It defines menu-items to add delete extra PAIRs.

\(See function: `ipe-edit-mode').")

;; -------------------------------------------------------------------
;;;;; 'Edit PAIR Definitions' sub-menu:
;; -------------------------------------------------------------------

(defun ipe-menu--edit-pair-defns-init ()
  "The Keymap for `Edit > Pairs > Edit PAIR Definitions >' sub-menu.

This sub-menu is displayed under the standard Emacs \"Edit\" menu,
under the \"Pairs\" > \"Edit PAIRS Definitions\" sub-menu.

It defines menu-items to add / update / delete Insert Pair Edit
\(ipe) PAIR definitions.

\(See function: `ipe-edit-mode')."

  (let ((km (make-sparse-keymap
	     "Edit - Pairs - Edit PAIR Definitions")))

    (define-key km [add-pair]
		(list 'menu-item
		      "Add PAIR Definition..."
		      (lambda () (interactive) (ipe-defn--ui-add-pair))
		      :keys (if (where-is-internal 'ipe-defn--edit-pair global-map)
				(substitute-command-keys "\\<global-map>\
\\[ipe-defn--edit-pair]")
			      "")
		      :help "Add a new 'Insert Pair Edit' (ipe) PAIR definition."
		      :visible '(not ipe-edit-mode)))

    ;; 2nd copy for `ipe-edit-mode' key-bindings
    (define-key-after km [ipe-add-pair]
      (list 'menu-item
	    "Add PAIR Definition..."
	    (lambda () (interactive) (ipe-defn--ui-add-pair))
	    :keys (if (where-is-internal
		       'ipe-defn--edit-pair ipe-edit-mode-map)
		      (substitute-command-keys "\\<ipe-edit-mode-map>\
\\[ipe-defn--edit-pair]")
		    "")
	    :help "Add a new Mode-Specific 'Insert Pair Edit' (ipe) PAIR\
 definition."
	    :visible 'ipe-edit-mode)
      'add-pair)

    (define-key-after km [add-mode-pair]
      (list 'menu-item
	    "Add Mode-Specific PAIR Definition..."
	    (lambda () (interactive) (ipe-defn--ui-add-mode-pair))
	    :keys (if (where-is-internal 'ipe-defn--edit-pair global-map)
		      (substitute-command-keys "\\<global-map>\
\\[ipe-defn--edit-mode-pair]")
		    "")
	    :help "Add a new Mode-Specific 'Insert Pair Edit' (ipe) PAIR\
 definition."
	    :visible '(not ipe-edit-mode))
      'ipe-add-pair)

    ;; 2nd copy for `ipe-edit-mode' key-bindings.
    (define-key-after km [ipe-add-mode-pair]
      (list 'menu-item
	    "Add Mode-Specific PAIR Definition..."
	    (lambda () (interactive) (ipe-defn--ui-add-mode-pair))
	    :keys (if (where-is-internal
		       'ipe-defn--edit-pair ipe-edit-mode-map)
		      (substitute-command-keys "\\<ipe-edit-mode-map>\
\\[ipe-defn--edit-mode-pair]")
		    "")
	    :help "Add a new Mode-Specific 'Insert Pair Edit' (ipe) PAIR \
definition."
	    :visible 'ipe-edit-mode)
      'add-mode-pair)

    (define-key-after km [sep-1]
      '(menu-item "--" nil)
      'ipe-add-mode-pair)

    (define-key-after km
      [edit-current-pair-defn]
      (list 'menu-item
	    "Edit Current PAIR Definition..."
	    (lambda () (interactive)
	      (if (ipe--mode-pair ipe--mnemonic major-mode)
		  (ipe-defn--ui-edit-mode-pair major-mode ipe--mnemonic)
		(ipe-defn--ui-edit-pair ipe--mnemonic)))
	    :keys (if (where-is-internal 'ipe-defn--edit-current-pair
					 ipe-edit-mode-map)
		      (substitute-command-keys "\\<ipe-edit-mode-map>\
 \\[ipe-defn--edit-current-pair]")
		    "")
	    :help "Edits the currently active 'Insert Pair Edit' (ipe) PAIR\
 definition."
	    :enable 'ipe--mnemonic
	    :visible 'ipe-edit-mode)
      'sep-1)

    (define-key-after km
      [edit-pair-mnemonic-defn]
      (list 'menu-item
	    "Edit MNEMONIC Definition..."
	    (lambda () (interactive) (ipe-defn--change-pair-mnemonic))
	    :keys (if (where-is-internal 'ipe-defn--change-pair-mnemonic
					 global-map)
		      (substitute-command-keys "\\<global-map>\
 \\[ipe-defn--change-pair-mnemonic]")
		    "")
	    :help "Changes the MNEMONIC for an 'Insert Pair Edit' (ipe) PAIR\
 definition."
	    :enable '(ipe-menu--pairs-p)
	    :visible '(not ipe-edit-mode))
      'edit-current-pair-defn)

    ;; 2nd Copy for `ipe-edit-mode' key-bindings.
    (define-key-after km
      [ipe-edit-pair-mnemonic-defn]
      (list 'menu-item
	    "Edit MNEMONIC Definition..."
	    (lambda () (interactive) (ipe-defn--change-pair-mnemonic))
	    :keys (if (where-is-internal 'ipe-defn--change-pair-mnemonic
					 ipe-edit-mode-map)
		      (substitute-command-keys "\\<ipe-edit-mode-map>\
 \\[ipe-defn--change-pair-mnemonic]")
		    "")
	    :help "Changes the MNEMONIC for an 'Insert Pair Edit' (ipe) PAIR\
 definition."
	    :enable '(ipe-menu--pairs-p)
	    :visible 'ipe-edit-mode)
      'edit-pair-mnemonic-defn)

    (define-key-after km
      [edit-mode-pair-mnemonic-defn]
      (list 'menu-item
	    "Edit Mode-Specific MNEMONIC Definition..."
	    (lambda () (interactive) (ipe-defn--change-mode-pair-mnemonic))
	    :keys (if (where-is-internal 'ipe-defn--change-mode-pair-mnemonic
					 global-map)
		      (substitute-command-keys "\\<global-map>\
 \\[ipe-defn--change-mode-pair-mnemonic]")
		    "")
	    :help "Changes the MNEMONIC for a Mode-Specific\
 'Insert Pair Edit' (ipe) PAIR definition."
	    :enable '(ipe-menu--mode-pairs-p major-mode)
	    :visible '(not ipe-edit-mode))
      'ipe-edit-pair-mnemonic-defn)

    ;; 2nd Copy for `ipe-edit-mode' key-bindings.
    (define-key-after km
      [ipe-edit-mode-pair-mnemonic-defn]
      (list 'menu-item
	    "Edit Mode-Specific MNEMONIC Definition..."
	    (lambda () (interactive) (ipe-defn--change-mode-pair-mnemonic))
	    :keys (if (where-is-internal 'ipe-defn--change-mode-pair-mnemonic
					 ipe-edit-mode-map)
		      (substitute-command-keys "\\<ipe-edit-mode-map>\
 \\[ipe-defn--change-mode-pair-mnemonic]")
		    "")
	    :help "Changes the MNEMONIC for a Mode-Specific\
 'Insert Pair Edit' (ipe) PAIR definition."
	    :enable '(ipe-menu--mode-pairs-p major-mode)
	    :visible 'ipe-edit-mode)
      'edit-mode-pair-mnemonic-defn)

    (define-key-after km [sep-2]
      '(menu-item "--" nil)
      'ipe-edit-mode-pair-mnemonic-defn)

    (define-key-after km [delete-pair-defn-]
      (list 'menu-item
	    "Delete PAIR Definition..."
	    (lambda () (interactive) (ipe-defn--delete-pair))
	    :keys (if (where-is-internal 'ipe-defn--delete-pair
					 global-map)
		      (substitute-command-keys "\\<global-map>\
\\[ipe-defn--delete-pair]")
		    "")
	    :help "Delete an 'Insert Pair Edit' (ipe) PAIR definition."
	    :enable '(ipe-menu--pairs-p)
	    :visible '(not ipe-edit-mode))
      'sep-2)

    ;; 2nd Copy for `ipe-edit-mode' key-bindings.
    (define-key-after km
      [ipe-delete-pair-defn-]
      (list 'menu-item
	    "Delete PAIR Definition..."
	    (lambda () (interactive) (ipe-defn--delete-pair))
	    :keys (if (where-is-internal 'ipe-defn--delete-pair
					 ipe-edit-mode-map)
		      (substitute-command-keys "\\<ipe-edit-mode-map>\
\\[ipe-defn--delete-pair]")
		    "")
	    :help "Delete an 'Insert Pair Edit' (ipe) PAIR definition."
	    :enable '(ipe-menu--pairs-p)
	    :visible 'ipe-edit-mode)
      'delete-pair-defn-)

    (define-key-after km
      [delete-mode-pair-defn-]
      (list 'menu-item
	    "Delete Mode-Specific PAIR Definition..."
	    (lambda () (interactive) (ipe-defn--delete-mode-pair))
	    :keys (if (where-is-internal 'ipe-defn--delete-mode-pair
					 global-map)
		      (substitute-command-keys "\\<global-map>>\
\\[ipe-defn--delete-mode-pair]")
		    "")
	    :help "Delete a Mode-Specific 'Insert Pair Edit' (ipe) PAIR\
 definition."
	    :enable '(ipe-menu--mode-pairs-p major-mode)
	    :visible '(not ipe-edit-mode))
      'ipe-delete-pair-defn-)

    ;; 2nd Copy for `ipe-edit-mode' key-bindings.
    (define-key-after km
      [ipe-delete-mode-pair-defn-]
      (list 'menu-item
	    "Delete Mode-Specific PAIR Definition..."
	    (lambda () (interactive) (ipe-defn--delete-mode-pair))
	    :keys (if (where-is-internal 'ipe-defn--delete-mode-pair
					 ipe-edit-mode-map)
		      (substitute-command-keys "\\<ipe-edit-mode-map>\
\\[ipe-defn--delete-mode-pair]")
		    "")
	    :help "Delete a Mode-Specific 'Insert Pair Edit' (ipe) PAIR\
 definition."
	    :enable '(ipe-menu--mode-pairs-p major-mode)
	    :visible 'ipe-edit-mode)
      'delete-mode-pair-defn-)

    (define-key-after km [sep-3]
      '(menu-item "--" nil)
      'ipe-delete-mode-pair-defn-)

    km))

(defvar ipe-menu--edit-pair-defns-map (ipe-menu--edit-pair-defns-init)
  "The Keymap for `Edit > Pairs > Edit PAIR Definitions >' sub-menu.

This sub-menu is displayed under the standard Emacs \"Edit\" menu,
under the \"Pairs\" > \"Edit PAIRS Definitions\" sub-menu.

It defines menu-items to add / update / delete Insert Pair Edit
\(ipe) PAIR definitions.

\(See function: `ipe-edit-mode').")

;; -------------------------------------------------------------------
;;;; Emacs "Edit" menu addition:
;; -------------------------------------------------------------------

(defun ipe-menu--emacs-edit-pairs-init ()
  "Generate the 'Edit' > 'Pairs' sub-menu.

This new menu item will provide a sub-menu of items of the form:

- Edit >
  - Pairs >
    - Insert PAIR >
      - '<OPEN>' ... '<CLOSE>'
      - ...
    - Update PAIR >
      - '<OPEN>' ... '<CLOSE>'
      - ...
    - Delete PAIR >
      - '<OPEN>' ... '<CLOSE>'
      - ...
    - Edit PAIR Definitions >
      - ...
    - Options
    - Info
    - Help

Which will call `ipe-insert-pair-edit' / `ipe-insert-pair-edit-update'
/ or `ipe-insert-pair-edit-delete' with one of the current set of
`ipe-pairs' / `ipe-mode-pairs' PAIRs.

This changes based upon the current buffers MAJOR-MODE."

  (interactive)

  (let ((km (make-sparse-keymap "Pairs")))

    (define-key-after km [pairs--sep-1]
      '(menu-item "--" nil))

    (define-key-after km
      [pairs--pair-defns]
      (list 'menu-item "Edit PAIR Definitions"
	    ipe-menu--edit-pair-defns-map))

    (define-key-after km [pairs--sep-2]
      '(menu-item "--" nil))

    (define-key-after km [pairs--options]
      '(menu-item
	"Options" ipe-edit--options
	:help "Customize 'Insert Pair Edit' (ipe)."))

    (define-key-after km [pairs--info]
      '(menu-item
	"Info" ipe-help--info
	:help "Display the 'Insert Pair Edit' (ipe) info file."))

    (define-key-after km [pairs--help]
      '(menu-item
	"Help" ipe-help
	:help "Display the 'Insert Pair Edit' (ipe) help."))
    km))

(defvar ipe-menu--emacs-edit-pairs-map
  (ipe-menu--emacs-edit-pairs-init)
  "The Keymap for the `Edit > Pairs >' sub-menu.

This sub-menu is displayed under the standard Emacs \"Edit\" menu,
under the \"Pairs\" sub-menu.

It defines menu-items to insert / update / delete Insert Pair Edit
\(ipe) PAIRs.

\(See function: `ipe-edit-mode').")

;; -------------------------------------------------------------------
;;;; Minor-mode sub-menu:
;;
;; Insert Pair Edit
;; ---
;; - Insert PAIR
;; - Insert And >
;; ---
;; - Move OPEN Start
;; - Move OPEN Up
;; - Move OPEN Forward
;; - Move OPEN Backward
;; - Move OPEN Down
;; - Move OPEN End
;; ---
;; - Move CLOSE Start
;; - Move CLOSE Up
;; - Move CLOSE Forward
;; - Move CLOSE Backward
;; - Move CLOSE Down
;; - Move CLOSE End
;; ---
;; - Change PAIR >
;; - Change Movement >
;; - Edit CONTENTS >
;; - Next / Previous >
;; - Multiple >
;; ---
;; - Edit PAIR Definitions >
;; ---
;; - Abort
;; - Options
;; - Info
;; - Help
;;
;; -------------------------------------------------------------------

;; 'Insert Pair Edit' Minor Mode Maps
(defun ipe-menu--mode-init ()
  "Create the Keymap for the Insert Pair Edit Minor Mode menu.

This menu is displayed when the Insert Pair Edit (function:
`ipe-edit-mode') Minor Mode is active.

It defines menu-items to call various `ipe-edit--*' functions."

  (let ((km (make-sparse-keymap "Insert Pair Edit")))

    ;; Insert Commands.
    (define-key-after km [insert-pair]
      '(menu-item
	"Insert PAIR" ipe-edit--insert-pair
	:help "Insert the 'Insert Pair Edit' (ipe) OPEN and CLOSE into\
 the buffer, and exit 'ipe-edit-mode'."))

    (define-key-after km [insert-and]
      (list 'menu-item "Insert And" ipe-menu--insert-and-map)
      'insert-pair)

    (define-key-after km [sep-1]
      '(menu-item "--" nil)
      'insert-and)

    ;; OPEN Movement.
    (define-key-after km [open-beg]
      '(menu-item
	"Move OPEN Beginning" ipe-edit--open-beg
	:help "Move the OPEN of the 'Insert Pair Edit' (ipe)\
 PAIR to the 'beginning'.")
      'sep-1)

    (define-key-after km [open-up]
      '(menu-item
	"Move OPEN Up" ipe-edit--open-up
	:help "Move the OPEN of the 'Insert Pair Edit' (ipe)\
 PAIR 'upwards'.")
      'open-beg)

    (define-key-after km [open-backward]
      '(menu-item
	"Move OPEN Backward" ipe-edit--open-backward
	:help "Move the OPEN of the 'Insert Pair Edit' (ipe)\
 PAIR 'backwards'.")
      'open-up)

    (define-key-after km [open-forward]
      '(menu-item
	"Move OPEN Forward" ipe-edit--open-forward
	:help "Move the OPEN of the 'Insert Pair Edit' (ipe)\
 PAIR 'forwards'.")
      'open-backward)

    (define-key-after km [open-down]
      '(menu-item
	"Move OPEN Down" ipe-edit--open-down
	:help "Move the OPEN of the 'Insert Pair Edit' (ipe)\
 PAIR 'downwards'.")
      'open-forward)

    (define-key-after km [open-end]
      '(menu-item
	"Move OPEN End" ipe-edit--open-end
	:help "Move the OPEN of the 'Insert Pair Edit' (ipe)\
 PAIR to the 'end'.")
      'open-down)

    ;; CLOSE Movement.
    (define-key-after km [sep-2]
      '(menu-item "--" nil)
      'open-end)

    (define-key-after km [close-beg]
      '(menu-item
	"Move CLOSE Beginning" ipe-edit--close-beg
	:help "Move the CLOSE of the 'Insert Pair Edit' (ipe)\
 PAIR to the 'beginning'.")
      'sep-2)

    (define-key-after km [close-up]
      '(menu-item
	"Move CLOSE Up" ipe-edit--close-up
	:help "Move the CLOSE of the 'Insert Pair Edit' (ipe)\
 PAIR 'upwards'.")
      'close-beg)

    (define-key-after km [close-backward]
      '(menu-item
	"Move CLOSE Backward" ipe-edit--close-backward
	:help "Move the CLOSE of the 'Insert Pair Edit' (ipe)\
 PAIR 'backwards'.")
      'close-up)

    (define-key-after km [close-forward]
      '(menu-item
	"Move CLOSE Forward" ipe-edit--close-forward
	:help "Move the CLOSE of the 'Insert Pair Edit' (ipe)\
 PAIR 'forwards'.")
      'close-backward)

    (define-key-after km [close-down]
      '(menu-item
	"Move CLOSE Down" ipe-edit--close-down
	:help "Move the CLOSE of the 'Insert Pair Edit' (ipe)\
 PAIR 'downwards'.")
      'close-forward)

    (define-key-after km [close-end]
      '(menu-item
	"Move CLOSE End" ipe-edit--close-end
	:help "Move the CLOSE of the 'Insert Pair Edit' (ipe)\
 PAIR to the 'end'.")
      'close-down)

    (define-key-after km [sep-3]
      '(menu-item "--" nil)
      'close-end)

    ;; Misc Commands,
    (define-key-after km [change-pair]
      (list 'menu-item "Change PAIR" (ipe-menu--change-pair))
      'sep-3)

    (define-key-after km [change-movement]
      (list 'menu-item "Change Movement" ipe-menu--movement-by-map)
      'change-pair)

    (define-key-after km [edit-contents]
      (list 'menu-item "Edit CONTENTS" ipe-menu--edit-contents-map)
      'change-movement)

    (define-key-after km [next-prev]
      (list 'menu-item "Next / Previous" ipe-menu--next-prev-map)
      'edit-contents)

    (define-key-after km [multiple]
      (list 'menu-item "Multiple" ipe-menu--multiple-map)
      'next-prev)

    (define-key-after km [sep-4]
      '(menu-item "--" nil)
      'multiple)

    (define-key-after km [toggle-escapes]
      '(menu-item
	"Show ESCAPES"
	ipe-edit--toggle-escapes
	:button (:toggle . ipe--escapes-show-p)
	:visible (ipe-menu--escapes-p)
	:help "Process the :escapes within a 'Insert Pair Edit' (ipe)\
 PAIR definition.")
      'sep-4)

    ;; PAIR Definition Commands.
    (define-key-after km [edit-pair-defns]
      (list 'menu-item
	    "Edit PAIR Definitions"
	    ipe-menu--edit-pair-defns-map)
      'toggle-escapes)

    (define-key-after km [sep-5]
      '(menu-item "--" nil)
      'edit-pair-defns)

    (define-key-after km [abort]
      '(menu-item
	"Abort" ipe-edit--abort
	:help "Exit 'ipe-edit-mode' without inserting OPEN and CLOSE to\
 the buffer.")
      'sep-5)

    ;; Help / Configuration Commands.
    (define-key-after km [options]
      '(menu-item
	"Options" ipe-edit--options
	:help "Customize 'Insert Pair Edit' (ipe).")
      'abort)

    (define-key-after km [info]
      '(menu-item
	"Info" ipe-help--info
	:help "Display the 'Insert Pair Edit' (ipe) info file.")
      'options)

    (define-key-after km [help]
      '(menu-item
	"Help" ipe-help--mode-help
	:help "Display the 'Insert Pair Edit' (ipe) help.")
      'info)

    km))

;; 'Insert Pair Edit' Minor Mode Maps
(defvar ipe-menu--mode-map (ipe-menu--mode-init)
  "The Keymap for the Insert Pair Edit Minor Mode menu.

This menu is displayed when the Insert Pair Edit (function:
`ipe-edit-mode') Minor Mode is active.

It defines menu-items to call various `ipe-edit--*' functions.")

;; -------------------------------------------------------------------
;;;; Mouse menu:
;; -------------------------------------------------------------------

(defun ipe-menu--mouse-map-init ()
  "The Keymap for the Insert Pair Edit `Right Click' menu.

This menu is displayed by a Right Click when the Insert Pair Edit
Minor Mode is active.

It defines menu-items to perform basic `ipe-edit--*' functions.

\\(See function: `ipe-edit-mode')."

  (let ((km (make-sparse-keymap "Insert Pair Edit")))

    (define-key km [ipe-mouse-insert]
		'(menu-item
		  "Insert PAIR" ipe-edit--insert-pair
		  :help "Insert the 'Insert Pair Edit' (ipe) OPEN and CLOSE\
 into the buffer and exit 'ipe-edit-mode'."))

    (define-key-after km [ipe-mouse-ia]
      (list 'menu-item "Insert And" ipe-menu--insert-and-map)
      'ipe-mouse-insert)

    (define-key-after km [ipe-mouse-sep-1]
      '(menu-item "--" nil)
      'ipe-mouse-ia)

    (define-key-after km [ipe-mouse-delete-pair]
      (list 'menu-item "Delete PAIR" 'ipe-edit--delete-first-pair)
      'ipe-mouse-sep-1)

    (define-key-after km [ipe-mouse-change-pair]
      (list 'menu-item "Change PAIR" (ipe-menu--change-pair))
      'ipe-mouse-delete-pair)

    (define-key-after km [ipe-mouse-edit-contents]
      (list 'menu-item "Edit CONTENTS" ipe-menu--edit-contents-map)
      'ipe-mouse-edit-pair)

    (define-key-after km [ipe-mouse-next-prev]
      (list 'menu-item "Next / Previous" ipe-menu--next-prev-map)
      'ipe-mouse-edit-contents)

    (define-key-after km [ipe-mouse-sep-2]
      '(menu-item "--" nil)
      'ipe-mouse-next-prev)

    (define-key-after km [ipe-mouse-abort]
      '(menu-item
	"Abort" ipe-edit--abort
	:help "Exit 'ipe-edit-mode' without inserting OPEN and CLOSE to\
 the buffer.")
      'ipe-mouse-sep-2)

    (define-key-after km [ipe-mouse-help]
      '(menu-item
	"Help" ipe-help--mode-help
	:help "Display the 'Insert Pair Edit' (ipe) help.")
      'ipe-mouse-abort)

    km))

(defvar ipe-menu--mouse-map (ipe-menu--mouse-map-init)
  "The Keymap for the Insert Pair Edit `Right Click' menu.

This menu is displayed by a Right Click when the Insert Pair Edit
Minor Mode is active.

It defines menu-items to perform basic `ipe-edit--*' functions.

\\(See function: `ipe-edit-mode').")

;; -------------------------------------------------------------------
;;;; Dynamically generated menus:
;; -------------------------------------------------------------------

(defun ipe-menu--create-sub-menu (pairs menu-path menu-item-fn)
  "Generate a SUB-MENU for the `ipe' PAIRS under MENU-PATH.

Where:

- PAIRS is a list of Insert Pair Edit (ipe) PAIR definitions (see:
  `ipe-pairs')
- MENU-PATH is a `/'-separated string, representing the path to the
  sub-menu being generated from the PAIRS list.
- MENU-ITEM-FN is a function used to generate the `'menu-items' within
  the returned SUB-MENU for each eligible PAIR within PAIRS.

Eligible definitions within PAIRS are determined by querying the :menu
property of the PAIR.  If the value of the :menu property for the
given PAIR matches MENU-PATH, a `'menu-item' for the given PAIR is
added to the returned KEYMAP by calling MENU-ITEM-FN to generate a
suitable Emacs `'menu-item' structure.

- MENU-ITEM-FN is expected to be a function which takes two arguments:

  (MENU-ITEM-FN KEYMAP MNEMONIC)

  and which adds a `'menu-item' suitable for use within an Emacs menu
  to the given KEYMAP.

The `ipe-menu--create-sub-menu' function will recursively descend the
`/'-separated SUB-MENUs within PAIRs to generate further sub-menus,
and will returns a list:

  (\\='menu-item MENU-NAME KEYMAP)

Suitable for use as an Emacs menu.

- MENU-NAME is the last `/'-separated string within MENU-PATH.  This
  is the name of the menu displayed by the Emacs UI within the
  menu-bar.
- KEYMAP is the recursive set of sub-menus generated from the `ipe'
  PAIRS using the MENU-ITEM-FN."

  (let ((menu-keymap (make-sparse-keymap menu-path))
	(menu-name   (replace-regexp-in-string ".*/" "" menu-path)))

    ;; Generate the keymap for the `menu-path'.
    (ipe-menu--sub-menu-keymap pairs
			       menu-keymap
			       menu-path
			       menu-item-fn)

    ;; Return the `menu-keymap' as a sub-menu.
    (list 'menu-item
	  menu-name
	  menu-keymap
	  :help (concat ":menu group for \"" menu-name "\""))))

(defun ipe-menu--insert-pair-menu-item (keymap mnemonic)
  "Add an \"Insert\" `'menu-item' for an `ipe' PAIR to a KEYMAP.

Add a `'menu-item' that calls `ipe-insert-pair-edit' for the `ipe'
PAIR with the given MNEMONIC to KEYMAP."

  (define-key-after keymap
    (vector (intern (concat "mnemonic-" mnemonic)))
    (list 'menu-item
	  (ipe-menu--text mnemonic major-mode)
	  (list 'lambda nil (list 'interactive)
		(list 'ipe-insert-pair-edit nil mnemonic))
	  :keys mnemonic
	  :help (concat "Insert an 'Insert Pair Edit' (ipe) PAIR "
			"with an "
			(ipe--mnemonic-describe mnemonic
						major-mode)))))

(defun ipe-menu--insert-pair ()
  "Generate the \"Insert PAIR\" sub-menu.

This returns a new `'menu-item' which provides a tree of sub-menus of
the form:

- Edit >
  - Pairs >
    - Insert PAIR >
      - <custom-menu-1>
	- <OPEN> ... <CLOSE>
	...
      - <OPEN> ... <CLOSE>
      - ...

Which call `ipe-insert-pair-edit' with one of the current set of
`ipe-pairs' / `ipe-mode-pairs' PAIRs.

This contents of this sub-menu change based upon the current buffers
MAJOR-MODE."

  (interactive)
  (let ((km    (make-sparse-keymap "Insert PAIR"))
	(pairs (ipe--pairs major-mode)))
    (ipe-menu--sub-menu-keymap pairs
			       km
			       nil
			       'ipe-menu--insert-pair-menu-item)
    km))

(defun ipe-menu--update-pair-menu-item (keymap mnemonic)
  "Add an \"Update\" `'menu-item' for `ipe' PAIR with MNEMONIC to \
KEYMAP.

Add a `'menu-item' that calls `ipe-insert-pair-edit-update' for the
`ipe' PAIR with the given MNEMONIC to KEYMAP."

  (define-key-after keymap
    (vector (intern (concat "mnemonic-" mnemonic)))
    (list 'menu-item
	  (ipe-menu--text mnemonic major-mode)
	  (list 'lambda nil (list 'interactive)
		(list 'ipe-insert-pair-edit-update mnemonic))
	  :keys mnemonic
	  :help (concat "Update the position the "
			(ipe--mnemonic-describe mnemonic major-mode)
			" of an existing 'Insert Pair Edit' (ipe) PAIR."))))

(defun ipe-menu--update-pair ()
  "Generate the 'Update PAIR' sub-menu.

This returns a new menu item which provides a sub-menu of items of the
form:

- Edit >
  - Pairs >
    - Update PAIR >
      - <custom-menu-1>
	- '<OPEN>' ... '<CLOSE>'
	- ...
      - '<OPEN>' ... '<CLOSE>'
      - ...

Which will call `ipe-insert-pair-edit-update' with one of the current
set of `ipe-pairs' / `ipe-mode-pairs' PAIRs.

This contents of this sub-menu changes based upon the current buffers
MAJOR-MODE."

  (interactive)
  (let ((km    (make-sparse-keymap "Update PAIR"))
	(pairs (ipe--pairs major-mode)))
    (ipe-menu--sub-menu-keymap pairs
			       km
			       nil
			       'ipe-menu--update-pair-menu-item)
    km))

(defun ipe-menu--delete-pair-menu-item (keymap mnemonic)
  "Add a \"Delete\" `'menu-item' for an `ipe' PAIR to a KEYMAP.

Add a `'menu-item' that calls `ipe-insert-pair-edit-delete' for the
`ipe' PAIR with the given MNEMONIC to KEYMAP."

  (define-key-after keymap
    (vector (intern (concat "mnemonic-" mnemonic)))
    (list 'menu-item
	  (ipe-menu--text mnemonic major-mode)
	  (list 'lambda nil (list 'interactive)
		(list 'ipe-insert-pair-edit-delete mnemonic))
	  :keys mnemonic
	  :help (concat "Delete the "
			(ipe--mnemonic-describe mnemonic
						major-mode)
			" of an existing 'Insert Pair Edit' (ipe) PAIR."))))

(defun ipe-menu--delete-pair ()
  "Generate the 'Delete PAIR' sub-menu.

This returns a new menu item which provides a sub-menu of items of the
form:

- Edit >
  - Pairs >
    - Delete PAIR >
      - <custom-menu-1>
	- '<OPEN>' ... '<CLOSE>'
	- ...
      - '<OPEN>' ... '<CLOSE>'
      - ...

Which will call `ipe-insert-pair-edit-delete' with one of the current
set of `ipe-pairs' / `ipe-mode-pairs' PAIRs.

This contents of this sub-menu changes based upon the current buffers
MAJOR-MODE."

  (interactive)
  (let ((km    (make-sparse-keymap "Delete PAIR"))
	(pairs (ipe--pairs major-mode)))
    (ipe-menu--sub-menu-keymap pairs
			       km
			       nil
			       'ipe-menu--delete-pair-menu-item)
    km))

(defun ipe-menu--edit-pair-defn ()
  "Generate the \"Edit PAIR Definition\" sub-menu.

This returns a new menu item which provides a sub-menu of items of the
form:

- Edit >
  - Pairs >
    - Edit PAIR Definitions >
      - Edit PAIR Definition >
	- '<OPEN>' ... '<CLOSE>'
	- ...

Which will call `ipe-defn--ui-edit-pair' with one of the current set
of `ipe-pairs' PAIRs.

The contents of this sub-menu changes based upon the current buffers
MAJOR-MODE."

  (interactive)
  (let ((km (make-sparse-keymap "Edit PAIR Definition")))
    (mapc
     (lambda (mnemonic)
       (define-key-after km
	 (vector (intern (concat "mnemonic-" mnemonic)))
	 (list 'menu-item (ipe-menu--text mnemonic t)
	       (list 'lambda nil (list 'interactive)
		     (list 'ipe-defn--ui-edit-pair mnemonic))
	       :keys mnemonic
	       :help (concat "Edit the 'Insert Pair Edit' (ipe) "
			     "PAIR definition with the MNEMONIC '"
			     mnemonic
			     "' and the definition ("
			     (ipe--mnemonic-describe mnemonic t)
			     ")."))))
     (ipe--mnemonic-list t))
    km))

(defun ipe-menu--edit-mode-pair-defn ()
  "Generate 'Edit Mode-Specific PAIR Definitions' sub-menu.

This returns a new menu item which provides a sub-menu of items of the
form:

- Edit >
  - Pairs >
    - Edit PAIR Definitions >
      - Edit Mode-Specific PAIR Definitions >
	- '<OPEN>' ... '<CLOSE>'
	- ...

Which will call `ipe-defn--ui-edit-mode-pair' with one of the current
set of `ipe-mode-pairs' PAIRs.

The contents of this sub-menu changes based upon the current buffers
MAJOR-MODE."

  (interactive)
  (let ((km   (make-sparse-keymap
	       "Edit Mode-Specific PAIR Definitions"))
	(mode major-mode))
    (mapc
     (lambda (mnemonic)
       (define-key-after km
	 (vector (intern (concat (symbol-name mode)
				 "-mnemonic-" mnemonic)))
	 (list 'menu-item (ipe-menu--text mnemonic mode)
	       (list 'lambda nil (list 'interactive)
		     (list 'ipe-defn--ui-edit-mode-pair
			   (list 'quote mode) mnemonic))
	       :keys mnemonic
	       :help (concat "Edit the 'Insert Pair Edit' (ipe) "
			     "Mode-Specific PAIR definition for the '"
			     (symbol-name mode)
			     "' mode with the MNEMONIC '"
			     mnemonic
			     "' and the definition ("
			     (ipe--mnemonic-describe mnemonic mode)
			     ")."))))
     (ipe--mode-mnemonic-list mode))
    km))

(defun ipe-menu--delete-pair-defn ()
  "Generate the 'Delete PAIR Definitions' sub-menu.

This returns a new menu item which provides a sub-menu of items of the
form:

- Edit >
  - Pairs >
    - Edit PAIR Definitions >
      - Delete PAIR Definition >
	- '<OPEN>' ... '<CLOSE>'
	- ...

Which will call `ipe-defn--delete-pair' with one of the current set of
`ipe-pairs' PAIRs.

The contents of this sub-menu changes based upon the current buffers
MAJOR-MODE."

  (interactive)
  (let ((km (make-sparse-keymap "Delete PAIR Definition")))
    (mapc
     (lambda (mnemonic)
       (define-key-after km
	 (vector (intern (concat "mnemonic-" mnemonic)))
	 (list 'menu-item (ipe-menu--text mnemonic t)
	       (list 'lambda nil (list 'interactive)
		     (list 'ipe-defn--delete-pair mnemonic))
	       :keys mnemonic
	       :help (concat "Delete the 'Insert Pair Edit' (ipe) "
			     "PAIR definition with the MNEMONIC '"
			     mnemonic
			     "' and the definition ("
			     (ipe--mnemonic-describe mnemonic t)
			     ")."))))
     (ipe--mnemonic-list t))
    km))

(defun ipe-menu--delete-mode-pair-defn ()
  "Generate 'Delete Mode-Specific PAIR Definitions' sub-menu.

This returns a new menu item which provides a sub-menu of items of the
form:

- Edit >
  - Pairs >
    - Edit PAIR Definitions >
      - Delete Mode-Specific PAIR Definitions >
	- '<OPEN>' ... '<CLOSE>'
	- ...

Which will call `ipe-defn--delete-mode-pair' with one of the current
set of `ipe-mode-pairs' PAIRs.

The contents of this sub-menu changes based upon the current buffers
MAJOR-MODE."

  (interactive)
  (let ((km   (make-sparse-keymap
	       "Delete Mode-Specific PAIR Definition"))
	(mode major-mode))
    (mapc
     (lambda (mnemonic)
       (define-key-after km
	 (vector (intern (concat (symbol-name mode)
				 "-mnemonic-" mnemonic)))
	 (list 'menu-item (ipe-menu--text mnemonic mode)
	       (list 'lambda nil (list 'interactive)
		     (list 'ipe-defn--delete-mode-pair
			   (list 'quote mode) mnemonic))
	       :keys mnemonic
	       :help (concat "Delete the 'Insert Pair Edit' (ipe) "
			     "Mode-Specific PAIR definition for the '"
			     (symbol-name mode)
			     "' mode with the MNEMONIC '"
			     mnemonic
			     "' and the definition ("
			     (ipe--mnemonic-describe mnemonic mode)
			     ")."))))
     (ipe--mode-mnemonic-list mode))
    km))

;; -------------------------------------------------------------------
;;;; Menu update functions:
;; -------------------------------------------------------------------

(defun ipe-menu--insert-pair-update ()
  "Update the contents of the 'Insert PAIR' sub-menu.

This updates the contents of the:

- Edit >
  - Pairs >
    - Insert PAIR >
      - ...

Sub-menu based upon the current buffers MAJOR-MODE."

  (define-key ipe-menu--emacs-edit-pairs-map [insert-pair]
	      (list 'menu-item "Insert PAIR"
		    (ipe-menu--insert-pair)
		    :keys (if (where-is-internal 'ipe-insert-pair-edit)
			      (substitute-command-keys "\\[ipe-insert-pair-edit]")
			    "")
		    :help "Insert an 'Insert Pair Edit' (ipe) PAIR."
		    :enable '(ipe-menu--pairs-p))))

(defun ipe-menu--update-pair-update ()
  "Update the contents of the 'Update PAIR' sub-menu.

This updates the contents of the:

- Edit >
  - Pairs >
    - Update PAIR >
      - ...

Sub-menu based upon the current buffers MAJOR-MODE."

  (define-key-after ipe-menu--emacs-edit-pairs-map [update-pair]
    (list 'menu-item "Update PAIR"
	  (ipe-menu--update-pair)
	  :keys (if (where-is-internal 'ipe-insert-pair-edit-update)
		    (substitute-command-keys "\\[ipe-insert-pair-edit-update]")
		  (if (where-is-internal 'ipe-insert-pair-edit)
		      (substitute-command-keys "\\[universal-argument]\
 \\[ipe-insert-pair-edit]")
		    ""))
	  :help "Update an 'Insert Pair Edit' (ipe) PAIR."
	  :enable '(ipe-menu--pairs-p))
    'insert-pair))

(defun ipe-menu--delete-pair-update ()
  "Update the contents of the 'Delete PAIR' sub-menu.

This updates the contents of the:

- Edit >
  - Pairs >
    - Delete PAIR >
      - ...

Sub-menu based up on the current buffers MAJOR-MODE."

  (define-key-after ipe-menu--emacs-edit-pairs-map [delete-pair]
    (list 'menu-item "Delete PAIR"
	  (ipe-menu--delete-pair)
	  :keys (if (where-is-internal 'ipe-insert-pair-edit-delete)
		    (substitute-command-keys "\\[ipe-insert-pair-edit-delete]")
		  (if (where-is-internal 'ipe-insert-pair-edit)
		      (substitute-command-keys "\\[universal-argument]\
 \\[universal-argument] \\[ipe-insert-pair-edit]")
		    ""))
	  :help "Delete an 'Insert Pair Edit' (ipe) PAIR."
	  :enable '(ipe-menu--pairs-p))
    'update-pair))

(defun ipe-menu--edit-pair-defn-update ()
  "Update the contents of the 'Edit PAIR Definition' sub-menu.

This updates the contents of the:

- Edit >
  - Pairs >
    - Edit PAIR Definitions >
      - Edit PAIR Definition >
	- ...

Sub-menu based upon the current buffers MAJOR-MODE."

  (define-key-after ipe-menu--edit-pair-defns-map [edit-pair-defn]
    (list 'menu-item "Edit PAIR Definition"
	  (ipe-menu--edit-pair-defn)
	  :help "Edit an 'Insert Pair Edit' (ipe) PAIR definition."
	  :enable '(ipe-menu--pairs-p))
    'edit-current-pair-defn))

(defun ipe-menu--edit-mode-pair-defn-update ()
  "Update the 'Edit Mode-Specific PAIR Definition' sub-menu.

This updates the contents of the:

- Edit >
  - Pairs >
    - Edit PAIR Definitions >
      - Edit Mode-Specific PAIR Definition >
	- ...

Sub-menu based up the current buffers MAJOR-MODE."

  (define-key-after ipe-menu--edit-pair-defns-map
    [edit-mode-pair-defn]
    (list 'menu-item "Edit Mode-Specific PAIR Definition"
	  (ipe-menu--edit-mode-pair-defn)
	  :help "Edit a Mode-Specific 'Insert Pair Edit' (ipe) PAIR\
 definition."
	  :enable '(ipe-menu--mode-pairs-p major-mode))
    'edit-pair-defn))

(defun ipe-menu--delete-pair-defn-update ()
  "Update the contents of the 'Delete PAIR Definition' sub-menu.

This updates the contents of the:

- Edit >
  - Pairs >
    - Edit PAIR Definitions >
      - Delete PAIR Definition >
	- ...

Sub-menu based upon the current buffers MAJOR-MODE."

  (define-key-after ipe-menu--edit-pair-defns-map [delete-pair-defn]
    (list 'menu-item "Delete PAIR Definition"
	  (ipe-menu--delete-pair-defn)
	  :help "Delete an 'Insert Pair Edit' (ipe) PAIR definition."
	  :enable '(ipe-menu--pairs-p))
    'delete-pair-defn))

(defun ipe-menu--delete-mode-pair-defn-update ()
  "Update the 'Delete Mode-Specific PAIR Definition' sub-menu.

This updates the contents of the:

- Edit >
  - Pairs >
    - Edit PAIR Definitions >
      - Delete Mode-Specific PAIR Definition >
	- ...

Sub-menu based upon the current buffers MAJOR-MODE."

  (define-key-after ipe-menu--edit-pair-defns-map
    [delete-mode-pair-defn]
    (list 'menu-item "Delete Mode-Specific PAIR Definition"
	  (ipe-menu--delete-mode-pair-defn)
	  :help "Delete a Mode-Specific 'Insert Pair Edit' (ipe) PAIR\
 definition."
	  :enable '(ipe-menu--mode-pairs-p major-mode))
    'delete-mode-pair-defn))

(defun ipe-menu--change-pair-update ()
  "Update the contents of the 'Change PAIR' mode menu.

This updates the contents of the:

- Insert Pair Edit >
  - Change PAIR >
    - ...

Sub-menu based upon the current buffers MAJOR-MODE."

  (define-key-after ipe-menu--mode-map [change-pair]
    (list 'menu-item "Change PAIR" (ipe-menu--change-pair)
	  :keys (substitute-command-keys "\\<ipe-edit-mode-map>\
 \\[ipe-edit--change-pair]")
	  :help "Replace the current 'Insert Pair Edit' (ipe) PAIR with\
 another PAIR."
	  :enable '(ipe-menu--pairs-p))
    'sep-3)

  (define-key-after ipe-menu--mouse-map [ipe-mouse-change-pair]
    (list 'menu-item "Change PAIR" (ipe-menu--change-pair)
	  :keys (substitute-command-keys "\\<ipe-edit-mode-map>\
 \\[ipe-edit--change-pair]")
	  :help "Replace the current 'Insert Pair Edit' (ipe) PAIR with\
 another PAIR."
	  :enable '(ipe-menu--pairs-p))
    'ipe-mouse-delete-pair))

(defun ipe-menu--emacs-edit-pairs-update ()
  "Add 'Pairs' sub-menu to the 'Edit' menu.

This updates the contents of the:

Edit >
  - Pairs >
    - ...

Sub-menu based upon the `ipe-menu-display-in-edit-p' `customize'-ation
variable."

  (define-key-after menu-bar-edit-menu [ipe]
    (list 'menu-item "Pairs" ipe-menu--emacs-edit-pairs-map
	  :visible ipe-menu-display-in-edit-p
	  :enable '(not ipe-edit-mode))))

(defun ipe-menu--update ()
  "Update the Insert Pair Edit menus.

Added to the `menu-bar-update-hook' to ensure that the `ipe' menus are
updated according to the current buffers MAJOR-MODE."

  (condition-case nil
      (progn
	(ipe-menu--emacs-edit-pairs-update)
	(ipe-menu--insert-pair-update)
	(ipe-menu--update-pair-update)
	(ipe-menu--delete-pair-update)
	(ipe-menu--edit-pair-defn-update)
	(ipe-menu--edit-mode-pair-defn-update)
	(ipe-menu--delete-pair-defn-update)
	(ipe-menu--delete-mode-pair-defn-update)
	(ipe-menu--change-pair-update))
    (t (progn (message "Error updating `ipe' menus.")))))

;; -------------------------------------------------------------------
;;;; Menu control:
;; -------------------------------------------------------------------

(defun ipe-menu--install ()
  "Install the menu bindings for the Insert Pair Edit mode."

  (define-key ipe-edit-mode-map [menu-bar ipe]
	      (cons "Insert Pair Edit" ipe-menu--mode-map))

  (define-key ipe-edit-mode-map [mouse-3]
	      ipe-menu--mouse-map)

  (define-key ipe-edit-mode-map [M-mouse-3]
	      (ipe-menu--change-pair))

  (ipe-menu--update)

  (add-hook 'menu-bar-update-hook 'ipe-menu--update))

(defun ipe-menu--uninstall ()
  "Uninstall the menu bindings for the Insert Pair Edit mode."

  (remove-hook 'menu-bar-update-hook 'ipe-menu--update)

  (define-key menu-bar-edit-menu [ipe]         nil)

  (define-key ipe-edit-mode-map [M-mouse-3]    nil)
  (define-key ipe-edit-mode-map [mouse-3]      nil)
  (define-key ipe-edit-mode-map [menu-bar ipe] nil))

(when ipe-menu-support-p
  (ipe-menu--install))

(provide 'ipe-menu)

;;; ipe-menu.el ends here
