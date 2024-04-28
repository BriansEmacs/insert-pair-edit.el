;;; ipe-mouse.el --- Insert Pair Edit - mouse support -*- lexical-binding: t; -*-
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
;; This file adds mouse support for the `ipe' (Insert Pair
;; Edit) package.
;;
;; Bindings for various mouse actions are added to the
;; `ipe-edit-mode-map' keymap.
;;
;;  [mouse-1]        - Move the `ipe' OPEN string to mouse click POS.
;;  [mouse-2]        - Move the `ipe' CLOSE string to mouse click POS.
;;  [mouse-3]        - Display the 'Insert Pair Edit' Context Menu.
;;  [drag-mouse-1]   - Move both the `ipe' OPEN and CLOSE strings.
;;  [double-mouse-1] - Surround the current lexical unit with a PAIR.
;;
;;  [C-mouse-1]      - Add an `ipe' PAIR around mouse click POS.
;;  [C-mouse-2]      - Remove `ipe' PAIR closest to mouse click POS.
;;  [C-drag-mouse-1] - Add an `ipe' PAIR around a region.
;;
;;  [wheel-up]       - Move the `ipe' OPEN string backwards.
;;  [wheel-down]     - Move the `ipe' CLOSE string forwards.
;;  [C-wheel-up]     - Move the `ipe' OPEN string forwards.
;;  [C-wheel-down]   - Move the `ipe' CLOSE string backwards.
;;  [S-wheel-up]     - Move the `ipe' OPEN string backwards (alt)
;;  [S-wheel-down]   - Move the `ipe' CLOSE string forwards (alt)
;;  [C-S-wheel-up]   - Move the `ipe' OPEN string forwards (alt)
;;  [C-S-wheel-down] - Move the `ipe' CLOSE string backwards (alt)
;;  [M-wheel-up]     - Change to the 'next' move-by function.
;;  [M-wheel-down]   - Change to the 'previous' move-by function.
;;

;; -------------------------------------------------------------------
;;; Installation:
;;
;; Add the following to your `.emacs' file:
;;
;;  (require 'ipe-mouse)
;;

;; -------------------------------------------------------------------
;;; Code:

(require 'ipe-compat)
(require 'ipe-edit)

;; -------------------------------------------------------------------
;;;; Utility Functions.
;; -------------------------------------------------------------------

(defun ipe-mouse--mode-check (event)
  "Check `ipe-edit-mode' is active for ipe-mouse--* commands.

EVENT is mouse event, describing the WINDOW and BUFFER in which the
check is to be made."

  (let ((window (posn-window (cadr event))))
    (with-selected-window window
      (with-current-buffer (window-buffer window)
	(if ipe-edit-mode
	    (if (ipe--pos-count)
		t
	      (ipe-edit-mode -1)
	      nil)
	  (message (concat "This command should only be run from within\
 `ipe-edit-mode'."))
	  nil)))))

(defmacro ipe-mouse--with-event (event &rest body)
  "Wrap BODY so it is run in the window / buffer of a mouse EVENT."

  `(when (and (ipe-mouse--mode-check ,event))
     (let ((window (posn-window (cadr ,event))))
       (with-selected-window window
	 (with-current-buffer (window-buffer window)
	   ,@body)))))

;; -------------------------------------------------------------------
;;;; 'Click' event handlers.
;; -------------------------------------------------------------------

(defun ipe-mouse--open (event)
  "Move the `ipe' OPEN string to the mouse position given in EVENT.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the OPEN overlay to a position
specified by a mouse click EVENT."

  (interactive "e")
  (ipe-mouse--with-event
   event
   (let ((pos (posn-point (cadr event))))
     (ipe--pos-list-singular)
     (ipe--open-init 0 pos 1)
     (when (> (ipe--pos-open 0) (ipe--pos-close 0))
       (ipe--close-init 0 pos 1))
     (ipe--pair-pos-redisplay))))

(defun ipe-mouse--close (event)
  "Move the `ipe' CLOSE string to the mouse position given in EVENT.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the CLOSE overlay to a position
specified by a mouse click EVENT."

  (interactive "e")
  (ipe-mouse--with-event
   event
   (let ((pos (posn-point (cadr event))))
     (ipe--pos-list-singular)
     (ipe--close-init 0 pos 1)
     (when (< (ipe--pos-close 0) (ipe--pos-open 0))
       (ipe--open-init 0 pos 1))
     (ipe--pair-pos-redisplay))))

(defun ipe-mouse--init (event)
  "Surround the lexical unit at a mouse click with an `ipe' PAIR.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the OPEN overlay and CLOSE
overlay of a PAIR to surround the current lexical unit within an
`ipe' PAIR.  This is called in response to a mouse EVENT."

  (interactive "e")
  (ipe-mouse--with-event
   event
   (ipe--pos-list-singular)
   (let ((pos (posn-point (cadr event))))
     (ipe--pair-pos-init 0 pos 1)
     (ipe--pos-point 0 (ipe--pos-open 0))
     (ipe--pair-pos-redisplay))))

(defun ipe-mouse--region (event)
  "Move the `ipe' OPEN and CLOSE to beginning and end of a mouse drag.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the OPEN overlay and CLOSE
overlay of a PAIR to the region specified by the start and finish of
a mouse drag EVENT."

  (interactive "e")
  (ipe-mouse--with-event
   event
   (when (= (length event) 3)
     (ipe--pos-list-singular)
     (let* ((p1  (posn-point (cadr event)))
	    (p2  (posn-point (ipe-compat--caddr event)))
	    (beg (if (and p1 p2 (< p1 p2)) p1 p2))
	    (end (if (and p1 p2 (< p1 p2)) p2 p1)))
       (when (and beg end)
	 (ipe--open-init 0 (+ beg 1) 1)
	 (ipe--close-init 0 end 1)
	 (ipe--pair-pos-redisplay))))))

(defun ipe-mouse--add-pair (event)
  "Add a new `ipe' PAIR at the mouse position given in EVENT.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to add a new PAIR at a position specified
by a mouse click EVENT."

  (interactive "e")
  (ipe-mouse--with-event
   event
   (let ((pos (posn-point (cadr event)))
	 (n   (ipe--pos-count)))
     (ipe--pair-pos-init n pos 1)
     (ipe--pos-point n (ipe--pos-open n))
     (ipe--pair-pos-redisplay))))

(defun ipe-mouse--delete-pair (event)
  "Delete the `ipe' PAIR closest to the mouse position given in EVENT.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to delete a PAIR from a position specified
by a mouse click EVENT."

  (interactive "e")
  (ipe-mouse--with-event
   event
   (let* ((pos (posn-point (cadr event)))
	  (n   (ipe--pos-list-nearest pos)))
     (if (<= (ipe--pos-count) 1)
	 (progn
	   (ipe--undo-accept)
	   (ipe-edit--abort))
       (ipe--pair-pos-hide n)
       (ipe-edit--redisplay)))))

(defun ipe-mouse--add-pair-region (event)
  "Add an `ipe' PAIR around the beginning and end of a mouse drag.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to add a PAIR of OPEN and CLOSE overlays
to the region specified by the start and finish of a mouse drag EVENT.

This function will not remove existing PAIRs."

  (interactive "e")
  (ipe-mouse--with-event
   event
   (when (= (length event) 3)
     (let* ((p1  (posn-point (cadr event)))
	    (p2  (posn-point (ipe-compat--caddr event)))
	    (beg (if (and p1 p2 (< p1 p2)) p1 p2))
	    (end (if (and p1 p2 (< p1 p2)) p2 p1))
	    (n   (ipe--pos-count)))
       (when (and beg end)
	 (ipe--open-init n (+ beg 1) 1)
	 (ipe--close-init n end 1)
	 (ipe--pos-property-set n :initial-n n)
	 (ipe--pos-point n (ipe--pos-open n))

	 (ipe--pair-pos-redisplay))))))

;; -------------------------------------------------------------------
;;;; 'Mouse Wheel' event handlers.
;; -------------------------------------------------------------------

(defun ipe-mouse--open-forward (event)
  "Move the `ipe' OPEN overlay forward.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the OPEN overlay of a PAIR
`forward' in response to a mouse wheel EVENT.

Movement units are determined by the current value of
`ipe--movement'."

  (interactive "e")
  (ipe-mouse--with-event
   event
   (if (eq ipe--movement 'line)
       (ipe-edit--open-down 1)
     (ipe-edit--open-forward 1))))

(defun ipe-mouse--open-forward-alt (event)
  "Move the `ipe' OPEN overlay forward (alternate movement).

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the OPEN overlay of a PAIR
`forward' `by-char' in response to a mouse wheel EVENT."

  (interactive "e")
  (ipe-mouse--with-event
   event
   (if (eq ipe--movement 'line)
       (ipe-edit--open-forward 1)
     (let ((ipe--movement 'char))
       (ipe-edit--open-forward 1)))))

(defun ipe-mouse--open-backward (event)
  "Move the `ipe' OPEN overlay backward.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the OPEN overlay of a PAIR
`backward' in response to a mouse wheel EVENT.

Movement units are determined by the current value of
`ipe--movement'."

  (interactive "e")
  (ipe-mouse--with-event
   event
   (if (eq ipe--movement 'line)
       (ipe-edit--open-up 1)
     (ipe-edit--open-backward 1))))

(defun ipe-mouse--open-backward-alt (event)
  "Move the `ipe' OPEN overlay backward (alternate movement).

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the OPEN overlay of a PAIR
`backward' `by-char' in response to a mouse wheel EVENT."

  (interactive "e")
  (ipe-mouse--with-event
   event
   (if (eq ipe--movement 'line)
       (ipe-edit--open-backward 1)
     (let ((ipe--movement 'char))
       (ipe-edit--open-backward 1)))))

(defun ipe-mouse--close-forward (event)
  "Move the `ipe' CLOSE overlay forward.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the CLOSE overlay of a PAIR
`forward' in response to a mouse wheel EVENT.

Movement units are determined by the current value of
`ipe--movement'."

  (interactive "e")
  (ipe-mouse--with-event
   event
   (if (eq ipe--movement 'line)
       (ipe-edit--close-down 1)
     (ipe-edit--close-forward 1))))

(defun ipe-mouse--close-forward-alt (event)
  "Move the `ipe' CLOSE overlay forward (alternate movement).

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the CLOSE overlay of a PAIR
`forward' `by-char' in response to a mouse wheel EVENT."

  (interactive "e")
  (ipe-mouse--with-event
   event
   (if (eq ipe--movement 'line)
       (ipe-edit--close-forward 1)
     (let ((ipe--movement 'char))
       (ipe-edit--close-forward 1)))))

(defun ipe-mouse--close-backward (event)
  "Move the `ipe' OPEN overlay backward.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the CLOSE overlay of a PAIR
`backward' in response to a mouse wheel EVENT.

Movement units are determined by the current value of
`ipe--movement'."

  (interactive "e")
  (ipe-mouse--with-event
   event
   (if (eq ipe--movement 'line)
       (ipe-edit--close-up 1)
     (ipe-edit--close-backward 1))))

(defun ipe-mouse--close-backward-alt (event)
  "Move the `ipe' OPEN overlay backward (alternate movement).

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the CLOSE overlay of a PAIR
`backward' `by-char' in response to a mouse wheel EVENT."

  (interactive "e")
  (ipe-mouse--with-event
   event
   (if (eq ipe--movement 'line)
       (ipe-edit--close-backward 1)
     (let ((ipe--movement 'char))
       (ipe-edit--close-backward 1)))))

(defun ipe-mouse--next-movement (event)
  "Set movement made by Insert Pair Edit to the `next' movement.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to update the `ipe--movement' variable to
the `next' movement symbol within the `car' of the elements within the
`ipe-move-by-movements' list.  It is called in response to a mouse
wheel EVENT."

  (interactive "e")
  (ipe-mouse--with-event
   event
   (let ((n (ipe--list-element ipe-move-by-movements
			       ipe--movement
			       (lambda (movement x)
				 (equal movement (car x))))))
     (setq ipe--movement
	   (if n
	       (car (nth (min (1- (length ipe-move-by-movements)) (1+ n))
			 ipe-move-by-movements))
	     'word))
     (ipe--pair-pos-movement-reset ipe--movement))))

(defun ipe-mouse--previous-movement (event)
  "Set movement made by Insert Pair Edit to the `previous' movement.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to update the `ipe--movement' variable to
the `previous' movement symbol within the `car' of the elements within
the `ipe-move-by-movements' list.  It is called in response to a mouse
wheel EVENT."

  (interactive "e")
  (ipe-mouse--with-event
   event
   (let ((n (ipe--list-element ipe-move-by-movements
			       ipe--movement
			       (lambda (movement x)
				 (equal movement (car x))))))
     (setq ipe--movement
	   (if n
	       (car (nth (max (1- n) 0)
			 ipe-move-by-movements))
	     'word))
     (ipe--pair-pos-movement-reset ipe--movement))))

;; -------------------------------------------------------------------
;;;; Minor Mode Mouse Control.
;; -------------------------------------------------------------------

(defun ipe-mouse--install ()
  "Install the mouse bindings for the Insert Pair Edit mode."

  (define-key ipe-edit-mode-map [mouse-1]
	      'ipe-mouse--open)
  (define-key ipe-edit-mode-map [mouse-2]
	      'ipe-mouse--close)
  (define-key ipe-edit-mode-map [drag-mouse-1]
	      'ipe-mouse--region)
  (define-key ipe-edit-mode-map [double-mouse-1]
	      'ipe-mouse--init)

  (define-key ipe-edit-mode-map [C-down-mouse-1]
	      'self-insert-command)
  (define-key ipe-edit-mode-map [C-mouse-1]
	      'ipe-mouse--add-pair)
  (define-key ipe-edit-mode-map [C-down-mouse-2]
	      'self-insert-command)
  (define-key ipe-edit-mode-map [C-mouse-2]
	      'ipe-mouse--delete-pair)
  (define-key ipe-edit-mode-map [C-drag-mouse-1]
	      'ipe-mouse--add-pair-region)

  (define-key ipe-edit-mode-map [wheel-up]
	      'ipe-mouse--open-backward)
  (define-key ipe-edit-mode-map [wheel-down]
	      'ipe-mouse--close-forward)

  (define-key ipe-edit-mode-map [S-wheel-up]
	      'ipe-mouse--open-backward-alt)
  (define-key ipe-edit-mode-map [S-wheel-down]
	      'ipe-mouse--close-forward-alt)

  (define-key ipe-edit-mode-map [C-wheel-up]
	      'ipe-mouse--open-forward)
  (define-key ipe-edit-mode-map [C-wheel-down]
	      'ipe-mouse--close-backward)

  (define-key ipe-edit-mode-map [C-S-wheel-up]
	      'ipe-mouse--open-forward-alt)
  (define-key ipe-edit-mode-map [C-S-wheel-down]
	      'ipe-mouse--close-backward-alt)

  (define-key ipe-edit-mode-map [M-wheel-up]
	      'ipe-mouse--next-movement)
  (define-key ipe-edit-mode-map [M-wheel-down]
	      'ipe-mouse--previous-movement))

(defun ipe-mouse--uninstall ()
  "Uninstall the mouse bindings for the Insert Pair Edit mode."

  (mapcar (lambda (key) (define-key ipe-edit-mode-map key nil))
	  '([mouse-1]
	    [mouse-2]
	    [drag-mouse-1]
	    [double-mouse-1]
	    [C-mouse-1]
	    [C-down-mouse-1]
	    [C-mouse-2]
	    [C-down-mouse-2]
	    [C-drag-mouse-1]
	    [wheel-up]
	    [wheel-down]
	    [C-wheel-up]
	    [C-wheel-down]
	    [S-wheel-up]
	    [S-wheel-down]
	    [C-S-wheel-up]
	    [C-S-wheel-down]
	    [M-wheel-up]
	    [M-wheel-down])))

(when ipe-mouse-support-p
  (ipe-mouse--install))

(provide 'ipe-mouse)

;;; ipe-mouse.el ends here
