;;; ipe-mouse.el --- Insert Pair Edit - mouse support -*- lexical-binding: t; -*-
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

(defun ipe-mouse--mode-check ()
  "Check `ipe-edit-mode' is active for `ipe-mouse--*' commands."

  (if ipe-edit-mode
      (if (ipe--pos-count)
	  t
	(ipe-edit-mode -1)
	nil)
    (message (concat "This command should only be run from within\
 `ipe-edit-mode'."))
    nil))

;; -------------------------------------------------------------------
;;;; 'Click' event handlers.
;; -------------------------------------------------------------------

(defun ipe-mouse--open (event)
  "Move the `ipe' OPEN string to the mouse position given in EVENT.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the OPEN overlay to a position
specified by a mouse click EVENT."

  (interactive "e")
  (when (and (ipe-mouse--mode-check)
	     (mouse-event-p event))
    (ipe--pos-list-singular)
    (ipe--open-init 0 (posn-point (cadr event)) 1)

    (when (> (ipe--pos-open 0) (ipe--pos-close 0))
      (ipe--close-init 0 (posn-point (cadr event)) 1))

    (ipe--pair-pos-redisplay)))

(defun ipe-mouse--close (event)
  "Move the `ipe' CLOSE string to the mouse position given in EVENT.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the CLOSE overlay to a position
specified by a mouse click EVENT."

  (interactive "e")
  (when (and (ipe-mouse--mode-check)
	     (mouse-event-p event))
    (ipe--pos-list-singular)
    (ipe--close-init 0 (posn-point (cadr event)) 1)

    (when (< (ipe--pos-close 0) (ipe--pos-open 0))
      (ipe--open-init 0 (posn-point (cadr event)) 1))

    (ipe--pair-pos-redisplay)))

(defun ipe-mouse--drag (event)
  "Move the `ipe' OPEN and CLOSE to beginning and end of a mouse drag.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the OPEN overlay and CLOSE
overlays of a PAIR to the region specified by the start and finish of
a mouse drag event.

EVENT is the mouse drag event."

  (interactive "e")
  (when (and (ipe-mouse--mode-check)
	     (mouse-event-p event)
	     (= (length event) 3))
    (ipe--pos-list-singular)
    (let* ((p1  (posn-point (cadr event)))
	   (p2  (posn-point (ipe-compat--caddr event)))
	   (beg (if (and p1 p2 (< p1 p2)) p1 p2))
	   (end (if (and p1 p2 (< p1 p2)) p2 p1)))

      (when (and beg end)
	(ipe--open-init 0 (+ beg 1) 1)
	(ipe--close-init 0 end 1)

	(ipe--pair-pos-redisplay)))))

;; -------------------------------------------------------------------
;;;; 'Mouse Wheel' event handlers.
;; -------------------------------------------------------------------

(defun ipe-mouse--open-forward ()
  "Move the `ipe' OPEN overlay forward.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the OPEN overlay of a PAIR
`forward' in response to a mouse wheel event.

Movement units are determined by the current value of
`ipe--movement'."

  (interactive)
  (when (ipe-mouse--mode-check)
    (if (eq ipe--movement 'line)
	(ipe-edit--open-down 1)
      (ipe-edit--open-forward 1))))

(defun ipe-mouse--open-forward-alt ()
  "Move the `ipe' OPEN overlay forward (alternate movement).

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the OPEN overlay of a PAIR
`forward' `by-char' in response to a mouse wheel event."

  (interactive)
  (when (ipe-mouse--mode-check)
    (if (eq ipe--movement 'line)
	(ipe-edit--open-forward 1)
      (let ((ipe--movement 'char))
	(ipe-edit--open-forward 1)))))

(defun ipe-mouse--open-backward ()
  "Move the `ipe' OPEN overlay backward.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the OPEN overlay of a PAIR
`backward' in response to a mouse wheel event.

Movement units are determined by the current value of
`ipe--movement'."

  (interactive)
  (when (ipe-mouse--mode-check)
    (if (eq ipe--movement 'line)
	(ipe-edit--open-up 1)
      (ipe-edit--open-backward 1))))

(defun ipe-mouse--open-backward-alt ()
  "Move the `ipe' OPEN overlay backward (alternate movement).

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the OPEN overlay of a PAIR
`backward' `by-char' in response to a mouse wheel event."

  (interactive)
  (when (ipe-mouse--mode-check)
    (if (eq ipe--movement 'line)
	(ipe-edit--open-backward 1)
      (let ((ipe--movement 'char))
	(ipe-edit--open-backward 1)))))

(defun ipe-mouse--close-forward ()
  "Move the `ipe' CLOSE overlay forward.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the CLOSE overlay of a PAIR
`forward' in response to a mouse wheel event.

Movement units are determined by the current value of
`ipe--movement'."

  (interactive)
  (when (ipe-mouse--mode-check)
    (if (eq ipe--movement 'line)
	(ipe-edit--close-down 1)
      (ipe-edit--close-forward 1))))

(defun ipe-mouse--close-forward-alt ()
  "Move the `ipe' CLOSE overlay forward (alternate movement).

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the CLOSE overlay of a PAIR
`forward' `by-char' in response to a mouse wheel event."

  (interactive)
  (when (ipe-mouse--mode-check)
    (if (eq ipe--movement 'line)
	(ipe-edit--close-forward 1)
      (let ((ipe--movement 'char))
	(ipe-edit--close-forward 1)))))

(defun ipe-mouse--close-backward ()
  "Move the `ipe' OPEN overlay backward.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the CLOSE overlay of a PAIR
`backward' in response to a mouse wheel event.

Movement units are determined by the current value of
`ipe--movement'."

  (interactive)
  (when (ipe-mouse--mode-check)
    (if (eq ipe--movement 'line)
	(ipe-edit--close-up 1)
      (ipe-edit--close-backward 1))))

(defun ipe-mouse--close-backward-alt ()
  "Move the `ipe' OPEN overlay backward (alternate movement).

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to move the CLOSE overlay of a PAIR
`backward' `by-char' in response to a mouse wheel event."

  (interactive)
  (when (ipe-mouse--mode-check)
    (if (eq ipe--movement 'line)
	(ipe-edit--close-backward 1)
      (let ((ipe--movement 'char))
	(ipe-edit--close-backward 1)))))

(defun ipe-mouse--next-movement ()
  "Set movement made by Insert Pair Edit to the `next' movement.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to update the `ipe--movement' variable to
the `next' movement symbol within the `car' of the elements within the
`ipe-move-by-movements' list."

  (interactive)
  (when (ipe-mouse--mode-check)
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

(defun ipe-mouse--previous-movement ()
  "Set movement made by Insert Pair Edit to the `previous' movement.

This command is used within the Insert Pair Edit (ipe) minor-mode
\(command: `ipe-edit-mode') to update the `ipe--movement' variable to
the `previous' movement symbol within the `car' of the elements within
the `ipe-move-by-movements' list."

  (interactive)
  (when (ipe-mouse--mode-check)
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
	      'ipe-mouse--drag)

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
	    [wheel-up]
	    [wheel-down]
	    [S-wheel-up]
	    [S-wheel-down]
	    [C-wheel-up]
	    [C-wheel-down]
	    [C-S-wheel-up]
	    [C-S-wheel-down]
	    [M-wheel-up]
	    [M-wheel-down])))

(when ipe-mouse-support-p
  (ipe-mouse--install))

(provide 'ipe-mouse)

;;; ipe-mouse.el ends here
