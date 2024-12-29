;;; ipe-fade.el --- Insert Pair Edit - fade faces between colors -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Brian Kavanagh

;; Author: Brian Kavanagh <brians.emacs@gmail.com>
;; Maintainer: Brian Kavanagh <brians.emacs@gmail.com>
;; Created: 15 October, 2024
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
;; This file defines functions used by the 'Insert Pair Edit' (ipe)
;; package to fade font-lock faces between colors.
;;

;; -------------------------------------------------------------------
;;; Code:

;; -------------------------------------------------------------------
;;;; Functions
;; -------------------------------------------------------------------

(defun ipe-fade--rgb (face &optional background invert)
  "Return 3-tuple representing the RGB color of FACE.

By default this will return the :foreground color of FACE as a list of
three positive integers (0-65535) representing the (Red Green Blue)
values of the :foreground color.

If BACKGROUND is non-nil, this will instead return a list representing
the :background color of FACE.
If INVERT is non-nil, this will invert the returned RGB values.

If the given :foreground / :background color is `unspecified', then
this will return (0 0 0) (or (65535 65535 65535) if INVERT is
non-nil.)"

  (let* ((color (face-attribute face
				(if background
				    :background
				  :foreground)
				nil
				t))
	 (rgb   (if (or (not color)
			(equal color 'unspecified)
			(not (color-values color)))
		    '(0 0 0)
		  (color-values color))))
    (if invert
	(list (- 65535 (car rgb))
	      (- 65535 (cadr rgb))
	      (- 65535 (ipe-compat--caddr rgb)))
      rgb)))

(defun ipe-fade--inc-color (i increments from-rgb to-rgb)
  "Return a color string representing an `ipe-fade' face color.

This function will return a string of the form #RRRRGGGGBBBB,
representing a color between colors FROM-RGB and TO-RGB.

Both FROM-RGB and TO-RGB are expected to be lists of 3 numbers
\(0-65535) representing RGB colors.

`I' is the increment (0 <= I <= INCREMENTS) representing the fraction
\(I / INCREMENTS) between FROM-RGB color and TO-RGB color that is to
be returned."

  (let* ((x (/ (float i) (float increments))))

    (unless to-rgb
      (setq to-rgb '(0 0 0)))

    (unless from-rgb
      (setq from-rgb '(0 0 0)))

    (format "#%04x%04x%04x"
	    (+ (car from-rgb)   (* (- (car to-rgb)  (car from-rgb))  x))
	    (+ (cadr from-rgb)  (* (- (cadr to-rgb) (cadr from-rgb)) x))
	    (+ (ipe-compat--caddr from-rgb)
	       (* (- (ipe-compat--caddr to-rgb)
		     (ipe-compat--caddr from-rgb))
		  x)))))

(defun ipe-fade--set-inc (face i increments from-fg from-bg to-fg
			       to-bg)
  "Set the color of FACE to a faded increment between two colors.

This function will set the :foreground and :background attributes of
FACE to be `I' INCREMENTS between the foreground and background colors
represented by the FROM-FG / TO-FG and FROM-BG / TO-BG parameters.

FROM-FG, FROM-BG, TO-FG and TO-BG are all expected to be lists of 3
numbers (0-65535) representing RGB colors."

  (let ((fg (ipe-fade--inc-color i increments from-fg to-fg))
	(bg (ipe-fade--inc-color i increments from-bg to-bg)))

    (set-face-foreground face fg)
    (set-face-background face bg)))

;; -------------------------------------------------------------------
;;;; Public Functions
;; -------------------------------------------------------------------

(defun ipe-fade (from-face to-face time increments &optional done)
  "Fade FROM-FACE to TO-FACE over TIME seconds in INCREMENTS.

This function will, over the course of TIME seconds, change the
:foreground and :background colors of FROM-FACE to incremental colors
between FROM-FACE and TO-FACE, to represent the fading of FROM-FACE to
TO-FACE.

If DONE is non-nil, it is assumed to be a function which will be run
\(with no arguments) once the FADE is complete."

  (let* ((orig-fg     (face-attribute from-face :foreground nil t))
	 (orig-bg     (face-attribute from-face :background nil t))
	 (from-iv     (face-attribute from-face :inverse-video nil t))
	 (to-iv       (face-attribute to-face   :inverse-video nil t))
	 (from-invert (and from-iv (not (equal from-iv 'unspecified))))
	 (to-invert   (and to-iv   (not (equal to-iv   'unspecified))))
	 (invert      (ipe-compat--xor to-invert from-invert))
	 (to-fg       (ipe-fade--rgb to-face nil invert))
	 (to-bg       (ipe-fade--rgb to-face t   invert))
	 (from-fg     (if (equal orig-fg 'unspecified)
			  to-fg
			(ipe-fade--rgb from-face)))
	 (from-bg     (if (equal orig-bg 'unspecified)
			  to-bg
			(ipe-fade--rgb from-face t))))

    ;; Set up timers to gradually fade the from-face color.
    (dotimes (i increments)
      (run-at-time (* (/ time (float increments)) i)
		   nil
		   'ipe-fade--set-inc
		   from-face
		   i
		   increments
		   from-fg
		   from-bg
		   to-fg
		   to-bg))

    (when done
      (run-at-time time nil done))

    ;; Restore the 'face FROM-FACE to it original color.
    (run-at-time time
		 nil
		 (lambda (fg bg)
		   (set-face-foreground from-face fg)
		   (set-face-background from-face bg))
		 (or orig-fg 'unspecified)
		 (or orig-bg 'unspecified))))

;; *******************************************************************

(provide 'ipe-fade)

;;; ipe-fade.el ends here
