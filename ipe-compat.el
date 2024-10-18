;;; ipe-compat.el --- Insert Pair Edit - backwards compatibility -*- lexical-binding: t; -*-
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
;; This file defines some functions used by the Insert Pair Edit (ipe)
;; library which are not available within earlier versions of Emacs.

;; -------------------------------------------------------------------
;;; Code:

;; -------------------------------------------------------------------
;;;; Emacs < 25
;; -------------------------------------------------------------------

(when (<= emacs-major-version 23)
  (error "Insert Pair Edit (ipe) requires Emacs >= 24.3"))

;; (if (functionp #'alist-get)
;;     (defalias 'ipe-compat--alist-get #'alist-get)

(defun ipe-compat--alist-get (key alist
				  &optional default remove testfn)
  "Insert Pair Edit (ipe) compatibility function.

Find the first element of ALIST whose `car' equals KEY, and return its
`cdr'.

If KEY is not found in ALIST, return DEFAULT.

If TESTFN is supplied, compare KEY using TESTFN.

\(Introduced in Emacs 25.)"
  (ignore remove)
  (let ((x (if (not testfn)
	       (assq key alist)
	     (assoc key alist testfn))))
    (if x (cdr x) default)))

;; -------------------------------------------------------------------
;;;; Emacs < 26
;; -------------------------------------------------------------------

(if (functionp #'caddr)
    (defalias 'ipe-compat--caddr #'caddr)
  (defun ipe-compat--caddr (x)
    "Insert Pair Edit (ipe) compatibility function.

Return the `car' of the `cdr' of the `cdr' of X.

\(Introduced Emacs 26.)"
    (car (cdr (cdr x)))))

(if (functionp #'cadddr)
    (defalias 'ipe-compat--cadddr #'cadddr)
  (defun ipe-compat--cadddr (x)
    "Insert Pair Edit (ipe) compatibility function.

Return the `car' of the `cdr' of the `cdr' of the `cdr' of X.

\(Introduced Emacs 26.)"
    (car (cdr (cdr (cdr x))))))

(if (functionp #'mapcan)
    (defalias 'ipe-compat--mapcan #'mapcan)
  (defun ipe-compat--mapcan (func sequence)
    "Insert Pair Edit (ipe) compatibility function.

Apply FUNC to each element of SEQUENCE, and concatenate the results by
altering them (using `nconc').  SEQUENCE may be a list, a vector, a
bool-vector, or a string.

(Introduced Emacs 26.)"
    (apply #'nconc (mapcar func sequence))))

;; -------------------------------------------------------------------
;;;; Emacs < 27
;; -------------------------------------------------------------------

(if (functionp #'xor)
    (defalias 'ipe-compat--xor #'xor)
  (defun ipe-compat--xor (cond1 cond2)
    "Insert Pair Edit (ipe) compatibility function.

Return the boolean exclusive-or of COND1 and COND2.

(Introduced Emacs 27)"
    (or (and cond1 (not cond2))
	(and (not cond1) cond2))))

;; -------------------------------------------------------------------
(provide 'ipe-compat)

;;; ipe-compat.el ends here

