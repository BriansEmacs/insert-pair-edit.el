;;; ipe-test-custom.el --- Insert Pair Edit - customize (s/g)etter Tests -*- lexical-binding: t; -*-
;; Copyright (C) 2023 Brian Kavanagh

;; Author: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Maintainer: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Created: 28 June, 2020
;; Version: 1.1
;; Package: ipe
;; Keywords: internal local
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
;; This file defines a set of `ert' (Emacs Regression Test)s for the
;; `ipe' (Insert Pair Edit) package.
;;
;; The tests within this file are used to test the `customize'
;; setters, and getters used by the `ipe' custom widgets.  The ERT
;; Tests within this file test the:
;;
;;   `ipe-custom--intermediate-defn-get-p'
;;   `ipe-custom--advanced-defn-get-p'
;;
;; functions.

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)

(ert-deftest ipe-test-custom--basic-1 ()
  "Test `ipe-custom--basic-defn-get-p'."

  ;; Invalid Basic PAIR definitions.
  (mapc
   (lambda (x)
     (should (eq (ipe-custom--basic-defn-get-p x) nil)))
   '(()
     ("(")
     ("(" "(")
     ("(" "(" ")" ())
     ("(" "(" ")" (:movement line))))

  ;; Valid Basic PAIR definitions.
  (should (eq (ipe-custom--basic-defn-get-p '("" "" "")) t)))

(ert-deftest ipe-test-custom--intermediate-1 ()
  "Test `ipe-custom--intermediate-defn-get-p'."

  ;; Invalid Intermediate PAIR definitions.
  (mapc
   (lambda (x)
     (should (eq (ipe-custom--intermediate-defn-get-p x) nil)))
   '(()
     ("(")
     ("(" "(")
     ("(" "(" ")" ())
     ("(" "(" ")" (:movement char :move-point      open-start))
     ("(" "(" ")" (:movement char :indent-function current))
     ;; TODO: Add regexp update matching.
     ;;   ("(" "(" ")" (:movement char :open-regexp     ".*"))
     ;;   ("(" "(" ")" (:movement char :close-regexp    ".*"))
     ("(" "(" ")" (:movement char :menu            "Test"))))

  ;; Valid Intermediate PAIR definitions.
  (mapc
   (lambda (x)
     (should (eq (ipe-custom--intermediate-defn-get-p x) t)))
   '(("(" "(" ")" (:movement char))
     ("(" "(" ")" (:movement line))
     ("(" "(" ")" (:movement word))
     ("(" "(" ")" (:movement list)))))

(ert-deftest ipe-test-custom--advanced-1 ()
  "Test `ipe-custom--advanced-defn-get-p'."

  ;; Invalid Advanced PAIR definitions.
  (mapc
   (lambda (x)
     (should (eq (ipe-custom--advanced-defn-get-p x) nil)))
   '(()
     ("(")
     ("(" "(")
     ("(" "(" ")" ())
     ("(" "(" ")" (:movement custom))))

  ;; Valid Advanced PAIR definitions.
  (mapc
   (lambda (x)
     (should (eq (ipe-custom--advanced-defn-get-p x) t)))
   '(("(" "(" ")" (:movement char))
     ("(" "(" ")" (:movement line))
     ("(" "(" ")" (:movement word))
     ("(" "(" ")" (:movement list))
     ("(" "(" ")" (:movement char :move-point      open-start))
     ("(" "(" ")" (:movement char :indent-function current))
     ;; TODO: Add regexp update matching.
     ;;   ("(" "(" ")" (:movement char :open-regexp     ".*"))
     ;;   ("(" "(" ")" (:movement char :close-regexp    ".*"))
     ("(" "(" ")" (:movement char :menu            "Test")))))

(provide 'ipe-test-custom)

;;; ipe-test-custom.el ends here
