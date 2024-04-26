;;; ipe-test.el --- Insert Pair Edit - ERT Test Helpers -*- lexical-binding: t; -*-
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
;; This file defines a set of macros / helper functions which wrap the
;; `ert' (Emacs Regression Test) package's `ert-deftest' macro with
;; an `ipe' (Insert Pair Edit) specific `ipe-test-def-kbd' macro.
;;
;; The `ipe-test-def-kbd' macro is used to test the interactive
;; functions within `ipe-edit-mode' by executing a set of keystrokes
;; against a buffer containing text, and comparing the result (both
;; output text, and cursor positions) with an 'expected' output.
;;
;; This file also defines two `interactive' functions:
;;
;;    `ipe-test-run'
;;    `ipe-test-run-all'
;;
;; Which are 'convenience functions' used to run `ert' against `ipe'
;; specific ERT test cases.

;; -------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'ipe)

(defconst ipe-test--point-indicator "|"
  "The position of POINT within \"ipe-test--.*\" tests.

This string is inserted within the input / expected output string
passed to the \"ipe-test--.*\" `ert' tests to indicate the position of
POINT.")

(defconst ipe-test--mark-indicator "@"
  "The position of the MARK within \"ipe-test--.*\" tests.

This string is inserted within the input string passed to the
\"ipe-test--.*\" `ert' tests to indicate the position of mark.")

(defconst ipe-test--mc-point-indicator "!"
  "The position of a multiple-cursor within \"ipe-test--.*\".

This string is inserted within the input / expected output string
passed to the \"ipe-test--.*\" `ert' tests to indicate the position of
a `multiple-cursors' fake-cursor.")

;;  "The keystrokes for the currently running \"ipe-test--.*\".
(defvar-local ipe-test--keystrokes nil
  "The keystrokes for the currently running \"ipe-test--.*\".")

(setq ipe-test--keystrokes nil)

(defvar ipe-test--buffer-text nil
  "The buffer text for the currently running \"ipe-test--.*\".")

(defvar ipe-test--names '()
  "A history list for `completing-read' for \"ipe-test--.*\" names.")

(defvar ipe-test--buffer-text-alist-p t
  "Whether to populate `ipe-test--buffer-text-alist'.")

(defvar ipe-test--buffer-text-alist '()
  "An alist of `ipe-test--name' to their associated 'BUFFER-TEXT.")

;; -------------------------------------------------------------------
;;;; Helper functions
;; -------------------------------------------------------------------

(defun ipe-test--set-point ()
  "Set POINT to the location defined by `ipe-test--point-indicator'.

This function will search the current buffer for a string that matches
`ipe-test--point-indicator', and, if found, deletes the
`ipe-test--point-indicator' from the buffer, and sets POINT to the
starting location of the indicator.

Additionally:

* If an `ipe-test--mark-indicator' is found, it is deleted, the
  mark is set to the starting location of the indicator, and the
  region is activated.
* If any `ipe-test--mc-point-indicator's are found, they are deleted,
  and if the `multiple-cursors' library is loaded, new mc cursors
  are created at the location of the `ipe-test--mc-point-indicator'."

  ;; If 'multiple-cursors is loaded, set up the multiple-cursors.
  (goto-char (point-min))
  (when (re-search-forward ipe-test--mc-point-indicator
			   (point-max) t)

    (when (functionp 'multiple-cursors-mode)
      (funcall 'multiple-cursors-mode t))

    (goto-char (point-min))
    (while (re-search-forward ipe-test--mc-point-indicator
			      (point-max)
			      t)
      (delete-region (match-beginning 0) (match-end 0))
      (when (functionp 'mc/create-fake-cursor-at-point)
	(funcall 'mc/create-fake-cursor-at-point))))

  ;; Search for a mark indicator.
  (goto-char (point-min))
  (when (re-search-forward ipe-test--mark-indicator (point-max) t)
    (delete-region (match-beginning 0) (match-end 0))
    (push-mark-command nil t)
    (transient-mark-mode nil))

  ;; Search for a point indicator.
  (goto-char (point-min))
  (if (not (re-search-forward ipe-test--point-indicator
			      (point-max) t))
      nil
    (delete-region (match-beginning 0) (match-end 0))
    t))

(defun ipe-test--show-point ()
  "Insert `ipe-test--point-indicator' at POINT."
  (insert ipe-test--point-indicator)
  (when (functionp 'mc/execute-command-for-all-fake-cursors)
    (funcall 'mc/execute-command-for-all-fake-cursors
	     'ipe-test--show-mc)))

(defun ipe-test--show-mc ()
  "Insert `ipe-test--mc-point-indicator' at POINT."
  (interactive)
  (insert ipe-test--mc-point-indicator))

(defun ipe-test--count-string-lines (string)
  "Count the number of lines within STRING."
  (let ((pos   -1)
	(lines 1))
    (while (setq pos (string-match "[\n\C-m]" string (+ 1 pos)))
      (setq lines (1+ lines)))
    lines))

(defun ipe-test--get-string-line (string line)
  "Return the text at LINE from STRING."
  (let ((pos   -1)
	(lines 1))
    (while (and (> line lines)
		(string-match "[\n\C-m]" string (+ 1 pos)))
      (setq lines (1+ lines)
	    pos   (match-beginning 0)))

    (substring string (+ 1 pos)
	       (string-match "[\n\C-m]" string (+ 1 pos)))))

(defun ipe-test--ert-equal-explainer (buffer-actual buffer-expected)
  "`ert' explainer for `ipe-test--ert-equal'.

This will output a slightly more user-friendly explanation of a
mismatch between the BUFFER-EXPECTED and BUFFER-ACTUAL strings within
an `ipe-ert-test'.  If the BUFFER-EXPECTED and BUFFER-ACTUAL strings
differ, this will output an explanation of the form:

	Text differs at line: LINE

    Expected: <expected LINE produced by test>
    Actual:   <actual LINE produced by test>
    ----------^

Where the final \"---*^\" string will point to the location of the
first character difference between the two lines."
  (let*
      ((actual      (substring-no-properties buffer-actual))
       (expected    (substring-no-properties buffer-expected))
       (mc-point    (regexp-quote ipe-test--mc-point-indicator))
       (expected-mc (replace-regexp-in-string mc-point
					      ""
					      expected))
       (compare     (compare-strings actual   0 (length actual)
				     expected 0 (length expected)))
       (same        (if (integerp compare) (1- (abs compare)) 0))
       (prefix      (substring actual 0 same))
       (lineno      (ipe-test--count-string-lines prefix))
       (line        (number-to-string lineno))
       (actual-line (ipe-test--get-string-line actual lineno)))

    (when (and (not (equal actual expected))
	       (or (featurep 'multiple-cursors)
		   (not (equal actual expected-mc))))
      (let*
	  ((expected-lines   (ipe-test--count-string-lines expected))
	   (expected-line    (ipe-test--get-string-line expected lineno))
	   (mc-point         (regexp-quote ipe-test--mc-point-indicator))
	   (point-indicator  (regexp-quote ipe-test--point-indicator))
	   (actual-line-mc   (replace-regexp-in-string mc-point
						       ""
						       actual-line))
	   (expected-line-mc (replace-regexp-in-string mc-point
						       ""
						       expected-line))
	   (actual-line-nc   (replace-regexp-in-string point-indicator
						       ""
						       actual-line-mc))
	   (expected-line-nc (replace-regexp-in-string point-indicator
						       ""
						       expected-line-mc))
	   (compare-line     (compare-strings
			      actual-line   0 (length actual-line)
			      expected-line 0 (length expected-line)))
	   (compare-line-nc  (compare-strings
			      actual-line-nc   0 (length actual-line-nc)
			      expected-line-nc 0 (length expected-line-nc)))
	   (same-line    (if (integerp compare-line)
			     (1- (abs compare-line))
			   0)))

	(concat "\n\n"
		(if (and ipe-test--keystrokes
			 ipe-test--buffer-text)
		    (concat
		     "\tWhen typing:\n\n\t\t'"
		     ipe-test--keystrokes
		     "'\n\n\tIn buffer:\n\n\t\t"
		     (replace-regexp-in-string
		      "\n" "\n\t\t"
		      (concat ipe-test--buffer-text))
		     (if (> expected-lines 1)
			 (concat
			  "\n\n\tWe expect:\n\n\t\t"
			  (replace-regexp-in-string
			   "\n" "\n\t\t"
			   buffer-expected)
			  "\n\n\tBut got:\n\n\t\t"
			  (replace-regexp-in-string
			   "\n" "\n\t\t"
			   buffer-actual))
		       "")
		     "\n\n")
		  "")
		"\t"
		(if (equal compare-line-nc t) "Cursor position " "Text ")
		"differs at "
		(if (> expected-lines 1) (concat "Line: " line " ") "")
		"Position: " (number-to-string same-line) "\n\n"
		"\t\tExpected: '" expected-line "'\n"
		"\t\tActual:   '" actual-line "'\n"
		"\t\t-----------" (make-string same-line ?-) "^\n")))))

;; Equal with custom 'ert-explainer property.
(defun ipe-test--ert-equal (actual expected)
  "Rename of `equal' to allow \='ert-explainer binding.

This version of `equal' is used by the \"ipe-test--.*\" tests to
perform a `should' test with a custom \='ert-explainer.
\(`ipe-test--ert-equal-explainer').

It provides a description of the differences between the ACTUAL result
and the EXPECTED result."
  (equal-including-properties actual expected))

;; Add the 'ert-explainer to the custom 'equal.
(put 'ipe-test--ert-equal
     'ert-explainer
     'ipe-test--ert-equal-explainer)

(defun ipe-test--names ()
  "Return the `ert' test cases with a \"ipe-test--.*\" prefix.

This function returns a list of strings representing the names of the
\"ipe-test--*\" test cases.  It is expected to be used by a
`completing-read' within `ipe-test-run'."

  (mapcar (lambda (s) (substring (symbol-name s)
				 (length "ipe-test--")))
	  (apropos-internal
	   "ipe-test--.*"
	   (lambda (s) (and (ert-test-boundp s)
			    (string-match "^ipe-test--.*"
					  (symbol-name s)))))))

;; -------------------------------------------------------------------
;;;; Test Macros
;; -------------------------------------------------------------------

(defmacro ipe-test-def (name _param doc custom buffer-text expected
			     &rest body)
  "Customized version of `ert-deftest' for `ipe' unit testing.

This will run a set of commands, BODY, against the BUFFER-TEXT within
a temporary buffer, and compare the result to the EXPECTED result.

NAME - The name of the `ert' test.  This will be prefixed with
  \"ipe-test--\".
_PARAM - Placeholder - for future extensibility.
DOC - Documentation string for the `ert-deftest' definition.
CUSTOM - List of `ipe' customizations for the test.
BUFFER-TEXT - Starting text within the temporary buffer.
EXPECTED - Expected text within the temporary buffer after test
  completion.
BODY - Commands to be executed against the temporary buffer."

  (declare (indent defun))
  (when (or (functionp 'multiple-cursors-mode)
	    (not (string= "mc-" (substring (symbol-name name) 0 3))))

    `(ert-deftest
	 ,(intern (concat "ipe-test--" (symbol-name name)))
	 ()
       ,doc
       (with-temp-buffer
	 (let* ,(eval (identity custom))
	   (buffer-enable-undo)
	   (ipe-edit--keymap-init)
	   (if (listp ,buffer-text)
	       (insert (mapconcat 'identity ,buffer-text "\n"))
	     (insert ,buffer-text))
	   (ipe-test--set-point)
	   (let ((inhibit-message t))
	     (condition-case nil
		 ,@body
	       (t nil)))
	   (ipe-test--show-point)
	   (let* ((actual (buffer-string))
		  (concat (if (listp ,expected)
			      (mapconcat 'identity ,expected "\n")
			    ,expected))
		  (expected (if (functionp 'multiple-cursors-mode)
				concat
			      (replace-regexp-in-string
			       ipe-test--mc-point-indicator
			       ""
			       concat)))
		  (ipe-test--buffer-text
		   (if (listp ,buffer-text)
		       (mapconcat 'identity ,buffer-text "\n")
		     ,buffer-text)))
	     (should (ipe-test--ert-equal actual expected)))))
       (ipe-edit--keymap-init))))

(defmacro ipe-test-def-kbd
    (name param doc custom setup buffer-text expected keystrokes
	  &optional teardown)
  "Customized version of `ert-deftest' for `ipe' unit testing.

This will run a set of keyboard commands, KEYSTROKES, against the
BUFFER-TEXT within a temporary buffer, and compare the result to the
EXPECTED result.

`ipe-insert-pair-edit' is bound to <M-(>.

NAME - The name of the `ert' test.  This will be prefixed with
  \"ipe-test--\".
PARAM - Placeholder - for future extensibility.
DOC - Documentation string for the `ert-deftest' definition.
CUSTOM - List of `ipe' customizations for the test.
SETUP - Commands used to initialize the temporary test buffer.
BUFFER-TEXT - Starting text within the temporary buffer.
EXPECTED - Expected text within the temporary buffer after test
  completion.
KEYSTROKES - The keystrokes to be executed as user input against the
  temporary buffer.
TEARDOWN - Commands used to clean up the temporary test buffer."

  (declare (indent defun))
  (when ipe-test--buffer-text-alist-p
    (setq ipe-test--buffer-text-alist
	  (cons (cons (symbol-name name)
		      (list buffer-text keystrokes))
		(assq-delete-all (symbol-name name)
				 ipe-test--buffer-text-alist))))
  `(ipe-test-def ,name ,param ,doc ,custom ,buffer-text ,expected
     (save-window-excursion
       (set-window-buffer nil (current-buffer))
       (let ((original-binding (global-key-binding (kbd "M-("))))

	 (setq ipe-test--keystrokes (concat ,keystrokes))

	 (unwind-protect
	     (progn
	       (when ,setup
		 (progn (,setup)))
	       (local-set-key (kbd "M-(") 'ipe-insert-pair-edit)
	       (execute-kbd-macro (kbd (concat ,keystrokes))))
	   (local-set-key (kbd "M-(") original-binding)
	   (when ,teardown
	     (progn (,teardown))))))))

(defun ipe-test--find-ert-test (symbol)
  "`find-function-regexp-alist' entry for \"ipe-test--.*\" SYMBOLs.

This function replaces the standard `ert--find-test-regexp' variable
within the `find-function-regexp-alist' for \='ert-test SYMBOLs.

It extends the regular expression search to include the `ipe-test-def'
and `ipe-test-def-kbd' macros."

  (let* ((name (symbol-name symbol))
	 (search-symbol (if (string-prefix-p "ipe-test--" name)
			    (substring name (length "ipe-test--"))
			  name)))
    (goto-char (point-min))
    (re-search-forward (concat find-function-space-re
			       "\\s-*("
			       "\\(?:ert-deftest"
			       "\\|ipe-test-def"
			       "\\|ipe-test-def-kbd"
			       "\\)"
			       find-function-space-re
			       search-symbol
			       "\\(\\s-\\|$\\)")
		       nil
		       t)))

;; Add the `ipe-test--find-ert-test' replacement to the
;; `find-function-regexp-alist'.
(if (assoc 'ert--test find-function-regexp-alist)
    (setf (cdr (assoc 'ert--test find-function-regexp-alist))
	  'ipe-test--find-ert-test)
  (push '(ert--test . ipe-test--find-ert-test)
	find-function-regexp-alist))

;; -------------------------------------------------------------------
;;;; Interactive `ipe' Test Functions
;; -------------------------------------------------------------------

(defun ipe-test-run (pattern)
  "Run an `ert' test that has prefix \"ipe-test--.*\".

Prompt for the name of an `ipe-test-def' or `ipe-test-def-kbd'
test to be run, and run it.  The input PATTERN is a regular expression
that will match the NAMEs of the tests to be run."
  (interactive (list (completing-read "IPE Test: "
				      (ipe-test--names)
				      nil
				      nil
				      nil
				      ipe-test--names)))
  (ert (concat "^ipe-test--.*" pattern ".*")))

(defun ipe-test-run-all ()
  "Run all of the `ert' test cases that start with \"ipe-test--.*\".

This interactive function runs all of the tests with `ipe-test-def' or
`ipe-test-def-kbd'."
  (interactive)
  (ert "^ipe-test--.*"))

(defun ipe-test-run-all-and-exit (&optional infix quiet)
  "Run all of the `ert' test cases that start with \"ipe-test--.*\".

This function runs all of the tests with `ipe-test-def' or
`ipe-test-def-kbd' and then exits.

IF INFIX is non-nil, only run the tests which start with
\"ipe-test--\" INFIX \".*\".
If QUIET is non-nil, set `ert-quiet'."

  (let ((ert-quiet quiet))
    (ert-run-tests-batch-and-exit
     (concat "^ipe-test--" (if infix infix "") ".*"))))

(defun ipe-test-buffer (name)
  "Create a buffer containing the BUFFER-TEXT from an `ipe-test-def'.

Create a temporary buffer \"*ipe-test-buffer*\"' and populate it with
the BUFFER-TEXT for the `ipe-test-def' macro named NAME.  The position
of POINT will be set according to the `ipe-test--point-indicator'
within the BUFFER-TEXT.

If BUFFER-TEXT contains `ipe-test--mark-indicator', MARK will be set
to the according to the position indicated.

If BUFFER-TEXT contains one or more `ipe-test--mc-point-indicator's,
`multiple-cursors-mode' will be activated, and a `mc' cursor will be
added to each position indicated."

  (interactive (list (completing-read "IPE Test: "
				      (ipe-test--names)
				      nil
				      nil
				      nil
				      ipe-test--names)))

  (let ((buffer-text (cadr  (assoc name ipe-test--buffer-text-alist)))
	(keystrokes  (ipe-compat--caddr
		      (assoc name ipe-test--buffer-text-alist))))
    (if (stringp keystrokes)
	(progn
	  (with-current-buffer (get-buffer-create "*ipe-test-buffer-keys*")
	    (setq buffer-read-only nil)
	    (delete-region (point-min) (point-max))
	    (insert keystrokes)
	    (goto-char (point-min))
	    (setq buffer-read-only t))
	  (split-window-vertically)
	  (switch-to-buffer-other-window "*ipe-test-buffer-keys*")
	  (shrink-window-if-larger-than-buffer))
      (when (get-buffer "*ipe-test-buffer-keys*")
	(kill-buffer "*ipe-test-buffer-keys*")))

    (with-current-buffer (get-buffer-create "*ipe-test-buffer*")
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (if (listp buffer-text)
	  (insert (mapconcat 'identity (cadr buffer-text) "\n"))
	(insert buffer-text))
      (ipe-test--set-point))
    (switch-to-buffer-other-window "*ipe-test-buffer*")
    (when (stringp keystrokes)
      (message (replace-regexp-in-string "%" "%%" keystrokes)))))

(defun ipe-test--ert-test-buffer (test-name)
  "Display, in another window, the BUFFER-TEXT for ipe-test TEST-NAME.

As a default, use the symbol at POINT, or, the test at point if in the
ERT results buffer."

  (interactive (list (ert-read-test-name-at-point
		      "Find ipe-test definition: ")))
  (let ((ipe-test-name
	 (if (string-prefix-p "ipe-test--" (symbol-name test-name))
	     (substring (symbol-name test-name) (length "ipe-test--"))
	   (symbol-name test-name))))
    (ipe-test-buffer ipe-test-name)))

(defun ipe-test--occur-failed ()
  "Run `occur' to search for failed test strings within *ert*."

  (interactive)

  (if (eq major-mode 'ert-results-mode)
      (occur "^F ")
    (message "This command should only be run from within\
 `ert-results-mode'.")))

(progn
  (define-key ert-results-mode-map (kbd "e")
	      'ipe-test--ert-test-buffer)
  (define-key ert-results-mode-map (kbd "f")
	      'ipe-test--occur-failed))

(provide 'ipe-test)

;;; ipe-test.el ends here
