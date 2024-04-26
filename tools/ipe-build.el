;;; ipe-build.el --- Tools called by the `ipe' Makefile
;; Copyright (C) 2024 Brian Kavanagh

;; Author: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Maintainer: Brian Kavanagh (concat "Brians.Emacs" "@" "gmail.com")
;; Created: 20 March, 2024
;; Package: ipe
;; Keywords: tools
;; Homepage: https://github.com/BriansEmacs/insert-pair-edit.el

;; -------------------------------------------------------------------
;;; Commentary:
;;
;; The functions within this file are called by the Insert Pair Edit
;; (ipe) GNU Makefile to perform SDLC (Software Development LifeCycle)
;; checks (clean, test, lint, build, version) on the `ipe' source
;; code.

;; -------------------------------------------------------------------
;;; Code:

(require 'package)

(defvar ipe-build--elisp-files
  '("ipe.el"
    "ipe-core.el"
    "ipe-char.el"
    "ipe-compat.el"
    "ipe-custom.el"
    "ipe-doc.el"
    "ipe-defn.el"
    "ipe-edit.el"
    "ipe-help.el"
    "ipe-line.el"
    "ipe-list.el"
    "ipe-mc.el"
    "ipe-menu.el"
    "ipe-mouse.el"
    "ipe-read.el"
    "ipe-updt.el"
    "ipe-word.el"
    "modes/ipe-c-mode.el"
    "modes/ipe-html-mode.el"
    "modes/ipe-markdown-mode.el"
    "modes/ipe-texinfo-mode.el")
  "List of Emacs Lisp files which make up the `ipe' package.")

(defvar ipe-build--test-elisp-files
  '("test/ipe-test.el"
    "test/ipe-test-add.el"
    "test/ipe-test-adjust.el"
    "test/ipe-test-big-tags.el"
    "test/ipe-test-char.el"
    "test/ipe-test-contents.el"
    "test/ipe-test-custom.el"
    "test/ipe-test-defn.el"
    "test/ipe-test-edit.el"
    "test/ipe-test-escape.el"
    "test/ipe-test-forward-first.el"
    "test/ipe-test-indent.el"
    "test/ipe-test-infix.el"
    "test/ipe-test-insert-and.el"
    "test/ipe-test-line.el"
    "test/ipe-test-list.el"
    "test/ipe-test-matching.el"
    "test/ipe-test-mc.el"
    "test/ipe-test-menu.el"
    "test/ipe-test-missing.el"
    "test/ipe-test-options.el"
    "test/ipe-test-other.el"
    "test/ipe-test-update.el"
    "test/ipe-test-word.el")
  "List of Emacs Lisp files which make up the `ipe' ert tests.")

(defvar ipe-build--ert-tests
  '(ipe-test-add
    ipe-test-adjust
    ipe-test-big-tags
    ipe-test-char
    ipe-test-contents
    ipe-test-custom
    ipe-test-defn
    ipe-test-edit
    ipe-test-escape
    ipe-test-forward-first
    ipe-test-indent
    ipe-test-infix
    ipe-test-insert-and
    ipe-test-line
    ipe-test-list
    ipe-test-matching
    ipe-test-mc
    ipe-test-menu
    ipe-test-missing
    ipe-test-options
    ipe-test-other
    ipe-test-update
    ipe-test-word)
  "List of Emacs Regression Test features.")

(defvar ipe-build-test-names ""
  "The infix used to filter the tests run by `ipe-build--test'.

This infix is appended to the string \"ipe-test-\" and passed to
`ert-run-tests-batch-and-exit' to filter the list of `ipe' ERT tests
run by the `ipe-build--test' function.")

(defvar ipe-build--verbose
  (or (string-to-number ipe-build-verbose)
      1)
  "A number, 0-3, describing how verbose to be when building.")

(defun ipe-build--log (level format-string &rest args)
  "Log a message if LEVEL <= `ipe-build--verbose'.

If LEVEL < `ipe-build--verbose', the call `message' with
FORMAT-STRING and ARGS."

  (when (<= level ipe-build--verbose)
    (apply 'message format-string args)))

;; ------------------------------------------------------------------
;;; SDLC Functions.
;; ------------------------------------------------------------------

(defun ipe-build--clean-pkgs ()
  "Clean the pakcage directories used in `ipe' source code build.

This function is called from the top level `ipe' `Makefile' to clean
up the package directories created for the build of the `ipe' package
from its Emacs Lisp source code."

  (ipe-build--log 1 "Cleaning build-packages files...")

  (when (file-directory-p ipe-build-pkgs-dir)
    (ipe-build--log 2 "Deleting build directory `%s'." ipe-build-pkgs-dir)
    (delete-directory ipe-build-pkgs-dir t))

  (ipe-build--log 1 "Cleaning build-packages files.  Done"))

(defun ipe-build--pkgs ()
  "Download the build tools from a package archive.

This function is called from the top level `ipe' `Makefile' to
download the build tools package dependencies from a package archive."

  (ipe-build--log 1 "Downloading 'build-packages' Dependencies...")

  (setq package-user-dir ipe-build-pkgs-dir)
  (package-initialize)
  (setq package-archives (list (cons "archive" ipe-package-url)))

  (unless package-archive-contents
    (let ((url-show-status (> ipe-build--verbose 2))
	  (package-refresh-contents))))

  (let ((package-list '(dash
			package-lint
			elisp-lint
			multiple-cursors)))

    (dolist (package package-list)
      (if (package-installed-p package)
	  (ipe-build--log 2 "Package `%s' already installed in `%s'"
			  (symbol-name package)
			  package-user-dir)
	(ipe-build--log 2 "Installing package `%s' in `%s'"
			(symbol-name package)
			package-user-dir)
	(let ((inhibit-message (< ipe-build--verbose 3)))
	  (package-install package))

	(unless (package-installed-p package)
	  (ipe-build--log 0 "Failed to install package `%s' in `%s'"
			  (symbol-name package)
			  package-user-dir)))))

  (ipe-build--log 1 "Downloading 'build-packages' Dependencies.  Done."))

(defun ipe-build--clean ()
  "Clean the directories used in `ipe' source code build.

This function is called from the top level `ipe' `Makefile' to clean
up the *.elc files created for the build of the `ipe' package from
its Emacs Lisp source code."

  (ipe-build--log 1 "Cleaning *.elc files...")

  (dolist (file (directory-files ipe-base-dir t "\\(\\.elc\\|~\\)$"))
    (ipe-build--log 2 "Deleting file `%s'." file)
    (delete-file file))

  (dolist (file (directory-files
		 (concat ipe-base-dir "/modes")
		 t
		 "\\(\\.elc\\|~\\)$"))
    (ipe-build--log 2 "Deleting file `%s'." file)
    (delete-file file))

  (dolist (file (directory-files
		 (concat ipe-base-dir "/test")
		 t
		 "\\(\\.elc\\|~\\)$"))
    (ipe-build--log 2 "Deleting file `%s'." file)
    (delete-file file))

  (when (file-exists-p "ipe-autoloads.el")
    (delete-file "ipe-autoloads.el"))

  (ipe-build--log 1 "Cleaning *.elc files.  Done"))

(defun ipe-build--test ()
  "Run the `ipe' ERT (Emacs Regression Test) tests.

This function is called from the top level `ipe' `Makefile' to run the
`ipe' ERT (Emacs Regression Tests) against the `ipe' Emacs Lisp source
code."

  (ipe-build--log
   1
   "Running 'ert' ERTs (Emacs Regression Tests) (%s)..."
   emacs-version)

  ;; Set up the test load path.
  (add-to-list 'load-path ipe-base-dir)
  (add-to-list 'load-path (concat ipe-base-dir "/test"))

  ;; Load ERT Tests.
  (dolist (test ipe-build--ert-tests)
    (require test))

  (setq ipe-menu-support-p t)
  (ipe-menu--install)

  (ipe-test-run-all-and-exit ipe-build-test-names
                             (< ipe-build--verbose 2)))

(defun ipe-build--lint ()
  "Run elint over the `ipe' Emacs Lisp source code.

This function is called from the top level `ipe' `Makefile' to run
elint against the `ipe' Emacs Lisp source code."

  (ipe-build--log 1 "Linting `ipe' Emacs Lisp source code...")

  (add-to-list 'load-path ipe-base-dir)
  (setq package-user-dir ipe-build-pkgs-dir)
  (package-initialize)
  (require 'elisp-lint)
  (require 'package-lint)

  (let ((success t)
	(file-success)
	(dir (expand-file-name ipe-base-dir))
	(package-lint-main-file (expand-file-name "ipe.el")))

    (dolist (file ipe-build--elisp-files)

      (cd-absolute dir)

      (setq file-success t)

      (let ((file-dir (file-name-parent-directory (expand-file-name file))))
	(when (not (member file-dir load-path))
	  (add-to-list 'load-path file-dir)))

      ;; 'elisp-lint
      (ipe-build--log 3 "elisp-lint:   %s" file)
      (when (not (elisp-lint-file (expand-file-name file)))
	(setq success      nil
	      file-success nil))

      ;; 'package-lint
      (ipe-build--log 3 "package-lint: %s" file)

      (find-file (expand-file-name file))
      (emacs-lisp-mode)
      (indent-tabs-mode -1)

      (let* ((checking-result (package-lint-buffer)))
	(when checking-result
	  (setq success      nil
		file-success nil)
	  (dolist (result checking-result)
	    (message "%s:%d:%d: %s %s"
		     file
		     (nth 0 result)
		     (nth 1 result)
		     (nth 2 result)
		     (nth 3 result)))))

      ;; Log the result.
      (if file-success
	  (or (< ipe-build--verbose 2)
	      (elisp-lint--print 'green "lint: %s: OK" file))
	(elisp-lint--print 'red "lint: %s: FAIL" file)))

    (if success
	(progn (when (> ipe-build--verbose 0)
		 (ipe-build--log 1 "Linting `ipe' Emacs Lisp source code.  Done.")
		 (kill-emacs 0)))
      (elisp-lint--print 'red
			 "Linting `ipe' Emacs Lisp source code. FAILED.")
      (kill-emacs 1))))

(defun ipe-build--linttest ()
  "Run elint over the `ipe' Emacs Lisp test source code.

This function is called from the top level `ipe' `Makefile' to run a
Emacs Lisp lint agains the `ipe' Emacs Lisp test source code."

  (ipe-build--log 1 "Linting `ipe' Emacs Lisp test source code...")

  (add-to-list 'load-path ipe-base-dir)
  (setq package-user-dir ipe-build-pkgs-dir)
  (package-initialize)
  (require 'elisp-lint)

  (let ((success t))
    (dolist (file ipe-build--test-elisp-files)
      (if (elisp-lint-file (expand-file-name file))
	  (or (< ipe-build--verbose 2)
	      (elisp-lint--print 'green "lint: %s: OK" file))
	(elisp-lint--print 'red "lint: %s: FAIL" file)
	(setq success nil)))

    (if success
	(progn (when (> ipe-build--verbose 0)
		 (ipe-build--log 1 "Linting `ipe' Emacs Lisp test source code.  Done.")
		 (kill-emacs 0)))
      (elisp-lint--print 'red
			 "Linting `ipe' Emacs Lisp test source code. FAILED.")
      (kill-emacs 1))))

(defun ipe-build--build ()
  "Build the `ipe' source code.

This function is called from the top level `ipe' `Makefile' to build the
`ipe' package from its Emacs Lisp source code."

  (ipe-build--log
   1
   "Building `ipe' Emacs Lisp (%s) code..."
   emacs-version)

  (add-to-list 'load-path ipe-base-dir)
  (setq package-user-dir ipe-build-pkgs-dir)
  (package-initialize)

  (let ((success t))
    (dolist (file ipe-build--elisp-files)
      (if (byte-compile-file file)
	  (ipe-build--log 2 "build: %s: OK" file)
	(ipe-build--log 0 "build: %s: FAIL" file)
	(setq success nil)))

    (if success
	(progn (ipe-build--log 1 "Building `ipe' Emacs Lisp code. Done.")
	       (kill-emacs 0))
      (ipe-build--log 0 "Building `ipe' Emacs Lisp code. FAILED.")
      (kill-emacs 1))))

(defun ipe-build--version (version)
  "Update the `ipe' source code version to VERSION.

This function is called from the top level `ipe' `Makefile' to update
the `;; Version: X.X.XXX' headers within the `ipe' Emacs Lisp source
files."

  (ipe-build--log 1 "Versioning `ipe' files...")

  ;; Update the ';; Version: X.X.XXX' comment.
  (let ((files ipe-build--elisp-files))
    (dolist (file files)
      (find-file file)
      (goto-char (point-min))
      (when (re-search-forward "^;; Version: .*")
	(replace-match (concat ";; Version: " version))
	(ipe-build--log 2 "%s Updated to version %s" file version))
      (basic-save-buffer)
      (kill-buffer)))

  ;; Update the ';; Version: X.X.XXX' comment.
  (let ((files ipe-build--test-elisp-files))
    (dolist (file files)
      (find-file file)
      (goto-char (point-min))
      (when (re-search-forward "^;; Version: .*")
	(replace-match (concat ";; Version: " version))
	(ipe-build--log 2 "%s Updated to version %s" file version))
      (basic-save-buffer)
      (kill-buffer)))

  (ipe-build--log 1 "Versioning `ipe' files.  Done."))

(provide 'ipe-build)

;;; ipe-build.el ends here
