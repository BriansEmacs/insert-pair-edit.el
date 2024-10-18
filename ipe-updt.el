;;; ipe-updt.el --- Insert Pair Edit - update functions -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Brian Kavanagh

;; Author: Brian Kavanagh <brians.emacs@gmail.com>
;; Maintainer: Brian Kavanagh <brians.emacs@gmail.com>
;; Created: 30 December, 2023
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
;; This file defines the update functionality required by the 'Insert
;; Pair Edit' (ipe) package.
;;
;; The functions within this file are used to search a buffer for
;; existing 'Insert Pair Edit' (ipe) PAIRs, remove them from the
;; buffer, and replace them with entries set up within the
;; `ipe--pair-pos-list'.

;; -------------------------------------------------------------------
;;; Code:

(require 'ipe-core)

;; -------------------------------------------------------------------
;;;; Utility Functions.
;; -------------------------------------------------------------------

(defun ipe-updt--min-escape (list)
  "Return the `ipe' ESCAPE within LIST with the smallest BEG.

LIST is either: a list of (BEG . END); or, nil.

  * BEG is the beginning of an ESCAPE.
  * END is the end of an ESCAPE."

  (if (<= (length list) 1)
      (car list)
    (let ((min (ipe-updt--min-escape (cdr list))))
      (if (or (not (car min))
	      (and (caar list)
		   (< (caar list) (car min))))
	  (car list)
	min))))

(defun ipe-updt--max-escape (list)
  "Return the `ipe' ESCAPE within LIST with the largest BEG.

LIST is either: a list of (BEG . END); or, nil.

  * BEG is the beginning of an ESCAPE.
  * END is the end of an ESCAPE."

  (if (<= (length list) 1)
      (car list)
    (let ((max (ipe-updt--max-escape (cdr list))))
      (if (or (not (car max))
	      (and (caar list)
		   (> (caar list) (car max))))
	  (car list)
	max))))

;; -------------------------------------------------------------------
;;;; 'Previous' Functions.
;; -------------------------------------------------------------------

(defun ipe-updt--previous-escape (pos escapes bound)
  "Return the position of the previous `ipe' ESCAPE.

- POS is the position from which to begin the search.
- ESCAPES is a list of `ipe' escapes, (MATCH REPLACE), each
  representing a single ESCAPE.
- BOUND is the limit of the search.  If nil, search to the start of
  the accessible portion of the buffer.

If an ESCAPE is found before POS and after BOUND, return the position
\(MATCH-BEGINNING . MATCH-END) the ESCAPE.
If no ESCAPE is found, return nil."

  (save-excursion
    (ipe-updt--max-escape
     (mapcar
      (lambda (escape)
	(goto-char (+ pos (length (cadr escape)) -1))
	(when (search-backward (cadr escape) bound t)
	  (cons (match-beginning 0) (match-end 0))))
      escapes))))

(defun ipe-updt--previous-open (pos open infix close escapes bound
				    &optional count)
  "Return the start position of the previous `ipe' OPEN string.

Search backward from POS for an `ipe' OPEN string, but take into
account:

* That the OPEN may be empty, but may have an INFIX, and search
  backward for the first INFIX in a sequence consecutive `INFIX'es.
* That the OPEN may be a substring of the INFIX, and search backwards
  over any consecutive `INFIX'es until it finds the first OPEN.
* That the characters within the `ipe' OPEN string may have been used
  in an ESCAPE, and ignore any matches to the ESCAPE.

- POS is the position from which to begin the search.
- OPEN is the OPEN string of the PAIR for which the search is being
  made.
- INFIX is the INFIX string of the PAIR for which the search is being
  made.
- ESCAPES is a list of `ipe' escapes, (MATCH REPLACE), each
  representing a single ESCAPE.
- BOUND is the limit of the search.  If nil, search to the beginning
  of the accessible portion of the buffer.
- COUNT, if non-nil, is a number representing how many OPEN strings
  backward to search.

Return the position of the start of the `COUNT'th previous OPEN
string, or nil, if no OPEN string is found."

  (save-excursion
    (goto-char (+ pos (length open) -1))

    (let ((prev) (escape))
      (ipe-dotimes (or count 1)

	;; Find the positions of the previous ESCAPE and OPEN.
	(setq escape (ipe-updt--previous-escape (point) escapes bound)
	      prev   (search-backward open bound t))

	;; Skip backward over ESCAPE'd OPEN's.
	(while (and prev escape (< prev (cdr escape)))

	  (when (>= prev (car escape))
	    (goto-char (car escape))
	    (setq prev (search-backward open bound t)))

	  (goto-char (car escape))
	  (setq escape (ipe-updt--previous-escape (point) escapes bound)))

	;; Skip backward over matching INFIX'es.
	(when (and (> (length infix) 0)
		   prev
		   (or (= (length open) 0)
		       (ipe--string-starts-with-p infix open)))
	  (setq prev (ipe-updt--previous-infix-beg prev infix close bound))))

      prev)))

(defun ipe-updt--previous-infix (pos infix &optional bound)
  "Search backward from POS for previous line starting with an INFIX.

- POS is the position from which to begin the search.
- INFIX is the INFIX string for the current `ipe' PAIR.
- BOUND is the limit of the search, if nil, search to the beginning of
  the accessible portion of the buffer.

Return either the start of the previous line containing INFIX, or
nil if no line starting with INFIX was found."

  (save-excursion
    (goto-char (+ pos (length infix)))
    (re-search-backward (concat "^ *" (regexp-quote infix))
			bound
			t)))

(defun ipe-updt--previous-infix-beg (pos infix close &optional bound)
  "Return the bound of lines before POS starting with INFIX.

- POS is the position from which to begin the search.
- INFIX is the INFIX string for the current `ipe' PAIR.
- CLOSE is the CLOSE string for the current `ipe' PAIR.
- BOUND is the limit of the search, if nil, search to the beginning of
  the accessible portion of the buffer.

This function will first search backward from POS for any line
containing an INFIX string (optionally preceded by whitespace).  If it
finds one, it will then search line-by-line from this point for the
`first' line in the group of continuous lines which contains a INFIX
string (optionally preceded by whitespace).  It will then return the
position within the buffer at which this `first' INFIX in the block of
INFIX'es is located.

If no INFIX is found in a line before POS, return POS."

  (let* ((infix-beg  (ipe-updt--previous-infix pos infix bound))
	 (infix-prev))

    (if (not infix-beg)
	pos

      (save-excursion
	(goto-char infix-beg)

	(setq infix-prev (ipe--infix-pos infix-beg infix))

	;; While the current line contains an INFIX.
	(while (and infix-prev
		    (or (not bound)
			(>= (point) bound))
		    (not (bobp)))

	  ;; Assume the current line is the start of the 'block'.
	  (setq infix-beg infix-prev)

	  ;; Goto the previous line.
	  (forward-line -1)

	  ;; Search for another INFIX on the previous line.
	  (setq infix-prev (ipe--infix-pos (point) infix))

	  ;; If the previous line also contains a CLOSE, we have gone
	  ;; too far back.
	  (when (and infix-prev
		     (ipe--to-eol-contains-p (+ infix-prev (length infix))
					     close))
	    (setq infix-prev nil)))

	(if (or (not infix-prev)
		(and bound (< (point) bound)))
	    infix-beg
	  infix-prev)))))

(defun ipe-updt--previous-close (pos close escapes bound
				     &optional count)
  "Return the end position of the previous `ipe' CLOSE string.

Search backward from POS for an `ipe' CLOSE string, but take into
account:

* That the characters within the `ipe' CLOSE string may have been used
  in an ESCAPE, and ignore any matches to the ESCAPE.

- POS is the position from which to begin the search.
- CLOSE is the CLOSE string of the PAIR for which the search is being
  made.
- ESCAPES is a list of `ipe' escapes, (MATCH REPLACE), each
  representing a single ESCAPE.
- BOUND is the limit of the search.  If nil, search to the beginning
  of the accessible portion of the buffer.
- COUNT, if non-nil, is a number representing how many CLOSE strings
  backward to search.

Return the position of the end of the `COUNT'th previous CLOSE string,
or nil, if no CLOSE string is found."

  (save-excursion
    (goto-char (if (and (>= pos (point-max))
			(> 0 (- pos (length close))))
		   (- pos (length close))
		 pos))

    (let ((prev) (escape))
      (ipe-dotimes (or count 1)

	;; Find the positions of the previous ESCAPE and CLOSE.
	(setq escape (ipe-updt--previous-escape (point) escapes bound)
	      prev   (search-backward close bound t))

	;; Skip backward over ESCAPE'd CLOSE's.
	(while (and prev escape (< prev (cdr escape)))

	  (when (>= prev (car escape))
	    (goto-char (car escape))
	    (setq prev (search-backward close bound t)))

	  (goto-char (car escape))
	  (setq escape (ipe-updt--previous-escape (point) escapes bound)))

	(when prev
	  (setq prev (+ prev (length close)))))

      prev)))

(defun ipe-updt--previous-matched-open (pos open infix close escapes
					    bound)
  "Return the start of the previous matched `ipe' OPEN string.

Search backward from POS for a matching `ipe' OPEN string, taking
into account:

* That the OPEN may be empty.
* That the OPEN / CLOSE may be either the same as, or prefixes of, the
  INFIX string.
* Nested OPEN + CLOSE PAIRS within the outer OPEN + CLOSE PAIR.
* That the characters within the OPEN and CLOSE strings may have been
  used in an ESCAPE, (and ignore any matches to the ESCAPE.)

- POS is the position from which to begin the search.
- OPEN is the OPEN string of the PAIR being matched.
- INFIX is the INFIX string of the PAIR being matched.
- CLOSE is the CLOSE string of the PAIR being matched.
- ESCAPES is a list of `ipe' escapes, (MATCH REPLACE), each
  representing a single ESCAPE for the PAIR being matched.
- BOUND is the limit of the search.

Return the position of the start of the previous matched OPEN
string, or nil, if no OPEN string is found."

  (save-excursion
    (goto-char (if (and (>= pos (point-max))
			(> 0 (- pos (length close) (length open))))
		   (- pos (length close) (length open))
		 pos))

    (let ((pos-open (ipe-updt--previous-open pos open infix
					     close escapes bound))
	  (pos-close (ipe-updt--previous-close pos close escapes
					       bound)))

      (while (and pos-open
		  pos-close
		  (> (length open) 0)
		  (> pos-close (+ pos-open (length open))))
	(setq pos (ipe-updt--previous-matched-open (+ pos-open (length open))
						   open
						   infix
						   close
						   escapes
						   bound))
	(if pos
	    (setq pos-open (ipe-updt--previous-open pos open infix
						    close escapes bound)
		  pos-close (ipe-updt--previous-close pos close
						      escapes bound))
	  (setq pos-open nil)))

      pos-open)))

(defun ipe-updt--previous-pair (pos open infix close escapes min max
				    count)
  "Return the position of the previous `ipe' PAIR.

Search backward from POS for an `ipe' PAIR, but take into account:

* That the OPEN / CLOSE may be empty.
* That the OPEN / CLOSE may be either the same as, or prefixes of, the
  INFIX string.
* That the characters within the OPEN / CLOSE strings may have been
  used in an ESCAPE, (and ignore any matches to the ESCAPE.)

- POS is the position from which to begin the search.
- OPEN is the OPEN string of the PAIR for which the search is being
  made.
- INFIX is the INFIX string of the PAIR for which the search is being
  made.
- CLOSE is the CLOSE string of the PAIR for which the search is being
  made.
- ESCAPES is a list of `ipe' escapes, (MATCH REPLACE), each
  representing a single ESCAPE for the PAIR for which the search is
  being made.
- MIN, if non-nil, is the lower bound for the search.
- MAX, if non-nil, is the upper bound for the search.
- COUNT, if non-nil, is a number representing how many PAIRs backward
  to search.

If a PAIR is found, return a CONS cell whose `car' contains position
of the start of the `COUNT'th previous OPEN string, and whose `cdr'
contains the end of the corresponding CLOSE string.

If no PAIR is found, return nil."

  (let* ((len-open  (length open))
	 (len-infix (length infix))
	 (len-close (length close))
	 (start)
	 (beg       pos)
	 (end))

    (save-excursion
      (ipe-dotimes count
	(setq start beg)

	(while start

	  ;; The beginning of the PAIR (BEG) is determined by:
	  ;;
	  ;; - If OPEN & CLOSE & INFIX is empty, POS.
	  ;; - If OPEN & CLOSE is empty, search backward for the first INFIX.
	  ;; - If OPEN & INFIX is empty, search backward for either:
	  ;;   - The initial movement position.
	  ;;   - The previous CLOSE.
	  ;; - If OPEN is empty, search backward for the previous CLOSE.
	  ;; - Otherwise, search backward for the previous OPEN.
	  (setq beg
		(cond
		 ((zerop (+ len-open len-infix len-close))
		  start)

		 ((zerop (+ len-open len-close))
		  (ipe-updt--previous-infix-beg start infix close min))

		 ((zerop (+ len-open len-infix))
		  (let ((open-init      (ipe--open-init-pos 0
							    start))
			(previous-close (ipe-updt--previous-close start
								  close
								  escapes
								  min)))
		    (if previous-close
			(max open-init previous-close)
		      (min start open-init))))

		 ((zerop len-open)
		  (ipe-updt--previous-infix-beg start infix close min))

		 (t
		  (ipe-updt--previous-open start
					   open
					   infix
					   close
					   escapes
					   min))))

	  ;; The end of the PAIR (END) is determined by:
	  ;;
	  ;; - If no beginning was found, nil.
	  ;; - If CLOSE is empty, we end at either:
	  ;;     The end of the next OPEN, or
	  ;;     If there is no next OPEN,
	  ;;       The end of the buffer.
	  ;; - If CLOSE = INFIX, we end at the last INFIX.
	  ;; - Otherwise, we search forwards from BEG for the nearest
	  ;;   matched CLOSE.
	  (setq end
		(cond
		 ((not beg)
		  nil)

		 ((zerop (+ len-close len-infix))
		  (ipe--close-init-pos 0 beg))

		 ((or (zerop len-close)
		      (and (not (zerop len-infix)) (string= close infix)))
		  (let ((close-init (ipe--close-init-pos 0 (+ beg len-open)))
			(next-open  (ipe-updt--next-infix-end (+ beg len-open)
							      infix
							      close
							      max)))
		    (max close-init next-open)))

		 ((zerop len-open)
		  (let* ((next-infix (ipe-updt--next-infix-end beg
							       infix
							       close
							       max))
			 (next-close (ipe-updt--next-close (- next-infix len-infix)
							   close
							   escapes
							   max)))
		    next-close))

		 (t
		  (ipe-updt--next-matched-close (+ beg len-open)
						open
						infix
						close
						escapes
						max))))

	  ;; If we have found just the beginning of a PAIR, keep
	  ;; searching, (We may have OPEN = CLOSE, or, we may have an
	  ;; unclosed OPEN and need to keep searching backwards for a
	  ;; closed OPEN.)
	  (setq start (if (and beg
			       (not end)
			       (> beg (or min (point-min)))
			       (or (zerop len-infix)
				   (not (<= (ipe--bol beg)
					    (or min (point-min))))))
			  (if (zerop len-open)
			      (1- beg)
			    beg)
			nil))))

      (when (and beg end)
	(cons beg end)))))

;; -------------------------------------------------------------------
;;;; 'Next' Functions.
;; -------------------------------------------------------------------

(defun ipe-updt--next-escape (pos escapes bound)
  "Return the position of the next `ipe' ESCAPE.

- POS is the position from which to begin the search.
- ESCAPES is a list of `ipe' escapes, (MATCH REPLACE), each
  representing a single ESCAPE.
- BOUND is the limit of the search.  If nil, search to the end of the
  accessible portion of the buffer.

If an ESCAPE is found after POS and before BOUND, return the position
\(MATCH-BEGINNING . MATCH-END) of ESCAPE.
If no ESCAPE is found, return nil."

  (save-excursion
    (ipe-updt--min-escape
     (mapcar
      (lambda (escape)
	(goto-char pos)
	(when (search-forward (cadr escape) bound t)
	  (cons (match-beginning 0) (match-end 0))))
      escapes))))

(defun ipe-updt--next-open (pos open infix close escapes bound
				&optional count no-skip-infix)
  "Return the start position of the next `ipe' OPEN string.

Search forward from POS for an `ipe' OPEN string, but take into
account:

* That the characters within the `ipe' OPEN string may have been used
  in an ESCAPE, and ignore any matches to the ESCAPE.
* That the OPEN may be a substring of the INFIX, and search forward
  over any consecutive `INFIX'es before searching for the next OPEN.

- POS is the position from which to begin the search.
- OPEN is the OPEN string of the PAIR for which the search is being
  made.
- INFIX is the INFIX string of the PAIR for which the search is being
  made.
- CLOSE is the CLOSE string of the PAIR for which the search is being
  made.
- ESCAPES is a list of `ipe' escapes, (MATCH REPLACE), each
  representing a single ESCAPE.
- BOUND is the limit of the search, if nil, search to the end of the
  accessible portion of the buffer.
- COUNT, if non-nil, is a number representing how many OPEN strings
  forward to search.
- NO-SKIP-INFIX is a flag, which if true, indicates that if POS is
  within a line containing an INFIX, not to skip over the INFIX'es
  before beginning the search.

Return the position of the start of the `COUNT'th next OPEN string, or
nil, if no OPEN string is found."

  (save-excursion
    (goto-char pos)

    (let ((next) (escape))
      (ipe-dotimes (or count 1)

	;; Skip forward over any 'leading' infixes.
	(unless no-skip-infix
	  (goto-char (ipe-updt--next-infix-end (point) infix close bound)))

	;; Find the positions of the next ESCAPE and OPEN.
	(setq escape (ipe-updt--next-escape (point) escapes bound)
	      next   (search-forward open bound t))

	;; Skip forward over ESCAPE'd OPEN's.
	(while (and next escape (> next (car escape)))

	  (when (<= next (cdr escape))
	    (goto-char (1+ (cdr escape)))
	    (setq next (search-forward open bound t)))

	  (goto-char (cdr escape))
	  (setq escape (ipe-updt--next-escape (point) escapes bound)))

	(when next
	  (setq next (- next (length open)))))

      next)))

(defun ipe-updt--next-infix (pos infix &optional bound)
  "Search forward from POS for next line starting within an INFIX.

- POS is the position from which to begin the search.
- INFIX is the INFIX string for the current `ipe' PAIR.
- BOUND is the limit of the search, if nil, search to the end of the
  accessible portion of the buffer.

Return either the start of the next line containing INFIX, or
POS if no line starting with INFIX was found after POS."
  (save-excursion
    (goto-char pos)

    (if (re-search-forward (concat "^ *" (regexp-quote infix))
			   bound
			   t)
	(ipe--bol (point))
      pos)))

(defun ipe-updt--next-infix-end (pos infix close &optional bound)
  "Return the bound of the lines after POS starting with INFIX.

- POS is the position from which to begin the search.
- INFIX is the INFIX string for the current `ipe' PAIR.
- CLOSE is the CLOSE string for the current `ipe' PAIR.
- BOUND is the limit of the search, if nil, search to the end of
  the accessible portion of the buffer.

Return either the end of the last line containing INFIX after POS, or,
POS if no line starting with INFIX was found."

  (if (zerop (length infix))
      pos

    (let ((infix-end pos)
	  (infix-pos (ipe--infix-pos pos infix)))

      (save-excursion
	(goto-char infix-end)

	;; While the current line contains an INFIX.
	(while (and infix-pos
		    (or (not bound)
			(< (point) bound))
		    (not (eobp)))

	  (setq infix-end (if bound
			      (min (ipe--eol (point)) bound)
			    (ipe--eol (point))))

	  (if (ipe--to-eol-contains-p (+ infix-pos (length infix)) close)
	      (setq infix-pos nil)
	    (forward-line 1)
	    (setq infix-pos (ipe--infix-pos (point) infix)))))

      infix-end)))

(defun ipe-updt--next-close (pos close escapes bound &optional count)
  "Return the end position the next `ipe' CLOSE string.

Search forward from POS for an `ipe' CLOSE string, but take into
account:

* That the characters within the `ipe' CLOSE string may have been used
  in an ESCAPE, and ignore matches to the ESCAPE.

- POS is the position from which to begin the search.
- CLOSE is the CLOSE string of the PAIR for which the search is being
  made.
- ESCAPES is a list of `ipe' escapes, (MATCH REPLACE), each
  representing a single ESCAPE.
- BOUND is the limit of the search.  If nil, search to the end of the
  accessible portion of the buffer.
- COUNT, if non-nil, is a number representing how many CLOSE strings
  forward to search.

Return the position of the end of the `COUNT'th next CLOSE string, or
nil, if no CLOSE string is found."

  (save-excursion
    (goto-char pos)

    (let ((next) (escape))
      (ipe-dotimes (or count 1)

	;; Find the positions of the next CLOSE and ESCAPE.
	(setq escape (ipe-updt--next-escape (point) escapes bound)
	      next   (search-forward close bound t))

	;; Skip forward over ESCAPE'd CLOSE's.
	(while (and next escape (> next (car escape)))

	  (when (<= next (cdr escape))
	    (goto-char (cdr escape))
	    (setq next (search-forward close bound t)))

	  (goto-char (cdr escape))
	  (setq escape (ipe-updt--next-escape (point) escapes bound))))

      next)))

(defun ipe-updt--next-matched-close (pos open infix close escapes
					 bound)
  "Return the end of the next matched `ipe' CLOSE string.

Search forward from POS for a matching `ipe' CLOSE string, taking into
account:

* That the CLOSE may be empty.
* That the OPEN / CLOSE may be either the same as, or prefixes of, the
  INFIX string.
* Nested OPEN + CLOSE PAIRS within the outer OPEN + CLOSE PAIR.
* That the characters within the OPEN and CLOSE strings may have been
  used in an ESCAPE, (and ignore any matches to the ESCAPE.)

- POS is the position from which to begin the search.
- OPEN is the OPEN string of the PAIR being matched.
- INFIX is the INFIX string of the PAIR for which the search is being
  made.
- CLOSE is the CLOSE string of the PAIR being matched.
- ESCAPES is a list of `ipe' escapes, (MATCH REPLACE), each
  representing a single ESCAPE.
- BOUND is the limit of the search.

Return the position of the end of the next matched CLOSE string, or
nil, if no CLOSE string is found."

  (save-excursion
    (goto-char pos)

    (let ((pos-close (ipe-updt--next-close pos close escapes bound))
	  (pos-open  (ipe-updt--next-open  pos open infix close
					   escapes bound)))

      (while (and pos-open
		  pos-close
		  (> (length open) 0)
		  (> pos-close (+ pos-open (length open))))
	(setq pos (ipe-updt--next-matched-close (+ pos-open (length open))
						open
						infix
						close
						escapes
						bound))
	(if pos
	    (setq pos-close (ipe-updt--next-close pos close escapes bound)
		  pos-open  (ipe-updt--next-open  pos open infix close
						  escapes
						  bound))
	  (setq pos-close nil)))

      pos-close)))

(defun ipe-updt--next-pair (pos open infix close escapes max count)
  "Return the position of the next `ipe' PAIR.

Search forward from POS for an `ipe' PAIR, but take into account:

* That the OPEN / CLOSE may be empty.
* That the OPEN / CLOSE may be either the same as, or prefixes of, the
  INFIX string.
* That the characters within the OPEN / CLOSE strings may have been
  used in an ESCAPE, (and ignore any matches to the ESCAPE.)

- POS is the position from which to begin the search.
- OPEN is the OPEN string of the PAIR for which the search is being
  made.
- INFIX is the INFIX string of the PAIR for which the search is being
  made.
- CLOSE is the CLOSE string of the PAIR for which the search is being
  made.
- ESCAPES is a list of `ipe' escapes, (MATCH REPLACE), each
  representing a single ESCAPE for the PAIR for which the search is
  being made.
- MAX, if non-nil, is the bound for the search.
- COUNT, if non-nil, is a number representing how many PAIRs forward
  to search.

If a PAIR is found, return a CONS cell whose `car' contains position
of the start of the `COUNT'th next OPEN string, and whose `cdr'
contains the end of the corresponding CLOSE string.

If no PAIR is found, return nil."

  (let* ((len-open  (length open))
	 (len-infix (length infix))
	 (len-close (length close))
	 (start)
	 (beg)
	 (end       pos)
	 (bound     max))

    (save-excursion
      (ipe-dotimes count
	(setq start end)

	(while start

	  ;; The beginning of the PAIR (BEG) is determined by:
	  ;;
	  ;; - If OPEN & INFIX & CLOSE is empty, POS.
	  ;; - If OPEN & CLOSE is empty, search forward for next INFIX.
	  ;; - If OPEN is empty, search for next CLOSE.
	  ;; -   If there is no next CLOSE, POS.
	  ;; - Otherwise, search forward for OPEN.
	  (setq beg
		(cond
		 ((zerop (+ len-open len-infix len-close))
		  start)

		 ((zerop (+ len-open len-close))
		  (ipe-updt--next-infix start infix))

		 ((zerop len-open)
		  (setq bound (ipe-updt--next-infix-end start
							infix
							close
							max))

		  (or (ipe-updt--next-close bound close escapes max)
		      start))

		 (t
		  (ipe-updt--next-open start
				       open
				       infix
				       close
				       escapes
				       max
				       1
				       t))))

	  ;; The end of the PAIR (END) is determined by:
	  ;;
	  ;; - If no beginning was found, nil.
	  ;; - If CLOSE & INFIX is empty, we end at the PAIR init
	  ;;   position.
	  ;; - If CLOSE = INFIX, we end at the last INFIX.
	  ;; - If CLOSE is empty, we end at ether:
	  ;;     The end of the current INFIX, or
	  ;;     If there is no next INFIX,
	  ;;       The start of the next OPEN.
	  ;; - Otherwise, we search forwards from BEG for the nearest
	  ;;   matched CLOSE.
	  (setq end
		(cond
		 ((not beg)
		  nil)

		 ((zerop (+ len-close len-infix))
		  (ipe--close-init-pos 0 beg))

		 ((ipe--string-starts-with-p infix close)
		  (let* ((close-init (ipe--close-init-pos 0 (+ beg len-open)))
			 (next-infix (ipe-updt--next-infix-end (+ beg len-open)
							       infix
							       close
							       max)))
		    (max close-init next-infix)))

		 ((zerop len-close)
		  (let ((close-init (ipe--close-init-pos 0 (+ beg len-open)))
			(infix-end  (ipe-updt--next-infix-end (+ beg len-open)
							      infix
							      close
							      max)))
		    (max close-init infix-end)))

		 ((zerop len-open)
		  (let* ((infix-end (ipe-updt--next-infix-end beg
							      infix
							      close
							      max))
			 (next-close (ipe-updt--next-close infix-end
							   close
							   escapes
							   max)))
		    next-close))

		 (t
		  (ipe-updt--next-matched-close (+ beg len-open)
						open
						infix
						close
						escapes
						max))))

	  ;; If we have found just the beginning of a PAIR, keep
	  ;; searching, (We may have OPEN = CLOSE, or, we may have an
	  ;; unclosed OPEN and need to keep searching forwards for a
	  ;; closed OPEN.)
	  (setq start (if (and beg
			       (not end)
			       (< beg (or max (point-max)))
			       (or (zerop len-infix)
				   (not (>= (ipe--eol beg)
					    (or max (point-max))))))
			  (1+ beg)
			nil))))

      (when (and beg end)
	(cons beg end)))))

;; -------------------------------------------------------------------
;;;; 'Find' Functions.
;; -------------------------------------------------------------------

(defun ipe-updt--find-indents (pair beg end)
  "Return the indentation around an `ipe' PAIR.

This will search a region (BEG END) for the whitespace before and
after the OPEN and CLOSE strings of an `ipe' PAIR.

- PAIR is used to look up the OPEN and CLOSE strings.
- BEG and END are expected to represent the positions of the start of
  the OPEN and the end of the CLOSE strings within the current buffer.

Returns a 8-TUPLE containing the indentation around the OPEN and
CLOSE strings.

  (OPEN-PRE-INDENT  OPEN-POST-INDENT OPEN-TOGGLE
   INFIX-PRE-INDENT INFIX-POST-INDENT)
   CLOSE-PRE-INDENT CLOSE-POST-INDENT CLOSE-TOGGLE)

Where:

- OPEN-PRE-INDENT - Is the whitespace before the OPEN string.
- OPEN-POST-INDENT - Is the whitespace after the OPEN string.
- OPEN-TOGGLE - Indicates that the OPEN string has a trailing newline.
- INFIX-PRE-INDENT - Is the minimum whitespace before the INFIX
  string.
- INFIX-POST-INDENT - Is the minimum whitespace after the INFIX
  string.
- CLOSE-PRE-INDENT - Is the whitespace before the CLOSE string.
- CLOSE-POST-INDENT - Is the whitespace after the CLOSE string.
- CLOSE-TOGGLE - Indicates that the CLOSE string has a trailing
  newline."

  (let* ((open      (ipe--pair-open-string  pair))
	 (infix     (ipe--pair-infix-string pair))
	 (close     (ipe--pair-close-string pair))
	 (has-open  (and open  (> (length open)  0)))
	 (has-infix (and infix (> (length infix) 0)))
	 (has-close (and close (> (length close) 0)))
	 (bol-beg   (ipe--bol beg))
	 (bol-end   (ipe--bol end))
	 (open-pre-indent   0)
	 (open-post-indent  0)
	 (open-toggle)
	 (infix-pre-indent)
	 (infix-post-indent)
	 (close-pre-indent  0)
	 (close-post-indent 0)
	 (close-toggle))

    (save-excursion

      (when (eq (ipe--pair-movement-initial pair) 'line)

	;; Whitespace from bol -> beg.
	(goto-char bol-beg)
	(when (re-search-forward (concat "^\\( *\\)" (regexp-quote open))
				 (- end (length close))
				 t)
	  (setq open-pre-indent (length (match-string 1))))

	;; Check for possible open-toggle.
	(goto-char bol-beg)
	(when (re-search-forward (concat "^ *" (regexp-quote open) " *$")
				 (- end (length close))
				 t)
	  (setq open-toggle t))

	;; Whitespace from beg -> non-whitespace.
	(when (= open-pre-indent (- beg bol-beg))
	  (goto-char beg)
	  (when (re-search-forward (concat (regexp-quote open) "\\( *\\)")
				   (- end (length close))
				   t)
	    (setq open-post-indent (length (match-string 1)))))

	(unless has-open
	  (setq open-pre-indent (- open-pre-indent open-post-indent)))

	;; Find whitespace around INFIX'es.
	(goto-char beg)
	(beginning-of-line (if has-open 2 1))
	(let ((infix-match (concat "^\\( *\\)"
				   (regexp-quote (or infix ""))
				   "\\( *\\)")))

	  (when (and (< (point) bol-end)
		     (re-search-forward infix-match (ipe--eol (point)) t))
	    (setq infix-pre-indent  (length (match-string 1))
		  infix-post-indent (length (match-string 2)))
	    (forward-line 1)

	    (while (and (< (point) bol-end)
			(re-search-forward infix-match (ipe--eol (point)) t))

	      (setq infix-pre-indent  (min (length (match-string 1))
					   infix-pre-indent)
		    infix-post-indent (min (length (match-string 2))
					   infix-post-indent))
	      (forward-line 1))))

	(if (and infix-pre-indent infix-post-indent)
	    (progn
	      ;; When no infix string, adjust infix indent based on open
	      ;; indent.
	      (unless has-infix
		(setq infix-post-indent (max 0 (- infix-pre-indent
						  open-pre-indent))
		      infix-pre-indent  (min infix-pre-indent
					     open-pre-indent)))

	      (when (and (not open-toggle) has-open)
		(setq infix-post-indent (max infix-post-indent
					     open-post-indent))))
	  (setq infix-pre-indent 0
		infix-post-indent 0))

	;; Whitespace from . -> end.
	(goto-char bol-end)
	(when (looking-at (concat "^\\( *\\)" (regexp-quote close) " *$"))
	  (setq close-pre-indent
		(if has-close
		    (min (length (match-string 1))
			 (- end bol-end))
		  (if (< (- end bol-end) (length (match-string 1)))
		      (- end bol-end)
		    0))))

	(when (looking-at (concat "^ *" (regexp-quote close) "$"))
	  (setq close-toggle t))

	;; Whitespace from end -> non-whitespace.
	(goto-char end)
	(when (looking-at (concat "\\( *\\))"))
	  (setq close-post-indent (length (match-string 1))))

	(unless has-close
	  (setq close-pre-indent (- close-pre-indent close-post-indent))))

      ;; Return the indents
      (list open-pre-indent
	    open-post-indent
	    open-toggle
	    infix-pre-indent
	    infix-post-indent
	    close-pre-indent
	    close-post-indent
	    close-toggle))))

(defun ipe-updt--find-pair (pair pos &optional min max)
  "Search for the nearest `ipe' PAIR.

If `ipe-update-forward-first-p' is non-nil, searches forward from POS
for the nearest OPEN and CLOSE of the `ipe' PAIR.

If `ipe-update-forward-first-p' is nil, searches backward from POS for
the nearest OPEN and CLOSE of the `ipe' PAIR.

It takes into account:

* That the OPEN / CLOSE may be empty.
* That the OPEN / CLOSE may be either the same as, or prefixes of, the
  INFIX string.
* That the characters within the OPEN / CLOSE strings may have been
  used in an ESCAPE, (and ignore any matches to the ESCAPE.)

- POS is the position from which to begin the search.
- MIN is the minimum backward boundary to which to search for the
  start of the PAIR.
- MAX is the maximum forward boundary to which to search for the end
  of the PAIR.

If a PAIR is found, return a CONS cell whose `car' contains position
of the start of the OPEN string, and whose `cdr' contains the position
of the end of the corresponding CLOSE string.

If no PAIR is found, return nil."

  (let* ((open    (ipe--pair-open-string  pair))
	 (infix   (ipe--pair-infix-string pair))
	 (escapes (ipe--pair-escapes      pair))
	 (close   (ipe--pair-close-string pair))
	 (search  (if (zerop (length open)) pos (1+ pos)))
	 (start
	  (if ipe-update-forward-first-p
	      (or (ipe-updt--previous-close pos close escapes
					    (or min (point-min)))
		  min
		  (point-min))
	    (or (ipe-updt--next-open search open infix close escapes
				     (or max (point-max)))
		max
		search))))

    (if ipe-update-forward-first-p
	(or (ipe-updt--next-pair start open infix close escapes max 1)
	    (ipe-updt--previous-pair pos open infix
				     close escapes min max 1))
      (or (ipe-updt--previous-pair start open infix
				   close escapes min max 1)
	  (ipe-updt--next-pair pos open infix close escapes max 1)))))

;; -------------------------------------------------------------------
;;;; Deletion.
;; -------------------------------------------------------------------

(defun ipe-updt--delete-infixes (pair beg end indents)
  "Delete the Insert Pair Edit (ipe) `INFIX'es between BEG and END.

Delete all of the matches for `ipe--pair-infix-string' for the ipe
PAIR with the given MNEMONIC in the region between BEG and END."

  (if (eq (ipe--pair-movement-initial pair) 'line)
      (let* ((infix             (ipe--pair-infix-string pair))
	     (has-infix         (and infix (> (length infix) 0)))
	     (infix-post-indent (nth 4 indents))
	     (match             (if has-infix
				    (concat "^ *"
					    (regexp-quote infix)
					    (make-string infix-post-indent ? ))
				  "^ +"))
	     (change            0)
	     (deleted           0))

	(save-excursion
	  ;; Delete the INFIX'es + whitespace.
	  (goto-char beg)
	  (while (and (<= (point) (ipe--bol (+ end change)))
		      (re-search-forward match (+ end change) t))
	    (setq deleted (ipe--pos-delete (match-beginning 0)
					   (match-end       0))
		  change  (- change deleted))
	    (forward-line 1)))

	change)
    0))

(defun ipe-updt--delete-escapes (pair beg end)
  "Delete the `ipe' `ESCAPE's between BEG and END.

Use the :escapes property of PAIR to search for ESCAPE's and replace
matches to the ESCAPE'd text with un-ESCAPE'd text.

The :escapes property of a PAIR is expected to be a list with entries
of the form:

   (MATCH . REPLACE)

For each element within this list, this function will search the
region between BEG and END for matches to REPLACE, delete them, and
replace the deleted text with MATCH."

  (let ((escapes (ipe--pair-escapes pair))
	(change  0)
	(replace 0))
    (when (and escapes ipe--escapes-show-p)
      (save-excursion
	(dolist (escape escapes)
	  (let ((re-escape (ipe--indent-escape (cadr escape))))
	    (goto-char beg)
	    (while (re-search-forward re-escape (+ end change) t)
	      (setq replace (ipe--pos-replace (match-beginning 0)
					      (match-end 0)
					      (car escape))
		    change  (+ change replace)))))))
    change))

(defun ipe-updt--delete-pair (pair n pos-open pos-close
				   &optional set-pair-p)
  "Delete the `ipe' PAIR from the current buffer.

This function will:

* Delete the `ipe' PAIR OPEN string located at POS-OPEN.
* Delete the `ipe' PAIR CLOSE string located at POS-CLOSE.
* Search for any `ESCAPE's associated with the PAIR between POS-OPEN
  and POS-CLOSE and replace them with their un-`ESCAPE'd REPLACE
  string.
* Search for any `INFIX'es associated with the PAIR between POS-OPEN
  and POS-CLOSE and delete them.

If SET-PAIR-P is non-nil, the positions of the deleted PAIR will be
recorded in `ipe--pair-pos-list' as the `N'th pair.

Return the number of characters deleted from the buffer."

  (let* ((i           (if n n (ipe--pos-count)))
	 (open        (ipe--pair-open-string  pair))
	 (close       (ipe--pair-close-string pair))
	 (infix       (ipe--pair-infix-string pair))
	 (len-open    (length open))
	 (len-infix   (length infix))
	 (len-close   (length close))
	 (indents     (ipe-updt--find-indents pair pos-open pos-close))
	 (pos         (point))
	 (beg-open)
	 (end-open)
	 (beg-close)
	 (deleted     0))

    ;; Adjust the beg-open for indents.
    (setq beg-open (- pos-open (nth 0 indents)))

    ;; Adjust the end-open for indents.
    (setq end-open
	  (+ beg-open (nth 0 indents) len-open (nth 1 indents)
	     (if (nth 2 indents)
		 (+ 1 (nth 3 indents) len-infix (nth 4 indents))
	       0)))

    ;; Adjust the beg-close for indents.
    (setq beg-close
	  (if (nth 7 indents)
	      (- pos-close len-close 1 (nth 5 indents))
	    (- pos-close len-close (nth 5 indents))))

    ;; Check if we need to set up an IPE PAIR.
    (when set-pair-p
      (ipe--pos-open-set  i beg-open)
      (ipe--pos-close-set i beg-close)
      (ipe--pos-point     i
			  (if (or (region-active-p)
				  (and (<= beg-open pos)
				       (<  pos end-open)))
			      beg-open
			    (if (and (<= beg-close pos)
				     (<  pos pos-close))
				beg-close
			      (if (>= pos (point-max))
				  'eob
				pos))))

      (ipe--pos-property-set i
			     :initial-n    i
			     :point-open
			     (ipe--pos-location pos
						beg-open
						end-open)
			     :point-close
			     (ipe--pos-location pos
						beg-close
						pos-close)

			     ;; TODO: Move indent-1 / indent-2 to "ipe-line".
			     :indent-1     (nth 0 indents)
			     :open-toggle  (nth 2 indents)
			     :indent-2     (if (or (nth 2 indents)
						   (= len-open 0))
					       (nth 4 indents)
					     (nth 1 indents))
			     :close-toggle (nth 7 indents)))

    ;; Delete OPEN + indents.
    (unless (zerop len-open)
      (setq deleted    (ipe--pos-delete beg-open end-open)
	    beg-close  (- beg-close deleted)
	    pos-close  (- pos-close deleted)))

    ;; Delete CLOSE + indents.
    (unless (zerop len-close)
      (setq deleted (+ deleted
		       (ipe--pos-delete beg-close pos-close))))

    ;; Delete ESCAPE's.
    (when (ipe--pair-escapes pair)
      (let ((change (ipe-updt--delete-escapes pair
					      beg-open
					      beg-close)))
	(setq beg-close (+ beg-close change)
	      deleted   (+ deleted   change))))

    ;; Delete INFIX'es + indents.
    (when (ipe--pair-infix-string pair)
      (let ((change (ipe-updt--delete-infixes pair beg-open
					      beg-close indents)))
	(setq deleted (+ deleted change))))

    (ipe--pos-property-set i
			   :open  nil
			   :infix nil
			   :close nil)

    deleted))

(defun ipe-updt--delete-at-pos (pair n pos-open pos-close)
  "Delete PAIR at POS-OPEN and POS-CLOSE and replace with `ipe' PAIR.

Delete the OPEN and CLOSE strings for the current Insert Pair Edit
PAIR from the buffer and replace them with an `ipe' PAIR.

- POS-OPEN is assumed to be the start of the OPEN string to be
  replaced.
- POS-CLOSE is assumed to be the end of the CLOSE string to be
  replaced.

Any :infix or :escapes properties associated with the current `ipe'
PAIR will also be deleted from the buffer.

The newly created `ipe' PAIR is recorded within `ipe--pair-pos-list'
as the `N'th `ipe' PAIR."

  (when (and pos-open pos-close)
    (ipe-updt--delete-pair pair n pos-open pos-close t)))

(defun ipe-updt--delete-find-pairs (mnemonic &optional set-pair-p)
  "Delete `ipe' PAIR(s) for a given MNEMONIC from the current buffer.

If the REGION is NOT active, this function will search for the nearest
matching OPEN and CLOSE strings for the `ipe' PAIR identified by
MNEMONIC.  (The direction of search is determined by the value of the
`ipe-update-forward-first-p' variable.)  These strings (and any
associated `INFIX'es / `ESCAPE's) will then be deleted from the
buffer.

If the REGION is active, this function will search for all of the
`ipe' the OPEN and CLOSE strings within the REGION for the `ipe' PAIR
identified by MNEMONIC, and delete them all.

If SET-PAIR-P is non-nil, the positions of the deleted PAIR(s) will be
recorded in `ipe--pair-pos-list'."

  (let* ((pair      (ipe--pair mnemonic))
	 (open      (ipe--pair-open-string  pair))
	 (infix     (ipe--pair-infix-string pair))
	 (close     (ipe--pair-close-string pair))
	 (len-open  (length open))
	 (len-infix (length infix))
	 (len-close (length close))
	 (beg)
	 (end)
	 (pos-open)
	 (pos-close))

    ;; Check if we are searching over a region.
    (if (region-active-p)
	(when (/= (+ len-open len-infix len-close) 0)
	  (setq beg (region-beginning)
		end (region-end))

	  ;; Start searching at the beginning of the region.
	  (setq pos-open  beg
		pos-close beg)

	  (while (and pos-close
		      (< pos-close end))

	    (let ((pos-pair (ipe-updt--find-pair pair beg beg end)))
	      (setq pos-open  (car pos-pair)
		    pos-close (cdr pos-pair))

	      (when (and pos-open pos-close
			 (<= pos-close end))
		(save-excursion
		  (goto-char pos-open)

		  (let ((deleted (ipe-updt--delete-pair pair nil
							pos-open pos-close
							set-pair-p)))
		    (setq pos-close (- pos-close deleted)
			  end       (- end       deleted)))))))

	  (setq mark-active nil))

      ;; Not a region, use the 'nearest' PAIR.
      (let ((pos-pair (ipe-updt--find-pair pair (point))))
	(setq pos-open  (car pos-pair)
	      pos-close (cdr pos-pair))

	;; Check if we have found a 'nearest' PAIR.
	(when (and pos-open pos-close)
	  (ipe-updt--delete-pair pair nil
				 pos-open pos-close
				 set-pair-p))))

    (unless (and pos-open pos-close)
      (message (concat "Could not find PAIR with "
		       (ipe--mnemonic-describe mnemonic))))))

(provide 'ipe-updt)

;;; ipe-updt.el ends here
