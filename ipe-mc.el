;; -------------------------------------------------------------------
;;;; Multiple-cursors compatibility
;; -------------------------------------------------------------------

;; Ensure the ipe-edit commands are only run once by multiple-cursors.
(eval-after-load "multiple-cursors"
  '(progn
     (mc/load-lists)
     (let ((update))
       (mapc
	(lambda (command)
	  (when (not (memq command mc/cmds-to-run-once))
	    (add-to-list 'mc/cmds-to-run-once command)
	    (setq update t)))
	'(ipe-defn--change-mode-pair-mnemonic
	  ipe-defn--change-pair-mnemonic
	  ipe-defn--delete-mode-pair
	  ipe-defn--delete-pair
	  ipe-defn--edit-pair
	  ipe-defn--edit-current-pair
	  ipe-defn--edit-mode-pair
	  ipe-edit--abort
	  ipe-edit--add-next-contents
	  ipe-edit--add-next-pair
	  ipe-edit--add-previous-contents
	  ipe-edit--add-previous-pair
	  ipe-edit--change-pair
	  ipe-edit--close-backward
	  ipe-edit--close-beg
	  ipe-edit--close-down
	  ipe-edit--close-end
	  ipe-edit--close-forward
	  ipe-edit--close-up
	  ipe-edit--contents-capitalize
	  ipe-edit--contents-copy
	  ipe-edit--contents-downcase
	  ipe-edit--contents-kill
	  ipe-edit--contents-replace
	  ipe-edit--contents-trim
	  ipe-edit--contents-upcase
	  ipe-edit--contents-yank
	  ipe-edit--delete-first-pair
	  ipe-edit--delete-last-pair
	  ipe-edit--ia-copy-text
	  ipe-edit--ia-goto-close
	  ipe-edit--ia-goto-open
	  ipe-edit--ia-kill-text
	  ipe-edit--ia-resume
	  ipe-edit--insert-pair
	  ipe-edit--movement-by-char
	  ipe-edit--movement-by-line
	  ipe-edit--movement-by-list
	  ipe-edit--movement-by-word
	  ipe-edit--open-backward
	  ipe-edit--open-beg
	  ipe-edit--open-down
	  ipe-edit--open-end
	  ipe-edit--open-forward
	  ipe-edit--open-up
	  ipe-edit--options
	  ipe-edit--toggle-escapes
	  ipe-edit--update-next-close
	  ipe-edit--update-next-contents
	  ipe-edit--update-next-open
	  ipe-edit--update-next-pair
	  ipe-edit--update-previous-close
	  ipe-edit--update-previous-contents
	  ipe-edit--update-previous-open
	  ipe-edit--update-previous-pair
	  ipe-help
	  ipe-help--info
	  ipe-help--mode-help
	  ipe-test-run))
       (when update
	 (mc/save-lists)))))

