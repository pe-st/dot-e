;; Pesche' Tools
;;
;;      Author: Peter Steiner <pesche@schlau.ch>
;;         $Id$
;;     $Change$
;;   $DateTime$
;;     $Author$
;;    $Created: 1999/06/02 $


;; Funktionen ------------------------------------------------------------------

(defun insert-timestamp ()
  "Insert the current time."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))
  )

(defun uncomment-region (beg end &optional arg)
  "Counterpart to comment-region."
  ;; there is still something wrong for arg > 1 or prefix args, but
  ;; for just uncommenting, it works
  (interactive "r/nP")
  (if (consp arg)
      (comment-region beg end (- arg))
    (comment-region beg end (- 1))
    )
  )

(defun modify-syntax-for-umlaut ()
  "Sets the german umlauts to 'word constituent' in current syntax table."
  (interactive)
  (modify-syntax-entry ?ä "w")
  (modify-syntax-entry ?ö "w")
  (modify-syntax-entry ?ü "w")
  (modify-syntax-entry ?Ä "w")
  (modify-syntax-entry ?Ö "w")
  (modify-syntax-entry ?Ü "w")
  )

(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))


;; diese Funktion habe ich aus dem .emacs von Jack Repenning <jackr@informix.com>
(defun toggle-line-wrap ()
  "Toggles the line-wrap function.
Covers (and equates) both horizontal and vertical splits."
  (interactive)
  (setq truncate-lines (setq truncate-partial-width-windows (not
                                                             truncate-lines)))
  (recenter (- (count-lines (window-start) (point))
               (if (bolp) 0 1)))
  )

;; diese Funktion habe ich aus dem .emacs von Anders Lindgren <andersl@csd.uu.se>
(defun unbury-buffer (&optional buf)
  "Select buffer BUF, or the last one in the buffer list.
This function is the opposite of `bury-buffer'."
  (interactive)
  (or buf (setq buf (car (reverse (buffer-list)))))
  (switch-to-buffer buf))


(require 'thingatpt)
(defun search-quick ()
  "quick search."
  (interactive)
  (let ((string (thing-at-point 'word)))
    (if (stringp string)
        (nonincremental-search-forward string))))

(defun tags-quick ()
  "quick tags search."
  (interactive)
  (let ((string (thing-at-point 'word)))
    (if (stringp string)
        (tags-search string))))

;; from EmacsWiki, page "Scrolling"
(defun sfp-page-down ()
  (interactive)
  (setq this-command 'next-line)
  (next-line
   (- (window-text-height)
      next-screen-context-lines)))

(defun sfp-page-up ()
  (interactive)
  (setq this-command 'previous-line)
  (previous-line
   (- (window-text-height)
      next-screen-context-lines)))

;; Modul abschliessen ----------------------------------------------------------
(provide 'pesche-tools)

;; eof


