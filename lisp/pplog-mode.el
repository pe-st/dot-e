;;; pplog-mode.el --- major mode for personal process logging
;;                                   ^        ^       ^^^

;;File:     evalpplog - evaluate pplog-file(s)
;;Author:   Markus Middendorf <markusm@writeme.com>
;;          Lutz Prechelt <prechelt@computer.org>
;;created:  May through August 1998
;;changed:  1998-09-02   Markus Middendorf
;;RCS:      $Id: pplog-mode.el,v 1.3 1998/09/04 07:09:56 prechelt Exp $

;; This programm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;; WARNING:
;; This is a beta version!
;; Suitable for XEmacs 19.14, GNU Emacs 19.34 and newer versions.
;; Be very careful when using with older versions!
;; This program causes some older emacs version to
;; hang with loss of unsaved data!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; written as a project work at the Institute for Program Organisation and
;; Data Structures, Department of Computer Science, University of Karlsruhe,
;; Germany
;;
;; derived from
;; ; Timestamps (for PSP etc.)
;; ;  [sth] 20-dec-95                initial version
;; ;  Oliver Gramberg 1996-01-16     automatic logbuffer creation
;; ;  Lutz Prechelt 1996-01-18       logbuffer deletion, clean customization
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:

(defconst pplog-mode-version "0.9.2 ps") ;; revision
(defconst pplog-mode-last-change "2005-11-22") ;; date

;;; Code:

;;==================== customize here ====================

;;--------------------     keys       --------------------
;; NOTE: 'pplog-hot-key' and 'pplog-return-key' may be equal.
;;       'pplog-return-key', 'pplog-next-or-new-key' and
;;       'pplog-newlog-key' must be different from each other.

(defconst pplog-hot-key [f8]
  "*Change to the 'first' log buffer and insert a timestamp.
If no pplog buffer is open, prompts for a filename.")


(defvar pplog-mode-map ()
  "Keymap used in PPLog mode.")
(if pplog-mode-map
  ()
  (setq pplog-mode-map (make-sparse-keymap))
  (define-key pplog-mode-map [C-f8] 'pplog-open-log-buffer-now)       ; do what you want
;  This key forces emacs to immediately prompt for the name of a new or existing file. The current buffer must be a pplog buffer. If there are n unvisited pplog buffers, pressing this key is equivalent to pressing (n+1) times the 'pplog-next-or-new-key'.

;  (define-key pplog-mode-map [f7] 'pplog-nextlog)                   ; do what you want
;  Pressing this key will switch from one pplog buffer to the next pplog buffer, using the emacs buffer hierarchy. You may use this key to pass through all pplog buffers. If there is no unvisited buffer left, you will be prompted for the name of a new or existing file.

  (define-key pplog-mode-map [f8] 'pplog-return-key-event-handling) ; do what you want
;  This is the key to return to the buffer where you came from.

  (define-key pplog-mode-map [f9] 'pplog-insert-timestamp)

  (define-key pplog-mode-map "\C-cn" 'pplog-last-timestamp-is-now)  ; do what you want
  (define-key pplog-mode-map "\C-ct" 'pplog-last-timestamp-correct) ; do what you want
;  (define-key pplog-mode-map "\r"   'electric-pplog-terminate-line) ; don't change!
  (define-key pplog-mode-map "\C-l" 'pplog-special-recenter)        ; be careful!
)


;;;-------------------appearence: colors ------------------
;; The begin of unterminated phases, errors and interruptions will be highlighted.
;; Choose your favorite colors! Make sure that the colors exist in your system!
;; If you are running X, have a look at <XRoot>/lib/X11/rgb.txt
(defconst phase-fore-color "Blue")       ;"black" )
(defconst phase-back-color "WhiteSmoke") ;"blue"  )
(defconst itr-fore-color   "Red")        ;"black" )
(defconst itr-back-color   "WhiteSmoke") ;"seagreen")
(defconst error-fore-color "Yellow")     ;"black" )
(defconst error-back-color "LightBlue")  ;"red"   )

;;;---------------------- behaviour -----------------------

;; OBSOLETE     One of the main purposes of this major mode is to help you when protocolling
;; OBSOLETE     'events' like phases, errors and interruptions. All phase names you use must
;; OBSOLETE     be definded in the header of the pplog file. This header will be inserted
;; OBSOLETE     automatically when creating a new logfile (see below). In every case,
;; OBSOLETE     all event endings will be checked for matching event beginning. The
;; OBSOLETE     following two variable tell Emacs what to do when an error is encountered.

;; Obviously, the 'events' (i.e. phases, errors, interruptions) depend in some
;; way from each other. For example, it should be clear, that you cannot
;; proceed to the next phase while you are working on an error, and you can't
;; work or even proceed to the next phase, while an interruption is active.
;; But should phases within other phases be allowed?

(defconst pplog-phase-in-phase t
  "Set 'pplog-phase-in-phase' to non-nil, if you want to allow phases within other phases. Otherwise, set it to nil.")

(defconst pplog-error-in-error t
  "Set 'pplog-error-in-error' to non-nil, if you want to allow starting new errors while working at old ones. Otherwise, set it to nil.")

(defconst pplog-itr-in-itr t
  "Set 'pplog-itr-in-itr' to non-nil, if you want to allow new interruptions while other interruptions are active. Otherwise, set it to nil.")

;; ------------------------- menu entry -------------------------

(defconst pplog-in-global-menu t
  "Set 'pplog-in-global-menu' to non-nil, to have an \"Change to PPLog buffer\"-entry in the \"Tools\"-Menu of all buffers.")

;;-------------------- logfile header --------------------
(defun pplog-insert-log-buffer-header ()
  "*Customized header to be inserted at top of log buffer upon creation."
;; 1. lines, that are just for the user's information and will not be
;;    interpretated neither by the evaluation script nor by pplog-mode.
;;    These lines must start with '#'.
;;    DON'T remove this '#'-character, and don't use '!' as second character
  (insert "#----- PSP time and defect log-file -----\n")
  (insert "#  Project/Program:   \n")
  (insert "#  Author:            \n")
  (insert "#  Date of creation:  ")
  (insert (pplog-time-string (current-time)))
  (insert "\n\n")
;; 2. lines, that define the names of phases and the abbreviations for
;;    'begin' and 'end', for 'defect' and 'interruptions' (ONE
;;    character each) and for the phases (any number of characters)
;;    These lines must start with '#!'
  (insert "#! begin=b; end=e; interrupt=i; defect=e  \n")
  (insert "#! phase=ds\n")
  (insert "#! phase=\"design review\"=dr\n")
  (insert "#! phase=\"code\"=cd\n")
  (insert "#! phase=cr; phase=cp; phase=te; phase=pm\n")
  (insert "#! phase=\"test\"=te\n")
  (insert "\n#--------------------------------------------------\n")
)

;; ========================= READ THIS =========================
;; You are using GNU Emacs 19.34.1 or XEmacs 19.14 or newer versions?
;; --> No customization below this point, please
;; You are using older versions?
;; --> If the menus cause any problems,
;;     you should remove the 'menu entries' paragraph!

;; ------------------------- general preparations -------------------------

; -------------------- menu entries --------------------

(if (string-match "XEmacs" emacs-version)
  (progn
    (defconst pplog-menu
        '("PPLog"
; comment out the following line for old emacs versions:
          :included (equal major-mode 'pplog-mode)
          ["Switch to next pplog buffer"        pplog-nextlog t]
          ["Create new pplog buffer"            pplog-open-log-buffer-now t]
          ["Return to original buffer"          pplog-return-key-event-handling t]
          "---"
          ["Set last timestamp to now"          pplog-last-timestamp-is-now t]
          ["Subtract 1 min from last timestamp" pplog-last-timestamp-correct t]
        ))
    (if pplog-in-global-menu
      (progn
        (add-menu-item '("Tools") "Change to PPLog buffer" 'pplog-hot-key-event-handling '(not (equal major-mode 'pplog-mode)))
        (add-menu-item '("Tools") "--------" () t "Change to PPLog buffer")
      )
    )
  )
;; Menu for GNU Emacs
  (define-key pplog-mode-map
    [menu-bar pplog]
    (cons "PPLog" (make-sparse-keymap)))

  (define-key pplog-mode-map
    [menu-bar pplog nextbuffer]
    '("Switch to next pplog buffer" . pplog-nextlog))

  (define-key pplog-mode-map
    [menu-bar pplog newbuffer]
    '("Create new pplog buffer" . pplog-open-log-buffer-now))

  (define-key pplog-mode-map
    [menu-bar pplog return]
    '("Return to original buffer" . pplog-return-key-event-handling))

  (define-key pplog-mode-map
    [menu-bar pplog timestamp-now]
    '("Set last timestamp to now" . pplog-last-timestamp-is-now))

  (define-key pplog-mode-map
    [menu-bar pplog timestamp-cor]
    '("Subtract 1 min from last timestamp" . pplog-last-timestamp-correct))
)

;; =============== No customization below this point, please ===============

;; ------------------------- variable declarations -------------------------

(defvar pplog-last-pos)
(defvar pplog-next-timestamp)
(defvar pplog-prop-list)
(defvar pplog-min-sub)
(defvar pplog-sec-sub)
(defvar pplog-months-abbr)
(defvar pplog-month-str)
(defvar pplog-match-string)

(defvar counter-i)
(defvar counter-j)

(defvar pplog-year)
(defvar pplog-month)
(defvar pplog-day)
(defvar pplog-hour)
(defvar pplog-min)
(defvar pplog-sec)

(defvar pplog-time-as-list)
(defvar pplog-high-time)
(defvar pplog-low-time)

(defvar pplog-target)
(defvar pplog-phase-string)
(defvar pplog-phase-abbr)
(defvar pplog-phase-abbr-list)

(defvar pplog-buffer)
(defvar pplog-all-buffers)
(defvar pplog-pplog-buffers)
(defvar pplog-original-buffer)
(defvar pplog-unvisited-pplog-buffers)
(defvar pplog-next-buffer)

(defvar pplog-phase-re)
(defvar pplog-itr-char)
(defvar pplog-error-char)
(defvar pplog-begin-char)
(defvar pplog-end-char)

(defvar pplog-keyword)
(defvar pplog-kwb)
(defvar pplog-kwe)

(defvar list-element)
(defvar pplog-oldpoint)

(defvar pplog-default-major-mode)
(defvar pplog-last-time-as-list)

(defvar pplog-line-number)
(setq pplog-line-number 0)

(defvar pplog-parse-success)
(setq pplog-parse-success nil)

; creating faces:
(make-face 'phase-face)
(copy-face 'default 'phase-face)
(make-face 'itr-face)
(copy-face 'default 'itr-face)
(make-face 'error-face)
(copy-face 'default 'error-face)
(set-face-foreground 'phase-face phase-fore-color)
(set-face-background 'phase-face phase-back-color)
(set-face-foreground 'itr-face itr-fore-color)
(set-face-background 'itr-face itr-back-color)
(set-face-foreground 'error-face error-fore-color)
(set-face-background 'error-face error-back-color)

;; global key-binding:
(global-set-key pplog-hot-key 'pplog-hot-key-event-handling)
;; make pplog-files recognizable
(setq auto-mode-alist (cons '("\\.ppl\\'" . pplog-mode) auto-mode-alist))

;; CHECK HERE
(defconst pplog-new-timestamp-re "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-1][0-9]\\)-\\([0-3][0-9]\\) \\([0-2][0-9]\\):\\([0-5][0-9]\\):\\([0-5][0-9]\\)")
(defconst pplog-special-timestamp-re "^\\(\\([0-9][0-9][0-9][0-9]\\)-\\([0-1][0-9]\\)-\\([0-3][0-9]\\)\\) \\([0-2][0-9]\\):\\([0-5][0-9]\\):\\([0-5][0-9]\\)")

;; -------------------------- 'helper' functions --------------------------

(defun current-line ()
  "Return the vertical position of point within the current buffer."
  (+ (count-lines (point-min) (point))
     (if (= (current-column) 0) 1 0)
  )
)

(defun pplog-buffer-check ()
  "Check buffer for syntax errors.
(Re)read the header, check consistence of entries and test, whether the timestamps are in increasing order."
  (save-excursion
;; CHECK HERE
    (if (string-match "XEmacs" emacs-version)
      (set-menubar-dirty-flag)
    )
;;    (beep)
    (pplog-parse-header)
    (pplog-parse-entries)
;;    (beep)
    (pplog-check-timestamp-order)
;;    (beep)
  )
)

(defun pplog-special-recenter (line-number-arg)
  "Check buffer syntax (if there is no prefix argument) before 'recenter'.
Execute a \"normal\" 'recenter' (with or without prefix arguments) afterwards."
  (interactive "P")

  ;; the faces set by pplog-buffer-check are overwritten by font-lock-mode...
  (font-lock-mode 0)
  (if (not line-number-arg)
    (pplog-buffer-check)
  )
  (recenter line-number-arg)
)

;; ---------- functions, that handle keystrike events ---------------------

;; CHECK HERE
(defun pplog-hot-key-event-handling (arg1)
  "This function is called when 'pplog-hot-key' is pressed. If the current buffer is NOT a pplog buffer, a request for a log buffer is created. The results of this request will depend on which buffers exist (see 'pplog-buffer-request')."
  (interactive "P")
  (if (not (equal major-mode 'pplog-mode))  ; if the current buffer is NO log buffer...
     (pplog-buffer-request arg1)            ;   -> create request for a log buffer
  )
)


(defun pplog-return-key-event-handling ()
  "This function is called when 'pplog-return-key' is pressed. It returns from the pplog buffer to the original buffer."
  (interactive)
  (if (equal major-mode 'pplog-mode)            ; are we in  log buffer?
      (progn
        (bury-buffer)                           ; move pplog buffer to end of list
        (switch-to-buffer pplog-original-buffer); yes -> back to original buffer
        )
    (error "Impossible event!?!")               ; no -> IMPOSSIBLE ???
  )
)

;; more functions that handle keystrike events, see following sections

;; ----------------- creating/switching buffers -----------------

;; ----- functions needed when switching to log buffer -----


(defun pplog-buffer-list ()
  "Create a list of all buffers with pplog mode."
  (save-excursion
    (setq pplog-all-buffers (buffer-list))
    (setq pplog-pplog-buffers (list))
    (while (not (equal pplog-all-buffers ()))
      (setq list-element (car pplog-all-buffers))
      (set-buffer list-element)
      (if (equal major-mode 'pplog-mode)
	(progn
;;; PROBLEM:
;;; The order of the buffers is changed!
          (setq pplog-pplog-buffers (cons list-element pplog-pplog-buffers))
        )
        ()
      )
      (setq pplog-all-buffers (cdr pplog-all-buffers))
    )
    (defvar pplog-unvisited-pplog-buffers)
;;; Correct the order of the buffers:
    (setq pplog-unvisited-pplog-buffers (reverse pplog-pplog-buffers))
  )
)


(defun pplog-open-log-buffer-now ()
  "Alias for 'pplog-open-log-buffer' without any arguments. For further information see there!"
  (interactive)
  (pplog-open-log-buffer nil)
)

;; CHECK HERE
(defun pplog-buffer-request (arg2)
  "Handle a request for a pplog buffer.
If there are still open buffers with pplog mode, change to the first of these buffer and insert a timestamp. "
  (interactive)
  (setq pplog-original-buffer (current-buffer)) ; remember where we came from
; get a list of all buffers with pplog-mode
  (pplog-buffer-list)
; get first buffer of this list
  (setq pplog-next-buffer (car pplog-unvisited-pplog-buffers))
; maybe this list is empty
  (if (equal pplog-next-buffer nil)
; create new buffer or open existing one
    (pplog-open-log-buffer arg2)
; otherwise: change to existing buffer and insert timestamp
    (progn
      (switch-to-buffer pplog-next-buffer)
      (if (equal arg2 nil)
; insert timestamp if no prefix-argument is given
	(pplog-insert-timestamp)
	()
      )
      (use-local-map pplog-mode-map)
    )
  )
  (if (string-match "XEmacs" emacs-version)
    (set-menubar-dirty-flag)
  )
)

(defun pplog-nextlog ()
  "Change from one pplog buffer to the next pplog buffer.
If there is no pplog-buffer left, the user is prompted for the name of an new or an existing pplog file. All entries in the current buffer made since the last timestamp-insertion will be removed!"
  (interactive)
;; remove all entries in current buffer since last timestamp-insertion
  (delete-region pplog-last-pos (point-max))
  (setq pplog-unvisited-pplog-buffers (cdr pplog-unvisited-pplog-buffers))
; get first buffer of this list
  (setq pplog-next-buffer (car pplog-unvisited-pplog-buffers))
; maybe this list is empty
  (if (equal pplog-next-buffer nil)
; create new buffer or open existing one
    (pplog-open-log-buffer nil)
; otherwise: change to existing buffer and insert timestamp
    (progn
      (message "Changed to next buffer: %s" pplog-next-buffer)
      (switch-to-buffer pplog-next-buffer)
      (pplog-insert-timestamp)
      (use-local-map pplog-mode-map)
    )
  )
)

(defun pplog-open-log-buffer (arg3)
  "Open an existing pplog-buffer or create new pplog buffer and insert instructions for use"
  (interactive)
  (message "Please choose a pplog file name:")
;ps  (sit-for 2)
;; prompt user for filename
  (call-interactively 'find-file)
;;;; force new file to be in pplog mode (doesn't work with old Emacs -> deleted!)
;; CHECK HERE
;;  (setq pplog-default-major-mode default-major-mode)
;;  (setq default-major-mode 'pplog-mode)
;;  (set-buffer-major-mode (current-buffer))
;;  (setq default-major-mode pplog-default-major-mode)
  (set-auto-mode)
  (auto-save-mode 1)                           ; auto-save on
;; is this a new file?
  (if (eq 1 (point-max))
    (pplog-insert-log-buffer-header)
    (message "Loading existing pplog file")
  )
  (pplog-buffer-check)
  (if (equal arg3 nil)
    (pplog-insert-timestamp)
    ()
  )
)


;; ---------- timestamp functions ----------

(defun pplog-time-string (loc-time-list)
  (format-time-string "%Y-%m-%d %H:%M:%S" loc-time-list)
)

(defun pplog-insert-timestamp ()
  "Insert a timestamp at the beginning of a new line at the end of the current buffer.
Delete whitespaces, tabs and newlines at the end of the buffer before. You can use this function in any buffer, not just in pplog buffers."
  (interactive)
  (goto-char (point-max))
;; remove whitespaces...
;;; WARNING: this might not work with DOS-newlines!!!!
  (skip-chars-backward " \n\t")
  (delete-region (point) (point-max))
;; save current point-max
  (make-local-variable 'pplog-last-pos)
  (setq pplog-last-pos (point-max))
  (insert "\n")
;; CHECK HERE
  (insert (pplog-time-string (current-time)))             ; enter time stamp into log
  (insert " ")
  (setq pplog-line-number (current-line))
  (setq pplog-buffer (current-buffer))
  (add-hook 'post-command-hook 'pplog-watch-for-line-change)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pplog-last-timestamp-is-now ()
  "Search forward and backward for the next timestamp. If the forward search succeeds, set the preceding timestamp to the same value as the following one. Otherwise, set the preceding timestamp to the current date and time."
  (interactive)
  (setq pplog-oldpoint (point))
  (if (re-search-forward pplog-new-timestamp-re nil t)
    (progn
      (setq pplog-next-timestamp (match-string 0))
      (goto-char pplog-oldpoint)
      (if (re-search-backward pplog-new-timestamp-re nil t)
        (progn
          (setq pplog-prop-list (text-properties-at (match-beginning 0)))
          (replace-match pplog-next-timestamp)
;; CHECK HERE
          (set-text-properties (match-beginning 0) (match-end 0) pplog-prop-list)
        )
        (progn
          (beep)
          (message "Nothing to replace!")
        )
      )
    )
    (progn
      (if (re-search-backward pplog-new-timestamp-re nil t)
        (progn
          (setq pplog-prop-list (text-properties-at (match-beginning 0)))
;; CHECK HERE
          (replace-match (pplog-time-string (current-time)))
;; CHECK HERE
          (set-text-properties (match-beginning 0) (match-end 0) pplog-prop-list)
        )
        (progn
          (beep)
          (message "Nothing to replace!")
        )
      )
    )
  )
  (goto-char pplog-oldpoint)
)

;; CHECK HERE
(defun time-string-as-list (time-as-string)
  "Convert the time, given as string, into a list.
The string must have the format \"1998-10-26 13:14:15\". The list consist of three integers: (high low microsec). The values of 'high' and 'low' combine to give the number of seconds since 0:00 January 1, 1970."
  (save-match-data
    (string-match pplog-new-timestamp-re time-as-string)
    (setq pplog-year  (string-to-number (match-string 1 time-as-string)))
    (setq pplog-month (string-to-number (match-string 2 time-as-string)))
    (setq pplog-day   (string-to-number (match-string 3 time-as-string)))
    (setq pplog-hour  (string-to-number (match-string 4 time-as-string)))
    (setq pplog-min   (string-to-number (match-string 5 time-as-string)))
    (setq pplog-sec   (string-to-number (match-string 6 time-as-string)))
  )
  (encode-time pplog-sec pplog-min pplog-hour pplog-day pplog-month pplog-year)
)

(defun pplog-check-timestamp-order ()
  "Check whether the timestamps within a buffer are in increasing order."
  (interactive)
  (save-excursion
    (setq pplog-last-year 1900)
    (setq pplog-last-mon 1)
    (setq pplog-last-day 1)
    (setq pplog-last-hour 0)
    (setq pplog-last-min 0)
    (setq pplog-last-sec 0)
    (setq pplog-last-date "1900-01-01")

    (goto-char 1)
;; CHECK HERE
; warum habe ich nicht einfach (while re-search...
    (while
      (if (re-search-forward pplog-special-timestamp-re nil t)
       (if (string= (match-string 1) pplog-last-date)
; if date has not changed...
        (progn
	  (setq pplog-hour (string-to-number (match-string 5)))
	  (setq pplog-min (string-to-number (match-string 6)))
	  (setq pplog-sec (string-to-number (match-string 7)))
	  (or (and (= pplog-last-hour pplog-hour)
		   (or (< pplog-last-min pplog-min)
		       (and (= pplog-last-min pplog-min)
			    (<= pplog-last-sec pplog-sec))))
	      (< pplog-last-hour pplog-hour)
              (progn
                (message "Line %s: inconsistent timestamp order!" (current-line))
                (beep)
                (pplog-highlight (match-beginning 0) (match-end 0) 3)
              )
          )
	  (setq pplog-last-hour pplog-hour)
	  (setq pplog-last-min  pplog-min)
	  (setq pplog-last-sec  pplog-sec)
        )
; if date has changed...
        (progn
	  (setq pplog-year (string-to-number (match-string 2)))
	  (setq pplog-mon (string-to-number (match-string 3)))
	  (setq pplog-day (string-to-number (match-string 4)))
	  (setq pplog-hour (string-to-number (match-string 5)))
	  (setq pplog-min (string-to-number (match-string 6)))
	  (setq pplog-sec (string-to-number (match-string 7)))
; date has changed --> no check for seconds, minutes and hours necessary
          (or (and (= pplog-last-year pplog-year)
                   (or (and (= pplog-last-mon pplog-mon)
			    (< pplog-last-day pplog-day))
		       (< pplog-last-mon pplog-mon)))
	      (< pplog-last-year pplog-year)
              (progn
                (message "Line %s: inconsistent timestamp order!" (current-line))
                (beep)
                (pplog-highlight (match-beginning 0) (match-end 0) 3)
              )
          )
	  (setq pplog-last-year pplog-year)
	  (setq pplog-last-mon  pplog-mon)
	  (setq pplog-last-day  pplog-day)
	  (setq pplog-last-hour pplog-hour)
	  (setq pplog-last-min  pplog-min)
	  (setq pplog-last-sec  pplog-sec)
	  (setq pplog-last-date (match-string 1))
        )
       )
      )
    )
  )
)

(defun pplog-last-timestamp-correct (arg)
  "Subtract 'arg' minutes from last timestamp or 1 minute, if no prefix is given.
You cannot subtract more then 2097152 minutes at once (no joke)!"
  (interactive "P")
  (setq pplog-min-sub (if (equal arg nil) (quote 1) arg))
  (setq pplog-sec-sub (* pplog-min-sub 60))
  (save-excursion
    (re-search-backward pplog-new-timestamp-re)
    (setq pplog-time-as-list (time-string-as-list (match-string 0)))
    (setq pplog-high-time (- (nth 0 pplog-time-as-list) (/ pplog-sec-sub 65536)))
    (setq pplog-low-time   (- (nth 1 pplog-time-as-list)  (% pplog-sec-sub 65536)))
    (cond
      ( (< pplog-low-time 0)
        (progn
          (setq pplog-high-time (- pplog-high-time 1))
          (setq pplog-low-time  (+ pplog-low-time 65536))
        )
      )
      ( (> pplog-low-time 65535)
        (progn
          (setq pplog-high-time (+ pplog-high-time 1))
          (setq pplog-low-time  (- pplog-low-time 65536))
        )
      )
    )
    (setq pplog-offset-correction (- (nth 0 (current-time-zone)) (nth 0 (current-time-zone (list pplog-high-time pplog-low-time)))))
; correct time
    (setq pplog-low-time (+ pplog-low-time pplog-offset-correction))
    (cond
      ( (< pplog-low-time 0)
        (progn
          (setq pplog-high-time (- pplog-high-time 1))
          (setq pplog-low-time  (+ pplog-low-time 65536))
        )
      )
      ( (> pplog-low-time 65535)
        (progn
          (setq pplog-high-time (+ pplog-high-time 1))
          (setq pplog-low-time  (- pplog-low-time 65536))
        )
      )
    )
    (setq pplog-prop-list (text-properties-at (match-beginning 0)))
;; CHECK HERE
    (replace-match (pplog-time-string (list pplog-high-time pplog-low-time 0)))
    (set-text-properties (match-beginning 0) (match-end 0) pplog-prop-list)
  )
)

;------------------------------------------------------------
; Functions needed in some 'old' Emacs versions:

(or (fboundp 'match-string)
    ;; Introduced in Emacs 19.29.
  (defun match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
    (if (match-beginning num)
      (if string
        (substring string (match-beginning num) (match-end num))
        (buffer-substring (match-beginning num) (match-end num))
      )
    )
  )
)


(if (fboundp 'encode-time)
  (message "'encode-time' is a known function!")
  (message "Load 'encode-time'!") ;ps (sit-for 2)
  (defun encode-time (sec min hour day month year)
    "Encode time, given as list (sec min hour day month year), as two-ingeger list (high-sec low-sec).
These integers combine to give the number of seconds since 0:00 January 1, 1970. The function should return exactly the same as the original 'encode-time' in newer Emacs versions, even during daylight savings time."
; encode year as seconds
    (defvar pplog-help)
    (defvar pplog-high-enc-time)
    (defvar pplog-low-enc-time)
    (setq pplog-help (- year 1970))
    (setq pplog-high-enc-time (* pplog-help 481))
    (setq pplog-low-enc-time (* pplog-help 13184))
; correct for leap years
    (setq pplog-help (/ (1+ pplog-help) 4))
    (setq pplog-high-enc-time (+ pplog-high-enc-time pplog-help))
    (setq pplog-low-enc-time (+ pplog-low-enc-time (* pplog-help 20864)))
; calculate day of the year
    (if (equal (% year 4) 0)
; for leap years
      (setq day (+ day (nth month '(0 0 31 60 91 121 152 182 213 244 274 305 335)) -1))
; for "normal" years
      (setq day (+ day (nth month '(0 0 31 59 90 120 151 181 212 243 273 304 334)) -1))
    )
    (setq pplog-high-enc-time (+ pplog-high-enc-time day))
    (setq pplog-low-enc-time (- (+ pplog-low-enc-time (* day 20864) (* hour 3600) (* min 60) sec) (nth 0 (current-time-zone)) ))
    (setq pplog-high-enc-time (+ pplog-high-enc-time (/ pplog-low-enc-time 65536)))
    (setq pplog-low-enc-time (% pplog-low-enc-time 65536))

    (list pplog-high-enc-time pplog-low-enc-time)
  )
)

;------------------------------------------------------------

(defun pplog-highlight (hl-begin hl-end hl-time)
  "Highlight the given area of the current buffer for 'hl-time' seconds."
  (add-text-properties hl-begin hl-end '(face highlight))
  (sit-for hl-time)
  (remove-text-properties hl-begin hl-end '(face highlight))
)
;-------------------- syntax support --------------------

(defun electric-pplog-terminate-line ()
  "Indent all new lines automatically for easy recognition."
  (interactive)
  (insert "\n   ")
)

;;------------------------------------------------------------

(defun pplog-parse-header ()
  "Extract informations from the header of a pplog file.
The header should contain the characters that mark the begin/the end of an event and the (single-character) abbreviations for interruptions and errors and the abbreviations and/or full names of the phases!"
  (interactive)
  (make-variable-buffer-local 'pplog-begin-char)
  (make-variable-buffer-local 'pplog-end-char)
  (make-variable-buffer-local 'pplog-itr-char)
  (make-variable-buffer-local 'pplog-error-char)
  (make-variable-buffer-local 'pplog-phase-names)
;; HIER NOCH KORRIGIEREN !!!
;; CHECK HERE
  (setq pplog-begin-char "b")
  (setq pplog-end-char "e")
  (setq pplog-itr-char "i")
  (setq pplog-error-char "e")
  (setq pplog-phase-abbr-list nil)
  (defvar pplog-row)
  ; save old point
  (setq pplog-oldpoint (point))
  (goto-char 1)
  (setq counter-i 1)
; scan maximum 50 lines !!!! (for performance reasons!)
  (while
    (if (and (re-search-forward "\\(^#!\\)\\(.*\\)" nil t)
	     (< counter-i 50))
      (prog1
        t
        (setq pplog-row (match-string 2))
        (save-excursion
          (if (string-match "\\(^\\| \\|;\\|\t\\)begin=" pplog-row)
            (setq pplog-begin-char (char-to-string (elt pplog-row (match-end 0))))
          )
          (if (string-match "\\(^\\| \\|;\\|\t\\)end=" pplog-row)
            (setq pplog-end-char (char-to-string (elt pplog-row (match-end 0))))
          )
          (if (string-match "\\(^\\| \\|;\\|\t\\)interrupt=" pplog-row)
            (setq pplog-itr-char (char-to-string (elt pplog-row (match-end 0))))
          )
          (if (string-match "\\(^\\| \\|;\\|\t\\)defect=" pplog-row)
            (setq pplog-error-char (char-to-string (elt pplog-row (match-end 0))))
          )
          (setq pplog-phase-re "phase\\( *= *\\([A-Za-z0-9]+\\|\"[^\"]+\"\\)\\)+")
          (while
            (if (string-match pplog-phase-re pplog-row)
              (prog1
                t
                (setq pplog-phase-string (match-string 0 pplog-row))
		(setq pplog-row (substring pplog-row (match-end 0)))
                (setq pplog-start-srch 0)
		(while (string-match "\\( *= *\\(\\([A-Za-z0-9]+\\)\\|\"\\([^\"]+\\)\"\\)\\)\\(\\( *= *\\([A-Za-z0-9]+\\|\"[^\"]+\"\\)\\)*\\)" pplog-phase-string)
		  (prog1
		    t
;		    (message pplog-phase-string)
		    (if (match-string 3 pplog-phase-string)
		       (setq pplog-phase-abbr (match-string 3 pplog-phase-string))
		       (setq pplog-phase-abbr (match-string 4 pplog-phase-string))
		    )
		    (setq pplog-phase-string (match-string 5 pplog-phase-string))
		    (or (string-match " " pplog-phase-abbr)
                        (string-match "\t" pplog-phase-abbr)
                        (member pplog-phase-abbr pplog-phase-abbr-list)
                        (setq pplog-phase-abbr-list (cons pplog-phase-abbr pplog-phase-abbr-list))
		    )
		  )
		)
              )
            )
          )
        )
        (setq counter-i (1+ counter-i))
      )
    )
  )
)

(defvar pplog-line-re (concat "\\(" pplog-new-timestamp-re "\\)\\([^\n]*\\)$"))


(defun pplog-parse-entries ()
  "Parse file entries and search for errors."
  (interactive)
  (remove-text-properties (point-min) (point-max) '(face nil b-o-lv nil b-o-p nil b-o-e nil b-o-i nil))
; from the beginning of the file
  (goto-char 1)
  (setq counter-i 1)
  (while
; pay attention only to lines beginning with a timestamp
    (if (re-search-forward pplog-new-timestamp-re nil t)
      (progn
        (setq pplog-mb (match-beginning 0))
        (setq pplog-me (match-end 0))
        ; parse rest of line
        (if (re-search-forward (concat "\\=[ \t]*\\(\\([" pplog-begin-char pplog-end-char "]\\)\\([^\n\t ]*\\)[^\n]*\\|\n\\)") nil t)
          ; empty line or line beginning with begin- or end-of-event-character?
          (if (string= (match-string 1) "\n")
            ; only whitespaces and tabs? --> ignore
            (progn
              (message "empty line")
              t  ; <-- continue parsing with next line
            )
            ; begin- or end-of-event-character? --> check entry!
            (progn
              (setq pplog-ms3 (match-string 3))
              (if (equal (match-string 2) pplog-begin-char)
                ; begin of an event?
                (cond
                  ((equal pplog-ms3 pplog-itr-char)   (pplog-parse-inside-itr pplog-mb pplog-me))
                  ((equal pplog-ms3 pplog-error-char) (pplog-parse-inside-error pplog-mb pplog-me))
                  ((member pplog-ms3 pplog-phase-abbr-list) (pplog-parse-inside-phase pplog-mb pplog-me pplog-ms3))
                  (t (pplog-parse-error pplog-mb pplog-me (concat "Unknown event '" pplog-ms3 "'!")))
                )
                ; end of an event? --> error
                (pplog-parse-error pplog-mb (match-end 3) "No 'event' to end!")
              )
            )
          )
          ; else: invalid line: error!
          (progn
            (re-search-forward "\\=[^\n]*")
            (pplog-parse-error pplog-mb (match-end 0) "Unknown line format!")
            t  ; <-- continue parsing with next line as if line was empty
          )
        )
      )
    )
  )
)


(defun pplog-parse-inside-phase (pplog-begin-mark pplog-end-mark pplog-phase-abbr)
  "..."
  (interactive)
  (setq pplog-parse-success nil)
  (add-text-properties pplog-begin-mark pplog-end-mark '(face phase-face b-o-p t b-o-lv t))
; from the current position
  (while
    (if (re-search-forward pplog-new-timestamp-re nil t)
      (progn
        (setq pplog-mb (match-beginning 0))
        (setq pplog-me (match-end 0))
        ; parse rest of line
        (if (re-search-forward (concat "\\=[ \t]*\\(\\([" pplog-begin-char pplog-end-char "]\\)\\([^\n\t ]*\\)[^\n]*\\|\n\\)") nil t)
          ; empty line or line beginning with begin- or end-of-event-character?
          (if (string= (match-string 1) "\n")
            ; only whitespaces and tabs? --> ignore
            (progn
              (message "empty line")
              t  ; <-- continue parsing with next line
            )
            ; begin- or end-of-event-character? --> check entry!
            (progn
              (setq pplog-ms3 (match-string 3))
              (if (equal (match-string 2) pplog-begin-char)
                ; begin of an event?
                (cond
                  ((equal pplog-ms3 pplog-itr-char)
                   (prog1
                     (pplog-parse-inside-itr pplog-mb pplog-me)
                     (setq pplog-parse-success nil)
                   )
                  )
                  ((equal pplog-ms3 pplog-error-char)
                   (prog1
                     (pplog-parse-inside-error pplog-mb pplog-me)
                     (setq pplog-parse-success nil)
                   )
                  )
                  ((member pplog-ms3 pplog-phase-abbr-list)
                   (prog1
                     (if pplog-phase-in-phase
                       (pplog-parse-inside-phase pplog-mb pplog-me pplog-ms3)
                       (pplog-parse-error pplog-mb pplog-me "Phase within other phase are not allowed!")
                     )
                     (setq pplog-parse-success nil)
                   )
                  )
                  (t (pplog-parse-error pplog-mb pplog-me (concat "Unknown event '" pplog-ms3 "'!")))
               )
                ; end of an event? --> entry is valid, only if this is the end of a phase
                (cond
                  ((equal pplog-ms3 pplog-phase-abbr)
	           (remove-text-properties pplog-begin-mark pplog-end-mark '(face phase-face b-o-i t b-o-lv t))
                   (setq pplog-parse-success t)
                   ()
                  )
                  (t (pplog-parse-error pplog-mb (match-end 3) (concat "Only '" pplog-phase-abbr "' can end here!")) nil)
                )
              )
            )
          )
          ; else: invalid line: error!
          (progn
            (re-search-forward "\\=[^\n]*")
            (pplog-parse-error pplog-mb (match-end 0) "Unknown line format!")
            t  ; <-- continue parsing with next line as if line was empty
          )
        )
      )
    )
  )
  (setq pplog-parse-success pplog-parse-success)
)


(defun pplog-parse-inside-error (pplog-begin-mark pplog-end-mark)
  "..."
  (interactive)
  (setq pplog-parse-success nil)
  (add-text-properties pplog-begin-mark pplog-end-mark '(face error-face b-o-e t b-o-lv t))
;; from the current position
  (while
    (if (re-search-forward pplog-new-timestamp-re nil t)
      (progn
        (setq pplog-mb (match-beginning 0))
        (setq pplog-me (match-end 0))
        ; parse rest of line
        (if (re-search-forward (concat "\\=[ \t]*\\(\\([" pplog-begin-char pplog-end-char "]\\)\\([^\n\t ]*\\)[^\n]*\\|\n\\)") nil t)
          ; empty line or line beginning with begin- or end-of-event-character?
          (if (string= (match-string 1) "\n")
            ; only whitespaces and tabs? --> ignore
            (progn
              (message "empty line")
              t  ; <-- continue parsing with next line
            )
            ; begin- or end-of-event-character? --> check entry!
            (progn
              (setq pplog-ms3 (match-string 3))
              (if (equal (match-string 2) pplog-begin-char)
                ; begin of an event?
                (cond
                  ((equal pplog-ms3 pplog-itr-char)
                   (prog1
                     (pplog-parse-inside-itr pplog-mb pplog-me)
                     (setq pplog-parse-success nil)
                   )
                  )
                  ((equal pplog-ms3 pplog-error-char)
                   (prog1
                     (if pplog-error-in-error
                       (pplog-parse-inside-error pplog-mb pplog-me)
                       (pplog-parse-error pplog-mb pplog-me "No errors within other error allowed!")
                     )
                     (setq pplog-parse-success nil)
                   )
                  )
                  ((member pplog-ms3 pplog-phase-abbr-list)
                   (prog1
                     (pplog-parse-error pplog-mb pplog-me "No phase within error allowed!")
                     (setq pplog-parse-success nil)
                   )
                  )
                  (t (pplog-parse-error pplog-mb pplog-me (concat "Unknown event '" pplog-ms3 "'!")))
                )
                ; end of an event? --> entry is valid, only if this is the end of an error
                (cond
                  ((equal pplog-ms3 pplog-error-char)
	           (remove-text-properties pplog-begin-mark pplog-end-mark '(face error-face b-o-e t b-o-lv t))
                   (setq pplog-parse-success t)
                   ()
                  )
                  (t (pplog-parse-error pplog-mb (match-end 3) "Only an error can end here!") nil)
                )
              )
            )
          )
          ; else: invalid line: error!
          (progn
            (re-search-forward "\\=[^\n]*")
            (pplog-parse-error pplog-mb (match-end 0) "Unknown line format!")
            t  ; <-- continue parsing with next line as if line was empty
          )
        )
      )
    )
  )
  (setq pplog-parse-success pplog-parse-success)
)

(defun pplog-parse-inside-itr (pplog-begin-mark pplog-end-mark)
  "..."
  (interactive)
  (setq pplog-parse-success nil)
  (add-text-properties pplog-begin-mark pplog-end-mark '(face itr-face b-o-i t b-o-lv t))
  (while
    (if (re-search-forward pplog-new-timestamp-re nil t)
      (progn
        (setq pplog-mb (match-beginning 0))
        (setq pplog-me (match-end 0))
        ; parse rest of line
        (if (re-search-forward (concat "\\=[ \t]*\\(\\([" pplog-begin-char pplog-end-char "]\\)\\([^\n\t ]*\\)[^\n]*\\|\n\\)") nil t)
          ; empty line or line beginning with begin- or end-of-event-character?
          (if (string= (match-string 1) "\n")
            ; only whitespaces and tabs? --> ignore
            (progn
              (message "empty line")
              t  ; <-- continue parsing with next line
            )
            ; begin- or end-of-event-character? --> check entry!
            (progn
              (setq pplog-ms3 (match-string 3))
              (if (equal (match-string 2) pplog-begin-char)
                ; begin of an event?
                (cond
                  ((equal pplog-ms3 pplog-itr-char)
                   (prog1
                     (if pplog-itr-in-itr
                       (pplog-parse-inside-itr pplog-mb pplog-me)
                       (pplog-parse-error pplog-mb pplog-me "Interruption within other interruption is not allowed!")
                     )
                     (setq pplog-parse-success nil)
                   )
                  )
                  ((equal pplog-ms3 pplog-error-char)
                   (prog1
                     (pplog-parse-error pplog-mb pplog-me "Error within interruption is not allowed!")
                     (setq pplog-parse-success nil)
                   )
                  )
                  ((member pplog-ms3 pplog-phase-abbr-list)
                   (prog1
                     (pplog-parse-error pplog-mb pplog-me "Phase within interruption is not allowed!")
                     (setq pplog-parse-success nil)
                   )
                  )
                  (t (pplog-parse-error pplog-mb pplog-me (concat "Unknown event '" pplog-ms3 "'!")))
                )
                ; end of an event? --> entry is valid, only if this is the end of an interruption
                (cond
                  ((equal pplog-ms3 pplog-itr-char)
	           (remove-text-properties pplog-begin-mark pplog-end-mark '(face itr-face b-o-i t b-o-lv t))
                   (setq pplog-parse-success t)
                   ()
                  )
                  (t (pplog-parse-error pplog-mb (match-end 3) "Only an interruption can end here!") nil)
                )
              )
            )
          )
          ; else: invalid line: error!
          (progn
            (re-search-forward "\\=[^\n]*")
            (pplog-parse-error pplog-mb (match-end 0) "Unknown line format!")
            t  ; <-- continue parsing with next line as if line was empty
          )
        )
      )
    )
  )
  (setq pplog-parse-success pplog-parse-success)
)


(defun pplog-parse-error (pplog-begin-mark pplog-end-mark pplog-error-msg)
  "Print an error message for parsing errors."
  (beep)
  (add-text-properties pplog-begin-mark pplog-end-mark '(face highlight))
  (message "Parsing error: %s" pplog-error-msg)
;ps  (sit-for 2)
  (remove-text-properties pplog-begin-mark pplog-end-mark '(face highlight))
  ()
)

;; --------------------------------------------------------------------------------

(defun pplog-watch-for-line-change ()
  "..."
  (if (and (equal (current-line) pplog-line-number)
           (equal (current-buffer) pplog-buffer))
    ()
    (progn
      (remove-hook 'post-command-hook 'pplog-watch-for-line-change)
      (save-excursion
        (set-window-buffer (selected-window) pplog-buffer)
        (pplog-parse-header)
        (pplog-parse-entries)
      )
      (set-window-buffer (selected-window) (current-buffer))
    )
  )
)

;; --------------------------------------------------------------------------------
(defun pplog-mode ()
  "Major mode for personal process logging.
This mode helps you protocol your personal working processes. It was initially
intended to help programmers when using a Personal Software Process (PSP), but
may be useful for other purposes, too. For detailed information about the
Personal Software Process, please refer to
\"Watts S. Humphrey. A Discipline for Software Engineering.\"

The basic idea of the process logging is finding out how you spend your
time and in particular how many and what kinds of errors you make,
at what time, why, and how long it takes you to fix these errors.

This major mode helps you to collect the appropriate date while programming
using GNU Emacs or XEmacs. Using a single key or a menu item, you can switch
into a log buffer or create a new one and insert a timestamp. Your entries
will be checked for correct syntax to avoid incorrect log data.


Crash course:
=============
Here is a short section from an example pplog file:

1998-08-24 13:09:08 bds
1998-08-24 15:14:47 eds
1998-08-24 15:14:58 bcd
1998-08-24 15:38:02 be
1998-08-24 15:38:35 ee ds log cm start.c
   must accomodate multiple -pp options!
1998-08-24 15:42:19 bi
   Harry called
1998-08-24 15:48:13 ei
1998-08-24 19:43:17 ecd

pplog files have a well-defined (but very flexible) syntax in order to
allow for automatic analysis.
Each line is one entry, starting with a timestamp. Lines starting with
spaces are arbitrary comments for whatever purpose you want them.
Each entry is a 'b' (begin) or 'e' (end).
'ds' and 'cd' are names of phases (or working styles or projects or whatever
you need as toplevel entities). The recommended default set of phases for
software development is ds, dr, cd, cr, cp, te (design, design review, code,
code review, compilation/build, test). You can use arbitrary names, but must
declare them at the top of the pplog file, e.g.:
#! phase=\"design review\"=dr

'be'/'ee' are 'begin error'/'end error' entries, made when one detects that
there is some defect. Errors nest into phases.
Error entries are the most useful part of using pplog mode.
'bi'/'ei' are 'begin interrupt'/'end interrupt' entries, made when one
does shortly not work. These are useful for finding out how (un)disturbed
ones work is and whether interruptions may correlate with defects.
Interrupts nest into phases and into errors, as required.

'End error' entries are decorated with defect classifications:
The first one is the name of the phase in which the defect was created.
Subsequently you can have an arbitrary number of arbitrary independent
defect classifications. In the example there are 3 such classifications:
defect type (here: 'log', for 'logic error'), defect reason (here: 'cm',
for 'commission'. Recommended set: omission, commission, typo, documentation,
knowledge), and defect location (here: name of a source file).

Use whatever entries you like, but they should form some kind of enumeration
type, so that counting their frequency makes sense.
In particular the defect type classification needs to be tuned to your
personal requirements in order to be most useful.
Phase names and defect classes can be arbitrary sequences of printable
non-whitespace characters.

The Perl script 'evalpplog' can be used to produce nice tables from
pplog files, summarizing frequencies, costs, relative costs etc.


Invoking pplog mode:
====================
There are two possibilities to invoke pplog mode:
1. You explicitely load the pplog mode library by \\[load-file] pplog-mode.el
or \\[load-file] pplog-mode.elc (after byte-compiling 'pplog-mode.el').
OR:
2. Add ONE of the following lines to your .emacs file, where PATH must be the
   directory, where pplog-mode.el(c) may be found:
   (load-file PATH/pplog-mode.el)
   (load-file PATH/pplog-mode.elc)


Creating log buffers/switching to existing buffers:
===================================================
Please do not create log buffers by hand. Use the following functions
instead.
\\[pplog-hot-key-event-handling]:
Switch to an open pplog buffer or open a new one.
When prompted for a filename, please choose a name ending in \".ppl\" to
 enable Emacs to recognise the file automatically when reloading the file.
If there are pplog buffers open, this function will bring you
to the least recently used of these buffers.
This is typically the same key as
\\[pplog-return-key-event-handling]:
Return to the buffer where you originally came from.
\\[pplog-nextlog]:
Switch to the next pplog buffer.
\\[pplog-open-log-buffer-now]:
Create a new pplog buffer.


Log entries:
============
When changing to a logbuffer with one of the defined key-sequences,
a timestamp with the current time is automatically inserted. After that, you
can make entries for the beginning or the end of an 'event'. 'Events' are
phases (like 'design', 'coding', 'testing' - abbreviated by a multi-character
sequence), error and interruptions (abbreviated by a single character). 'Begin'
and 'end' must be abbreviated by a single character too. The abbreviations may
be defined by you (see below: Customisation). For example, the following line
might denotate the beginning of a phase abbreviated 'te':
Apr 27 17:33:08 1998 bte
Be careful not to use whitespaces between the begin/end-character and the
event-abbreviations.


Modifying entries:
==================
There are some functions that help you to modify the timestamps:
\\[pplog-last-timestamp-correct]:
Subtract one minute from the last timestamp or any other number of minutes,
given as prefix argument. Negative prefix arguments are allowed to add minutes
to the timestamps.
\\[pplog-last-timestamp-is-now]:
Set the preceeding timestamp to the following timestamp or the current time,
if no following timestamp is present. The preceeding timestamp is the first
timestamp backwards from point.
Of course, all modifications may also be done by hand.
These functions might not work correctly on the day when daylight savings
time is switched on or off.


Problem handling:
=================
If there should be any problems in recognising 'events' or other entries,
first try \\[pplog-special-recenter] to reparse the complete buffer.
If the problem remains, submit a bug report (see pplog-mode.el source code).


Customisation:
==============
Keys:
-----
For easy use, the most important functions of pplog-mode may be controlled by
simple key-sequences, that you may change.
pplog-hot-key:
Brings you to most recent pplog buffer or creates a new one.
pplog-return-key:
Returns you to the buffer where you came from.
pplog-open-log-buffer-now:
Create additional pplog buffer. The current buffer must be a pplog buffer.
If there are n unvisited pplog buffers, pressing this key is equivalent to
pressing (n+1) times pplog-nextlog.
pplog-nextlog:
Switch from one pplog buffer to the next pplog buffer,

pplog-last-timestamp-is-now:
Set timestamp to current time.
Search forward and backward for the next timestamp. If the forward search
succeeds, set the preceding timestamp to the same value as the following one.
Otherwise, set the preceding timestamp to the current date and time.
pplog-last-timestamp-correct  \\[pplog-last-timestamp-correct]:
Subtract 'argument' minutes from last timestamp or 1 minute, if no prefix is
given. Please don't subtract more then 2097152 minutes at once (no joke)!

pplog-special-recenter  \\[pplog-special-recenter]:
Reparse the buffer header, test buffer for correct entries und consistent
timestamp order. Then perform a 'normal' recenter.

Behaviour:
----------
Obviously, the 'events' (i.e. phases, errors, interruptions) depend in some
way on each other. For example, it should be clear that you cannot
proceed to the next phase while you are working on an error, and you can't
work or proceed to the next phase, while an interruption is active.
There are 3 variables to adapt the behaviour to your needs:
Set 'pplog-phase-in-phase' to non-nil, if you want to allow phases within
other phases. Otherwise, set it to nil.
Set 'pplog-error-in-error' to non-nil, if you want to allow starting new
errors while working at old ones. Otherwise, set it to nil.
Set 'pplog-itr-in-itr' to t, if you want to allow new interruptions while
other interruptions are active. Otherwise, set it to nil.
The recommended settings for 'Standard PSP' is:
(defconst pplog-phase-in-phase nil),
(defconst pplog-error-in-error t),
(defconst pplog-itr-in-itr nil).

Known bugs:
===========
If you reparse a file (by pressing \\[pplog-special-recenter]), the old keymap
is not deleted and will still be active in the future.
Timestamp correction functions will possibly not work as intended on the day
when daylight savings time is switched off (i.e. when clocks are turned back
one hour), because there will be the same string representation for different
times.
If a pplog buffer does not end with a newline, this may cause a parse error.
You can ignore this error.


Annotations:
============
The latest version of pplog-mode is (hopefully) available via WWW at
http://wwwipd.ira.uka.de/PSP
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map pplog-mode-map)
  (setq mode-name "PPLog")
  (setq major-mode 'pplog-mode)
  (defvar pplog-oldpoint)
  (message "Personal process log mode (pplog-mode), %s (%s)."
           pplog-mode-version pplog-mode-last-change)
;ps  (sit-for 2)
  (if (string-match "XEmacs" emacs-version)
    (add-menu nil "PPLog"
      '(
        :included (equal major-mode 'pplog-mode)
        ["Switch to next pplog buffer" pplog-nextlog (equal major-mode 'pplog-mode)]
        ["Create new pplog buffer" pplog-open-log-buffer-now (equal major-mode 'pplog-mode)]
        ["Return to original buffer" pplog-return-key-event-handling (equal major-mode 'pplog-mode)]
        "---"
        ["Set last timestamp to now" pplog-last-timestamp-is-now (equal major-mode 'pplog-mode)]
        ["Subtract 1 min from last timestamp" pplog-last-timestamp-correct (equal major-mode 'pplog-mode)]
       )
    )
  )
)


(defun pplog-create-test-data ()
  (interactive)
  (setq cnt 1)
  (while (< cnt 500)
    (pplog-insert-timestamp)
    (pplog-last-timestamp-correct (- 0 (* cnt 121)))
    (insert "bcd Arbitrary text, just to have more volume to parse....\n")
    (pplog-insert-timestamp)
    (pplog-last-timestamp-correct (- 0 (+ (* cnt 121) 53)))
    (insert "ecd similar, only less much...\n")
    (setq cnt (+ cnt 1))
  )
)

(provide 'pplog-mode)

;; eof