;;----------------------------------------------------------- -*- Emacs-Lisp -*-
;;  Emacs Startup File
;;
;;      Author: Peter Steiner <unistein@isbe.ch>
;;     Created:	Wed Jul 6 19:52:18 1994
;;     changed: 


;; setup special variables and functions ---------------------------------------

;(defun insert-header ()
;  "Insert a header into the file. The header consists of the author's name,
;   his mailing address, the file's creationtime and the filename."
;  (interactive)
;  (insert "      Author: \t")
;  (insert (concat (user-full-name) " <" (user-login-name) "@iam.unibe.ch>"))
;  (insert "\n     Created:\t")
;  (insert (current-time-string))
;  (insert "\n    Filename:\t")
;  (insert (concat (buffer-file-name) "\n")))
;
;(defun insert-time-and-author ()
;  "Insert the current time and the username."
;  (interactive)
;  (insert (current-time-string))
;  (insert (concat " " (user-full-name) " <" (user-login-name) "@iam.unibe.ch>"))
;  )

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


;; general configuration -------------------------------------------------------

;; Emacs-Päckli auch in meinem lokalen emacs-Verzeichnis suchen
(if (file-accessible-directory-p "~/emacs/gnus/lisp/")
    (setq load-path (append (list "~/emacs/gnus/lisp/") load-path)))
(if (file-accessible-directory-p "~/emacs/custom/")
    (setq load-path (append (list "~/emacs/custom/") load-path)))
(setq-default load-path (append '("~/emacs") load-path))

(setq-default fill-column 77)           ;; column for line breaking in auto-fill-mode
(setq make-backup-files t)
(put 'eval-expression 'disabled nil)    ;; enable `eval-expression'

;; Zeilen nicht automatisch umbrechen, wenn sie zu lang sind; dafür
;; einen Minor-Mode laden, damit bei langen Zeilen automatisch gescrollt wird
(setq-default truncate-lines t)
(require 'hscroll)
(setq-default hscroll-mode t)
(setq-default hscroll-mode-name nil)
(hscroll-mode)
(require 'scroll-in-place)

(require 'pc-select)
(pc-bindings-mode)
(pc-selection-mode)

;; display configuration -------------------------------------------------------
(standard-display-european t)           ;; vollen 8-Bit Zeichensatz verwenden
(column-number-mode 1)
;(require 'column)
;(display-column-mode 1)

;; fonts -----------------------------------------------------------------------
;; Paare von Werten (mindestens für Lucida Sans Typewriter)
;; 10-75 / 11-82 / 12-90 / 13-97 / 14-105 / 15-112
(if (eq (string-match "PIAZZA" (system-name)) 0) 
    (defvar my-font-size "13-97")  ;; auf PIAZZA: etwas grösser (ca. 9.7 Punkt)
    (defvar my-font-size "11-82")  ;; sonst (ca. 8.2 Punkt)
    )

(defvar lucida-typewriter-regular
  (concat "-*-Lucida Sans Typewriter-normal-r-*-*-"
          my-font-size "-*-*-c-*-*-ansi-"))
(defvar lucida-typewriter-italic
  (concat "-*-Lucida Sans Typewriter-normal-i-*-*-" 
          my-font-size "-*-*-c-*-*-ansi-"))
(defvar lucida-typewriter-bold
  (concat "-*-Lucida Sans Typewriter-semibold-r-*-*-"
          my-font-size "-*-*-c-*-*-ansi-"))
(defvar lucida-typewriter-bold-italic
  (concat "-*-Lucida Sans Typewriter-semibold-i-*-*-"
          my-font-size "-*-*-c-*-*-ansi-"))

(cond (window-system
       ;; default-Parameter für alle Fenster
       (setq default-frame-alist
             '((width             . 81)
               (height            . 45)
               (foreground-color  . "Black")
               (background-color  . "WhiteSmoke")
               (cursor-color      . "MediumBlue")
;               (font              . "-*-Lucida Sans Typewriter-normal-r-*-*-13-97-*-*-c-*-*-ansi-")
               (icon-type         . t)         ; gnu picture as Emacs icon
               (icon-name         . nil)       ; use frame title
               ))
       ;; Spezial-Behandlung des ersten Fensters
       (setq initial-frame-alist
             '( ; (top . 1) (left . 400)
               (width . 90) (height . 50)
               ))
       (setq frame-title-format "Emacs - %b")  ; set frame title to buffer name
       (setq icon-title-format  "Emacs - %b")  ; set  icon title to buffer name
       ))

; WindowsNT und Linux haben immer noch verschiedene Schriften...
(if (eq window-system 'win32)
    (progn
      (setq win32-enable-italics t)
      (set-default-font           lucida-typewriter-regular)
      (set-face-font 'default     lucida-typewriter-regular)
      (set-face-font 'bold        lucida-typewriter-bold)
      (set-face-font 'italic      lucida-typewriter-italic)
      (set-face-font 'bold-italic lucida-typewriter-bold-italic)
      )
  )

;; keyboard configuration ------------------------------------------------------

(global-set-key "\C-g" 'goto-line)
;(global-set-key "\C-x\C-a" 'insert-time-and-author) ;; self-written
;(global-set-key "\C-x\C-m" 'insert-header)          ;; self-written
(global-set-key "\C-z" 'undo)
(define-key ctl-x-map [(control <)] 'uncomment-region)     ;; self-written
(define-key ctl-x-map [(control >)] 'comment-region)       ;; standard emacs lisp
(define-key global-map [(C-return)] 'duplicate-line)       ;; self-written
(define-key global-map [(C-kp-enter)] 'duplicate-line)     ;; self-written

(cond (window-system 
        (define-key global-map [S-mouse-2] 'imenu)))

;(if (eq window-system 'x)
;    (progn
;      (load "x-compose")
;      ;; use shift-control-section as compose key
;      (define-key global-map [(control degree)] compose-map)
;      ;; disable degree as a dead key
;      (define-key global-map [degree] 'self-insert-command)
;      ))

;;; conditional keyboard configuration
;(cond 
; ((eq pesche-emacs-version 'emacs-x) 
;  (progn
;    ;; Delete-Taste nicht wie BackSpace verwenden (wegen x-win.elc noetig)
;    (put 'delete 'ascii-character 'delete)
;    (define-key function-key-map [delete] [delete])
;    (define-key global-map [delete] 'delete-char)
;    ))
; ((eq pesche-emacs-version 'lucid)
;  (progn
;    (define-key global-map [delete] 'delete-char)
;    (define-key global-map [backspace] 'delete-backward-char)
;    (define-key shared-lisp-mode-map [delete] 'delete-char)
;    (define-key shared-lisp-mode-map [backspace] 'backward-delete-char-untabify)
;    ;; change mouse button 2 to insert at text cursor point and not mouse position
;    ;; (define-key global-map 'button2 'x-insert-selection)
;    ;; other keymaps: see below when setting up modes
;    ))
; (t
;  (progn
;    ;; terminal-based (with X these has been done by xmodmap)
;    (define-key function-key-map "\e[G" [begin])       ;; Keypad-5
;    ))
; )

;(if (eq pesche-emacs-version 'lucid)
;    (progn
;      (define-key global-map 'begin 'recenter)              ;; Keypad-5
;      (define-key global-map [(f31)] 'recenter)             ;; Keypad-5
;      (define-key global-map 'f9 'compile)
;      (define-key global-map '(control shift down) 'next-error)
;      (define-key global-map '(control f34) 'next-error)    ;; sometimes S-down = f34
;
;      ;; define combinations with the SUN `Alt' key as Umlauts
;      (define-key global-map [(symbol a)] [adiaeresis])
;      (define-key global-map [(symbol o)] [odiaeresis])
;      (define-key global-map [(symbol u)] [udiaeresis])
;      (define-key global-map [(symbol A)] [Adiaeresis])
;      (define-key global-map [(symbol O)] [Odiaeresis])
;      (define-key global-map [(symbol U)] [Udiaeresis])
;
;      ;; define the german umlauts as in LaTeX german option
;      (define-key global-map [adiaeresis] "\"a")
;      (define-key global-map [odiaeresis] "\"o")
;      (define-key global-map [udiaeresis] "\"u")
;      (define-key global-map [Adiaeresis] "\"A")
;      (define-key global-map [Odiaeresis] "\"O")
;      (define-key global-map [Udiaeresis] "\"U")
;
;      ;; horizontal scrolling
;      (define-key global-map [(symbol home)] 'scroll-right);; Alt-Home
;      (define-key global-map [(symbol end)] 'scroll-left);; Alt-End
;
;      ;; needed on SUN workstations
;      (define-key global-map [(f25)] "/");; grey-/
;      (define-key global-map [(f26)] "*");; grey-*
;      (define-key global-map [(control f27)] 'beginning-of-buffer);; Ctrl-Home
;      (define-key global-map [(control left)] 'backward-word)
;      (define-key global-map [(control right)] 'forward-word)
;      (define-key global-map [(f33)] 'end-of-line);; End (num)
;      (define-key global-map [(control r13)] 'end-of-buffer);; Ctrl-End
;      (define-key global-map [(control f33)] 'end-of-buffer);; Ctrl-End (num)
;      (define-key global-map [(control end)] 'end-of-buffer);; Ctrl-End (grey)
;      (define-key global-map [(control home)] 'beginning-of-buffer);; Ctrl-Home (grey)
;      (define-key global-map [(next)] 'scroll-up);; PgDn (grey)
;      (define-key global-map [(prior)] 'scroll-down);; PgUp (grey)
;      )
;  )
;(if (not (eq pesche-emacs-version 'lucid))
;    (progn

      (define-key global-map [(C-kp-space)] 'recenter)         ;; Ctrl-Keypad-5 (NT)
      (define-key global-map [(C-kp-begin)] 'recenter)         ;; Ctrl-Keypad-5 (Linux)
;      (define-key global-map [(home)] 'beginning-of-line)
;      (define-key global-map [(kp-7)] 'beginning-of-line)
;      (define-key global-map [(end)] 'end-of-line)
;      (define-key global-map [(kp-1)] 'end-of-line)
;      (define-key global-map [(C-home)] 'beginning-of-buffer);; Ctrl-Home (grey)
;      (define-key global-map [(C-end)] 'end-of-buffer);; Ctrl-End (grey)
;      (define-key global-map [(f9)] 'compile)
      (define-key global-map [(C-S-down)] 'next-error)

;      ;; define the german umlauts as in LaTeX german option
;      (define-key global-map [?\344] "\"a")
;      (define-key global-map [?\366] "\"o")
;      (define-key global-map [?\374] "\"u")
;      (define-key global-map [?\304] "\"A")
;      (define-key global-map [?\326] "\"O")
;      (define-key global-map [?\334] "\"U")
;      ))



;; mode specific configuration -------------------------------------------------
;;(setq default-major-mode 'text-mode)
;=(setq text-mode-hook 'turn-on-auto-fill)
;;(setq indent-tabs-mode 't)

;;; info system: find local info files
;(if (boundp 'Info-directory-list)
;    (setq Info-directory-list
;          (append Info-directory-list '("~/info/" "/opt/local/GNU/info/")))
;  )

;; WoMan stuff for reading man pages in emacs
(autoload 'woman "woman" 
  "Decode and browse a UN*X man page." t)
(autoload 'woman-find-file "woman"
  "Find, decode and browse a specific UN*X man-page file." t)
(setq man-path '("x:/gnuwin32/b18/man"))
;(setq woman-path '("x:\gnuwin32\b18\man" "e:\usr\man"))


;; lisp modes
(setq auto-mode-alist (append '(("\\.el$" . emacs-lisp-mode)
                                ("\\.emacs$" . emacs-lisp-mode)
                                ) auto-mode-alist))
(add-hook 'emacs-lisp-mode-hook
          (function (lambda ()
                      (setq-default tab-width        8
                                    indent-tabs-mode nil)
                      (imenu-add-to-menubar "Index")
                      )))

;; c++ mode
(setq auto-mode-alist (append '(("\\.C$"  . c-mode)
                                ("\\.cc$" . c++-mode)
                                ("\\.hh$" . c++-mode)
                                ("\\.c$"  . c-mode)
                                ("\\.h$"  . c-mode)
                                ) auto-mode-alist))
(add-hook 'c-mode-common-hook
          (function (lambda ()
                      (local-set-key [delete] 'delete-char)
                      (local-set-key [backspace] 'c-electric-delete)
                      ;; auch in C die neuen C++-Kommentare verwenden
                      (c-enable-//-in-c-mode)
                      (setq comment-start "// "
                            comment-end ""
                            comment-multi-line nil
                            font-lock-comment-start-regexp nil
                            c-double-slash-is-comments-p t)
                      ;; setup my personal indenting style
                      (c-set-style  "Stroustrup")
                      (c-set-offset 'case-label '+)
                      (c-set-offset 'statement-case-open '+)
                      (c-set-offset 'arglist-close 0)
                      (setq-default tab-width                8
                                    indent-tabs-mode         nil
                                    c-tab-always-indent      nil
                                    c++-tab-always-indent    nil)
                      (imenu-add-to-menubar "Index")
                      )))


;; makefile mode
;=(autoload 'makefile-mode "makefile" "Makefile Editing Mode" t)
(setq auto-mode-alist (append '(("[Mm]akefile$" . makefile-mode)
                                ("\\.mk$" . makefile-mode)
                                ("\\.mak$" . makefile-mode)
                                ) auto-mode-alist))
(add-hook 'makefile-mode-hook
          (function (lambda ()
                      (setq-default tab-width        8
                                    indent-tabs-mode t)
                      )))

;; TeX and related modes
;(cond
; ;; check if we can use AUC TeX:
; ;; change the name to your name if you have installed AUC TeX
; ((and (or (eq pesche-emacs-version 'emacs-x)
;           (eq pesche-emacs-version 'lucid))
;       (not (eq (string-match "psteiner" (user-login-name)) nil))
;       )
;  ;; this is to fix a bug in auctex occuring when using XEmacs 19.13
;  (defadvice set-text-properties (around ignore-strings activate)
;    "Ignore strings."
;    (or (stringp (ad-get-arg 3))
;        ad-do-it))
;  (require 'tex-site)
;  (setq TeX-parse-self t)
;  (setq TeX-auto-save t)
;  (setq-default TeX-master nil)
;  ;; xfig drawings
;  (setq auto-mode-alist (append '(("\\.pstex_t$" . LaTeX-mode)
;                                  ) auto-mode-alist))
;  )
;
; ;; else we have to use ordinary modes...
; (t
;  (setq auto-mode-alist (append '(("\\.tex$" . TeX-mode)
;                                  ("\\.txi$" . Texinfo-mode)
;                                  ("\\.texi$" . Texinfo-mode)
;                                  ("\\.bib$" . bibtex-mode)
;                                  ) auto-mode-alist))
;  (add-hook 'latex-mode-hook
;            (function (lambda ()
;;                        (local-set-key "\"" 'self-insert-command)
;                        (auto-fill-mode))))
;  (setq tex-default-mode 'latex-mode)
; )
;)
;

;;; hooks
;(add-hook 'TeX-mode-hook
;          (function (lambda ()
;                      (local-set-key [delete] 'delete-char)
;                      (local-set-key [backspace] 'backward-delete-char-untabify)
;                      )))
;(add-hook 'bibtex-mode-hook
;          (function (lambda ()
;                      (local-set-key "\"" 'self-insert-command))))


;;; html mode
;(autoload 'html-mode "html-mode" "HTML major mode." t)
;(setq auto-mode-alist (append '(("\\.html$" . html-mode)
;                                ("\\.htm$" . html-mode)
;                                ) auto-mode-alist))


;; packages configuration ------------------------------------------------------

;; hiliting
(setq font-lock-maximum-decoration t)
(add-hook 'emacs-lisp-mode-hook	   'turn-on-font-lock)
(add-hook 'lisp-mode-hook          'turn-on-font-lock)
(add-hook 'c-mode-hook		   'turn-on-font-lock)
(add-hook 'c++-mode-hook	   'turn-on-font-lock)
(add-hook 'asm-mode-hook           'turn-on-font-lock)
(add-hook 'makefile-mode-hook      'turn-on-font-lock)
(add-hook 'perl-mode-hook          'turn-on-font-lock)
(add-hook 'TeX-mode-hook           'turn-on-font-lock)
(add-hook 'tex-mode-hook           'turn-on-font-lock)
(add-hook 'bibtex-mode-hook        'turn-on-font-lock)
(add-hook 'texinfo-mode-hook       'turn-on-font-lock)
(add-hook 'postscript-mode-hook    'turn-on-font-lock)

(require 'font-lock)
(require 'fast-lock)
(add-hook 'font-lock-mode-hook 'turn-on-fast-lock)
(require 'pesche-font-lock)

(setq font-lock-face-attributes
      '((font-lock-comment-face       "SlateGray")
        (font-lock-string-face        "Sienna" "LightBlue")
        (font-lock-keyword-face       "Firebrick")
        (font-lock-function-name-face "Blue")
        (font-lock-variable-name-face "Black")
        (font-lock-type-face          "Black")
        (font-lock-reference-face     "ForestGreen")
        ))

(font-lock-make-faces)

; WindowsNT und Linux haben immer noch verschiedene Schriften...
(if (eq window-system 'win32)
    (progn
      (set-face-font 'font-lock-comment-face       lucida-typewriter-italic)
      (set-face-font 'font-lock-string-face        lucida-typewriter-regular)
      (set-face-font 'font-lock-keyword-face       lucida-typewriter-bold)
      (set-face-font 'font-lock-function-name-face lucida-typewriter-bold)
      (set-face-font 'font-lock-variable-name-face lucida-typewriter-regular)
      (set-face-font 'font-lock-type-face          lucida-typewriter-bold)
      (set-face-font 'font-lock-reference-face     lucida-typewriter-bold-italic)
      ))


;(eval-after-load
; "font-lock"
; '(setq font-lock-defaults-alist
;       (append '(;;(lisp-mode .           (lisp-font-lock-keywords-2))
;                 ;;(emacs-lisp-mode .     (lisp-font-lock-keywords-2))
;                 (cc-mode .             (c-font-lock-keywords-3 nil nil ((?\_ . "w"))))
;                 ;;(latex-mode .          (tex-font-lock-keywords-2))
;                 ;;(plain-tex-mode .      (tex-font-lock-keywords-2))
;                 ;;(tex-mode .            (tex-font-lock-keywords-2))
;                 (dired-mode .          (dired-font-lock-keywords))
;                 )
;               font-lock-defaults-alist)))
;(eval-after-load "font-lock" '(require 'font-lock-extra))


;; Klammer-Gegenstücke anfärben
(require 'stig-paren)
(setq blink-matching-paren nil)
(setq paren-dingaling-mode t)
(setq paren-sexp-mode nil)
;(global-set-key [?\C-\(] 'stig-paren-toggle-dingaling-mode)
;(global-set-key [?\C-\)] 'stig-paren-toggle-sexp-mode)

;; Gnus-Reader -----------------------------------------------------------------
;; weitere Konfiguration siehe .gnus
(autoload 'gnus-unplugged "gnus-agent" "Start Gnus unplugged." t)


;; gnuserv
(require 'gnuserv)
(setq gnuserv-frame (selected-frame)) ;; immer das gleiche Fenster verwenden
(gnuserv-start)

;; printing --------------------------------------------------------------------
(require 'ps-print)
(setq ps-paper-type 'ps-a4)
(setq ps-lpr-command "C:\\Progra~1\\gstools\\gs5.03\\gswin32")
(setq ps-lpr-switches '("-q -sDEVICE=djet500 -r300 -dNOPAUSE -IC:\\Progra~1\\gstools\\gs5.03;C:\\Progra~1\\gstools\\gs5.03\\fonts;c:\\psfonts"))
(setq ps-lpr-buffer (concat (getenv "TEMP") "\\psspool.ps"))

(defun win32-ps-print-buffer ()
  (interactive)
  (setq ps-print-color-p nil)
  (setq ps-bold-faces '(font-lock-keyword-face))
  (setq ps-italic-faces '(font-lock-comment-face))
  (ps-print-buffer-with-faces ps-lpr-buffer)
  (shell-command
   (apply 'concat (append (list ps-lpr-command " ")
                          ps-lpr-switches
                          (list " " ps-lpr-buffer " -c quit"))))
  )


;;; ********************
;;; Load ange-ftp, which uses the FTP protocol as a pseudo-filesystem.
;;; When this is loaded, the pathname syntax /user@host:/remote/path
;;; refers to files accessible through ftp.
;;;
;(require 'dired)
;(require 'ange-ftp)
;(setq ange-ftp-default-user "anonymous"      ; id to use for /host:/remote/path
;      ange-ftp-generate-anonymous-password t ; use $USER@`hostname`
;      ange-ftp-binary-file-name-regexp "."   ; always transfer in binary mode
;      )


;;; ********************
;;; Load the auto-save.el package, which lets you put all of your autosave
;;; files in one place, instead of scattering them around the file system.
;;;
;(setq auto-save-directory (expand-file-name "~/autosaves/")
;      auto-save-directory-fallback auto-save-directory
;      auto-save-hash-p nil
;      ;ange-ftp-auto-save t
;      ;ange-ftp-auto-save-remotely nil
;      ;; now that we have auto-save-timeout, let's crank this up
;      ;; for better interactive response.
;      auto-save-interval 2000
;      )
;(require 'auto-save)

;;; ********************
;;; Load crypt, which is a package for automatically decoding and reencoding
;;; files by various methods - for example, you can visit a .Z or .gz file,
;;; edit it, and have it automatically re-compressed when you save it again.
;;; 
;(setq crypt-encryption-type 'pgp   ; default encryption mechanism
;      crypt-confirm-password t     ; make sure new passwords are correct
;      ;crypt-never-ever-decrypt t  ; if you don't encrypt anything, set this to
;                                   ; tell it not to assume that "binary" files
;                                   ; are encrypted and require a password.
;      )
;(require 'crypt)


;; die letzte gespeicherte Session (= Desktop) laden
(load "desktop")
(desktop-load-default)
(desktop-read)
;; verschiedene Histories verkürzen, damit mit 'desktop' nicht zu viel 
;; gespeichert wird
(add-hook 'kill-emacs-hook
          '(lambda ()
             (desktop-truncate search-ring 3)
             (desktop-truncate regexp-search-ring 3)))


;; Verkünden wir, dass die Arbeit getan
(message "Finished initialization from .emacs")

;; eof .emacs
