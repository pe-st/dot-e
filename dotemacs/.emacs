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


;; general configuration -------------------------------------------------------

;; Emacs-Päckli auch in meinem lokalen emacs-Verzeichnis suchen
(setq-default load-path (append '("~/emacs") load-path))

(setq-default fill-column 77)           ;; column for line breaking in auto-fill-mode
(setq make-backup-files t)
;(put 'eval-expression 'disabled nil)    ;; enable `eval-expression'

;; Zeilen nicht automatisch umbrechen, wenn sie zu lang sind; dafür
;; einen Minor-Mode laden, damit bei langen Zeilen automatisch gescrollt wird
(setq-default truncate-lines t)
(require 'hscroll)
(setq-default hscroll-mode t)
(setq-default hscroll-mode-name nil)
(hscroll-mode)

(require 'pc-select)
(pc-bindings-mode)
(pc-selection-mode)

;; display configuration -------------------------------------------------------
(standard-display-european t)           ;; vollen 8-Bit Zeichensatz verwenden
(column-number-mode 1)
;(require 'column)
;(display-column-mode 1)

(cond (window-system
       (setq default-frame-alist
             '((width             . 81)
               (height            . 45)
;               (top               . 0)
;               (left              . 400)
;        (font              . "-*-Courier New-normal-r-*-*-13-97-*-*-c-*-*-ansi-")
               (foreground-color  . "Black")
               (background-color  . "WhiteSmoke")
               (cursor-color      . "MediumBlue")
               (icon-type         . t)         ; gnu picture as Emacs icon
               (icon-name         . nil)       ; use frame title
               ))
       (setq frame-title-format "Emacs - %b")  ; set frame title to buffer name
       (setq icon-title-format  "Emacs - %b")  ; set  icon title to buffer name
;       (setq initial-frame-alist
;             '((top               . 1)
;               (left              . 400)
;               ))
       ))

; WindowsNT und Linux haben immer noch verschiedene Schriften...
(if (eq window-system 'win32)
    (progn
      (setq win32-enable-italics t)
      ;(set-default-font "-*-Courier-normal-r-*-*-13-97-*-*-c-*-*-ansi-")
      (set-default-font "-*-Lucida Sans Typewriter-normal-r-*-*-13-97-*-*-c-*-*-ansi-")
      (set-face-font 'bold "-*-Lucida Sans Typewriter-semibold-r-*-*-13-97-*-*-c-*-*-ansi-")
      (set-face-font 'italic "-*-Lucida Sans Typewriter-normal-i-*-*-13-97-*-*-c-*-*-ansi-")
      (set-face-font 'bold-italic "-*-Lucida Sans Typewriter-semibold-i-*-*-13-97-*-*-c-*-*-ansi-")
      ))

;; keyboard configuration ------------------------------------------------------
;;; re-enable C-x C-c (is disabled by site-start.el)
;(global-set-key "\C-x\C-c" 'save-buffers-kill-emacs)

(global-set-key "\C-g" 'goto-line)
;(global-set-key "\C-x\C-a" 'insert-time-and-author) ;; self-written
;(global-set-key "\C-x\C-m" 'insert-header)          ;; self-written
(global-set-key "\C-z" 'undo)
(define-key ctl-x-map [(control <)] 'uncomment-region)       ;; self-written
(define-key ctl-x-map [(control >)] 'comment-region)       ;; standard emacs lisp


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

      (define-key global-map [(C-kp-space)] 'recenter)         ;; Ctrl-Keypad-5
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

;;; menu and toolbar customisations ---------------------------------------------
;(if (and (>= emacs-major-version 19)
;         (>= emacs-minor-version 12))
;    (progn
;      ;; remove buffer navigating items from menubar
;      (delete-menu-item '("Top"))
;      (delete-menu-item '("<<<"))
;      (delete-menu-item '(" . "))
;      (delete-menu-item '(">>>"))
;      (delete-menu-item '("Bot"))
;      )
;  )
;
;;; remove/don't display toolbar
;(if (and (featurep 'toolbar) 
;         (eq (device-type (selected-device)) 'x)) 
;    (progn
;      (remove-specifier default-toolbar 'global)
;      (add-spec-list-to-specifier default-toolbar 'nil)
;      )
;  )



;; mode specific configuration -------------------------------------------------
;;(setq default-major-mode 'text-mode)
;=(setq text-mode-hook 'turn-on-auto-fill)
;;(setq indent-tabs-mode 't)

;;; info system: find local info files
;(if (boundp 'Info-directory-list)
;    (setq Info-directory-list
;          (append Info-directory-list '("~/info/" "/opt/local/GNU/info/")))
;  )

;; lisp modes
(setq auto-mode-alist (append '(("\\.el$" . emacs-lisp-mode)
                                ("\\.emacs$" . emacs-lisp-mode)
                                ) auto-mode-alist))
(add-hook 'emacs-lisp-mode-hook
          (function (lambda ()
                      (setq-default tab-width        8
                                    indent-tabs-mode nil)
                      )))

;; c++ mode

;;       ;; take the newer c++ mode from cc-mode, not the BOCM (Boring Old C Mode)
;;       (fmakunbound 'c-mode)
;;       (makunbound 'c-mode-map)
;;       (fmakunbound 'c++-mode)
;;       (makunbound 'c++-mode-map)
;;       (makunbound 'c-style-alist)
;       (autoload 'c++-mode "cc-mode" "C++ Editing Mode" t)
;       (autoload 'c-mode   "cc-mode" "C Editing Mode" t)
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
			     )))


;; makefile mode
;=(autoload 'makefile-mode "makefile" "Makefile Editing Mode" t)
;=(setq auto-mode-alist (append '(("[Mm]akefile$" . makefile-mode)
;=                                ("\\.mk$" . makefile-mode)
;=                                ("\\.mak$" . makefile-mode)
;=                                ) auto-mode-alist))
;=(add-hook 'makefile-mode-hook
;=          (function (lambda ()
;=                      (setq-default tab-width        8
;=                                    indent-tabs-mode t)
;=                      )))

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
(add-hook 'TeX-mode-hook           'turn-on-font-lock)
(add-hook 'tex-mode-hook           'turn-on-font-lock)
(add-hook 'bibtex-mode-hook        'turn-on-font-lock)
(add-hook 'texinfo-mode-hook       'turn-on-font-lock)
(add-hook 'postscript-mode-hook    'turn-on-font-lock)
(add-hook 'makefile-mode-hook      'turn-on-font-lock)
(add-hook 'perl-mode-hook          'turn-on-font-lock)

(require 'font-lock)
(require 'fast-lock)
(add-hook 'font-lock-mode-hook 'turn-on-fast-lock)

(setq font-lock-face-attributes
      '((font-lock-comment-face "SlateGray")
        (font-lock-string-face "Sienna" "LightBlue")
        (font-lock-keyword-face "Firebrick")
        (font-lock-function-name-face "Blue")
        (font-lock-variable-name-face "Black")
        (font-lock-type-face "Black")
        (font-lock-reference-face "ForestGreen")))

(font-lock-make-faces)

; WindowsNT und Linux haben immer noch verschiedene Schriften...
(if (eq window-system 'win32)
    (progn
      (set-face-font 'font-lock-comment-face "-*-Lucida Sans Typewriter-normal-i-*-*-13-97-*-*-c-*-*-ansi-")
      (set-face-font 'font-lock-keyword-face "-*-Lucida Sans Typewriter-semibold-r-*-*-13-97-*-*-c-*-*-ansi-")
      (set-face-font 'font-lock-function-name-face "-*-Lucida Sans Typewriter-semibold-r-*-*-13-97-*-*-c-*-*-ansi-")
      (set-face-font 'font-lock-type-face "-*-Lucida Sans Typewriter-semibold-r-*-*-13-97-*-*-c-*-*-ansi-")
      (set-face-font 'font-lock-reference-face "-*-Lucida Sans Typewriter-semibold-i-*-*-13-97-*-*-c-*-*-ansi-")
      ))


;; Klammer-Gegenstücke anfärben
(require 'stig-paren)
(setq blink-matching-paren nil)
(setq paren-dingaling-mode t)
(setq paren-sexp-mode nil)
;(global-set-key [?\C-\(] 'stig-paren-toggle-dingaling-mode)
;(global-set-key [?\C-\)] 'stig-paren-toggle-sexp-mode)

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

;;; func-menu is a package that scans your source file for function definitions
;;; and makes a menubar entry that lets you jump to any particular function
;;; definition by selecting it from the menu.  The following code turns this on
;;; for all of the recognized languages.  Scanning the buffer takes some time,
;;; but not much.
;;;
;(cond ((string-match "Lucid" emacs-version)
;       (require 'func-menu)
;       (define-key global-map 'f8 'function-menu)
;       (add-hook 'find-file-hooks 'fume-add-menubar-entry)
;       (define-key global-map "\C-cg" 'fume-prompt-function-goto)
;       (define-key global-map '(shift button3) 'mouse-function-menu)
;       (define-key global-map '(meta  button1) 'fume-mouse-function-goto)
;       (setq fume-menubar-menu-location nil)
;       (setq fume-display-in-modeline-p nil)
;       ))


;; tell the user that we're through it
(message "Finished initialization from .emacs")

;; eof .emacs
