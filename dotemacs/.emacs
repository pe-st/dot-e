;;----------------------------------------------------------- -*- Emacs-Lisp -*-
;;  Emacs Startup File
;;
;;      Author: Peter Steiner <unistein@isbe.ch>
;;     Created: Wed Jul 6 19:52:18 1994
;;     $Source: g:/archiv/cvsroot/home/.emacs,v $
;;   $Revision: 1.17 $
;;       $Date: 1999/04/22 20:22:41 $
;;     $Author: pesche $


;; setup special variables and functions ---------------------------------------

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

;; general configuration -------------------------------------------------------

;; Emacs-Päckli auch in meinem lokalen emacs-Verzeichnis suchen
;; für Emacsen < 20 noch ein zusätzliches Verzeichnis emacs19
(if (<= emacs-major-version 19)
    (if (file-accessible-directory-p "~/emacs/emacs19")
        (setq load-path (append '("~/emacs/emacs19") load-path))))
(if (file-accessible-directory-p "~/emacs")
    (setq load-path (append '("~/emacs") load-path)))
(if (file-accessible-directory-p "~/site-lisp")
    (setq load-path (append '("~/site-lisp") load-path)))

(setq-default fill-column 77)           ;; column for line breaking in auto-fill-mode
(setq make-backup-files t)
(put 'eval-expression 'disabled nil)    ;; enable `eval-expression'

;; Zeilen nicht automatisch umbrechen, wenn sie zu lang sind; dafür
;; einen Minor-Mode laden, damit bei langen Zeilen automatisch gescrollt wird
;; (ab Emacs 20 über customize...)
(setq-default truncate-lines t)
(if (<= emacs-major-version 19)
    (progn
      (require 'hscroll)
      (setq-default hscroll-mode t)
      (setq-default hscroll-mode-name nil)
      (hscroll-mode)
      )
    )
(require 'scroll-in-place)

;; die gewohnten Windows-Shortcuts C-z, C-x, C-c, C-v möglichst beibehalten
; (load "cua-mode")
; (CUA-mode t)
(require 'pc-select)
(pc-bindings-mode)
(pc-selection-mode)


;; display configuration -------------------------------------------------------
(standard-display-european t)           ;; vollen 8-Bit Zeichensatz verwenden
(column-number-mode 1)


;; fonts -----------------------------------------------------------------------
;; Paare von Werten (mindestens für Lucida Sans Typewriter)
;; 10-75 / 11-82 / 12-90 / 13-97 / 14-105 / 15-112
(if (eq (string-match "PIAZZA" (system-name)) 0)
    (defvar my-font-size "13-97")  ;; auf PIAZZA: etwas grösser (ca. 9.7 Punkt)
    (defvar my-font-size "11-82")  ;; sonst (ca. 8.2 Punkt)
    )

(defvar lucida-typewriter-regular
  (concat "-*-Lucida Sans Typewriter-normal-r-*-*-"
          my-font-size "-*-*-c-*-iso8859-1"))
(defvar lucida-typewriter-italic
  (concat "-*-Lucida Sans Typewriter-normal-i-*-*-"
          my-font-size "-*-*-c-*-iso8859-1"))
(defvar lucida-typewriter-bold
  (concat "-*-Lucida Sans Typewriter-semibold-r-*-*-"
          my-font-size "-*-*-c-*-iso8859-1"))
(defvar lucida-typewriter-bold-italic
  (concat "-*-Lucida Sans Typewriter-semibold-i-*-*-"
          my-font-size "-*-*-c-*-iso8859-1"))


; WindowsNT und Linux haben immer noch verschiedene Schriften...
(if (or (eq window-system 'win32)
        (eq window-system 'w32))
    (progn
      ; Workaround für 20.3.7.1 von Jason Rumney <jasonr@altavista.net>
      ; (03 Apr 1999 in ntemacs-users)
      (if (not (eq (string-match "20.3.7.1" (emacs-version)) nil))
          (set-w32-system-coding-system 'raw-text))
      (setq win32-enable-italics t)
      (setq w32-enable-italics t)
      (set-default-font           lucida-typewriter-regular)
      (set-face-font 'default     lucida-typewriter-regular)
      (set-face-font 'bold        lucida-typewriter-bold)
      (set-face-font 'italic      lucida-typewriter-italic)
      (set-face-font 'bold-italic lucida-typewriter-bold-italic)
      )
  )

(cond (window-system
       ;; default-Parameter für alle Fenster
       (setq default-frame-alist
             '((width             . 81)
               (height            . 45)
               (foreground-color  . "Black")
               (background-color  . "WhiteSmoke")
               (cursor-color      . "MediumBlue")
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


;; keyboard configuration ------------------------------------------------------
;; der portabelste Weg zur Tasten-Definition scheint mittels (read-kbd-macro XX)
;; zu sein. In neueren Emacsen gibt es dafür die kürzere Form (kbd XX). In
;; älteren Emacsen kann mit folgendem Code auch schon (kbd XX) verwendet werden:
(or (fboundp 'kbd)                      ; nur wenn noch nicht definiert
    (defmacro kbd (keys)
      "Convert KEYS to the internal Emacs key representation.
KEYS should be a string constant in the format used for
saving keyboard macros (see insert-kbd-macro)."
      (let ((f 'read-kbd-macro))
        (funcall f keys))))


(global-set-key (kbd "C-g") 'goto-line)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-x C-<") 'uncomment-region)      ;; self-written
(global-set-key (kbd "C-x C-\\") 'uncomment-region)     ;; zB Dos-Box: C-< == C-\
(global-set-key (kbd "C-x C->") 'comment-region)        ;; standard emacs lisp
(global-set-key (kbd "C-<kp-space>") 'recenter)         ;; Shift-Ctrl-Keypad-5 (NT)
(global-set-key (kbd "C-<kp-begin>") 'recenter)         ;; Shift-Ctrl-Keypad-5 (Linux)
(global-set-key (kbd "C-<return>") 'duplicate-line)     ;; self-written
(global-set-key (kbd "C-<kp-enter>") 'duplicate-line)   ;; self-written

(global-set-key (kbd "<f6>") 'other-window)             ;; C-x o
(global-set-key (kbd "C-<f6>") 'bury-buffer)            ;;
(global-set-key (kbd "C-<tab>") 'bury-buffer)           ;;
(global-set-key (kbd "S-C-<tab>") 'unbury-buffer)       ;;

(global-set-key (kbd "C-f") 'nonincremental-re-search-forward)          ;;
(global-set-key (kbd "S-C-f") 'nonincremental-repeat-re-search-forward) ;;
(global-set-key (kbd "S-C-s") 'nonincremental-repeat-search-forward)    ;;
(global-set-key (kbd "<f3>") 'nonincremental-repeat-search-forward)     ;;
(global-set-key (kbd "S-C-q") 'search-quick)            ;; self-written

(global-set-key (kbd "<f9>") 'insert-timestamp)         ;; self-written

;(global-set-key (kbd "C-S-<down>") 'next-error)


; mit Maus erzeugtes imenu in der Konsole lässt emacs abstürzen.
(cond (window-system
       (global-set-key (kbd "S-<mouse-2>") 'imenu)))

;; für mehrere Modi gemeinsames Zeugs ------------------------------------------

; Menus etwas anpassen an grosse Bildschirme
(setq imenu-max-items 40)
(setq buffers-menu-max-size 30)

(require 'msb)
(setq msb-max-menu-items 30)
(setq msb-display-most-recently-used 30)

; Anzeige des Funktionsnamens in der Modeline
(setq which-func-maxout         0         ;; enabled, regardless buffer size
      which-func-mode-global    t)
(if (< emacs-major-version 20)
    (require 'which-function)
  (require 'which-func))
(add-to-list 'which-func-modes 'cperl-mode)
; aus irgendeinem Grund muss man den Modus zweimal toggeln...
(which-func-mode 0)
(which-func-mode 1)

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
;; den Pfad zu den Man-Pages aus GCC_EXEC_PREFIX basteln
(let ((path (getenv "GCC_EXEC_PREFIX")))
  (if (stringp path)
      (progn
        ;; die Backslashes durch Slashes ersetzen
        (while (setq i (string-match "[\\]" path))
          (aset path i ?/))
        ;; den hinteren Teil des Pfades durch 'man' ersetzen
        (string-match "H-i386-cygwin32/lib/gcc-lib/" path)
        (setq woman-manpath (list (replace-match "man" t t path)))
        )))

;(setq man-path '("x:/gnuwin32/b18/man"))
;(setq woman-path '("x:\gnuwin32\b18\man" "e:\usr\man"))


;; lisp modes
(setq auto-mode-alist (append '(("\\.el$" . emacs-lisp-mode)
                                ("\\.emacs$" . emacs-lisp-mode)
                                ) auto-mode-alist))
(defun pesche-emacs-lisp-mode-hook()
  (setq-default tab-width        8
                indent-tabs-mode nil)

  ;; Syntax etwas anpassen, damit (zB) Markieren mit Doppelklick nicht
  ;; bei '-' oder Umlauten Halt macht
  (modify-syntax-for-umlaut)
  (modify-syntax-entry ?- "w")

  ;; alle Kommentarzeilen, die mit mindestens drei '-' aufhören,
  ;; in das 'Outline'-Menü eintragen
  (setq imenu-generic-expression
        (append imenu-generic-expression
                '(("Outline" ";+[ \\t]+\\([ A-Za-z0-9äöüÄÖÜ/+]+\\)---*[ \\t]*$" 1))))
  (imenu-add-to-menubar "Index")
  )
(add-hook 'emacs-lisp-mode-hook 'pesche-emacs-lisp-mode-hook)

;; C mode und alle Verwandten --------------------------------------------------
;; cc-mode 5.21 kennt das "richtige" Verhalten von Delete und Backspace
;; (aber nur wenn delete-key-deletes-forward existiert)
(if (not (boundp 'delete-key-deletes-forward))
    (defvar delete-key-deletes-forward))
(setq delete-key-deletes-forward t)

(defun pesche-c-mode-common-hook()
  ;; das 'elektrische' automatische Einrücken bei Kommentaren ist lästig...
  (local-unset-key (kbd "*"))
  (local-unset-key (kbd "/"))
  ;; auch in C die neuen C++-Kommentare verwenden
  (if (fboundp 'c-enable-//-in-c-mode)
      (c-enable-//-in-c-mode))
  (setq comment-start "// "
        comment-end ""
        comment-multi-line nil
        font-lock-comment-start-regexp nil
        c-double-slash-is-comments-p t)
  ;; Syntax etwas anpassen, damit (zB) Markieren mit Doppelklick nicht
  ;; bei '_' oder Umlauten Halt macht
  (modify-syntax-entry ?_ "w")
  (modify-syntax-for-umlaut)
  ;; Mein Codierstil ist ein abgeänderter 'Stroustrup'
  (c-set-style  "Stroustrup")
  (c-set-offset 'case-label '+)
  (c-set-offset 'statement-case-open '+)
  (c-set-offset 'arglist-close 0)
  (setq-default tab-width                4
                indent-tabs-mode         nil
                c-tab-always-indent      nil
                c++-tab-always-indent    nil)

  ;; alle 'Kästchen' in das 'Outline'-Menü aufnehmen
  (setq imenu-generic-expression
        (append imenu-generic-expression
                '(("Outline"
                   "^/\\*[-]+\\+[ \t]*\n|[ \t]+\\([^ \t][- A-Za-zÄÖÜäöü0-9+]*\\).*|"
                   1)
                  ("Types"
                   "^[ \t]*typedef[ \t]+\\(struct[ \t]+[_A-Za-z0-9]+\\)"
                   1)
                  )))
  (imenu-add-to-menubar "Index")
  )
(add-hook 'c-mode-common-hook 'pesche-c-mode-common-hook)


;; makefile mode ---------------------------------------------------------------
;; Files auf .mak sollen auch Makefiles sein (ist offenbar nicht üblich)
(setq auto-mode-alist (append '(("\\.mak\\'" . makefile-mode)) auto-mode-alist))

;; Makefiles im msb-Buffermenü als Makefiles einordnen (bei cperl abgeschaut)
(defvar msb-menu-cond)
;(defvar makefile-msb-fixed)
(defun makefile-msb-fix()
  ;; Adds makefiles to msb menu, supposes that msb is already loaded
;  (setq makefile-msb-fixed t)
  (let* ((l (length msb-menu-cond))
         (last (nth (1- l) msb-menu-cond))
         (precdr (nthcdr (- l 2) msb-menu-cond)) ; cdr of this is last
         (handle (1- (nth 1 last))))
    (setcdr precdr (list
                    (list
                     '(eq major-mode 'makefile-mode)
                     handle
                     "Makefiles (%d)")
                    last))))

(defun pesche-makefile-mode-hook()
  (setq-default tab-width        8
                indent-tabs-mode t)

  ; korrigiere regexp für Zuweisungen (Fehler tritt nur
  ; bei imenu auf, font-lock ist okay)
  (setq makefile-macroassign-regex
        "^\\([^ \n\t#][^:#= \t\n]*\\)[ \t]*[*:+]?:?=")
  ;               --^--  dieses '#' fehlt in make-mode.el

  (imenu-add-to-menubar "Index")
;   (and (boundp 'msb-menu-cond)
;        (not makefile-msb-fixed)
;        (makefile-msb-fix))
  (makefile-msb-fix)
  )

(add-hook 'makefile-mode-hook 'pesche-makefile-mode-hook)


;; assembler mode --------------------------------------------------------------
(defun pesche-asm-mode-hook()
  (local-unset-key (kbd "<tab>"))
  (local-unset-key (kbd ":"))
  (local-unset-key (kbd ";"))
  (local-unset-key (kbd "RET"))
  (setq-default tab-width        8
                indent-tabs-mode t)

  ;; Syntax etwas anpassen, damit (zB) Markieren mit Doppelklick nicht
  ;; bei '_' oder Umlauten Halt macht
  (modify-syntax-for-umlaut)
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?. "w")  ; praktisch für move.l etc
  (modify-syntax-entry ?- "w")  ; praktisch in "C-Kommentaren"
  (modify-syntax-entry ?> "w")  ; praktisch in "C-Kommentaren"

;  (imenu-add-to-menubar "Index")
  )
(add-hook 'asm-mode-hook 'pesche-asm-mode-hook)

;; perl mode -------------------------------------------------------------------
(autoload 'cperl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
;; cperl-mode statt perl-mode verwenden
(setq auto-mode-alist
      (append '(("\\.[pP][Llm]$" . cperl-mode)) auto-mode-alist ))
(setq interpreter-mode-alist (append interpreter-mode-alist
         '(("miniperl" . cperl-mode))))

;; hairy ist etwas allzu haarig...
;(setq cperl-hairy t)


(defun pesche-cperl-mode-hook()
  ;; einrücken: 4 Zeichen, 'else' darf ohne '{' auf eigener Zeile stehen
  (cperl-set-style "C++")
  (imenu-add-to-menubar "Index")
  )

(add-hook 'cperl-mode-hook 'pesche-cperl-mode-hook)


;; html mode -------------------------------------------------------------------
(autoload 'html-helper-mode "html-helper-mode" "HTML major mode." t)
;; html-helper-mode statt html-mode verwenden
(setq auto-mode-alist
      (append '(("\\.s?html?\\'" . html-helper-mode)) auto-mode-alist))
(setq html-helper-use-expert-menu t)

;; HTML-Files im msb-Buffermenü als solche einordnen (bei cperl abgeschaut)
(defvar msb-menu-cond)
(defun html-msb-fix()
  (let* ((l (length msb-menu-cond))
         (last (nth (1- l) msb-menu-cond))
         (precdr (nthcdr (- l 2) msb-menu-cond)) ; cdr of this is last
         (handle (1- (nth 1 last))))
    (setcdr precdr (list
                    (list
                     '(eq major-mode 'html-helper-mode)
                     handle
                     "Web Files (%d)")
                    last))))

(defun pesche-html-helper-mode-hook()
  (html-msb-fix)
  )

(add-hook 'html-helper-mode-hook 'pesche-html-helper-mode-hook)


;; dynamische Abkürzungen ------------------------------------------------------
;; immer case-sensitiv !
(setq dabbrev-case-fold-search nil)
(setq dabbrev-case-replace nil)

;; font lock -------------------------------------------------------------------
; (global-font-lock-mode t)
; (setq font-lock-support-mode 'lazy-lock-mode)
(setq font-lock-maximum-decoration t)
(add-hook 'emacs-lisp-mode-hook    'turn-on-font-lock)
(add-hook 'lisp-mode-hook          'turn-on-font-lock)
(add-hook 'c-mode-hook             'turn-on-font-lock)
(add-hook 'c++-mode-hook           'turn-on-font-lock)
(add-hook 'asm-mode-hook           'turn-on-font-lock)
(add-hook 'makefile-mode-hook      'turn-on-font-lock)
(add-hook 'cperl-mode-hook         'turn-on-font-lock)
(add-hook 'html-helper-mode-hook   'turn-on-font-lock)
(add-hook 'TeX-mode-hook           'turn-on-font-lock)
(add-hook 'tex-mode-hook           'turn-on-font-lock)
(add-hook 'bibtex-mode-hook        'turn-on-font-lock)
(add-hook 'texinfo-mode-hook       'turn-on-font-lock)
(add-hook 'postscript-mode-hook    'turn-on-font-lock)
(add-hook 'outline-mode-hook       'turn-on-font-lock)


;; cperl verändert auf 'unfreundliche' Art constant-face,
;; aber wir setzen unserer Kopf durch!
(copy-face 'bold-italic 'font-lock-constant-face)
(set-face-foreground 'font-lock-constant-face "ForestGreen")
(copy-face 'font-lock-constant-face 'font-lock-reference-face)

(setq font-lock-face-attributes
      '((font-lock-comment-face       "SlateGray")
        (font-lock-string-face        "Sienna" "LightBlue")
        (font-lock-keyword-face       "Firebrick")
        (font-lock-function-name-face "Blue")
        (font-lock-variable-name-face "Blue")
        (font-lock-type-face          "Black")
; nicht mehr nötig, da dies weiter oben mit dem 'Brecheisen' geschieht
;        (font-lock-constant-face      "ForestGreen")    ;; 20.x
;        (font-lock-reference-face     "ForestGreen")    ;; 19.x
        (info-node                    "Blue")
        (info-xref                    "Blue")
        (makefile-space-face          nil "HotPink")
        ))

(require 'font-lock)
; (require 'fast-lock)
; (add-hook 'font-lock-mode-hook 'turn-on-fast-lock)
(require 'pesche-font-lock)

(cond (window-system
       (if (<= emacs-major-version 19)
           (font-lock-make-faces))))


; WindowsNT und Linux haben immer noch verschiedene Schriften...
(if (or (eq window-system 'win32)
        (eq window-system 'w32))
    (progn
      (set-face-font 'font-lock-comment-face       lucida-typewriter-italic)
      (set-face-font 'font-lock-string-face        lucida-typewriter-regular)
      (set-face-font 'font-lock-keyword-face       lucida-typewriter-bold)
      (set-face-font 'font-lock-function-name-face lucida-typewriter-bold)
      (set-face-font 'font-lock-variable-name-face lucida-typewriter-regular)
      (set-face-font 'font-lock-type-face          lucida-typewriter-bold)
; nicht mehr nötig, da dies weiter oben mit dem 'Brecheisen' geschieht
;       (set-face-font 'font-lock-constant-face      lucida-typewriter-bold-italic)
;       (if (eq window-system 'win32)
;           (set-face-font 'font-lock-reference-face lucida-typewriter-bold-italic))
      ))


;; verschiedene andere Modi ----------------------------------------------------
; die generic-Modi sollten erst nach dem font-lock Setup geladen werden,
; da sie sonst ihre eigene Initialisierung vornehmen
(require 'generic-pilrc)        ; Palm/Pilot Ressourcen-Definitionen
(require 'generic-x)
(require 'pplog-mode)


;; Klammer-Gegenstücke anfärben
(require 'stig-paren)
(setq blink-matching-paren nil)
(setq paren-dingaling-mode t)
(setq paren-sexp-mode nil)
;(global-set-key [?\C-\(] 'stig-paren-toggle-dingaling-mode)
;(global-set-key [?\C-\)] 'stig-paren-toggle-sexp-mode)

;; News und Mail ---------------------------------------------------------------
;; weitere Konfiguration siehe .gnus
(autoload 'epop3-mail "epop3mail" "Get mail from pop server" t)
(autoload 'gnus-unplugged "gnus-agent" "Start Gnus unplugged." t)
(setq gnus-directory "~/gnus/")


;; gnuserv (nur mit NT)
(if (or (eq window-system 'win32)
        (eq window-system 'w32))
    (progn
      (require 'gnuserv)
      (setq gnuserv-frame (selected-frame)) ;; immer das gleiche Fenster verwenden
      (gnuserv-start)
      ))

;; printing --------------------------------------------------------------------
(require 'pesche-print)

;; Pesches menu ----------------------------------------------------------------
(require 'pesche-menu)


;; desktop ---------------------------------------------------------------------
;; die letzte gespeicherte Session (= Desktop) laden
(load "desktop")
; für NT den Default-Namen ändern; für andere Systeme können wir seinlassen
(if (eq system-type 'windows-nt)
    (setq desktop-basefilename ".emacs.desktop"))
(setq desktop-missing-file-warning nil)
; die Tabulator-Einstellungen auch mit abspeichern
(setq-default desktop-locals-to-save (append '(tab-width
                                               indent-tabs-mode
                                               c-tab-always-indent
                                               c++-tab-always-indent)
                                             desktop-locals-to-save))
(desktop-load-default)

;; Bevor der gespeicherte Desktop geladen werden darf, ändern wir
;; noch einen während der Übersetzung von Emacs hart codierten Pfad.
;; Falls nämlich ein Info-Buffer existierte, versucht desktop ein
;; (require 'info), welches dann zu einem Zugriff auf source-directory
;; führt. Da dieses bei 20.3.1 auf d:\\irgendwas zeigt, kommt es zum
;; Zugriff auf das möglicherweise leere MO-Laufwerk.
(setq source-directory "c:\\emacs\\")   ;; Inhalt unwichtig, solange nicht D:
(desktop-read)

;; verschiedene Histories verkürzen, damit mit 'desktop' nicht zu viel
;; gespeichert wird
(add-hook 'kill-emacs-hook
          '(lambda ()
             (desktop-truncate search-ring 3)
             (desktop-truncate regexp-search-ring 3)))


;; Verkünden wir, dass die Arbeit getan
(message "Finished initialization from .emacs")

;; customize -------------------------------------------------------------------

;; falls unser emacs custom noch nicht hat, definieren wir einige Dummies,
;; damit es keine Fehlermeldungen gibt
(or (fboundp 'custom-set-variables)     ; nur wenn noch nicht definiert
    (defun custom-set-variables (&rest args)
      ;; do nothing
      ))
(or (fboundp 'custom-set-faces)         ; nur wenn noch nicht definiert
    (defun custom-set-faces (&rest args)
      ;; do nothing
      ))

(custom-set-variables
 '(hscroll-mode-name nil)
 '(resize-minibuffer-mode t nil (rsz-mini))
 '(scroll-preserve-screen-position t t)
 '(hscroll-global-mode t nil (hscroll)))
(custom-set-faces)

