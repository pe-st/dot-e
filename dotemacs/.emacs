;;----------------------------------------------------------- -*- Emacs-Lisp -*-
;;  Emacs Startup File
;;
;;      Author: Peter Steiner <pesche@schlau.ch>
;;         $Id: //netzadmin/emacs/pesche/.emacs#39 $
;;     $Change: 18323 $
;;   $DateTime: 2004/01/05 09:09:46 $
;;     $Author: peter.steiner $
;;    $Created: Wed Jul 6 19:52:18 1994 $


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

(require 'pesche-tools)
(require 'pesche-compile)

(setq make-backup-files t)
(put 'eval-expression 'disabled nil)    ;; enable `eval-expression'

;; filling and sentences -------------------------------------------------------
;; fill: Zeilenumbruch etc.
(setq-default fill-column 72)
(require 'filladapt)

;; Sätze mit einem Leerzeichen abschliessen. Vgl. Info Node 'Sentences'
(setq-default sentence-end-double-space nil
              sentence-end "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*")

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
(if (or (eq window-system 'win32)
        (eq window-system 'w32))
    (standard-display-european t))      ;; vollen 8-Bit Zeichensatz verwenden
(column-number-mode 1)

;; fonts -----------------------------------------------------------------------
;; Paare von Werten (gilt unter Windows; der erste Werte ist die Höhe in
;; Pixel, der zweite die Grösse in Zehntel-Punkt). Die Paare findet man
;; zB durch Evaluieren von (insert (prin1-to-string (w32-select-font)))
;; 9-67 / 10-75 / 11-82 / 12-90 / 13-97 / 14-105 / 15-112 / 16-120
;; Unter Linux kommt es auf die Auflösung drauf an.
;;  75 dpi : 10-100 / 12-120 / 14-140 / 18-180 / 24-240
;; 100 dpi : 11-80 / 14-100 / 17-120 / 20-140 / 25-180
(defvar pesche-font-size nil "Pesches bevorzugte Fontgrösse je nach Maschine")
(defvar pesche-family    nil "Pesches bevorzugte Schriftart je nach Maschine")

(cond
 ((or (eq window-system 'win32)
      (eq window-system 'w32))
  (progn
    ;; Workaround für 20.3.7.1 von Jason Rumney <jasonr@altavista.net>
    ;; (03 Apr 1999 in ntemacs-users)
    (if (not (eq (string-match "20.3.7.1" (emacs-version)) nil))
        (set-w32-system-coding-system 'raw-text))
    (setq win32-enable-italics t)
    (setq w32-enable-italics t)
    (defvar font09pix "9-67-*-*-c-*-")
    (defvar font10pix "10-75-*-*-c-*-")
    (defvar font11pix "11-82-*-*-c-*-")
    (defvar font12pix "12-90-*-*-c-*-")
    (defvar font13pix "13-97-*-*-c-*-")
    (defvar fontcharset "iso10646-1")
    (defvar fontstring-courier "Courier New")
    (defvar font-rr-courier "normal-r")
    (defvar font-ri-courier "normal-i")
    (defvar font-br-courier "bold-r")
    (defvar font-bi-courier "bold-i")
    (defvar fontstring-lucida "Lucida Sans Typewriter")
    (defvar font-rr-lucida "normal-r")
    (defvar font-ri-lucida "normal-i")
    (defvar font-br-lucida "semibold-r")
    (defvar font-bi-lucida "semibold-i")
    ))
 (t (progn ;; die Linux-Variante
      (defvar font10pix "10-100-75-75-m-*-")
      (defvar font12pix "12-120-75-75-m-*-")
      (defvar font14pix "14-140-75-75-m-*-")
      (defvar fontcharset "iso8859-1")
      (defvar fontstring-courier "courier")
      (defvar font-rr-courier "medium-r")
      (defvar font-ri-courier "medium-o")
      (defvar font-br-courier "bold-r")
      (defvar font-bi-courier "bold-o")
      (defvar fontstring-lucida "lucidatypewriter")
      (defvar font-rr-lucida "medium-r")
      (defvar font-ri-lucida "medium-i")
      (defvar font-br-lucida "bold-r")
      (defvar font-bi-lucida "bold-i")
      )))

(cond
 ((eq (string-match "DONNERVOGEL" (system-name)) 0)
  (progn (setq pesche-family "lucida")  (setq pesche-font-size font13pix)))
 ((eq (string-match "INUVIK" (system-name)) 0)
  (progn (setq pesche-family "lucida")  (setq pesche-font-size font11pix)))
 ((eq (string-match "PIAZZABOOK"  (system-name)) 0)
  (progn (setq pesche-family "courier") (setq pesche-font-size font12pix)))
 (t
  (progn (setq pesche-family "courier") (setq pesche-font-size font12pix)))
 )

; Beispiel "-outline-Lucida Sans Typewriter-normal-r-normal-normal-16-120-96-96-c-*-iso10646-1"
(defvar pesche-lucida-rr
  (concat "-*-" fontstring-lucida "-" font-rr-lucida "-*-*-" pesche-font-size fontcharset))
(defvar pesche-lucida-ri
  (concat "-*-" fontstring-lucida "-" font-ri-lucida "-*-*-" pesche-font-size fontcharset))
(defvar pesche-lucida-br
  (concat "-*-" fontstring-lucida "-" font-br-lucida "-*-*-" pesche-font-size fontcharset))
(defvar pesche-lucida-bi
  (concat "-*-" fontstring-lucida "-" font-bi-lucida "-*-*-" pesche-font-size fontcharset))

; Beispiel "-outline-Courier New-normal-r-normal-normal-16-120-96-96-c-*-iso10646-1"
(defvar pesche-courier-rr
  (concat "-*-" fontstring-courier "-" font-rr-courier "-*-*-" pesche-font-size fontcharset))
(defvar pesche-courier-ri
  (concat "-*-" fontstring-courier "-" font-ri-courier "-*-*-" pesche-font-size fontcharset))
(defvar pesche-courier-br
  (concat "-*-" fontstring-courier "-" font-br-courier "-*-*-" pesche-font-size fontcharset))
(defvar pesche-courier-bi
  (concat "-*-" fontstring-courier "-" font-bi-courier "-*-*-" pesche-font-size fontcharset))

(cond
 ((eq (string-match "lucida" pesche-family) 0)
  (progn
    (defvar pesche-default-regular     pesche-lucida-rr)
    (defvar pesche-default-italic      pesche-lucida-ri)
    (defvar pesche-default-bold        pesche-lucida-br)
    (defvar pesche-default-bold-italic pesche-lucida-bi)))
 (t
  (progn
    (defvar pesche-default-regular     pesche-courier-rr)
    (defvar pesche-default-italic      pesche-courier-ri)
    (defvar pesche-default-bold        pesche-courier-br)
    (defvar pesche-default-bold-italic pesche-courier-bi))))

;; Doch noch eine weitere Fallunterscheidung: Beim Mac arbeiten wir
;; bereits mit fontsets
(cond (window-system
       (if (not (eq window-system 'mac))
           (progn
             ; Windows oder Linux
             (set-default-font           pesche-default-regular)
             (set-face-font 'default     pesche-default-regular)
             (set-face-font 'bold        pesche-default-bold)
             (set-face-font 'italic      pesche-default-italic)
             (set-face-font 'bold-italic pesche-default-bold-italic)
             )
         (progn
           ; Mac

           (create-fontset-from-fontset-spec
            "-apple-monaco-medium-r-normal--9-*-*-*-*-*-fontset-monaco,
ascii:-apple-monaco-medium-r-normal--9-90-75-75-m-90-mac-roman,
latin-iso8859-1:-apple-monaco-medium-r-normal--9-90-75-75-m-90-mac-roman")

           (set-frame-font "fontset-monaco")
           (setq mac-keyboard-text-encoding kTextEncodingISOLatin1)
           )
         )))

(cond (window-system
       ;; default-Parameter für alle Fenster
       (setq default-frame-alist
             '((width             . 81)
               (height            . 45)
               (foreground-color  . "Black")
               (background-color  . "WhiteSmoke")
               (cursor-color      . "MediumBlue")
;               (icon-type         . t)         ; gnu picture as Emacs icon
               (icon-name         . nil)       ; use frame title
               ))
       ;; Spezial-Behandlung des ersten Fensters
       (setq initial-frame-alist
             '( ; (top . 1) (left . 400)
               (width . 90) (height . 50)
;               (icon-type         . t)         ; gnu picture as Emacs icon
               (icon-name         . nil)       ; use frame title
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

; Seit Emacs 20.4 können die drei zusätzlichen Windows-Tasten als Modifier
; verwendet werden. Ich verwende nur die linke Fenster-Taste (als Hyper)
(if (and (>= emacs-major-version 20)
         (>= emacs-minor-version 4)
         (eq window-system 'w32))
    (progn
      (setq w32-lwindow-modifier 'hyper)
;       (setq w32-rwindow-modifier 'meta)
;       (setq w32-apps-modifier 'super)
      (setq w32-pass-lwindow-to-system nil)
;       (setq w32-pass-rwindow-to-system nil)
      ))

(global-set-key (kbd "C-g") 'goto-line)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "H-z") 'undo)
(global-set-key (kbd "H-x") 'kill-region)
(global-set-key (kbd "H-c") 'copy-region-as-kill-nomark)
(global-set-key (kbd "H-v") 'yank)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-x C-<") 'uncomment-region)      ;; self-written
(global-set-key (kbd "C-x C-\\") 'uncomment-region)     ;; zB Dos-Box: C-< == C-\
(global-set-key (kbd "C-x C->") 'comment-region)        ;; standard emacs lisp
(global-set-key (kbd "C-<kp-space>") 'recenter)         ;; Shift-Ctrl-Keypad-5 (NT)
(global-set-key (kbd "C-<kp-begin>") 'recenter)         ;; Shift-Ctrl-Keypad-5 (Linux)
(global-set-key (kbd "C-<return>") 'duplicate-line)     ;; self-written
(global-set-key (kbd "C-<kp-enter>") 'duplicate-line)   ;; self-written

;; einige Korrekturen für den Mac
(global-set-key (kbd "C-<kp-home>") 'beginning-of-buffer)
(global-set-key (kbd "C-<kp-end>") 'end-of-buffer)
(global-set-key (kbd "<kp-delete>") 'delete-char)

;; Funktionstasten
(global-set-key (kbd "<f6>") 'other-window)             ;; C-x o
(global-set-key (kbd "C-<f6>") 'bury-buffer)            ;;

(global-set-key (kbd "<f5>") '(lambda()(interactive)(revert-buffer t)))
(global-set-key (kbd "S-<f5>") '(lambda()(interactive)(revert-buffer t t)))

;; IDE-Tastenkombinationen wie bei Visual Studio
(global-set-key (kbd "<f4>") 'next-error)
(global-set-key (kbd "S-<f4>") 'previous-error)

(global-set-key (kbd "C-f") 'nonincremental-re-search-forward)          ;;
(global-set-key (kbd "S-C-f") 'nonincremental-repeat-re-search-forward) ;;
(global-set-key (kbd "S-C-s") 'nonincremental-repeat-search-forward)    ;;
(global-set-key (kbd "<f3>") 'nonincremental-repeat-search-forward)     ;;
(global-set-key (kbd "S-C-q") 'search-quick)            ;; self-written

(global-set-key (kbd "<f9>") 'insert-timestamp)         ;; self-written

;(global-set-key (kbd "C-S-<down>") 'next-error)

;; pc-bufsw implementiert ein Umschalten zwischen Buffern mit Ctrl-Tab
;; ähnlich wie Alt-Tab unter Windows die Applikationen umschaltet
(require 'pc-bufsw)
(global-set-key (kbd "C-<tab>") 'pc-bufsw::previous)
(global-set-key (kbd "S-C-<tab>") 'pc-bufsw::lru)
; ; pc-bufsw 0.9 funktionierte noch rekursiv, denn mit dem default-Wert 300
; ; für folgende Variable ist ab ca. 40 Buffer Schluss mit Ctrl-Tab :-(
; ; mit 1000 ist dann bei etwa 80 Buffer finito, aber aus einem andern Grund
; (setq max-lisp-eval-depth 1000)

; Einfügen von beliebigen Zeichen mit C-q im Dezimalsystem (statt oktal)
(setq read-quoted-char-radix 10)

; mit Maus erzeugtes imenu in der Konsole liess emacs < 20 abstürzen.
; Aber auch in Emacs 20.3 funktionieren noch keine Menüs mit der Maus...
(cond (window-system
       (global-set-key (kbd "S-<mouse-2>") 'imenu)))

;; für mehrere Modi gemeinsames Zeugs ------------------------------------------

; Menus etwas anpassen an grosse Bildschirme
(setq imenu-max-items 40)
(setq imenu-sort-function 'imenu--sort-by-name)
(setq buffers-menu-max-size 30)

; Das Buffer-Menu verbessern
(require 'msb)
(msb-mode 1)
(setq msb-max-menu-items 30)
(setq msb-display-most-recently-used 30)

; im File-Menu die zuletzt geöffneten Files anzeigen (und speichern)
(require 'recentf)
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 30)
(recentf-mode 1)

; Anzeige des Funktionsnamens in der Modeline
(setq which-func-maxout         0         ;; enabled, regardless buffer size
      which-func-mode-global    t)
(if (< emacs-major-version 20)
    (require 'which-function)
  (require 'which-func))
(add-to-list 'which-func-modes 'cperl-mode)
; aus irgendeinem Grund muss man den Modus zweimal toggeln...
(if (not (eq window-system 'mac))
    (progn
      (which-func-mode 0)
      (which-func-mode 1)
      ))

; den Minibuffer konfigurieren -------------------------------------------------
(resize-minibuffer-mode 1)      ; automatische Grössenanpassung
; im Minibuffer sollen lange Zeilen umgebrochen werden (sonst ist ja
; der resize-minibuffer-mode für die Katz)
(add-hook 'minibuffer-setup-hook
          '(lambda () (setq truncate-lines nil)))

;; mode specific configuration -------------------------------------------------
;;(setq default-major-mode 'text-mode)
;;(setq indent-tabs-mode 't)

;;; info system: find local info files
;(if (boundp 'Info-directory-list)
;    (setq Info-directory-list
;          (append Info-directory-list '("~/info/" "/opt/local/GNU/info/")))
;  )

; ; Der Mauszeiger soll vor dem herannahenden Cursor flüchten
; (mouse-avoidance-mode 'exile)

;; ausgelagerte Mode-Anpassungen ----------------------------------------------
(require 'pesche-modes)

;; Welcher mode soll für HTML verwendet werden? in pesche-modes werden
;; sowohl html-helper-mode wie sgml-html-mode unterstützt.
(setq auto-mode-alist
;;      (append '(("\\.s?html?\\'" . sgml-html-mode))
      (append '(("\\.s?html?\\'" . html-helper-mode))
              auto-mode-alist))


;; dynamische Abkürzungen ------------------------------------------------------
;; immer case-sensitiv !
(setq dabbrev-case-fold-search nil)
(setq dabbrev-case-replace nil)

;; font lock -------------------------------------------------------------------

;; cperl verändert auf 'unfreundliche' Art constant-face,
;; aber wir setzen unserer Kopf durch!
(copy-face 'bold-italic 'font-lock-constant-face)
(set-face-foreground 'font-lock-constant-face "ForestGreen")
(copy-face 'font-lock-constant-face 'font-lock-reference-face)

; die Farben für die eingebauten Faces anpassen
(setq font-lock-face-attributes
      '((font-lock-comment-face       "SlateGray")
        (font-lock-string-face        "Sienna" "LightBlue")
        (font-lock-keyword-face       "Firebrick")
        (font-lock-function-name-face "Blue")
        (font-lock-variable-name-face "Blue")
        (font-lock-type-face          "Black")
        (font-lock-builtin-face       "DarkGoldenrod")
        (info-node                    "Blue")
        (info-xref                    "Blue")
        (makefile-space-face          nil "HotPink")
        ))

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t
      font-lock-maximum-size       1048576)     ; 1 MB
(require 'font-lock)
(if (not (eq window-system 'mac))
    (require 'pesche-font-lock))

(cond (window-system
       (if (<= emacs-major-version 19)
           (font-lock-make-faces))))


; Für alle Window-System ausser Mac
(if (and (not (eq window-system nil))
         (not (eq window-system 'mac)))
    (progn
      (set-face-font 'font-lock-comment-face       pesche-default-italic)
      (set-face-font 'font-lock-string-face        pesche-default-regular)
      (set-face-font 'font-lock-keyword-face       pesche-default-bold)
      (set-face-font 'font-lock-function-name-face pesche-default-bold)
      (set-face-font 'font-lock-variable-name-face pesche-default-regular)
      (set-face-font 'font-lock-type-face          pesche-default-bold)
      (set-face-font 'font-lock-builtin-face       pesche-default-bold)
      (set-face-font 'html-helper-bold-face        pesche-default-bold)
      (set-face-font 'html-helper-italic-face      pesche-default-italic)
      (set-face-font 'html-helper-underline-face   pesche-default-bold)
      (set-face-font 'html-tag-face                pesche-default-regular)
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
(setq pesche-menu-name "Pesche")
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
 '(scroll-preserve-screen-position t)
 '(hscroll-global-mode t nil (hscroll)))
(custom-set-faces)


