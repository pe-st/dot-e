;; Pesche' Menu
;;
;;     $Source: g:/archiv/cvsroot/site-lisp/pesche-menu.el,v $
;;   $Revision: 1.4 $
;;       $Date: 1998/10/10 21:48:01 $
;;     $Author: pesche $


;; Hauptmenu -------------------------------------------------------------------

; Menu definieren
(defvar menu-bar-pesche-menu (make-sparse-keymap "Pesche"))
(define-key global-map [menu-bar pesche-menu] (cons "Pesche" menu-bar-pesche-menu))

; Menu immer vor dem Hilfe-Menu anzeigen (zweitletztes Menu)
(setq menu-bar-final-items '(pesche-menu help-menu))

; die Einträge geschiehen in umgekehrter Reihenfolge...

; Tabulator-Untermenü einhängen
(define-key menu-bar-pesche-menu [tab-menu]
  '("Tabulators" . pesche-tab-menu))

(define-key menu-bar-pesche-menu [pesche-separator-4]
  '("--"))

; Suchen-Untermenü einhängen
(define-key menu-bar-pesche-menu [find-menu]
  '("Find special" . pesche-find-menu))

(define-key menu-bar-pesche-menu [pesche-separator-3]
  '("--"))

(define-key menu-bar-pesche-menu [pesche-eol-unix]
  '("EOL LF" . (lambda()(interactive) (set-buffer-file-coding-system 'undecided-unix))))

(define-key menu-bar-pesche-menu [pesche-eol-dos]
  '("EOL CRLF" . (lambda()(interactive) (set-buffer-file-coding-system 'undecided-dos))))

(define-key menu-bar-pesche-menu [pesche-separator-2]
  '("--"))

(define-key menu-bar-pesche-menu [pesche-tg-line-wrap]
  '("Zeilenumbruch toggeln" . toggle-line-wrap))

(define-key menu-bar-pesche-menu [pesche-tg-read-only]
  '("Schreibschutz toggeln" . toggle-read-only))

(define-key menu-bar-pesche-menu [pesche-separator-1]
  '("--"))

(define-key menu-bar-pesche-menu [pesche-re-font-lock]
  '("Font Lock aktualisieren" . font-lock-fontify-buffer))


;; Suchen Untermenü ------------------------------------------------------------
(defvar pesche-find-menu (make-sparse-keymap "Find-menu"))
(define-key global-map [menu-bar pesche-menu find-menu]
  (cons "Find special" pesche-find-menu))

(define-key pesche-find-menu [pesche-find-trails]
  '("Trailing Whitespace" .
    (lambda()(interactive)
      (nonincremental-re-search-forward "[ \t]$"))))

;; findet /**/, /*-*/, /*--*/ und /*---*/
(define-key pesche-find-menu [pesche-find-annot]
  '("Annotations /*-*/" .
    (lambda()(interactive)
      (nonincremental-re-search-forward "/\\*-?-?-?\\*/"))))


;; Tabulator Untermenü ---------------------------------------------------------
(defvar pesche-tab-menu (make-sparse-keymap "Tab-menu"))
(define-key global-map [menu-bar pesche-menu tab-menu]
  (cons "Tabulators" pesche-tab-menu))

(define-key pesche-tab-menu [pesche-indent-spc]
  '("Use Spaces" . (lambda()(interactive) (setq indent-tabs-mode nil))))

(define-key pesche-tab-menu [pesche-indent-tab]
  '("Use Tabs"   . (lambda()(interactive) (setq indent-tabs-mode t))))

(define-key pesche-tab-menu [pesche-separator-1]
  '("--"))

(define-key pesche-tab-menu [pesche-tab-8]
  '("Tabstop 8" . (lambda()(interactive) (setq tab-width 8) (redraw-display))))

(define-key pesche-tab-menu [pesche-tab-4]
  '("Tabstop 4" . (lambda()(interactive) (setq tab-width 4) (redraw-display))))

(define-key pesche-tab-menu [pesche-entab-region]
  '("Entab Region" . tabify))

; (define-key menu-bar-pesche-menu [pesche-entab-indent-region]
;   '("Entab-Indent Region" . (lambda()(interactive)
;                               (let ((tabify-regexp "^[ \t]+"))
;                                 (tabify)))))

(define-key pesche-tab-menu [pesche-entab-buffer]
  '("Entab Buffer" . (lambda()(interactive) (tabify (point-min) (point-max)))))

(define-key pesche-tab-menu [pesche-detab-region]
  '("Detab Region" . untabify))

(define-key pesche-tab-menu [pesche-detab-buffer]
  '("Detab Buffer" . (lambda()(interactive) (untabify (point-min) (point-max)))))


;; Modul abschliessen ----------------------------------------------------------
(provide 'pesche-menu)

;; eof


