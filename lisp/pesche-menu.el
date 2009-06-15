;; Pesche's Menu
;;
;;      Author: Peter Steiner <pesche@schlau.ch>
;;         $Id: //netzadmin/emacs/site-lisp/pesche-menu.el#11 $
;;     $Change: 18727 $
;;   $DateTime: 2004/02/26 10:33:54 $
;;     $Author: p4admin $
;;    $Created: Wed Jul 6 19:52:18 1994 $


;; Hilfsfunktionen -------------------------------------------------------------

; Standard Windows FileOpen Dialog verwenden
; (von Binu Jose Philip <binu@teil.soft.net>)
(defvar dlgopen-executable-path "getfile.exe"
  "*Executable path for open dialog")

(defvar dlgopen-give-focus-always t
  "*If multiple file selected give focus when opening or not")

(defun dlgopen-open-files ()
  "*Provides standard file-open dialog to open files.
Set the variable 'dlgopen-executable-path' to the path of the
executable 'getfile.exe'. If it is not in any of the system PATHs.
If a single file is selected it will be given focus always, if multiple
files are selected, depending on the value of 'dlgopen-give-focus-always'
files will be brought to foreground"
  (interactive)

  ; der Windows-Dialog mach t natülich nur mit Windows Sinn...
  (if (or (eq window-system 'win32)
          (eq window-system 'w32))
      (let ((buffer "") (file-fqn "") (lines-in-page 0) (dir-path ""))
        (setq buffer (generate-new-buffer "files-to-open"))

        (if (call-process dlgopen-executable-path nil buffer)
            (save-excursion
              (set-buffer buffer) (goto-line 1)
              (setq dir-path (get-current-line))

              ; if buffer empty user has cancelled or open failed
              ; if only one line in buffer only one file selected so give it focus
              (if (> (buffer-size) 0)
                  (if (= (setq lines-in-page (count-lines 1 (buffer-size))) 1)
                      (find-file dir-path)
                    (while (> lines-in-page 1)
                      (setq lines-in-page (- lines-in-page 1))
                      (next-line 1)
                      (setq file-fqn (concat dir-path "/" (get-current-line)))
                      (save-excursion
                        (if (eq dlgopen-give-focus-always t)
                            (find-file file-fqn)
                          (find-file-noselect file-fqn))))))))
        (kill-buffer buffer))
    )
  )

(defun get-current-line()
  (buffer-substring (save-excursion (beginning-of-line) (point))
                    (save-excursion (end-of-line) (point)))
  )


;; Hauptmenu -------------------------------------------------------------------

(defvar pesche-menu-name "Extra"
  "*Displayed Name of Pesche's extra menu.")

; Menu definieren
(defvar menu-bar-pesche-menu (make-sparse-keymap "Pesche"))
(define-key global-map [menu-bar pesche-menu]
  (cons pesche-menu-name menu-bar-pesche-menu))

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

(define-key menu-bar-pesche-menu [pesche-eol-mac]
  '("EOL CR" . (lambda()(interactive) (set-buffer-file-coding-system 'undecided-mac))))

(define-key menu-bar-pesche-menu [pesche-separator-2]
  '("--"))

(define-key menu-bar-pesche-menu [pesche-tg-line-wrap]
  '("Zeilenumbruch toggeln" . toggle-line-wrap))

(define-key menu-bar-pesche-menu [pesche-tg-read-only]
  '("Schreibschutz toggeln" . toggle-read-only))

(define-key menu-bar-pesche-menu [pesche-separator-1]
  '("--"))

(define-key menu-bar-pesche-menu [pesche-re-font-lock]
  '("Font Lock aktualisieren" .
    (lambda()(interactive)
      (if (eq major-mode 'perl-mode)
          (cperl-find-pods-heres)
          (font-lock-fontify-buffer)
          ))))


;; Suchen Untermenü ------------------------------------------------------------
(defvar pesche-find-menu (make-sparse-keymap "Find-menu"))
(define-key global-map [menu-bar pesche-menu find-menu]
  (cons "Find special" pesche-find-menu))

(define-key pesche-find-menu [pesche-remove-trails]
  '("Remove Trailing Whitespace" .
    (lambda()(interactive)
      (query-replace-regexp "[ \t]+$" ""))))

(define-key pesche-find-menu [pesche-find-trails]
  '("Trailing Whitespace" .
    (lambda()(interactive)
      (nonincremental-re-search-forward "[ \t]+$"))))

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

(define-key pesche-tab-menu [pesche-tab-3]
  '("Tabstop 3" . (lambda()(interactive) (setq tab-width 3) (redraw-display))))

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


;; andere Menüs anpassen -------------------------------------------------------
; weitere Menü-Anpassungen (das Drucken betreffend) sind in pesche-print.el

; den Windows FileOpen Dialog in das File-Menu einhängen
; (und zwar direkt nach dem normalen Open File)
(if (or (eq window-system 'win32)
        (eq window-system 'w32))
    (define-key-after menu-bar-files-menu [open-file-w32]
      '("Open File (Windows)..." . dlgopen-open-files)
      'open-file))


;; Modul abschliessen ----------------------------------------------------------
(provide 'pesche-menu)

;; eof


