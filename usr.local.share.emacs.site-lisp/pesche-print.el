;; Pesche's Druckerei
;;
;;     $Source: g:/archiv/cvsroot/site-lisp/pesche-print.el,v $
;;   $Revision: 1.3 $
;;       $Date: 1999/02/13 00:05:50 $
;;     $Author: pesche $

;; Wir benötigen als Basis das Postscript-Modul von Emacs
(require 'ps-print)

;; Konfiguration ---------------------------------------------------------------
(setq ps-paper-type 'a4)
(setq ps-print-header-frame nil)

; die Schrift etwas verstellen
(setq ps-font-size 8)
; Zeichenbreite zur Berechnung des Zeilenumbruchs meine Schrift anpassen
; Originalwerte für Courier 10: 5.6 6
(setq ps-avg-char-width (if (fboundp 'float) 4.48 5))
(setq ps-space-width (if (fboundp 'float) 4.48 5))
; Zeilenhöhe zur Berechnung des Seitenumbruchs meine Schrift anpassen
; Originalwerte für Courier 10: 11.29 11
(setq ps-line-height (if (fboundp 'float) 9.03 9))

; einiges ist zuhause anders als im Büro
(if (eq (string-match "PIAZZA" (system-name)) 0)
    (progn  ; Zuhause
      (defvar ghost-dir     "C:\\Progra~1\\gstools\\gs5.03")
      (defvar ghost-printer "-sDEVICE=djet500 -r300")
      (defvar ghost-fonts   "C:\\psfonts")
      (defvar ghost-view    "C:\\Progra~1\\gstools\\gsview\\gsview32.exe"))
    (progn  ; im Büro
      (defvar ghost-dir     "L:\\tools\\ghost\\gs5.03")
      (defvar ghost-printer "-sDEVICE=ljet4 -r600")
      (defvar ghost-fonts   "I:\\win\\psfonts")
      (defvar ghost-view    "L:\\tools\\ghost\\gsview\\gsview32.exe"))
    )


(setq ps-psnup-command "psnup") ; Name of n-up program (taking ps as input)
(setq ps-psnup-switches '(" -l -2 -pa4 ")) ; options for program above
(setq ps-lpr-command (concat "start /min " ghost-dir "\\gswin32"))
(setq ps-lpr-switches `(,(concat "-q -sPAPERSIZE=a4 "
                                ghost-printer " -dNOPAUSE -I"
                                ghost-dir ";" ghost-dir "\\fonts;" ghost-fonts)))
(setq ps-preview-command ghost-view)
(setq ps-lpr-buffer (concat (getenv "TEMP") "\\psspool.ps"))
(setq ps-psnup-buffer (concat (getenv "TEMP") "\\psnup.ps"))
(setq ps-print-color-p nil)
(setq ps-bold-faces '(font-lock-keyword-face info-xref info-node woman-bold-face))
(setq ps-italic-faces '(font-lock-comment-face info-node woman-italic-face))

;; Hilfsfunktionen -------------------------------------------------------------
(defun pesche-time-stamp ()
  "Format time and date for inclusion in print header."
  ;; die time-stamp-Funktionen sind aus time-stamp.el
  (concat (time-stamp-dd-mon-yy) " " (time-stamp-hh:mm:ss))
  )

;; 1up region ------------------------------------------------------------------
(defun pesche-printfile-region-with-faces (from to &optional buffer-p)
  (interactive (list (point) (mark) nil))
  ;; Änderungen an der Kopfzeile hier vornehmen, da Variable Buffer-local
  (setq ps-right-header
        (list "/pagenumberstring load" 'pesche-time-stamp))
  (ps-print-with-faces from to ps-lpr-buffer (not buffer-p))
  )

(defun pesche-print-region-with-faces (from to &optional buffer-p)
  (interactive (list (point) (mark) nil))
  (pesche-printfile-region-with-faces from to buffer-p)
  (shell-command
   (apply 'concat (append (list ps-lpr-command " ")
                          ps-lpr-switches
                          (list " " ps-lpr-buffer " -c quit"))))
  )

(defun pesche-preview-region-with-faces (from to &optional buffer-p)
  (interactive (list (point) (mark) nil))
  (pesche-printfile-region-with-faces from to buffer-p)
  (start-process-shell-command "gsview" "*Messages*"
                               ps-preview-command ps-lpr-buffer)
  )

;; 1up buffer ------------------------------------------------------------------
(defun pesche-printfile-buffer-with-faces ()
  (interactive)
  (pesche-printfile-region-with-faces (point-min) (point-max) t)
  )

(defun pesche-print-buffer-with-faces ()
  (interactive)
  (pesche-print-region-with-faces (point-min) (point-max) t)
  )

(defun pesche-preview-buffer-with-faces ()
  (interactive)
  (pesche-preview-region-with-faces (point-min) (point-max) t)
  )

;; 2up region ------------------------------------------------------------------
(defun pesche-printfile-2up-region-with-faces (from to &optional buffer-p)
  (interactive (list (point) (mark) nil))
  (pesche-printfile-region-with-faces from to buffer-p)
  (shell-command
   (apply 'concat (append (list ps-psnup-command " ")
                          ps-psnup-switches
                          (list " " ps-lpr-buffer " " ps-psnup-buffer))))
  )

(defun pesche-print-2up-region-with-faces (from to &optional buffer-p)
  (interactive (list (point) (mark) nil))
  (pesche-printfile-2up-region-with-faces from to buffer-p)
  (shell-command
   (apply 'concat (append (list ps-lpr-command " ")
                          ps-lpr-switches
                          (list " " ps-psnup-buffer " -c quit"))))
  )

(defun pesche-preview-2up-region-with-faces (from to &optional buffer-p)
  (interactive (list (point) (mark) nil))
  (pesche-printfile-2up-region-with-faces from to buffer-p)
  (start-process-shell-command "gsview" "*Messages*"
                               ps-preview-command ps-psnup-buffer)
  )

;; 2up buffer ------------------------------------------------------------------
(defun pesche-printfile-2up-buffer-with-faces ()
  (interactive)
  (pesche-printfile-2up-region-with-faces (point-min) (point-max) t)
  )

(defun pesche-print-2up-buffer-with-faces ()
  (interactive)
  (pesche-print-2up-region-with-faces (point-min) (point-max) t)
  )

(defun pesche-preview-2up-buffer-with-faces ()
  (interactive)
  (pesche-preview-2up-region-with-faces (point-min) (point-max) t)
  )

;; Menu ------------------------------------------------------------------------

; die Druck-Einträge aus dem Tools-Menu entfernen
(define-key menu-bar-tools-menu [print-region] nil)
(define-key menu-bar-tools-menu [print-buffer] nil)
(define-key menu-bar-tools-menu [ps-print-buffer] nil)
(define-key menu-bar-tools-menu [ps-print-region] nil)
(define-key menu-bar-tools-menu [separator-print] nil)
; die Druck-Einträge von Emacs 20.3 aus dem Print-Menu entfernen (vgl. menu-bar.el)
(define-key menu-bar-print-menu [ps-print-buffer] nil)
(define-key menu-bar-print-menu [ps-print-region] nil)
(define-key menu-bar-print-menu [separator-ps-print] nil)

; Submenus für Print/Print-to-file/Preview in das File-Menu einhängen
(define-key-after menu-bar-files-menu [preview]
  '("Preview" . menu-bar-preview-menu)
  'kill-buffer)
(define-key-after menu-bar-files-menu [printfile]
  '("Print to File" . menu-bar-printfile-menu)
  'kill-buffer)
(define-key-after menu-bar-files-menu [print]
  '("Print" . menu-bar-print-menu)
  'kill-buffer)
(define-key-after menu-bar-files-menu [separator-print]
  '("--")
  'kill-buffer)

; das Print-Submenu
(defvar menu-bar-print-menu (make-sparse-keymap "Print"))
(define-key global-map [menu-bar files print]
  (cons "Print" menu-bar-print-menu))
(define-key menu-bar-print-menu [print-region-2up] '("2up Region" . pesche-print-2up-region-with-faces))
(define-key menu-bar-print-menu [print-buffer-2up] '("2up Buffer" . pesche-print-2up-buffer-with-faces))
(define-key menu-bar-print-menu [print-region]     '("Region"     . pesche-print-region-with-faces))
(define-key menu-bar-print-menu [print-buffer]     '("Buffer"     . pesche-print-buffer-with-faces))

; das Printfile-Submenu
(defvar menu-bar-printfile-menu (make-sparse-keymap "Print to File"))
(define-key global-map [menu-bar files printfile]
  (cons "Print to File" menu-bar-printfile-menu))
(define-key menu-bar-printfile-menu [printfile-region-2up] '("2up Region" . pesche-printfile-2up-region-with-faces))
(define-key menu-bar-printfile-menu [printfile-buffer-2up] '("2up Buffer" . pesche-printfile-2up-buffer-with-faces))
(define-key menu-bar-printfile-menu [printfile-region]     '("Region"     . pesche-printfile-region-with-faces))
(define-key menu-bar-printfile-menu [printfile-buffer]     '("Buffer"     . pesche-printfile-buffer-with-faces))

; das Preview-Submenu
(defvar menu-bar-preview-menu (make-sparse-keymap "Preview"))
(define-key global-map [menu-bar files preview]
  (cons "Preview" menu-bar-preview-menu))
(define-key menu-bar-preview-menu [preview-region-2up] '("2up Region" . pesche-preview-2up-region-with-faces))
(define-key menu-bar-preview-menu [preview-buffer-2up] '("2up Buffer" . pesche-preview-2up-buffer-with-faces))
(define-key menu-bar-preview-menu [preview-region]     '("Region"     . pesche-preview-region-with-faces))
(define-key menu-bar-preview-menu [preview-buffer]     '("Buffer"     . pesche-preview-buffer-with-faces))

; alle *-region-* Funktionen dürfen nur bei einer aktiven Markierung
; verfügbar sein (sonst sind sie 'greyed out')
(put 'pesche-print-region-with-faces         'menu-enable 'mark-active)
(put 'pesche-printfile-region-with-faces     'menu-enable 'mark-active)
(put 'pesche-preview-region-with-faces       'menu-enable 'mark-active)
(put 'pesche-print-2up-region-with-faces     'menu-enable 'mark-active)
(put 'pesche-printfile-2up-region-with-faces 'menu-enable 'mark-active)
(put 'pesche-preview-2up-region-with-faces   'menu-enable 'mark-active)



(provide 'pesche-print)

;; eof


