;; Pesche's Druckerei
;;
;;         $Id: //netzadmin/emacs/site-lisp/pesche-print.el#7 $
;;     $Change: 20775 $
;;   $DateTime: 2005/03/11 11:10:32 $
;;     $Author: peter.steiner $
;;    $Created: 1997/12/19 $
;;  $Copyright: Peter Steiner <pesche@schlau.ch>

;; Wir benötigen als Basis das Postscript-Modul von Emacs
(require 'ps-print)

;; Konfiguration ---------------------------------------------------------------
(setq ps-paper-type 'a4)
(setq ps-landscape-mode nil)
(setq ps-print-header-frame nil)

; Default-Schriftgrösse
(setq ps-font-size 8)

; Zeichenbreite zur Berechnung des Zeilenumbruchs an Default-Schrift anpassen
; Originalwerte für Courier 10: 5.6 6
; Werte für Courier 8: 4.48 5
; Werte für Courier 7: 3.92 4
(setq ps-avg-char-width (if (fboundp 'float) 4.48 5))
(setq ps-space-width (if (fboundp 'float) 4.48 5))

; Zeilenhöhe zur Berechnung des Seitenumbruchs an Default-Schrift anpassen
; Originalwerte für Courier 10: 11.29 11
; Werte für Courier 8: 9.03 9
; Werte für Courier 7: 7.90 8
(setq ps-line-height (if (fboundp 'float) 9.03 9))

(cond
 ((or (eq window-system 'win32)
      (eq window-system 'w32))
  (progn
    (cond
     ((eq (string-match "DONNERVOGEL" (system-name)) 0)
      (progn                          ; Zuhause
          (defvar ghost-dir     "F:\\Progra~1\\gs\\gs7.00")
          (defvar ghost-printer "-sDEVICE=djet500 -r300")
          (defvar ghost-view    "F:\\Progra~1\\gs\\gsview\\gsview32.exe")))
     ((eq (string-match "INUVIK" (system-name)) 0)
      (progn                            ; im Büro
        (defvar ghost-dir     "L:\\tools\\ghost\\gs7.00")
        (defvar ghost-printer "-sDEVICE=ljet4 -r600")
        (defvar ghost-view    "L:\\tools\\ghost\\gsview\\gsview32.exe")))
     ((eq (string-match "PIAZZABOOK"  (system-name)) 0)
      (progn                            ; das Notebook
        (defvar ghost-dir     "C:\\L\\gs\\gs8.14")
        (defvar ghost-printer "-sDEVICE=ljet4 -r600")
        (defvar ghost-view    "C:\\L\\gs\\gsview\\gsview32.exe")))
     ((eq (string-match "GIMMELWALD"  (system-name)) 0)
      (progn                            ; der P4-Renner im Büro
        (defvar ghost-dir     "C:\\L\\gs\\gs8.50")
        (defvar ghost-printer "-sDEVICE=ljet4 -r600")
        (defvar ghost-view    "C:\\L\\gs\\gsview\\gsview32.exe")))
     (t
      (progn                            ; Default-Verzeichnisse
        (defvar ghost-dir     "C:\\gs\\gs8.14")
        (defvar ghost-printer "-sDEVICE=ljet4 -r600")
        (defvar ghost-view    "C:\\Progra~1\\ghostgum\\gsview\\gsview32.exe")))
     )

    (setq ps-lpr-buffer (concat (getenv "TEMP") "\\psspool.ps"))
    (setq ps-psnup-buffer (concat (getenv "TEMP") "\\psnup.ps"))
    (setq ps-pdf-buffer (concat (getenv "TEMP") "\\ps.pdf"))

    (setq ps-psnup-command "psnup") ; Name of n-up program (taking ps as input)
    (setq ps-psnup-switches '(" -l -2 -pa4 ")) ; options for program above
    (setq ps-pdf-command (concat "start /min " ghost-dir "\\bin\\gswin32c"))
    (setq ps-pdf-switches-1 `(,(concat "-q -dSAVER -dNOPAUSE -dBATCH "
                                       "-sDEVICE=pdfwrite -sPAPERSIZE=a4 "
                                       "-dPDFSETTINGS=/printer \"-sOutputFile="
                                       ps-pdf-buffer "\" -c .setpdfwrite -f ")))
    (setq ps-pdf-switches-2 "")
    (setq ps-lpr-command (concat "start /min " ghost-dir "\\bin\\gswin32"))
    (setq ps-lpr-switches `(,(concat "-q -sPAPERSIZE=a4 "
                                     ghost-printer " -dNOPAUSE")))
    (setq ps-preview-command ghost-view)
    ))
 (t (progn ;; die Unix-Variante
      (defvar ghost-printer "-sDEVICE=pdfwrite -r600")
      (defvar ghost-view    "open -a Preview")

      (setq ps-lpr-buffer "~/tmp/psspool.ps")
      (setq ps-psnup-buffer "~/tmp/psnup.ps")
      (setq ps-pdf-buffer "~/tmp/ps.pdf")

      (setq ps-psnup-command "/sw/bin/psnup") ; Name of n-up program (taking ps as input)
      (setq ps-psnup-switches '(" -l -2 -pa4 ")) ; options for program above
      (setq ps-pdf-command ". /sw/bin/init.sh; ps2pdf")
      (setq ps-pdf-switches-1 '(" -sPAPERSIZE=a4 -dPDFSETTINGS=/printer "))
      (setq ps-pdf-switches-2 (concat " " ps-pdf-buffer))
      (setq ps-lpr-command "/sw/bin/gs")
      (setq ps-lpr-switches `(,(concat "-q -sPAPERSIZE=a4 "
                                       ghost-printer " -dNOPAUSE")))
      (setq ps-preview-command ghost-view)
      )))


(setq ps-print-color-p nil)
(setq ps-bold-faces '(font-lock-keyword-face info-xref info-node woman-bold-face))
(setq ps-italic-faces '(font-lock-comment-face info-node woman-italic-face))

; Variablen zum Verschönern des Output
; (Zeilennummern und Zebra können via Menu ein- und ausgeschaltet werden,
; siehe toggle-print-line-numbers-mode und toggle-print-zebra-stripes-mode)
(setq ps-line-number t)
(setq ps-line-number-step 5)
(setq ps-line-number-start 5)
(setq ps-zebra-stripes nil)
(setq ps-zebra-stripe-height 1)
(setq ps-zebra-color 0.92)

;; Hilfsfunktionen -------------------------------------------------------------
(require 'time-stamp)
(defun pesche-time-stamp ()
  "Format time and date for inclusion in print header."
  ;; die time-stamp-Funktionen sind aus time-stamp.el
  (concat (time-stamp-dd-mon-yy) " " (time-stamp-hh:mm:ss))
  )

;; font size -------------------------------------------------------------------
(defun pesche-print-fontsize-7 ()
  (interactive)
  (setq ps-font-size 7)
  (setq ps-avg-char-width (if (fboundp 'float) 3.92 4))
  (setq ps-space-width (if (fboundp 'float) 3.92 4))
  (setq ps-line-height (if (fboundp 'float) 7.90 8))
  )
(defun pesche-print-fontsize-8 ()
  (interactive)
  (setq ps-font-size 8)
  (setq ps-avg-char-width (if (fboundp 'float) 4.48 5))
  (setq ps-space-width (if (fboundp 'float) 4.48 5))
  (setq ps-line-height (if (fboundp 'float) 9.03 9))
  )
(defun pesche-print-fontsize-10 ()
  (interactive)
  (setq ps-font-size 10)
  (setq ps-avg-char-width (if (fboundp 'float) 5.6 6))
  (setq ps-space-width (if (fboundp 'float) 5.6 6))
  (setq ps-line-height (if (fboundp 'float) 11.29 11))
  )

;; 1up region ------------------------------------------------------------------
(defun pesche-printfile-region-with-faces (from to &optional buffer-p)
  (interactive (list (point) (mark) nil))
  ;; Änderungen an der Kopfzeile hier vornehmen, da Variable Buffer-local
  (setq ps-right-header
        (list "/pagenumberstring load" 'pesche-time-stamp))
  (ps-print-with-faces from to ps-lpr-buffer (not buffer-p))
  )

(defun pesche-printpdf-region-with-faces (from to &optional buffer-p)
  (interactive (list (point) (mark) nil))
  (pesche-printfile-region-with-faces from to buffer-p)
  (shell-command
   (apply 'concat (append (list ps-pdf-command " ")
                          ps-pdf-switches-1
                          (list " " ps-lpr-buffer " " ps-pdf-switches-2))))
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
  (pesche-printpdf-region-with-faces from to buffer-p)
  (start-process-shell-command "preview" "*Messages*"
                               ps-preview-command ps-pdf-buffer)
  )

;; 1up buffer ------------------------------------------------------------------
(defun pesche-printfile-buffer-with-faces ()
  (interactive)
  (pesche-printfile-region-with-faces (point-min) (point-max) t)
  )

(defun pesche-printpdf-buffer-with-faces ()
  (interactive)
  (pesche-printpdf-region-with-faces (point-min) (point-max) t)
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

(defun pesche-printpdf-2up-region-with-faces (from to &optional buffer-p)
  (interactive (list (point) (mark) nil))
  (pesche-printfile-2up-region-with-faces from to buffer-p)
  (shell-command
   (apply 'concat (append (list ps-pdf-command " ")
                          ps-pdf-switches-1
                          (list " " ps-psnup-buffer " " ps-pdf-switches-2))))
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
  (pesche-printpdf-2up-region-with-faces from to buffer-p)
  (start-process-shell-command "preview" "*Messages*"
                               ps-preview-command ps-pdf-buffer)
  )

;; 2up buffer ------------------------------------------------------------------
(defun pesche-printfile-2up-buffer-with-faces ()
  (interactive)
  (pesche-printfile-2up-region-with-faces (point-min) (point-max) t)
  )

(defun pesche-printpdf-2up-buffer-with-faces ()
  (interactive)
  (pesche-printpdf-2up-region-with-faces (point-min) (point-max) t)
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

(if (< emacs-major-version 21)
    (progn
      ; die Druck-Einträge von Emacs 20.3 aus dem Print-Menu entfernen (vgl. menu-bar.el)
      (define-key menu-bar-print-menu [ps-print-buffer] nil)
      (define-key menu-bar-print-menu [ps-print-region] nil)
      (define-key menu-bar-print-menu [separator-ps-print] nil)
      )
  (progn
    ; die Druck-Einträge von Emacs 21 aus dem Print-Menu entfernen (vgl. menu-bar.el)
    (define-key menu-bar-files-menu [ps-print-region] nil)
    (define-key menu-bar-files-menu [ps-print-buffer] nil)
    (define-key menu-bar-files-menu [ps-print-region-faces] nil)
    (define-key menu-bar-files-menu [ps-print-buffer-faces] nil)
    (define-key menu-bar-files-menu [print-region] nil)
    (define-key menu-bar-files-menu [print-buffer] nil)
    (define-key menu-bar-files-menu [separator-print] nil)
    ))

; Submenus für Print/Print-to-file/Preview in das File-Menu einhängen
(define-key-after menu-bar-files-menu [preview]
  '("Preview" . menu-bar-preview-menu)
  'kill-buffer)
(define-key-after menu-bar-files-menu [printpdf]
  '("Print to PDF" . menu-bar-printpdf-menu)
  'kill-buffer)
(define-key-after menu-bar-files-menu [printfile]
  '("Print to File" . menu-bar-printfile-menu)
  'kill-buffer)
(define-key-after menu-bar-files-menu [print]
  '("Print" . menu-bar-print-menu)
  'kill-buffer)

; Separator im File-Menu
(define-key-after menu-bar-files-menu [separator-print-options]
  '("--")
  'kill-buffer)

; Submenu für Schriftgrösse
(define-key-after menu-bar-files-menu [printfont]
  '("Print Font Size" . menu-bar-printfont-menu)
  'kill-buffer)

; toggle für die Papier-Ausrichtung
(define-key-after menu-bar-files-menu [toggle-print-orientation-mode]
  (menu-bar-make-toggle toggle-print-orientation-mode ps-landscape-mode
                        "Paper Orientation Landscape"
                        "Landscape mode %s"
                        "Print Orientation Landscape")
  'kill-buffer)

; toggle für die Zebrastreifen beim Drucken
(define-key-after menu-bar-files-menu [toggle-print-zebra-stripes-mode]
  (menu-bar-make-toggle toggle-print-zebra-stripes-mode ps-zebra-stripes
                        "Print Zebra Stripes"
                        "Zebra Stripe mode %s"
                        "Print Zebra Stripes")
  'kill-buffer)

; toggle für das Zeilennummern-Drucken
(define-key-after menu-bar-files-menu [toggle-print-line-numbers-mode]
  (menu-bar-make-toggle toggle-print-line-numbers-mode ps-line-number
                        "Print Line Numbers"
                        "Line Number mode %s"
                        "Print line numbers")
  'kill-buffer)

; toggle für den Seitenkopf
(define-key-after menu-bar-files-menu [toggle-print-header]
  (menu-bar-make-toggle toggle-print-header ps-print-header
                        "Print Page Header"
                        "Page Header mode %s"
                        "Print Page Header")
  'kill-buffer)

; abschliessender Separator im File-Menu
(define-key-after menu-bar-files-menu [separator-print]
  '("--")
  'kill-buffer)


; das Schriftgrösse-Menu
(defvar menu-bar-printfont-menu (make-sparse-keymap "Print Font Size"))
(define-key global-map [menu-bar files printfont]
  (cons "Print Font Size" menu-bar-printfont-menu))
(define-key menu-bar-printfont-menu [printfont-10]
  '(menu-item "10 pt"
              pesche-print-fontsize-10
              :help "Print Font Size 10 pt"
              :visible (display-graphic-p)
              :button (:radio . (eq ps-font-size 10))))
(define-key menu-bar-printfont-menu [printfont-8]
  '(menu-item "8 pt"
              pesche-print-fontsize-8
              :help "Print Font Size 8 pt"
              :visible (display-graphic-p)
              :button (:radio . (eq ps-font-size 8))))
(define-key menu-bar-printfont-menu [printfont-7]
  '(menu-item "7 pt"
              pesche-print-fontsize-7
              :help "Print Font Size 7 pt"
              :visible (display-graphic-p)
              :button (:radio . (eq ps-font-size 7))))

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

; das PDF-Submenu
(defvar menu-bar-printpdf-menu (make-sparse-keymap "Print to PDF"))
(define-key global-map [menu-bar files printpdf]
  (cons "Print to PDF" menu-bar-printpdf-menu))
(define-key menu-bar-printpdf-menu [printpdf-region-2up] '("2up Region" . pesche-printpdf-2up-region-with-faces))
(define-key menu-bar-printpdf-menu [printpdf-buffer-2up] '("2up Buffer" . pesche-printpdf-2up-buffer-with-faces))
(define-key menu-bar-printpdf-menu [printpdf-region]     '("Region"     . pesche-printpdf-region-with-faces))
(define-key menu-bar-printpdf-menu [printpdf-buffer]     '("Buffer"     . pesche-printpdf-buffer-with-faces))

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
(put 'pesche-printpdf-region-with-faces      'menu-enable 'mark-active)
(put 'pesche-preview-region-with-faces       'menu-enable 'mark-active)
(put 'pesche-print-2up-region-with-faces     'menu-enable 'mark-active)
(put 'pesche-printfile-2up-region-with-faces 'menu-enable 'mark-active)
(put 'pesche-printpdf-2up-region-with-faces  'menu-enable 'mark-active)
(put 'pesche-preview-2up-region-with-faces   'menu-enable 'mark-active)



(provide 'pesche-print)

;; eof


