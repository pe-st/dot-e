;; Vorlage für dieses File auf http://www.rose-hulman.edu/~mgrtps/emacs/2-up.html
;;
;; ##################################################
;; A note on the gs options:
;; -q                            quiet
;; -sDEVICE=ljet4                the printer - works with HP LaserJet 4 Plus
;; -r300                         resolution 300x300
;; -dNOPAUSE                     don't wait for user intervention
;; -IE:\\GSTOOLS\\gs4.01;E:\\GSTOOLS\\gs4.01\\fonts
;;                               the directories needed for gs
;; -c quit                       this is added at the end to terminate gs

(require 'ps-print)

;; Konfiguration ---------------------------------------------------------------
(setq ps-paper-type 'a4)
;(setq ps-paper-type 'ps-a4)
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


(setq ps-psnup-command "psnup") ; Name of n-up program (taking ps as input)
(setq ps-psnup-switches '(" -l -2 -pa4 ")) ; options for program above
;(setq ps-lpr-command "C:\\Progra~1\\gstools\\gs5.03\\gswin32")
(setq ps-lpr-command "start /min C:\\Progra~1\\gstools\\gs5.03\\gswin32")
(setq ps-lpr-switches '("-q -sPAPERSIZE=a4 -sDEVICE=djet500 -r300 -dNOPAUSE -IC:\\Progra~1\\gstools\\gs5.03;C:\\Progra~1\\gstools\\gs5.03\\fonts;c:\\psfonts"))
(setq ps-preview-command  "c:\\Progra~1\\gstools\\gsview\\gsview32.exe")
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
(defun pesche-printfile-region-with-faces (from to)
  (interactive (list (point) (mark)))
  ;; Änderungen an der Kopfzeile hier vornehmen, da Variable Buffer-local
  (setq ps-right-header
        (list "/pagenumberstring load" 'pesche-time-stamp))
  (ps-print-region-with-faces from to ps-lpr-buffer)
  )

(defun pesche-print-region-with-faces (from to)
  (interactive (list (point) (mark)))
  (pesche-printfile-region-with-faces from to)
  (shell-command
   (apply 'concat (append (list ps-lpr-command " ")
                          ps-lpr-switches
                          (list " " ps-lpr-buffer " -c quit"))))
  )

(defun pesche-preview-region-with-faces (from to)
  (interactive (list (point) (mark)))
  (pesche-printfile-region-with-faces from to)
  (start-process-shell-command "gsview" "*Messages*"
                               ps-preview-command ps-lpr-buffer)
  )

;; 1up buffer ------------------------------------------------------------------
(defun pesche-printfile-buffer-with-faces ()
  (interactive)
  (pesche-printfile-region-with-faces (point-min) (point-max))
  )

(defun pesche-print-buffer-with-faces ()
  (interactive)
  (pesche-print-region-with-faces (point-min) (point-max))
  )

(defun pesche-preview-buffer-with-faces ()
  (interactive)
  (pesche-preview-region-with-faces (point-min) (point-max))
  )

;; 2up region ------------------------------------------------------------------
(defun pesche-printfile-2up-region-with-faces (from to)
  (interactive (list (point) (mark)))
  ;;(ps-print-region-with-faces from to ps-lpr-buffer)
  (pesche-printfile-region-with-faces from to)
  (shell-command
   (apply 'concat (append (list ps-psnup-command " ")
                          ps-psnup-switches
                          (list " " ps-lpr-buffer " " ps-psnup-buffer))))
  )

(defun pesche-print-2up-region-with-faces (from to)
  (interactive (list (point) (mark)))
  (pesche-printfile-2up-region-with-faces from to)
  (shell-command
   (apply 'concat (append (list ps-lpr-command " ")
                          ps-lpr-switches
                          (list " " ps-psnup-buffer " -c quit"))))
  )

(defun pesche-preview-2up-region-with-faces (from to)
  (interactive (list (point) (mark)))
  (pesche-printfile-2up-region-with-faces from to)
  (start-process-shell-command "gsview" "*Messages*"
                               ps-preview-command ps-psnup-buffer)
  )

;; 2up buffer ------------------------------------------------------------------
(defun pesche-printfile-2up-buffer-with-faces ()
  (interactive)
  (pesche-printfile-2up-region-with-faces (point-min) (point-max))
  )

(defun pesche-print-2up-buffer-with-faces ()
  (interactive)
  (pesche-print-2up-region-with-faces (point-min) (point-max))
  )

(defun pesche-preview-2up-buffer-with-faces ()
  (interactive)
  (pesche-preview-2up-region-with-faces (point-min) (point-max))
  )

;; Menu ------------------------------------------------------------------------

; die Druck-Einträge aus dem Tools-Menu entfernen
(define-key menu-bar-tools-menu [print-region] nil)
(define-key menu-bar-tools-menu [print-buffer] nil)
(define-key menu-bar-tools-menu [ps-print-buffer] nil)
(define-key menu-bar-tools-menu [ps-print-region] nil)
(define-key menu-bar-tools-menu [separator-print] nil)

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


