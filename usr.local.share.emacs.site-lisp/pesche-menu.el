;; Pesche' Menu


;; Menu ------------------------------------------------------------------------

; Menu definieren
(defvar menu-bar-pesche-menu (make-sparse-keymap "Pesche"))
(define-key global-map [menu-bar pesche-menu] (cons "Pesche" menu-bar-pesche-menu))

; Menu immer vor dem Hilfe-Menu anzeigen (zweitletztes Menu)
(setq menu-bar-final-items '(pesche-menu help-menu))

; und hier das Menu selbst
(define-key menu-bar-pesche-menu [pesche-re-font-lock]
  '("Font Lock aktualisieren" . font-lock-fontify-buffer))


(provide 'pesche-menu)

;; eof


