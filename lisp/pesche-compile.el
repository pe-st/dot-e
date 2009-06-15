;; Pesche's Hilfsfunktionen für compile
;;
;;         $Id: //netzadmin/emacs/site-lisp/pesche-compile.el#2 $
;;   $DateTime: 2003/10/13 23:55:56 $
;;     $Author: peter.steiner $
;;  $Copyright: Peter Steiner <pesche@schlau.ch>

(require 'compile)

(setq compilation-scroll-output t)

;; helper functions ------------------------------------------------------------
(defun pesche-basename (filename)
  (file-name-nondirectory (file-name-sans-extension filename)))


;; lint commands ---------------------------------------------------------------
(defun pesche-compile-lint-buffer ()
  (interactive)
  (let ((cmd (concat "dmake " (pesche-basename (buffer-file-name)) ".ll")))
    (save-some-buffers (not compilation-ask-about-save) nil)
    (compile-internal cmd "No more errors")))

(defun pesche-compile-lint ()
  (interactive)
  (let ((cmd "dmake lint"))
    (save-some-buffers (not compilation-ask-about-save) nil)
    (compile-internal cmd "No more errors")))




;; Modul abschliessen ----------------------------------------------------------
(provide 'pesche-compile)

;; eof


