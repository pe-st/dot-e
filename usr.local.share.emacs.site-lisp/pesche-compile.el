;; Pesche's Hilfsfunktionen für compile
;;
;;     $Source: g:/archiv/cvsroot/site-lisp/pesche-compile.el,v $
;;   $Revision: 1.1 $
;;       $Date: 2002/01/15 21:55:20 $
;;     $Author: donnerpesche $

(require 'compile)

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


