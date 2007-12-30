;;----------------------------------------------------------- -*- Emacs-Lisp -*-
;;  Emacs Site Startup File
;;
;;      Author: Peter Steiner <pesche@schlau.ch>
;;         $Id$
;;     $Change$
;;   $DateTime$
;;     $Author$
;;    $Created: 1998/11/06 $


;; nach einem Posting von Larry Smith <lsmith@cio2000.eds.com>
;; falls %HOME% nicht gesetzt ist oder den Wert "C:/" bzw. "C:\" hat
;; und %HOMEDRIVE% sowie %HOMEPATH% vorhanden sind, dann setze
;; %HOME% = %HOMEDRIVE%%HOMEPATH%
;; (und ersetze gleich noch alle '\' mit '/', damit zB msb.el Freude hat)
(cond ((and (or (null (getenv "HOME"))
                (string-match "^[Cc]:\\(/\\|\\\\\\)$" (getenv "HOME")))
            (getenv "HOMEDRIVE")
            (getenv "HOMEPATH"))
       (let ((home (concat (getenv "HOMEDRIVE")
                           (getenv "HOMEPATH"))))
         ;; Backslashes durch Slashes ersetzen
         (while (setq i (string-match "[\\]" home))
           (aset home i ?/))
         (setenv "HOME" home)
         )))

;; ****** Added by emacsw32-setup-base at Thu Jun 07 09:53:22 2007
;; Add EmacsW32/lisp to load-path if found.
(let ((lisp-dir (expand-file-name (concat exec-directory "../../EmacsW32/lisp/"))))
  (unless (file-accessible-directory-p lisp-dir)
    (message "Can't find %s" lisp-dir)
    ;(sit-for 10)
    )
  (when (file-accessible-directory-p lisp-dir)
    (message "Adding %s to load-path" lisp-dir)
    (add-to-list 'load-path lisp-dir)))

;; eof