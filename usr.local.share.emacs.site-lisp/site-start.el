;;----------------------------------------------------------- -*- Emacs-Lisp -*-
;;  Emacs Site Startup File
;;
;;      Author: Peter Steiner <unistein@isbe.ch>
;;     $Source: g:/archiv/cvsroot/site-lisp/site-start.el,v $
;;   $Revision: 1.1 $
;;       $Date: 1998/11/06 23:28:43 $
;;     $Author: pesche $


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


;; eof