;; Pesche' Modes
;;
;;     $Source: g:/archiv/cvsroot/site-lisp/pesche-modes.el,v $
;;   $Revision: 1.3 $
;;       $Date: 1999/08/11 13:45:30 $
;;     $Author: pesche $

;; lisp modes
(defun pesche-emacs-lisp-mode-hook()
  (setq tab-width        8
        indent-tabs-mode nil)

  ;; Syntax etwas anpassen, damit (zB) Markieren mit Doppelklick nicht
  ;; bei '-' oder Umlauten Halt macht
  (modify-syntax-for-umlaut)
  (modify-syntax-entry ?- "w")

  ;; alle Kommentarzeilen, die mit mindestens drei '-' aufh�ren,
  ;; in das 'Outline'-Men� eintragen
  (setq imenu-generic-expression
        (append imenu-generic-expression
                '(("Outline" ";+[ \\t]+\\([ A-Za-z0-9������/+]+\\)---*[ \\t]*$" 1))))
  (imenu-add-to-menubar "Index")
  )
(add-hook 'emacs-lisp-mode-hook 'pesche-emacs-lisp-mode-hook)

;; C mode und alle Verwandten --------------------------------------------------
;; Files auf .rh sollen auch Header Files sein (resource header)
(setq auto-mode-alist (append '(("\\.rh\\'" . c-mode)) auto-mode-alist))

;; cc-mode 5.21 kennt das "richtige" Verhalten von Delete und Backspace
;; (aber nur wenn delete-key-deletes-forward existiert)
(if (not (boundp 'delete-key-deletes-forward))
    (defvar delete-key-deletes-forward))
(setq delete-key-deletes-forward t)

(defun pesche-c-mode-common-hook()
  ;; das 'elektrische' automatische Einr�cken bei Kommentaren ist l�stig...
  (local-unset-key (kbd "*"))
  (local-unset-key (kbd "/"))
  ;; auch in C die neuen C++-Kommentare verwenden
  (if (fboundp 'c-enable-//-in-c-mode)
      (c-enable-//-in-c-mode))
  (setq comment-start "// "
        comment-end ""
        comment-multi-line nil
        font-lock-comment-start-regexp nil
        c-double-slash-is-comments-p t)

  ;; Syntax etwas anpassen, damit (zB) Markieren mit Doppelklick nicht
  ;; bei '_' oder Umlauten Halt macht
  (modify-syntax-entry ?_ "w")
  (modify-syntax-for-umlaut)

  ;; Mein Codierstil ist ein abge�nderter 'Stroustrup'
  (c-set-style  "Stroustrup")
  (c-set-offset 'case-label '+)
  (c-set-offset 'statement-case-open '+)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'inextern-lang 0)
  (c-set-offset 'cpp-macro 'c-lineup-dont-change)
  (c-set-offset 'cpp-macro-cont '+)

  ;; den Tabulator dressieren
  (setq tab-width                4
        indent-tabs-mode         nil
        c-tab-always-indent      nil
        c++-tab-always-indent    nil)

  ;; alle 'K�stchen' in das 'Outline'-Men� aufnehmen
  (setq imenu-generic-expression
        (append imenu-generic-expression
                '(("Outline"
                   "^/\\*[-]+\\+[ \t]*\n|[ \t]+\\([^ \t][- A-Za-z������0-9+]*\\).*|"
                   1)
                  ("Types"
                   "^[ \t]*typedef[ \t]+\\(struct[ \t]+[_A-Za-z0-9]+\\)"
                   1)
                  )))
  (imenu-add-to-menubar "Index")
  )
(add-hook 'c-mode-common-hook 'pesche-c-mode-common-hook)


;; makefile mode ---------------------------------------------------------------
;; Files auf .mak sollen auch Makefiles sein (ist offenbar nicht �blich)
(setq auto-mode-alist (append '(("\\.mak\\'" . makefile-mode)) auto-mode-alist))

;; Makefiles im msb-Buffermen� als Makefiles einordnen (bei cperl abgeschaut)
(defvar msb-menu-cond)
;(defvar makefile-msb-fixed)
(defun makefile-msb-fix()
  ;; Adds makefiles to msb menu, supposes that msb is already loaded
;  (setq makefile-msb-fixed t)
  (let* ((l (length msb-menu-cond))
         (last (nth (1- l) msb-menu-cond))
         (precdr (nthcdr (- l 2) msb-menu-cond)) ; cdr of this is last
         (handle (1- (nth 1 last))))
    (setcdr precdr (list
                    (list
                     '(eq major-mode 'makefile-mode)
                     handle
                     "Makefiles (%d)")
                    last))))

(defun pesche-makefile-mode-hook()
  (setq tab-width        8
        indent-tabs-mode t)

  ; korrigiere regexp f�r Zuweisungen (Fehler tritt nur
  ; bei imenu auf, font-lock ist okay)
  (setq makefile-macroassign-regex
        "^\\([^ \n\t#][^:#= \t\n]*\\)[ \t]*[*:+]?:?=")
  ;               --^--  dieses '#' fehlt in make-mode.el

  (imenu-add-to-menubar "Index")
;   (and (boundp 'msb-menu-cond)
;        (not makefile-msb-fixed)
;        (makefile-msb-fix))
  (makefile-msb-fix)
  )

(add-hook 'makefile-mode-hook 'pesche-makefile-mode-hook)


;; assembler mode --------------------------------------------------------------
(defun pesche-asm-mode-hook()
  (local-unset-key (kbd "<tab>"))
  (local-unset-key (kbd ":"))
  (local-unset-key (kbd ";"))
  (local-unset-key (kbd "RET"))
  (setq tab-width        8
        indent-tabs-mode t)

  ;; Syntax etwas anpassen, damit (zB) Markieren mit Doppelklick nicht
  ;; bei '_' oder Umlauten Halt macht
  (modify-syntax-for-umlaut)
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?. "w")  ; praktisch f�r move.l etc
  (modify-syntax-entry ?- "w")  ; praktisch in "C-Kommentaren"
  (modify-syntax-entry ?> "w")  ; praktisch in "C-Kommentaren"

;  (imenu-add-to-menubar "Index")
  )
(add-hook 'asm-mode-hook 'pesche-asm-mode-hook)

;; perl mode -------------------------------------------------------------------
(autoload 'cperl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
;; cperl-mode statt perl-mode verwenden
(setq auto-mode-alist
      (append '(("\\.[pP][Llm]\\'" . cperl-mode)) auto-mode-alist ))
(setq interpreter-mode-alist (append interpreter-mode-alist
         '(("miniperl" . cperl-mode))))

;; hairy ist etwas allzu haarig...
;(setq cperl-hairy t)


(defun pesche-cperl-mode-hook()
  ;; einr�cken: 4 Zeichen, 'else' darf ohne '{' auf eigener Zeile stehen
  (cperl-set-style "C++")
  (imenu-add-to-menubar "Index")
  )

(add-hook 'cperl-mode-hook 'pesche-cperl-mode-hook)


;; html/sgml/xml allgemeines ---------------------------------------------------


;; html-helper-mode mode -------------------------------------------------------
(autoload 'html-helper-mode "html-helper-mode" "HTML major mode." t)

; ;; html-helper-mode statt html-mode verwenden
; (setq auto-mode-alist
;       (append '(("\\.s?html?\\'" . sgml-html-mode)) auto-mode-alist))
; ;;      (append '(("\\.s?html?\\'" . html-helper-mode)) auto-mode-alist))
(setq html-helper-use-expert-menu t)

;; HTML-Files im msb-Buffermen� als solche einordnen (bei cperl abgeschaut)
(defvar msb-menu-cond)
(defun html-msb-fix()
  (let* ((l (length msb-menu-cond))
         (last (nth (1- l) msb-menu-cond))
         (precdr (nthcdr (- l 2) msb-menu-cond)) ; cdr of this is last
         (handle (1- (nth 1 last))))
    (setcdr precdr (list
                    (list
                     '(eq major-mode 'html-helper-mode)
                     handle
                     "Web Files (%d)")
                    last))))

(defun pesche-html-helper-mode-hook()
  (html-msb-fix)
  )

(add-hook 'html-helper-mode-hook 'pesche-html-helper-mode-hook)


;; sgml mode -------------------------------------------------------------------
(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t )

;; in sgml documents, parse dtd immediately to allow immediate
;; syntax coloring
(setq sgml-auto-activate-dtd t)

;; here we set the syntax color information for psgml
(setq-default sgml-set-face t)

(setq-default sgml-indent-data t)
(setq sgml-always-quote-attributes       t
      sgml-auto-insert-required-elements t
      sgml-auto-activate-dtd             t
      sgml-indent-data                   t
      sgml-indent-step                   2
      sgml-minimize-attributes           nil
      sgml-omittag                       nil
      sgml-shorttag                      nil
      )

;; PSGML menus for creating new documents
(setq sgml-custom-dtd
      '(
;         ("HTML 2.0 Strict Level 1"
;          "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0 Strict Level 1//EN\">")
;         ("HTML 2.0 Level 1"
;          "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0 Level 1//EN\">")
;         ("HTML 2.0"
;          "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">")
        ("HTML 3.2"
         "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">")
        ("HTML 4 Loose/Transitional"
         "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">")
        ("HTML 4 Strict"
         "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\">")
        ("HTML 4 Frameset"
         "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Frameset//EN\">")
        ("DocBook 3.1"
         "<!DOCTYPE Book PUBLIC \"-//OASIS//DTD DocBook V3.1//EN\">")
        )
      )

;; ecat support
(setq sgml-ecat-files
      (list
       (expand-file-name (concat (getenv "SGML") "/dtd/html/ecatalog"))
       (expand-file-name (concat (getenv "SGML") "/dtd/docbook-3.1/ecatalog"))
;       (expand-file-name "x:/l/sgml/dtd/html/ecatalog")
;       (expand-file-name "x:/l/sgml/dtd/docbook-3.1/ecatalog")
       ))

;; psgml-dsssl -----------------------------------------------------------------
(autoload 'sgml-dsssl-make-spec "psgml-dsssl" nil t)

;; psgml-jade ------------------------------------------------------------------

; the default commands are unix ones
(setq sgml-command-list
  (list
   (list "Jade" "jade -c%catalogs -t%backend -d%stylesheet %file"
         'sgml-run-command t
         '(("jade:\\(.*\\):\\(.*\\):\\(.*\\):E:" 1 2 3)))
   (list "JadeTeX" "tex \"&jadetex\" %tex"
         'sgml-run-command nil)
   (list "JadeTeX PDF" "virpdftex \"&pdfjadetex\" %tex"
         'sgml-run-command t)
   (list "dvips" "dvips %dvi"
         'sgml-run-command nil)
   (list "View dvi" "yap %dvi"
         'sgml-run-background t)
   (list "View PDF" "gsview32 %pdf"
         'sgml-run-command nil)
   (list "View ps" "gsview32 %ps"
         'sgml-run-command nil))
  )

; the default sgml-shell is hardcoded to /bin/sh
(setq sgml-shell shell-file-name)

;; load psgml-jade extension
(add-hook 'sgml-mode-hook '(lambda () (require 'psgml-jade)))


;; sgml-html-mode mode ---------------------------------------------------------
(defun sgml-html-mode ()
  "This version of html mode is just a wrapper around sgml mode."
  (interactive)
  (sgml-mode)
  (make-local-variable 'sgml-declaration)
  (make-local-variable 'sgml-default-doctype-name)
  (setq sgml-default-doctype-name    "html"
        sgml-declaration             (concat (getenv "SGML") "/dtd/html/html.dcl")
;        sgml-declaration             "x:/l/sgml/dtd/html/html.dcl"
        sgml-always-quote-attributes t
        sgml-indent-step             2
        sgml-indent-data             t
        sgml-minimize-attributes     nil
        sgml-omittag                 t
        sgml-shorttag                t
        )
  )

;; xml-mode --------------------------------------------------------------------
(setq auto-mode-alist
      (append '(("\\.xml\\'" . xml-mode)) auto-mode-alist))
(autoload 'xml-mode "psgml" nil t)
(setq sgml-xml-declaration (concat (getenv "SGML") "/dtd/html/xml.dcl"))
;(setq sgml-xml-declaration "X:/L/sgml/dtd/html/xml.dcl")


;; dtd-mode --------------------------------------------------------------------
(autoload 'dtd-mode "tdtd" "Major mode for SGML and XML DTDs." t)
(autoload 'dtd-etags "tdtd"
  "Execute etags on FILESPEC and match on DTD-specific regular expressions."
  t)
(autoload 'dtd-grep "tdtd" "Grep for PATTERN in files matching FILESPEC." t)

(setq auto-mode-alist
      (append
       (list
        '("\\.dcl\\'" . dtd-mode)
        '("\\.dec\\'" . dtd-mode)
        '("\\.dtd\\'" . dtd-mode)
        '("\\.ele\\'" . dtd-mode)
        '("\\.ent\\'" . dtd-mode)
        '("\\.mod\\'" . dtd-mode))
       auto-mode-alist))


;; Modul abschliessen ----------------------------------------------------------
(provide 'pesche-modes)

;; eof


