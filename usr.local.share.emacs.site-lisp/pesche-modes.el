;; Pesche' Modes
;;
;;         $Id: //netzadmin/emacs/site-lisp/pesche-modes.el#13 $
;;   $DateTime: 2003/11/25 19:04:06 $
;;     $Author: peter.steiner $
;;  $Copyright: Peter Steiner <pesche@schlau.ch>

;; lisp modes ------------------------------------------------------------------
(defun pesche-emacs-lisp-mode-hook()
  (setq tab-width        8
        indent-tabs-mode nil)

  ;; Syntax etwas anpassen, damit (zB) Markieren mit Doppelklick nicht
  ;; bei '-' oder Umlauten Halt macht
  (modify-syntax-for-umlaut)
  (modify-syntax-entry ?- "w")

  ;; alle Kommentarzeilen, die mit mindestens drei '-' aufhören,
  ;; in das 'Outline'-Menü eintragen
  (setq imenu-generic-expression
        (append imenu-generic-expression
                '(("Outline" ";+[ \\t]+\\([ A-Za-z0-9äöüÄÖÜ/+]+\\)---*[ \\t]*$" 1))))
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

;; `indent-region' hat Probleme mit javadoc-Kommentaren. Der folgende
;; workaround ist der JDE Funktion `jde-indent-java-region' abgeschaut.
(defun pesche-indent-region-fix (start end)
  "Indent each nonblank line in the region.
This is a workaround for `indent-region' not properly indenting
Javadoc comments."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (setq end (set-marker (make-marker) end))
    (c-indent-line)
    (while (< (point) end)
      (c-indent-line)
      (forward-line 1))))

;; Kommentare in einem temporären text-mode Buffer editieren
(require 'c-comment-edit)

(defun pesche-c-mode-common-hook()
  ;; auf der englischen Tastatur liegt diese Fkt. auf C-M-\
  (local-set-key (kbd "C-M-<") 'pesche-indent-region-fix)
  (local-set-key (kbd "C-M-\\") 'pesche-indent-region-fix)
  (local-set-key (kbd "C-M-e") 'c-comment-edit)
  (if (>= emacs-major-version 21)
      (local-set-key (kbd "M-j") 'c-indent-new-comment-line))

  ;; das 'elektrische' automatische Einrücken bei Kommentaren ist lästig...
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

  ;; Mein Codierstil ist ein abgeänderter 'Stroustrup'
  (c-set-style  "Stroustrup")
  (c-set-offset 'case-label '+)
  (c-set-offset 'statement-case-open '+)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'inextern-lang 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'cpp-macro 'c-lineup-dont-change)
  (c-set-offset 'cpp-macro-cont '+)

  ;; den Tabulator dressieren
  (setq tab-width                4
        indent-tabs-mode         nil
        c-tab-always-indent      nil
        c++-tab-always-indent    nil)

  ;; case sensitiv suchen
  (setq case-fold-search nil)

  ;; (ab cc-mode 5.26) filladapt verwenden
  (c-setup-filladapt)
  (filladapt-mode 1)
  (setq fill-column 80)     ; normalerweise mehr als der text-mode default

  ;; alle 'Kästchen' in das 'Outline'-Menü aufnehmen
  (setq imenu-generic-expression
        (append imenu-generic-expression
                '(("Outline"
                   "^/\\*[-]+\\+[ \t]*\n|[ \t]+\\([^ \t][- A-Za-zÄÖÜäöü0-9+]*\\).*|"
                   1)
                  ("Types"
                   "^[ \t]*typedef[ \t]+\\(struct[ \t]+[_A-Za-z0-9]+\\)"
                   1)
                  )))
  (imenu-add-to-menubar "Index")
  )
(add-hook 'c-mode-common-hook 'pesche-c-mode-common-hook)


;; doxymacs mode ---------------------------------------------------------------
(require 'doxymacs)
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)


;; makefile mode ---------------------------------------------------------------
;; Files auf .mak sollen auch Makefiles sein (ist offenbar nicht üblich)
(setq auto-mode-alist (append '(("\\.mak\\'" . makefile-mode)) auto-mode-alist))

;; Makefiles im msb-Buffermenü als Makefiles einordnen (bei cperl abgeschaut)
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

  ; korrigiere regexp für Zuweisungen (Fehler tritt nur
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


;; jam mode --------------------------------------------------------------------
(require 'jam-mode)


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
  (modify-syntax-entry ?. "w")  ; praktisch für move.l etc
  (modify-syntax-entry ?- "w")  ; praktisch in "C-Kommentaren"
  (modify-syntax-entry ?> "w")  ; praktisch in "C-Kommentaren"

;  (imenu-add-to-menubar "Index")
  )
(add-hook 'asm-mode-hook 'pesche-asm-mode-hook)

;; perl mode -------------------------------------------------------------------
;; cperl-mode statt perl-mode verwenden
(autoload 'cperl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
(setq auto-mode-alist
      (append '(("\\.[pP][Llm]\\'" . cperl-mode)) auto-mode-alist ))
(setq interpreter-mode-alist (append interpreter-mode-alist
         '(("miniperl" . cperl-mode))))

(defun pesche-cperl-mode-hook()
  ;; einrücken: 4 Zeichen, 'else' darf ohne '{' auf eigener Zeile stehen
  (cperl-set-style "C++")

  ;; den Tabulator dressieren
  (setq tab-width                4
        indent-tabs-mode         nil
        cperl-tab-always-indent  nil)

  ;; case sensitiv suchen
  (setq case-fold-search nil)

  (imenu-add-to-menubar "Index")
  )

(add-hook 'cperl-mode-hook 'pesche-cperl-mode-hook)


;; python-mode -----------------------------------------------------------------
(setq auto-mode-alist
      (append '(("\\.py\\'" . python-mode)
                ("\\.pyw\\'" . python-mode))
              auto-mode-alist))
(autoload 'python-mode "python-mode" nil t)

;; lingo-mode ------------------------------------------------------------------
(autoload 'lingo-mode "lingo-mode" "Major mode for editing Lingo files." t)
(setq auto-mode-alist (append '(("\\.[Ll][Ss]\\'" . lingo-mode)) auto-mode-alist))

(defun pesche-lingo-mode-hook()
  ;; den Tabulator dressieren
  (setq tab-width                2
        indent-tabs-mode         nil)

  (imenu-add-to-menubar "Index")
  )

(add-hook 'lingo-mode-hook 'pesche-lingo-mode-hook)

;; WoMan mode ------------------------------------------------------------------
;; mode zum Lesen von 'man' Dokumentation ohne externes Programm
(autoload 'woman "woman"
  "Decode and browse a UN*X man page." t)
(autoload 'woman-find-file "woman"
  "Find, decode and browse a specific UN*X man-page file." t)
;; den Pfad zu den Man-Pages aus GCC_EXEC_PREFIX basteln
(let ((path (getenv "GCC_EXEC_PREFIX")))
  (if (stringp path)
      (progn
        ;; die Backslashes durch Slashes ersetzen
        (while (setq i (string-match "[\\]" path))
          (aset path i ?/))
        ;; den hinteren Teil des Pfades durch 'man' ersetzen
        (string-match "H-i386-cygwin32/lib/gcc-lib/" path)
        (setq woman-manpath (list (replace-match "man" t t path)))
        )))

;; todo-mode -------------------------------------------------------------------
(autoload 'todo-mode "todo-mode"
  "Major mode for editing TODO lists." t)
(autoload 'todo-show "todo-mode"
  "Show TODO items." t)
(autoload 'todo-insert-item "todo-mode"
  "Add TODO item." t)

;; text-mode -------------------------------------------------------------------
(defun pesche-text-mode-hook()
  ;(turn-on-auto-fill)
  (filladapt-mode 1)
  (setq filladapt-mode-line-string nil)

  ;; das hier sollte eigentlich bei globalem font-lock nicht nötig sein...
;  (turn-on-font-lock)
)
(add-hook 'text-mode-hook 'pesche-text-mode-hook)


;; outline-mode ----------------------------------------------------------------
(defun pesche-outline-mode-hook()
  ;(turn-on-auto-fill)
;;   (filladapt-mode 1)
;;   (setq filladapt-mode-line-string nil)

  ;; das hier sollte eigentlich bei globalem font-lock nicht nötig sein...
;  (turn-on-font-lock)

  ;; imenu expression ist zwar eingebaut, aber nicht so gut...
  (setq imenu-generic-expression
        (list (list nil (concat "^" outline-regexp ".*$") 0)))
  (imenu-add-to-menubar "Index")
)
(add-hook 'outline-mode-hook 'pesche-outline-mode-hook)


;; html/sgml/xml allgemeines ---------------------------------------------------

;; HTML-Files im msb-Buffermenü als solche einordnen (bei cperl abgeschaut)
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


;; html-helper-mode mode -------------------------------------------------------
(autoload 'html-helper-mode "html-helper-mode" "HTML major mode." t)

(setq html-helper-use-expert-menu t)
(setq html-helper-mode-uses-visual-basic nil)
(setq html-helper-mode-uses-bold-italic t)

(defun pesche-html-helper-mode-hook()
  (html-msb-fix)
  )

(add-hook 'html-helper-mode-hook 'pesche-html-helper-mode-hook)


;; sgml mode -------------------------------------------------------------------
(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t )

(setq sgml-auto-insert-required-elements t
      sgml-auto-activate-dtd             t
      )

;; font-locking konfigurieren
(setq sgml-set-face t
      sgml-markup-faces
      '((comment   . font-lock-comment-face)
        (doctype   . font-lock-reference-face)
        (start-tag . font-lock-function-name-face)
        (end-tag   . font-lock-function-name-face)
        (entity    . font-lock-string-face)
        (ignored   . font-lock-comment-face)
        (ms-end    . font-lock-keyword-face)
        (ms-start  . font-lock-keyword-face)
        (pi        . font-lock-reference-face)
        (sgml      . font-lock-reference-face)
        (short-ref . font-lock-reference-face)
        ))

; Buffer-lokale Variabeln in einer Hook-Funktion setzen
(defun pesche-sgml-mode-hook()
  (setq sgml-always-quote-attributes       t
        sgml-indent-data                   t
        sgml-indent-step                   2
        sgml-minimize-attributes           nil
        sgml-omittag                       nil
        sgml-shorttag                      nil
        )
  )

(add-hook 'sgml-mode-hook 'pesche-sgml-mode-hook)


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
        sgml-omittag                 t  ; normales SGML hat hier nil
        sgml-shorttag                t  ; normales SGML hat hier nil
        )
  )

;; xml-mode --------------------------------------------------------------------
(setq auto-mode-alist
      (append '(("\\.xml\\'" . xml-mode)) auto-mode-alist))
(autoload 'xml-mode "psgml" nil t)
(setq sgml-xml-declaration (concat (getenv "SGML") "/dtd/html/xml.dcl"))


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


