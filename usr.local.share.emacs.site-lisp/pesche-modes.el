;; Pesche' Modes
;;
;;         $Id: //netzadmin/emacs/site-lisp/pesche-modes.el#28 $
;;     $Change: 24783 $
;;   $DateTime: 2006/06/18 11:30:07 $
;;     $Author: peter.steiner $
;;    $Created: 1999/06/02 $
;;  $Copyright: Peter Steiner <pesche@schlau.ch> $

;; Default-Liste für find-file löschen, damit wir unsere eigene pflegen können
(setq-default ff-other-file-alist nil)

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
;; Files auf .dox sollen auch C Files sein (doxygen files)
(setq auto-mode-alist (append '(("\\.rh\\'" . c-mode)
                                ("\\.dox\\'" . c-mode))
                              auto-mode-alist))

;; default aus find-file.el leicht abgeändert für .cxx und .cpp (nicht
;; nur .hh und .h)
(setq-default ff-other-file-alist
              (append '(
                        ("\\.cc$"  (".hh" ".h"))
                        ("\\.hh$"  (".cc" ".C"))

                        ("\\.c$"   (".h"))
                        ("\\.h$"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp"))

                        ("\\.C$"   (".H"  ".hh" ".h"))
                        ("\\.H$"   (".C"  ".CC"))

                        ("\\.CC$"  (".HH" ".H"  ".hh" ".h"))
                        ("\\.HH$"  (".CC"))

                        ("\\.cxx$" (".hxx" ".hpp" ".hh" ".h"))
                        ("\\.cpp$" (".hpp" ".hxx" ".hh" ".h"))
                        ) ff-other-file-alist))

;; ;; In bestimmten Verzeichnissen gehen wir von HW C Files aus...
;; (setq auto-mode-alist
;;       (append '(("/hal/[^/]+/[^/]+\\.[ch]\\'" . hw-c-mode)
;;                 ("/subsys/[^/]+/[^/]+\\.[ch]\\'" . hw-c-mode)
;;                 ("/vmi/[^/]+\\.[ch]\\'" . hw-c-mode))
;;               auto-mode-alist))
(autoload 'hw-c-mode "hw-c-mode" "Major mode for HW C files." t)

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
                   "^/\\*[-]+\\+[ \t]*\n|[ \t]+\\([^ \t][- A-Za-zÄÖÜäöü0-9+()]*\\).*|"
                   1)
                  ("Types"
                   "^[ \t]*typedef[ \t]+\\(struct[ \t]+[_A-Za-z0-9]+\\)"
                   1)
                  )))
  (imenu-add-to-menubar "Index")
  )
(add-hook 'c-mode-common-hook 'pesche-c-mode-common-hook)


;; C# mode ---------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))
(autoload 'csharp-mode "csharp-mode"
  "Major mode for editing C# code." t)


;; doxymacs mode ---------------------------------------------------------------
(require 'doxymacs)
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode)
          (eq major-mode 'hw-c-mode)
          (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)


;; compilation mode ------------------------------------------------------------
(if (>= emacs-major-version 22)
    (progn
      (require 'compile)
      ;; Standard Microsoft Format (msft) etwas angepasst, damit es auch mit
      ;; PC-Lint funktioniert
      (add-to-list 'compilation-error-regexp-alist
                   '("^ *\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\):?\\([0-9]+\\)?)\
 : \\(?:\\(?:[Ff]atal \\)[Ee]rror\\|\\([Nn]ote\\|[Ii]nfo\\|see\\|while\\)\\|[Ww]arnin\\(g\\)\\) \\(C?[0-9]+\\)?:?"
                     1 2 3 (5 . 4)))
      ))


;; eiffel mode -----------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.e\\'" . eiffel-mode))
(autoload 'eiffel-mode "eiffel" "Major mode for Eiffel programs" t)
(add-hook 'eiffel-mode-hook
          '(lambda ()
             (progn
               (setq eif-indent-increment 4
                     tab-width            4
                     indent-tabs-mode     t)
               )))

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
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(autoload 'python-mode "python-mode" nil t)

;; ruby-mode -------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(autoload 'ruby-mode "ruby-mode" "Load ruby-mode")

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

(setq woman-use-own-frame nil)

;; die Cygwin Man-Pages werden nicht sonst nicht automatisch gefunden...
(if (eq system-type 'windows-nt)
    (setq woman-manpath
          '("C:/cygwin/usr/man" "C:/mingw/man")))

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


;; xml-mode --------------------------------------------------------------------

;; wenn möglich nxml-mode verwenden (ab Emacs 21)
(if (and (fboundp 'make-hash-table)
         (boundp 'fontification-functions))
    (progn
      (load "rng-auto.el")
      (add-to-list 'auto-mode-alist
                   (cons (concat "\\." (regexp-opt '("xml" "xsl" "xsd" "sch" "rng" "xslt" "xhtml" "svg" "rss") t) "\\'")
                         'nxml-mode))
      (require 'fmode)
      (fmode-replace-default-mode 'xml-mode 'nxml-mode)
      (fmode-replace-default-mode 'sgml-mode 'nxml-mode)
      (fmode-replace-default-mode 'html-mode 'nxml-mode)
      (add-hook 'nxml-mode-hook
                '(lambda ()
                   (progn
                     ;; replace the nxml default for C-RET
                     (define-key nxml-mode-map "\M-_" 'nxml-complete)
                     (define-key nxml-mode-map [C-return] 'duplicate-line)
                     )))
;;       (add-to-list 'magic-mode-alist
;;                    '("<\\?xml " . nxml-mode) t)
      )
;;   (progn
;;     (setq auto-mode-alist
;;           (append '(("\\.xml\\'" . xml-mode)) auto-mode-alist))
;;     (autoload 'xml-mode "psgml" nil t)
;;     (setq sgml-xml-declaration (concat (getenv "SGML") "/dtd/html/xml.dcl"))
    )

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


