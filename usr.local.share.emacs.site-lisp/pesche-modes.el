;; Pesche' Modes
;;
;;     $Source: g:/archiv/cvsroot/site-lisp/pesche-modes.el,v $
;;   $Revision: 1.1 $
;;       $Date: 1999/06/02 21:09:17 $
;;     $Author: pesche $

;; lisp modes
(defun pesche-emacs-lisp-mode-hook()
  (setq-default tab-width        8
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

(defun pesche-c-mode-common-hook()
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
  (setq-default tab-width                4
                indent-tabs-mode         nil
                c-tab-always-indent      nil
                c++-tab-always-indent    nil)

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
  (setq-default tab-width        8
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


;; assembler mode --------------------------------------------------------------
(defun pesche-asm-mode-hook()
  (local-unset-key (kbd "<tab>"))
  (local-unset-key (kbd ":"))
  (local-unset-key (kbd ";"))
  (local-unset-key (kbd "RET"))
  (setq-default tab-width        8
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
(autoload 'cperl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
;; cperl-mode statt perl-mode verwenden
(setq auto-mode-alist
      (append '(("\\.[pP][Llm]\\'" . cperl-mode)) auto-mode-alist ))
(setq interpreter-mode-alist (append interpreter-mode-alist
         '(("miniperl" . cperl-mode))))

;; hairy ist etwas allzu haarig...
;(setq cperl-hairy t)


(defun pesche-cperl-mode-hook()
  ;; einrücken: 4 Zeichen, 'else' darf ohne '{' auf eigener Zeile stehen
  (cperl-set-style "C++")
  (imenu-add-to-menubar "Index")
  )

(add-hook 'cperl-mode-hook 'pesche-cperl-mode-hook)


;; html mode -------------------------------------------------------------------
(autoload 'html-helper-mode "html-helper-mode" "HTML major mode." t)
;; html-helper-mode statt html-mode verwenden
(setq auto-mode-alist
      (append '(("\\.s?html?\\'" . html-helper-mode)) auto-mode-alist))
(setq html-helper-use-expert-menu t)

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

(defun pesche-html-helper-mode-hook()
  (html-msb-fix)
  )

(add-hook 'html-helper-mode-hook 'pesche-html-helper-mode-hook)


;; Modul abschliessen ----------------------------------------------------------
(provide 'pesche-modes)

;; eof


