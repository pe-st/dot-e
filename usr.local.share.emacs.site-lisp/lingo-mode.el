;;; lingo-mode.el --- major mode for editing Lingo (Macromedia Director
;;; script language) in Emacs
;;
;; Copyright (C) 2002 Peter Steiner
;;
;; Author:  Peter Steiner
;; Created: 2002-05-13 17:39:58
;; Keywords: lingo, director

;; This file is not part of GNU Emacs.

(require 'generic)
(require 'font-lock)


(defvar lingo-keyword-list
  (list
   "beginRecording" "case" "castLib" "char" "down" "end" "endRecording" "exit"
   "field" "global" "if" "item" "in" "intersects" "line" "list" "loop" "member"
   "menu" "next" "of" "on" "otherwise" "property" "repeat" "return"
   "sprite" "the" "to" "version" "while" "window" "with" "within"
   )
  "Lingo keywords.")

(defvar lingo-command-list
  (list
   "abort" "add" "cancelIdleLoad"
   )
  "Lingo commands.")

(defvar lingo-property-list
  (list
   "actionsEnabled" "castLibNum"
   )
  "Cast member and sprite properties.")

(defvar lingo-movie-property-list
  (list
   "activeWindow" "actorList"
   )
  "Movie properties.")

(defvar lingo-system-property-list
  (list
   "activeCastLib"
   )
  "System properties.")

(defvar lingo-event-list
  (list
   "activateApplication" "activateWindow"
   )
  "Lingo events.")

(defvar lingo-function-list
  (list
   "abs"
   )
  "Lingo functions.")


(defun lingo-mode-setup ()
;;   ; damit font-lock auch bei Bezeichner mit einem '_' richtig funktioniert,
;;   ; muss die syntax-table geändert werden. Dazu gibt es extra eine lokale
;;   ; font-lock-syntax-table. Leider wird die von generic.el nicht unterstützt,
;;   ; aber die folgende Zeile hilft dem ab
;;   ; (und das 't' sagt, dass die Schlüsselwörter case-insensitiv sind)
;;   (nconc font-lock-defaults '(t ((?_ . "w"))))

  ; imenu support
;; ;;   (use-local-map (nconc (make-sparse-keymap) pilrc-mode-map))
;;   (setq imenu-generic-expression
;;         `((nil ,(concat "^\\("
;;                        ;; Use an optimized regexp.
;;                        (regexp-opt pilrc-command-list t)
;;                        "\\>.*\\)$")
;;                1))
;;         imenu-case-fold-search t)
;;   (imenu-add-to-menubar "Index")
  )


(define-generic-mode 'lingo-generic-mode
  (list "--") ; comment char
  lingo-keyword-list ; keywords
  ; font-lock-list
  (list
;;    ;; Autovalues
;;    (generic-make-keywords-list pilrc-autovalue-list
;;                                'font-lock-type-face)
   ;; Commands
   ;; Objects
   (generic-make-keywords-list
    (append lingo-command-list lingo-property-list)
    'font-lock-type-face)
   ;; function names
   '("\\bon\\b\\s +\\b\\(\\(\\w\\|_\\)+\\)\\b"
     (1 'font-lock-variable-name-face)
     )
;;    ;; ID-Attributes (with an ID parameter)
;;                                         ; defaultbtnid id helpid menuid
;;    '("\\b\\(MENUID\\|ID\\|HELPID\\|DEFAULTBTNID\\)\\b\\s +\\b\\(\\(\\w\\|_\\)+\\)\\b"
;;      (1 'font-lock-keyword-face)
;;      (2 'font-lock-variable-name-face)
;;      )
   )
  (list "\\.ls\\'") ; auto-mode-list
  (list 'lingo-mode-setup)
  "Major mode for lingo.")

