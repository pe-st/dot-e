;; Improvements to Electric Font Lock Mode
;;
;;         $Id: //netzadmin/emacs/site-lisp/pesche-font-lock.el#13 $
;;     $Change: 19560 $
;;   $DateTime: 2004/08/30 09:52:43 $
;;     $Author: peter.steiner $
;;    $Created: 1997/11/27 $
;;  $Copyright: Peter Steiner <pesche@schlau.ch>

;;; This package provides some improvements to `font-lock-mode':

;;;  a) c- and c++- keywords-1 recognize indented preprocessor commands.
;;;     This is done by overwriting the definition from font-lock.el
;;;     (perhaps there is a more elegant way; I tried font-lock-extra.el
;;;     but it didn't work)

;;;; Installation:

;;; Put
;;    (require 'pesche-font-lock)
;;; into your .emacs file (preferably shortly after (require 'font-lock)).

(require 'font-lock)

;; Definition tab und space face ---------------------------------------
;; für Tabs zwei Faces, eines für solche Modi, wo Tab nicht erwünscht ist
;; (strong) und eines für erwünschte Tabs (zB assembler/makefile)
(if (memq (framep (selected-frame)) '(x pc win32 w32 mac))
    (progn
      (make-face 'pesche-tab-face)
      (make-face 'pesche-strong-tab-face)
      (make-face 'pesche-space-face)
      (make-face 'pesche-hardspace-face)
      (make-face 'hw-keyword-face)
      (set-face-background 'pesche-tab-face        "LemonChiffon")
      (set-face-background 'pesche-strong-tab-face "Moccasin")
      (set-face-background 'pesche-space-face      "Gold")
      (set-face-background 'pesche-hardspace-face  "PaleGreen")
;      (set-face-background 'pesche-hardspace-face  "CornflowerBlue")
;      (set-face-background 'pesche-hardspace-face  "LightSalmon")
;      (set-face-background 'pesche-hardspace-face  "Wheat")
      (set-face-foreground 'hw-keyword-face        "Red")
      (defvar html-tag-face
        (defface html-tag-face
          '((t (:foreground "Firebrick" :bold t)))
          "Face to use for HTML tags."
          :group 'html-helper-faces))
      (defvar html-helper-bold-face
        (defface html-helper-bold-face
          '((t (:foreground "Peru" :bold t)))
          "Custom bold face."
          :group 'html-helper-faces))
      (defvar html-helper-italic-face
        (defface html-helper-italic-face
          '((t (:foreground "Peru" :italic t)))
          "Custom italic face."
          :group 'html-helper-faces))
      (defvar html-helper-bold-italic-face
        (defface html-helper-bold-italic-face
          '((t (:foreground "Orange" :bold t :italic t)))
          "Custom bold italic face."
          :group 'html-helper-faces))
      (defvar html-helper-underline-face
        (defface html-helper-underline-face
          '((t (:foreground "Peru" :underline t)))
          "Custom underline face."
          :group 'html-helper-faces))
      ))

;(if (fboundp 'facemenu-unlisted-faces)
;    (progn
;      (add-to-list 'facemenu-unlisted-faces 'pesche-tab-face)
;      (add-to-list 'facemenu-unlisted-faces 'pesche-space-face)))
(defvar pesche-tab-face 'pesche-tab-face
  "Face to use for highlighting tab characters in Font-Lock mode.")
(defvar pesche-strong-tab-face 'pesche-strong-tab-face
  "Face to use for strong highlighting tab characters in Font-Lock mode.")
(defvar pesche-space-face 'pesche-space-face
  "Face to use for highlighting trailing spaces in Font-Lock mode.")
(defvar pesche-hardspace-face 'pesche-hardspace-face
  "Face to use for highlighting hard spaces in Font-Lock mode.")
(defvar hw-keyword-face 'hw-keyword-face
  "Face to use for highlighting keywords according HW coding guide.")


;; ;; HW-Schlüsselwörter (nach Richtlinien) für C und C++
;; (let* ((hw-c-keywords
;;         (eval-when-compile
;;           (regexp-opt
;;            '("bool" "true" "false" "uint8" "uint16" "uint32" "int8" "int16" "int32"
;;              "smallint" "smalluint" "_plm_call" "_bit" "_rom_mem" "_rom_ptr"
;;              "_near_mem" "_near_ptr" "_fastfar_mem" "_fastfar_ptr" "_far_mem"
;;              "_far_ptr" "_huge_ptr" "_stk_ptr" "_stk_mem") t)))
;;        )


;; Korrekturen für make mode font lock ---------------------------------
; font-lock-Definitionen von make-mode.el korrigieren
(load "make-mode")

; korrigiere regexp für Abhängigkeiten
(setq makefile-dependency-regex
      "^\\([^ \n\t#:]+\\([ \t]+[^ \t\n#:=]+\\)*\\)[ \t]*:\\([ \t]*$\\|\\([^=\n].*$\\)\\)")
;   dieses '=' fehlt in make-mode.el  --^--

(setq makefile-font-lock-keywords
  (list
   ;; Zuweisungen
   (list makefile-macroassign-regex 1 'font-lock-variable-name-face)

   ;; Makro-Expansionen
   '("\\$[({]\\([a-zA-Z0-9_]+\\)[:})]" 1 font-lock-reference-face prepend)
   ;;                          --^-- dieses ':' fehlt in make-mode.el

   ;; Abhängigkeiten
   (list makefile-dependency-regex 1 'font-lock-function-name-face 'prepend)

   ;; Highlight lines that contain just whitespace.
   ;; They can cause trouble, especially if they start with a tab.
   '("^[ \t]+$" . makefile-space-face)

   ;; Highlight shell comments that Make treats as commands,
   ;; since these can fool people.
   '("^\t+#" 0 makefile-space-face t)

   ;; Highlight spaces that precede tabs.
   ;; They can make a tab fail to be effective.
   '("^\\( +\\)\t" 1 makefile-space-face)))


;; font lock für jeden major mode anpassen -----------------------------
(add-hook 'font-lock-mode-hook
          '(lambda()
             (setq font-lock-keywords
                   (append font-lock-keywords
                           '(("[\t]+" (0 'pesche-tab-face t))
                             ("[\240]+" (0 'pesche-hardspace-face t))
;                              ; Leerzeichen abwechselnd färben
;                              ("\\(\040\\)\\(\040\\)?"
;                               (1 pesche-strong-tab-face) (2 pesche-space-face nil t))
                             ("[\040\t]+$" (0 'pesche-space-face t)))))))

;; doxymacs mode ---------------------------------------------------------------
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

(provide 'pesche-font-lock)

;; eof
