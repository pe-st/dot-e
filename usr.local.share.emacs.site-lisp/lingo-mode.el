;;; lingo-mode.el --- major mode for editing Lingo (Macromedia Director
;;; script language) in Emacs
;;
;; Copyright (C) 2002 Peter Steiner
;;
;; Author:  Peter Steiner
;; Created: 2002-05-13 17:39:58
;; Keywords: lingo, director

;; This file is not part of GNU Emacs.

;;; Put this in your .emacs file to enable lingo-mode

;; (autoload 'lingo-mode "lingo-mode" "Major mode for editing Lingo files." t)
;; (setq auto-mode-alist (append '(("\\.[Ll][Ss]\\'" . lingo-mode)) auto-mode-alist))


(defconst lingo-version "0.1"
  "`lingo-mode' version number.")


(defconst lingo-handler-regexp "\\bon[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
  "Regular expression to match the name of a handler.")


(defvar lingo-font-lock-keywords
  (let ((lingo-keyword-list
         (mapconcat 'identity
                    '("beginRecording" "case" "castLib" "char" "down"
                      "else" "end"
                      "endRecording" "exit" "field" "global" "if" "item"
                      "in" "intersects" "line" "list" "loop" "member"
                      "menu" "next" "of" "on" "otherwise" "property"
                      "repeat" "return" "sprite" "the" "then" "to"
                      "version" "while" "window" "with" "within")
                    "\\|"))
        (lingo-command-list
         (mapconcat 'identity
                    '("abort" "add" "cancelIdleLoad"
                      "go")
                    "\\|"))
        (lingo-property-list
         (mapconcat 'identity
                    '("actionsEnabled" "castLibNum"
                      "locH" "locV")
                    "\\|"))
        (lingo-movie-property-list
         (mapconcat 'identity
                    '("activeWindow" "actorList")
                    "\\|"))
        (lingo-system-property-list
         (mapconcat 'identity
                    '("activeCastLib")
                    "\\|"))
        (lingo-event-list
         (mapconcat 'identity
                    '("activateApplication" "activateWindow" "startMovie")
                    "\\|"))
        (lingo-function-list
         (mapconcat 'identity
                    '("abs")
                    "\\|"))
        (lingo-constant-list (mapconcat 'identity '("false" "true") "\\|"))
        )
    (list
     ;; keywords
     (cons (concat "\\b\\("
                   lingo-keyword-list lingo-constant-list
                   "\\)\\b[ \n\t(]") 1)
     ;; commands
     `(,(concat "\\b\\("
               lingo-command-list
               "\\)\\b[ \n\t(]") 1 font-lock-type-face)
     ;; properties
     `(,(concat "\\b\\("
               lingo-property-list "\\|"
               lingo-movie-property-list "\\|"
               lingo-system-property-list
               "\\)\\b[ \n\t(]") 1 font-lock-builtin-face)
     ;; events
     `(,(concat "\\b\\("
               lingo-event-list
               "\\)\\b[ \n\t(]") 1 font-lock-function-name-face)
     ;; functions
     (list lingo-handler-regexp
       1 font-lock-variable-name-face)
     ;; constants
     `(,(concat "\\b\\("
               lingo-constant-list
               "\\)\\b[ \n\t(]") 1 font-lock-constant-face)
     ))
  "Additional expressions to highlight in Lingo mode.")


(defvar lingo-font-lock-defaults
  '(lingo-font-lock-keywords
    nil ; keywords-only
    t   ; case-fold
    nil ; syntax-alist
    nil ; syntax-begin
    )
  "Font-lock defaults used for Lingo syntax coloring.")



(defvar lingo-mode-hook nil
  "*Hook called by `lingo-mode'.")

(defvar lingo-mode-map ()
  "Keymap used in `lingo-mode' buffers.")
(if lingo-mode-map
    nil
  (setq lingo-mode-map (make-sparse-keymap))
;;   ;; electric keys
;;   (define-key lingo-mode-map ":" 'lingo-electric-colon)
;;   ;; indentation level modifiers
;;   (define-key lingo-mode-map "\C-c\C-l"  'lingo-shift-region-left)
  ;; Miscellaneous
  (define-key lingo-mode-map "\C-c\C-v" 'lingo-version)
  )


(defvar lingo-mode-syntax-table nil
  "Syntax table used in `lingo-mode' buffers.")
(if lingo-mode-syntax-table
    nil
  (setq lingo-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()" lingo-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" lingo-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" lingo-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lingo-mode-syntax-table)
;;   (modify-syntax-entry ?\{ "(}" lingo-mode-syntax-table)
;;   (modify-syntax-entry ?\} "){" lingo-mode-syntax-table)
;;   ;; Add operator symbols misassigned in the std table
;;   (modify-syntax-entry ?\$ "."  lingo-mode-syntax-table)
;;   (modify-syntax-entry ?\% "."  lingo-mode-syntax-table)
;;   (modify-syntax-entry ?\& "."  lingo-mode-syntax-table)
;;   (modify-syntax-entry ?\* "."  lingo-mode-syntax-table)
;;   (modify-syntax-entry ?\+ "."  lingo-mode-syntax-table)
;;   (modify-syntax-entry ?\- "."  lingo-mode-syntax-table)
;;   (modify-syntax-entry ?\/ "."  lingo-mode-syntax-table)
;;   (modify-syntax-entry ?\< "."  lingo-mode-syntax-table)
;;   (modify-syntax-entry ?\= "."  lingo-mode-syntax-table)
;;   (modify-syntax-entry ?\> "."  lingo-mode-syntax-table)
;;   (modify-syntax-entry ?\| "."  lingo-mode-syntax-table)
  ;; GNU conventions say underscore should be symbol class, but
  ;; there's a natural conflict between what major mode authors want
  ;; and what users expect from `forward-word' and `backward-word'.
  (modify-syntax-entry ?\_ "w"  lingo-mode-syntax-table)
;;   ;; Both single quote and double quote are string delimiters
;;   (modify-syntax-entry ?\' "\"" lingo-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" lingo-mode-syntax-table)
;;   ;; backquote is open and close paren
;;   (modify-syntax-entry ?\` "$"  lingo-mode-syntax-table)
  ;; comment delimiters
  (modify-syntax-entry ?\- ". 12"  lingo-mode-syntax-table)
  (modify-syntax-entry ?\n ">"  lingo-mode-syntax-table)
  )


(defun lingo-mode ()
  "Major mode for editing Lingo files.
This mode knows about Lingo indentation, tokens, comments and
continuation lines.

COMMANDS
\\{lingo-mode-map}
VARIABLES

lingo-indent-offset\t\tindentation increment"
  (interactive)
  ;; set up local variables
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
;;   (make-local-variable 'paragraph-separate)
;;   (make-local-variable 'paragraph-start)
;;   (make-local-variable 'require-final-newline)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)
;;   (make-local-variable 'comment-indent-function)
;;   (make-local-variable 'indent-region-function)
;;   (make-local-variable 'indent-line-function)
;;   (make-local-variable 'add-log-current-defun-function)
  ;;
  (set-syntax-table lingo-mode-syntax-table)
  (setq major-mode              'lingo-mode
        mode-name               "Lingo"
;;      local-abbrev-table      lingo-mode-abbrev-table
        font-lock-defaults      lingo-font-lock-defaults
;;      paragraph-separate      "^[ \t]*$"
;;      paragraph-start         "^[ \t]*$"
;;      require-final-newline   t
        comment-start           "-- "
        comment-end             ""
        comment-start-skip      "-- *"
        comment-column          40
;;      comment-indent-function 'lingo-comment-indent-function
;;      indent-region-function  'lingo-indent-region
;;      indent-line-function    'lingo-indent-line
;;      ;; tell add-log.el how to find the current function/method/variable
;;      add-log-current-defun-function 'lingo-current-defun
        )
  (setq imenu-generic-expression
        (list (list nil lingo-handler-regexp 1)))
  (use-local-map lingo-mode-map)
;;   ;; add the menu
;;   (if py-menu
;;       (easy-menu-add py-menu))
;;   ;; Emacs 19 requires this
;;   (if (boundp 'comment-multi-line)
;;       (setq comment-multi-line nil))
;;   ;; Install Imenu if available
;;   (when (py-safe (require 'imenu))
;;     (setq imenu-create-index-function #'py-imenu-create-index-function)
;;     (setq imenu-generic-expression py-imenu-generic-expression)
;;     (if (fboundp 'imenu-add-to-menubar)
;;      (imenu-add-to-menubar (format "%s-%s" "IM" mode-name)))
;;     )

  ;; Run the mode hook.
  (run-hooks 'lingo-mode-hook)

;;   ;; Now do the automagical guessing
;;   (if py-smart-indentation
;;     (let ((offset py-indent-offset))
;;       ;; It's okay if this fails to guess a good value
;;       (if (and (py-safe (py-guess-indent-offset))
;;             (<= py-indent-offset 8)
;;             (>= py-indent-offset 2))
;;        (setq offset py-indent-offset))
;;       (setq py-indent-offset offset)
;;       ;; Only turn indent-tabs-mode off if tab-width !=
;;       ;; py-indent-offset.  Never turn it on, because the user must
;;       ;; have explicitly turned it off.
;;       (if (/= tab-width py-indent-offset)
;;        (setq indent-tabs-mode nil))
;;       ))
;;   ;; Set the default shell if not already set
;;   (when (null py-which-shell)
;;     (py-toggle-shells py-default-interpreter))
  )

(defun lingo-version ()
  "Echo the current version of `lingo-mode' in the minibuffer."
  (interactive)
  (message "Using `lingo-mode' version %s" lingo-version)
;;;  (lingo-keep-region-active)
  )


(provide 'lingo-mode)
;;; lingo-mode.el ends here
