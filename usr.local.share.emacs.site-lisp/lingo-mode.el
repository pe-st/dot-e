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


(defconst lingo-indent-offset 2
  "basic indentaion steps")


(defconst lingo-handler-regexp "^[ \t]*on[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
  "Regular expression to match the name of a handler.")

;; all lines starting with "end" except "end if", "end case" and "end repeat"
(defconst lingo-handler-end-regexp
  (concat "^[ \t]*end[ \t]*\\("
          "[abd-hj-qs-z].*"
          ;; some lines missing here...
          "\\)?$"))

(defconst lingo-blank-regexp "^[ \t]*$")



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
        indent-region-function  'lingo-indent-region
        indent-line-function    'lingo-indent-line
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
  )


(defun lingo-indent-region (start end)
  "Perform lingo-indent-line on each line in region."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (while (and (not (eobp))
                (< (point) end))
      (if (not (looking-at lingo-blank-regexp))
          (lingo-indent-line))
      (forward-line 1)))

  (cond ((fboundp 'zmacs-deactivate-region)
         (zmacs-deactivate-region))
        ((fboundp 'deactivate-mark)
         (deactivate-mark))))


(defun lingo-indent-line ()
  "Indent current line for `lingo-mode'."
  (interactive)
  (lingo-indent-to (lingo-calculate-indent)))


(defun lingo-indent-to (col)
  (let* ((bol (save-excursion
                (beginning-of-line)
                (point)))
         (point-in-whitespace
          (<= (point) (+ bol (current-indentation))))
         (blank-line-p
          (save-excursion
            (beginning-of-line)
            (looking-at lingo-blank-regexp))))

    (cond ((/= col (current-indentation))
           (save-excursion
             (beginning-of-line)
             (back-to-indentation)
             (delete-region bol (point))
             (indent-to col))))

    ;; If point was in the whitespace, move back-to-indentation.
    (cond (blank-line-p
           (end-of-line))
          (point-in-whitespace
           (back-to-indentation)))))


;; borrowed from visual-basic-mode
(defun lingo-calculate-indent ()
  "calculate the indentation for the current line."
  (let ((original-point (point)))
    (save-excursion
      (beginning-of-line)
      ;; first version: detect beginning and end of handlers, all other
      ;; lines get constant indentation
      (cond
       ((or (looking-at lingo-handler-regexp)
            (looking-at lingo-handler-end-regexp))
        0)

       ;; all other cases
       (t lingo-indent-offset)
       ))))

;;        ;; The outdenting stmts, which simply match their original.
;;          ((or (looking-at visual-basic-else-regexp)
;;               (looking-at visual-basic-endif-regexp))
;;           (visual-basic-find-matching-if)
;;           (current-indentation))

;;          ;; All the other matching pairs act alike.
;;          ((looking-at visual-basic-next-regexp) ; for/next
;;           (visual-basic-find-matching-for)
;;           (current-indentation))

;;          ((looking-at visual-basic-loop-regexp) ; do/loop
;;           (visual-basic-find-matching-do)
;;           (current-indentation))

;;          ((looking-at visual-basic-wend-regexp) ; while/wend
;;           (visual-basic-find-matching-while)
;;           (current-indentation))

;;          ((looking-at visual-basic-end-with-regexp) ; with/end with
;;           (visual-basic-find-matching-with)
;;           (current-indentation))
           
;;          ((looking-at visual-basic-select-end-regexp) ; select case/end select
;;           (visual-basic-find-matching-select)
;;           (current-indentation))

;;          ;; A case of a select is somewhat special.
;;          ((looking-at visual-basic-case-regexp)
;;           (visual-basic-find-matching-select)
;;           (+ (current-indentation) visual-basic-mode-indent))

;;             ;; Added KJW: Make sure that this comes after the cases
;;             ;; for if..endif, end select because end-regexp will also
;;             ;; match "end select" etc.
;;          ((looking-at visual-basic-end-begin-regexp) ; begin/end 
;;           (visual-basic-find-matching-begin)
;;           (current-indentation))

;;          (t
;;           ;; Other cases which depend on the previous line.
;;           (visual-basic-previous-line-of-code)

;;           ;; Skip over label lines, which always have 0 indent.
;;           (while (looking-at visual-basic-label-regexp)
;;             (visual-basic-previous-line-of-code))

;;           (cond 
;;            ((looking-at visual-basic-continuation-regexp)
;;             (visual-basic-find-original-statement)
;;             ;; Indent continuation line under matching open paren,
;;             ;; or else one word in.
;;             (let* ((orig-stmt (point))
;;                    (matching-open-paren
;;                     (condition-case ()
;;                         (save-excursion
;;                           (goto-char original-point)
;;                           (beginning-of-line)
;;                           (backward-up-list 1)
;;                           ;; Only if point is now w/in cont. block.
;;                           (if (<= orig-stmt (point))
;;                               (current-column)))
;;                       (error nil))))
;;               (cond (matching-open-paren
;;                      (1+ matching-open-paren))
;;                     (t
;;                      ;; Else, after first word on original line.
;;                      (back-to-indentation)
;;                      (forward-word 1)
;;                      (while (looking-at "[ \t]")
;;                        (forward-char 1))
;;                      (current-column)))))
;;            (t
;;             (visual-basic-find-original-statement)

;;             (let ((indent (current-indentation)))
;;               ;; All the various +indent regexps.
;;               (cond ((looking-at visual-basic-defun-start-regexp)
;;                      (+ indent visual-basic-mode-indent))

;;                     ((and (or (looking-at visual-basic-if-regexp)
;;                               (looking-at visual-basic-else-regexp))
;;                           (not (and visual-basic-allow-single-line-if
;;                                     (looking-at visual-basic-ifthen-regexp))))
;;                      (+ indent visual-basic-mode-indent))

;;                     ((or (looking-at visual-basic-select-regexp)
;;                          (looking-at visual-basic-case-regexp))
;;                      (+ indent visual-basic-mode-indent))
                        
;;                     ((or (looking-at visual-basic-do-regexp)
;;                          (looking-at visual-basic-for-regexp)
;;                          (looking-at visual-basic-while-regexp)
;;                          (looking-at visual-basic-with-regexp)
;;                          (looking-at visual-basic-begin-regexp))
;;                      (+ indent visual-basic-mode-indent))

;;                     (t
;;                      ;; By default, just copy indent from prev line.
;;                      indent))))))))))



(defun lingo-version ()
  "Echo the current version of `lingo-mode' in the minibuffer."
  (interactive)
  (message "Using `lingo-mode' version %s" lingo-version)
;;;  (lingo-keep-region-active)
  )


(provide 'lingo-mode)
;;; lingo-mode.el ends here
