;; Hug-Witschi C Mode derived from CC Mode
;;
;;         $Id: //netzadmin/emacs/site-lisp/pesche-modes.el#14 $
;;     $Change: 19163 $
;;   $DateTime: 2004/05/28 09:35:56 $
;;     $Author: peter.steiner $
;;    $Created: 2004/08/28 from Martin Stjernholm's derived-mode-ex.el $

;;; Commentary:

;; This is a simple application of a separate mode derived from CC Mode
;; for C code in some Hug-Witschi projects. It defines some additional
;; keywords, but is otherwise like C.

;; Note: The interface used in this file requires CC Mode 5.30 or
;; later.

;;; Code:

(require 'cc-mode)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use C
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'hw-c-mode 'c-mode))

;; ;; list with all HW-C keywords
;; (c-lang-defconst hw-c-keywords
;;   hw-c '("bool" "true" "false" "uint8" "uint16" "uint32" "int8" "int16" "int32"
;;          "smallint" "smalluint" "_plm_call" "_bit" "_rom_mem" "_rom_ptr"
;;          "_near_mem" "_near_ptr" "_fastfar_mem" "_fastfar_ptr" "_far_mem"
;;          "_far_ptr" "_huge_ptr" "_stk_ptr" "_stk_mem"))

;; HW-C defines bool, _bit and fixed size integer types
(c-lang-defconst c-primitive-type-kwds
  hw-c (append '("bool" "_bit" "uint8" "uint16" "uint32" "int8" "int16" "int32"
                 "smallint" "smalluint")
               (c-lang-const c-primitive-type-kwds)))

;; treat all the _xxxx_ptr names like const and volatile
(c-lang-defconst c-type-modifier-kwds
  hw-c (append '("_rom_mem" "_rom_ptr" "_near_mem" "_near_ptr"
                 "_fastfar_mem" "_fastfar_ptr" "_far_mem" "_far_ptr"
                 "_huge_ptr" "_stk_ptr" "_stk_mem")
               (c-lang-const c-type-modifier-kwds)))

;; keywords specifying calling conventions
(c-lang-defconst c-other-decl-kwds
  hw-c (append '("_plm_call")
               (c-lang-const c-other-decl-kwds)))

(defcustom hw-c-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in HW-C mode.
Each list item should be a regexp matching a single identifier.")

(defconst hw-c-font-lock-keywords-1 (c-lang-const c-matchers-1 hw-c)
  "Minimal highlighting for HW-C mode.")

(defconst hw-c-font-lock-keywords-2 (c-lang-const c-matchers-2 hw-c)
  "Fast normal highlighting for HW-C mode.")

(defconst hw-c-font-lock-keywords-3 (c-lang-const c-matchers-3 hw-c)
  "Accurate normal highlighting for HW-C mode.")

(defvar hw-c-font-lock-keywords hw-c-font-lock-keywords-3
  "Default expressions to highlight in HW-C mode.")

(defvar hw-c-mode-syntax-table nil
  "Syntax table used in hw-c-mode buffers.")
(or hw-c-mode-syntax-table
    (setq hw-c-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table hw-c))))

(defvar hw-c-mode-abbrev-table nil
  "Abbreviation table used in hw-c-mode buffers.")
(c-define-abbrev-table 'hw-c-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)))

(defvar hw-c-mode-map (let ((map (c-make-inherited-keymap)))
                      ;; Add bindings which are only useful for HW-C
                      map)
  "Keymap used in hw-c-mode buffers.")

(easy-menu-define hw-c-menu hw-c-mode-map "HW-C Mode Commands"
                  ;; Can use `hw-c' as the language for `c-mode-menu'
                  ;; since its definition covers any language.  In
                  ;; this case the language is used to adapt to the
                  ;; nonexistence of a cpp pass and thus removing some
                  ;; irrelevant menu alternatives.
                  (cons "HW-C" (c-lang-const c-mode-menu hw-c)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hwc\\'" . hw-c-mode))

;;;###autoload
(defun hw-c-mode ()
  "Major mode for editing HW-C code.
There are some keywords more than for C Mode, but otherwise it is like C Mode.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `hw-c-mode-hook'.

Key bindings:
\\{hw-c-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table hw-c-mode-syntax-table)
  (setq major-mode 'hw-c-mode
        mode-name "HW-C"
        local-abbrev-table hw-c-mode-abbrev-table
        abbrev-mode t)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars hw-c-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'hw-c-mode)
  (easy-menu-add hw-c-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'hw-c-mode-hook)
  (c-update-modeline))


(provide 'hw-c-mode)

;;; hw-c-mode.el ends here
