;;----------------------------------------------------------- -*- Emacs-Lisp -*-
;; Improvements to Electric Font Lock Mode
;; Author: Pesche <unistein@isbe.ch>

;;; This package provides some improvements to `font-lock-mode':

;;;  a) c- and c++- keywords-1 recognize indented preprocessor commands.
;;;     This is done by overwriting the definition from font-lock.el
;;;     (perhaps there is a more elegant way; I tried font-lock-extra.el
;;;     but it didn't work)

;;;; Installation:

;;; Put
;;    (require 'pesche-font-lock)
;;; into your .emacs file (preferably short after (require 'font-lock)).

(require 'font-lock)

;; Definition tab und space face ---------------------------------------
(if (memq (framep (selected-frame)) '(x pc win32))
    (progn
      (make-face 'pesche-tab-face)
      (make-face 'pesche-space-face)
      (set-face-background 'pesche-tab-face   "Moccasin")
;      (set-face-background 'pesche-tab-face   "CornflowerBlue")
      (set-face-background 'pesche-space-face "Gold")
      ))

;(if (fboundp 'facemenu-unlisted-faces)
;    (progn
;      (add-to-list 'facemenu-unlisted-faces 'pesche-tab-face)
;      (add-to-list 'facemenu-unlisted-faces 'pesche-space-face)))
(defvar pesche-tab-face 'pesche-tab-face
  "Face to use for highlighting tab characters in Font-Lock mode.")
(defvar pesche-space-face 'pesche-space-face
  "Face to use for highlighting trailing spaces in Font-Lock mode.")

;; C/C++ font lock -----------------------------------------------------
;; Kopie aus font-lock.el (Emacs 19.34.6), 
;; aber Präprozessor-Kommandi dürfen eingerückt sein
(let ((c-keywords
;      ("break" "continue" "do" "else" "for" "if" "return" "switch" "while")
       "break\\|continue\\|do\\|else\\|for\\|if\\|return\\|switch\\|while")
      (c-type-types
;      ("auto" "extern" "register" "static" "typedef" "struct" "union" "enum"
;	"signed" "unsigned" "short" "long" "int" "char" "float" "double"
;	"void" "volatile" "const")
       (concat "auto\\|c\\(har\\|onst\\)\\|double\\|e\\(num\\|xtern\\)\\|"
	       "float\\|int\\|long\\|register\\|"
	       "s\\(hort\\|igned\\|t\\(atic\\|ruct\\)\\)\\|typedef\\|"
	       "un\\(ion\\|signed\\)\\|vo\\(id\\|latile\\)"))	; 6 ()s deep.
      (c++-keywords
;      ("break" "continue" "do" "else" "for" "if" "return" "switch" "while"
;	"asm" "catch" "delete" "new" "operator" "sizeof" "this" "throw" "try"
;       "protected" "private" "public")
       (concat "asm\\|break\\|c\\(atch\\|ontinue\\)\\|d\\(elete\\|o\\)\\|"
	       "else\\|for\\|if\\|new\\|"
	       "p\\(r\\(ivate\\|otected\\)\\|ublic\\)\\|return\\|"
	       "s\\(izeof\\|witch\\)\\|t\\(h\\(is\\|row\\)\\|ry\\)\\|while"))
      (c++-type-types
;      ("auto" "extern" "register" "static" "typedef" "struct" "union" "enum"
;	"signed" "unsigned" "short" "long" "int" "char" "float" "double"
;	"void" "volatile" "const" "class" "inline" "friend" "bool"
;	"virtual" "complex" "template")
       (concat "auto\\|bool\\|c\\(har\\|lass\\|o\\(mplex\\|nst\\)\\)\\|"
	       "double\\|e\\(num\\|xtern\\)\\|f\\(loat\\|riend\\)\\|"
	       "in\\(line\\|t\\)\\|long\\|register\\|"
	       "s\\(hort\\|igned\\|t\\(atic\\|ruct\\)\\)\\|"
	       "t\\(emplate\\|ypedef\\)\\|un\\(ion\\|signed\\)\\|"
	       "v\\(irtual\\|o\\(id\\|latile\\)\\)"))		; 11 ()s deep.
      )

 ;; c font lock keywords 1 ---------------------------------------------
 (setq c-font-lock-keywords-1
  (list
   ;;
   ;; These are all anchored at the beginning of line for speed.
   ;;
   ;; Fontify function name definitions (GNU style; without type on line).
   (list (concat "^\\(\\sw+\\)[ \t]*(") 1 'font-lock-function-name-face)
   ;;
   ;; Fontify filenames in #include <...> preprocessor directives as strings.
   '("^[ \t]*#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)" 1 font-lock-string-face)
   ;;
   ;; Fontify function macro names.
   '("^[ \t]*#[ \t]*define[ \t]+\\(\\sw+\\)(" 1 font-lock-function-name-face)
   ;;
   ;; Fontify symbol names in #elif or #if ... defined preprocessor directives.
   '("^[ \t]*#[ \t]*\\(elif\\|if\\)\\>"
     ("\\<\\(defined\\)\\>[ \t]*(?\\(\\sw+\\)?" nil nil
      (1 font-lock-reference-face) (2 font-lock-variable-name-face nil t)))
   ;;
   ;; Fontify otherwise as symbol names, and the preprocessor directive names.
   '("^[ \t]*#[ \t]*\\(\\sw+\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-reference-face) (2 font-lock-variable-name-face nil t))
   ))

 ;; c font lock keywords 2 ---------------------------------------------
 (setq c-font-lock-keywords-2
  (append c-font-lock-keywords-1
   (list
    ;;
    ;; Simple regexps for speed.
    ;;
    ;; Fontify all type specifiers.
    (cons (concat "\\<\\(" c-type-types "\\)\\>") 'font-lock-type-face)
    ;;
    ;; Fontify all builtin keywords (except case, default and goto; see below).
    (cons (concat "\\<\\(" c-keywords "\\)\\>") 'font-lock-keyword-face)
    ;;
    ;; Fontify case/goto keywords and targets, and case default/goto tags.
    '("\\<\\(case\\|goto\\)\\>[ \t]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-reference-face nil t))
    '("^[ \t]*\\(\\sw+\\)[ \t]*:" 1 font-lock-reference-face)
    )))

 ;; c font lock keywords 3 ---------------------------------------------
 (setq c-font-lock-keywords-3
  (append c-font-lock-keywords-2
   ;;
   ;; More complicated regexps for more complete highlighting for types.
   ;; We still have to fontify type specifiers individually, as C is so hairy.
   (list
    ;;
    ;; Fontify all storage classes and type specifiers, plus their items.
    (list (concat "\\<\\(" c-type-types "\\)\\>"
		  "\\([ \t*&]+\\sw+\\>\\)*")
	  ;; Fontify each declaration item.
	  '(font-lock-match-c++-style-declaration-item-and-skip-to-next
	    ;; Start with point after all type specifiers.
	    (goto-char (or (match-beginning 8) (match-end 1)))
	    ;; Finish with point after first type specifier.
	    (goto-char (match-end 1))
	    ;; Fontify as a variable or function name.
	    (1 (if (match-beginning 4)
		   font-lock-function-name-face
		 font-lock-variable-name-face))))
    ;;
    ;; Fontify structures, or typedef names, plus their items.
    '("\\(}\\)[ \t*]*\\sw"
      (font-lock-match-c++-style-declaration-item-and-skip-to-next
       (goto-char (match-end 1)) nil
       (1 (if (match-beginning 4)
	      font-lock-function-name-face
	    font-lock-variable-name-face))))
    ;;
    ;; Fontify anything at beginning of line as a declaration or definition.
    '("^\\(\\sw+\\)\\>\\([ \t*]+\\sw+\\>\\)*"
      (1 font-lock-type-face)
      (font-lock-match-c++-style-declaration-item-and-skip-to-next
       (goto-char (or (match-beginning 2) (match-end 1))) nil
       (1 (if (match-beginning 4)
	      font-lock-function-name-face
	    font-lock-variable-name-face))))
   ;;
   ;; Highlight tabs
   '("[\t]+" . pesche-tab-face)
   ;;
   ;; Highlight trailing whitespace
   '("[ \t]+$" . pesche-space-face)
    )))

 ;; c++ font lock keywords 1 -------------------------------------------
 (setq c++-font-lock-keywords-1
  (append
   ;;
   ;; The list `c-font-lock-keywords-1' less that for function names.
   (cdr c-font-lock-keywords-1)
   ;;
   ;; Fontify function name definitions, possibly incorporating class name.
   (list
    '("^\\(\\sw+\\)\\(::\\(\\sw+\\)\\)?[ \t]*("
      (1 (if (match-beginning 2)
	     font-lock-type-face
	   font-lock-function-name-face))
      (3 font-lock-function-name-face nil t))
    )))

 ;; c++ font lock keywords 2 -------------------------------------------
 (setq c++-font-lock-keywords-2
  (append c++-font-lock-keywords-1
   (list
    ;;
    ;; The list `c-font-lock-keywords-2' for C++ plus operator overloading.
    (cons (concat "\\<\\(" c++-type-types "\\)\\>") 'font-lock-type-face)
    ;;
    ;; Fontify operator function name overloading.
    '("\\<\\(operator\\)\\>[ \t]*\\([[(><!=+-][])><=+-]?\\)?"
      (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
    ;;
    ;; Fontify case/goto keywords and targets, and case default/goto tags.
    '("\\<\\(case\\|goto\\)\\>[ \t]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-reference-face nil t))
    '("^[ \t]*\\(\\sw+\\)[ \t]*:[^:]" 1 font-lock-reference-face)
    ;;
    ;; Fontify other builtin keywords.
    (cons (concat "\\<\\(" c++-keywords "\\)\\>") 'font-lock-keyword-face)
    )))

 ;; c++ font lock keywords 3 -------------------------------------------
 (setq c++-font-lock-keywords-3
  (append c++-font-lock-keywords-2
   ;;
   ;; More complicated regexps for more complete highlighting for types.
   (list
    ;;
    ;; Fontify all storage classes and type specifiers, plus their items.
    (list (concat "\\<\\(" c++-type-types "\\)\\>"
		  "\\([ \t*&]+\\sw+\\>\\)*")
	  ;; Fontify each declaration item.
	  '(font-lock-match-c++-style-declaration-item-and-skip-to-next
	    ;; Start with point after all type specifiers.
	    (goto-char (or (match-beginning 13) (match-end 1)))
	    ;; Finish with point after first type specifier.
	    (goto-char (match-end 1))
	    ;; Fontify as a variable or function name.
	    (1 (cond ((match-beginning 2) font-lock-type-face)
		     ((match-beginning 4) font-lock-function-name-face)
		     (t font-lock-variable-name-face)))
	    (3 (if (match-beginning 4)
		   font-lock-function-name-face
		 font-lock-variable-name-face) nil t)))
    ;;
    ;; Fontify structures, or typedef names, plus their items.
    '("\\(}\\)[ \t*]*\\sw"
      (font-lock-match-c++-style-declaration-item-and-skip-to-next
       (goto-char (match-end 1)) nil
       (1 (if (match-beginning 4)
	      font-lock-function-name-face
	    font-lock-variable-name-face))))
    ;;
    ;; Fontify anything at beginning of line as a declaration or definition.
    '("^\\(\\sw+\\)\\>\\([ \t*]+\\sw+\\>\\)*"
      (1 font-lock-type-face)
      (font-lock-match-c++-style-declaration-item-and-skip-to-next
       (goto-char (or (match-beginning 2) (match-end 1))) nil
       (1 (cond ((match-beginning 2) font-lock-type-face)
		((match-beginning 4) font-lock-function-name-face)
		(t font-lock-variable-name-face)))
       (3 (if (match-beginning 4)
	      font-lock-function-name-face
	    font-lock-variable-name-face) nil t)))
    )))
 )


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


(provide 'pesche-font-lock)

;; eof