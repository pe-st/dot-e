;;----------------------------------------------------------- -*- Emacs-Lisp -*-
;; Improvements to Electric Font Lock Mode
;; Author: Pesche <unistein@isbe.ch>
;;
;;     $Source: g:/archiv/cvsroot/site-lisp/pesche-font-lock.el,v $
;;   $Revision: 1.9 $
;;       $Date: 1999/12/11 18:34:08 $
;;     $Author: pesche $

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
;; für Tabs zwei Faces, eines für solche Modi, wo Tab nicht erwünscht ist
;; (strong) und eines für erwünschte Tabs (zB assembler/makefile)
(if (memq (framep (selected-frame)) '(x pc win32 w32))
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

;; C font lock ---------------------------------------------------------
;; Kopie aus font-lock.el (Emacs 20.5.1),
;; aber Präprozessor-Kommandi dürfen eingerückt sein
;; spezieller Font hw-keyword-face für HW-Schlüsselwörter (nach Richtlinien)
(let* ((c-keywords
        (eval-when-compile
          (regexp-opt '("break" "continue" "do" "else" "for" "if" "return"
                        "switch" "while" "sizeof"
                        ;; Type related, but we don't do anything special.
                        "typedef" "extern" "auto" "register" "static"
                        "volatile" "const"
                        ;; Dan Nicolaescu <done@gnu.org> says this is new.
                        "restrict") t)))
       (c-type-specs
        (eval-when-compile
          (regexp-opt '("enum" "struct" "union") t)))
       (c-type-specs-depth
        (regexp-opt-depth c-type-specs))
       (c-type-names
        `(mapconcat 'identity
          (cons
           (,@ (eval-when-compile
                 (regexp-opt
                  '("char" "short" "int" "long" "signed" "unsigned"
                    "float" "double" "void" "complex"))))
           c-font-lock-extra-types)
          "\\|"))
       (c-type-names-depth
        `(regexp-opt-depth (,@ c-type-names)))
       (hw-c-keywords
        (eval-when-compile
          (regexp-opt
           '("bool" "true" "false" "uint8" "uint16" "uint32" "int8" "int16" "int32"
             "smallint" "smalluint" "_plm_call" "_bit" "_rom_mem" "_rom_ptr"
             "_near_mem" "_near_ptr" "_fastfar_mem" "_fastfar_ptr" "_far_mem"
             "_far_ptr" "_huge_ptr" "_stk_ptr" "_stk_mem") t)))
       )

 ;; c font lock keywords 1 ---------------------------------------------
 (setq c-font-lock-keywords-1
  (list
   ;;
   ;; These are all anchored at the beginning of line for speed.
   ;; Note that `c++-font-lock-keywords-1' depends on `c-font-lock-keywords-1'.
   ;;
   ;; Fontify function name definitions (GNU style; without type on line).
   '("^\\(\\sw+\\)[ \t]*(" 1 font-lock-function-name-face)
   ;;
   ;; Fontify error directives.
   '("^[ \t]*#[ \t]*error[ \t]+\\(.+\\)" 1 font-lock-warning-face prepend)
   ;;
   ;; Fontify filenames in #include <...> preprocessor directives as strings.
   '("^[ \t]*#[ \t]*\\(import\\|include\\)[ \t]*\\(<[^>\"\n]*>?\\)"
     2 font-lock-string-face)
   ;;
   ;; Fontify function macro names.
   '("^[ \t]*#[ \t]*define[ \t]+\\(\\sw+\\)(" 1 font-lock-function-name-face)
   ;;
   ;; Fontify symbol names in #elif or #if ... defined preprocessor directives.
   '("^[ \t]*#[ \t]*\\(elif\\|if\\)\\>"
     ("\\<\\(defined\\)\\>[ \t]*(?\\(\\sw+\\)?" nil nil
      (1 font-lock-builtin-face) (2 font-lock-variable-name-face nil t)))
   ;;
   ;; Fontify otherwise as symbol names, and the preprocessor directive names.
   '("^[ \t]*#[ \t]*\\(\\sw+\\)\\>[ \t!]*\\(\\sw+\\)?"
     (1 font-lock-builtin-face) (2 font-lock-variable-name-face nil t))
   ))

 ;; c font lock keywords 2 ---------------------------------------------
 (setq c-font-lock-keywords-2
  (append c-font-lock-keywords-1
   (list
    ;;
    ;; Simple regexps for speed.
    ;;
    ;; Fontify all type names.
    `(eval .
      (cons (concat "\\<\\(" (,@ c-type-names) "\\)\\>") 'font-lock-type-face))
    ;;
    ;; Fontify all HW keywords.
    (cons (concat "\\<\\(" hw-c-keywords "\\)\\>") 'hw-keyword-face)
    ;;
    ;; Fontify all builtin keywords (except case, default and goto; see below).
    (concat "\\<\\(" c-keywords "\\|" c-type-specs "\\)\\>")
    ;;
    ;; Fontify case/goto keywords and targets, and case default/goto tags.
    '("\\<\\(case\\|goto\\)\\>[ \t]*\\(-?\\sw+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-constant-face nil t))
    ;; Anders Lindgren <andersl@andersl.com> points out that it is quicker to
    ;; use MATCH-ANCHORED to effectively anchor the regexp on the left.
    ;; This must come after the one for keywords and targets.
    '(":" ("^[ \t]*\\(\\sw+\\)[ \t]*:[ \t]*$"
           (beginning-of-line) (end-of-line)
           (1 font-lock-constant-face)))
    )))

 ;; c font lock keywords 3 ---------------------------------------------
 (setq c-font-lock-keywords-3
  (append c-font-lock-keywords-2
   ;;
   ;; More complicated regexps for more complete highlighting for types.
   ;; We still have to fontify type specifiers individually, as C is so hairy.
   (list
    ;;
    ;; Fontify all storage types, plus their items.
    `(eval .
      (list (concat "\\<\\(" (,@ c-type-names) "\\)\\>"
                    "\\([ \t*&]+\\sw+\\>\\)*")
            ;; Fontify each declaration item.
            (list 'font-lock-match-c-style-declaration-item-and-skip-to-next
                  ;; Start with point after all type specifiers.
                  (list 'goto-char (list 'or
                                         (list 'match-beginning
                                               (+ (,@ c-type-names-depth) 2))
                                         '(match-end 1)))
                  ;; Finish with point after first type specifier.
                  '(goto-char (match-end 1))
                  ;; Fontify as a variable or function name.
                  '(1 (if (match-beginning 2)
                          font-lock-function-name-face
                        font-lock-variable-name-face)))))
    ;;
    ;; Fontify all storage specs and types, plus their items.
    `(eval .
      (list (concat "\\<\\(" (,@ c-type-specs) "\\)\\>"
                    "[ \t]*\\(\\sw+\\)?")
          (list 1 'font-lock-keyword-face)
          (list (+ (,@ c-type-specs-depth) 2) 'font-lock-type-face nil t)
          (list 'font-lock-match-c-style-declaration-item-and-skip-to-next
                  nil nil
                  ;; Fontify as a variable or function name.
                  '(1 (if (match-beginning 2)
                          font-lock-function-name-face
                        font-lock-variable-name-face) nil t))))
    ;;
    ;; Fontify structures, or typedef names, plus their items.
    '("\\(}\\)[ \t*]*\\sw"
      (font-lock-match-c-style-declaration-item-and-skip-to-next
       (goto-char (match-end 1)) nil
       (1 font-lock-type-face)))
    ;;
    ;; Fontify anything at beginning of line as a declaration or definition.
    '("^\\(\\sw+\\)\\>\\([ \t*]+\\sw+\\>\\)*"
      (1 font-lock-type-face)
      (font-lock-match-c-style-declaration-item-and-skip-to-next
       (goto-char (or (match-beginning 2) (match-end 1))) nil
       (1 (if (match-beginning 2)
              font-lock-function-name-face
            font-lock-variable-name-face))))
    ;;
    ;; Highlight tabs
    '("[\t]+" . pesche-strong-tab-face)
    ;;
    ;; Highlight hard spaces
    '("[\240]+" . pesche-hardspace-face)
    ;;
    ;; Highlight trailing whitespace
    '("[ \t]+$" . pesche-space-face)
    )))
 )

;; C++ font lock -------------------------------------------------------
;; Kopie aus font-lock.el (Emacs 20.5.1),
;; aber Präprozessor-Kommandi dürfen eingerückt sein
(let* ((c++-keywords
        (eval-when-compile
          (regexp-opt
           '("break" "continue" "do" "else" "for" "if" "return" "switch"
             "while" "asm" "catch" "delete" "new" "sizeof" "this" "throw" "try"
             ;; Branko Cibej <branko.cibej@hermes.si> says this is new.
             "export"
             ;; Mark Mitchell <mmitchell@usa.net> says these are new.
             "mutable" "explicit"
             ;; Alain Picard <ap@abelard.apana.org.au> suggests treating these
             ;; as keywords not types.
             "typedef" "template"
             "extern" "auto" "register" "const" "volatile" "static"
             "inline" "friend" "virtual") t)))
       (c++-operators
        (eval-when-compile
          (regexp-opt
           ;; Taken from Stroustrup, minus keywords otherwise fontified.
           '("+" "-" "*" "/" "%" "^" "&" "|" "~" "!" "=" "<" ">" "+=" "-="
             "*=" "/=" "%=" "^=" "&=" "|=" "<<" ">>" ">>=" "<<=" "==" "!="
             "<=" ">=" "&&" "||" "++" "--" "->*" "," "->" "[]" "()"))))
       (c++-type-specs
        (eval-when-compile
          (regexp-opt
           '("class" "public" "private" "protected" "typename"
             "struct" "union" "enum" "namespace" "using"
             ;; Eric Hopper <hopper@omnifarious.mn.org> says these are new.
             "static_cast" "dynamic_cast" "const_cast" "reinterpret_cast") t)))
       (c++-type-specs-depth
        (regexp-opt-depth c++-type-specs))
       (c++-type-names
        `(mapconcat 'identity
          (cons
           (,@ (eval-when-compile
                 (regexp-opt
                  '("signed" "unsigned" "short" "long"
                    "int" "char" "float" "double" "void"
                    "bool" "complex"))))
           c++-font-lock-extra-types)
          "\\|"))
       (c++-type-names-depth `(regexp-opt-depth (,@ c++-type-names)))
       ;;
       ;; A brave attempt to match templates following a type and/or match
       ;; class membership.  See and sync the above function
       ;; `font-lock-match-c++-style-declaration-item-and-skip-to-next'.
       (c++-type-suffix (concat "\\([ \t]*<\\([^>\n]+\\)[ \t*&]*>\\)?"
                                "\\([ \t]*::[ \t*~]*\\(\\sw+\\)\\)*"))
       (c++-type-suffix-depth (regexp-opt-depth c++-type-suffix))
       ;; If the string is a type, it may be followed by the cruft above.
       (c++-type-spec (concat "\\(\\sw+\\)\\>" c++-type-suffix))
       (c++-type-spec-depth (regexp-opt-depth c++-type-spec))
       ;;
       ;; Parenthesis depth of user-defined types not forgetting their cruft.
       (c++-type-depth `(regexp-opt-depth
                         (concat (,@ c++-type-names) (,@ c++-type-suffix))))
       (hw-c-keywords
        (eval-when-compile
          (regexp-opt
           '("bool" "true" "false" "uint8" "uint16" "uint32" "int8" "int16" "int32"
             "smallint" "smalluint" "_plm_call" "_bit" "_rom_mem" "_rom_ptr"
             "_near_mem" "_near_ptr" "_fastfar_mem" "_fastfar_ptr" "_far_mem"
             "_far_ptr" "_huge_ptr" "_stk_ptr" "_stk_mem") t)))
       )

 ;; c++ font lock keywords 1 -------------------------------------------
 (setq c++-font-lock-keywords-1
  (append
   ;;
   ;; The list `c-font-lock-keywords-1' less that for function names.
   (cdr c-font-lock-keywords-1)
   (list
    ;;
    ;; Fontify function name definitions, possibly incorporating class names.
    (list (concat "^" c++-type-spec "[ \t]*(")
          '(1 (if (or (match-beginning 2) (match-beginning 4))
                  font-lock-type-face
                font-lock-function-name-face))
          '(3 font-lock-type-face nil t)
          '(5 font-lock-function-name-face nil t))
    )))

 ;; c++ font lock keywords 2 -------------------------------------------
 (setq c++-font-lock-keywords-2
  (append c++-font-lock-keywords-1
   (list
    ;;
    ;; The list `c-font-lock-keywords-2' for C++ plus operator overloading.
    `(eval .
      (cons (concat "\\<\\(" (,@ c++-type-names) "\\)\\>")
            'font-lock-type-face))
    ;;
    ;; Fontify operator overloading.
    (list (concat "\\<\\(operator\\)\\>[ \t]*\\(" c++-operators "\\)?")
          '(1 font-lock-keyword-face)
          '(2 font-lock-builtin-face nil t))
    ;;
    ;; Fontify case/goto keywords and targets, and case default/goto tags.
    '("\\<\\(case\\|goto\\)\\>[ \t]*\\(-?\\sw+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-constant-face nil t))
    ;; This must come after the one for keywords and targets.
    '(":" ("^[ \t]*\\(\\sw+\\)[ \t]*:\\($\\|[^:]\\)"
           (beginning-of-line) (end-of-line)
           (1 font-lock-constant-face)))
    ;;
    ;; Fontify other builtin keywords.
    (concat "\\<\\(" c++-keywords "\\|" c++-type-specs "\\)\\>")
    ;;
    ;; Eric Hopper <hopper@omnifarious.mn.org> says `true' and `false' are new.
    '("\\<\\(false\\|true\\)\\>" . font-lock-constant-face)
    ;;
    ;; Fontify all HW keywords not already fontified.
    (cons (concat "\\<\\(" hw-c-keywords "\\)\\>") 'hw-keyword-face)
    )))

 ;; c++ font lock keywords 3 -------------------------------------------
 (setq c++-font-lock-keywords-3
  (append c++-font-lock-keywords-2
   ;;
   ;; More complicated regexps for more complete highlighting for types.
   (list
    ;;
    ;; Fontify all storage classes and type specifiers, plus their items.
    `(eval .
      (list (concat "\\<\\(" (,@ c++-type-names) "\\)\\>" (,@ c++-type-suffix)
                    "\\([ \t*&]+" (,@ c++-type-spec) "\\)*")
            ;; The name of any template type.
            (list (+ (,@ c++-type-names-depth) 3) 'font-lock-type-face nil t)
            ;; Fontify each declaration item.
            (list 'font-lock-match-c++-style-declaration-item-and-skip-to-next
                  ;; Start with point after all type specifiers.
                  (list 'goto-char (list 'or (list 'match-beginning
                                                   (+ (,@ c++-type-depth) 2))
                                         '(match-end 1)))
                  ;; Finish with point after first type specifier.
                  '(goto-char (match-end 1))
                  ;; Fontify as a variable or function name.
                  '(1 (cond ((or (match-beginning 2) (match-beginning 4))
                             font-lock-type-face)
                            ((and (match-beginning 6) (c-at-toplevel-p))
                             font-lock-function-name-face)
                            (t
                             font-lock-variable-name-face)))
                  '(3 font-lock-type-face nil t)
                  '(5 (if (match-beginning 6)
                          font-lock-function-name-face
                        font-lock-variable-name-face) nil t))))
    ;;
    ;; Fontify all storage specs and types, plus their items.
    `(eval .
      (list (concat "\\<" (,@ c++-type-specs) "\\>" (,@ c++-type-suffix)
                    "[ \t]*\\(" (,@ c++-type-spec) "\\)?")
            ;; The name of any template type.
            (list (+ (,@ c++-type-specs-depth) 2) 'font-lock-type-face nil t)
            ;; The name of any type.
            (list (+ (,@ c++-type-specs-depth) (,@ c++-type-suffix-depth) 2)
                  'font-lock-type-face nil t)
            ;; Fontify each declaration item.
            (list 'font-lock-match-c++-style-declaration-item-and-skip-to-next
                  ;; Start with point after all type specifiers.
                  nil
                  ;; Finish with point after first type specifier.
                  nil
                  ;; Fontify as a variable or function name.
                  '(1 (cond ((or (match-beginning 2) (match-beginning 4))
                             font-lock-type-face)
                            ((and (match-beginning 6) (c-at-toplevel-p))
                             font-lock-function-name-face)
                            (t
                             font-lock-variable-name-face)))
                  '(3 font-lock-type-face nil t)
                  '(5 (if (match-beginning 6)
                          font-lock-function-name-face
                        font-lock-variable-name-face) nil t))
            ))
    ;;
    ;; Fontify structures, or typedef names, plus their items.
    '("\\(}\\)[ \t*]*\\sw"
      (font-lock-match-c++-style-declaration-item-and-skip-to-next
       (goto-char (match-end 1)) nil
       (1 font-lock-type-face)))
    ;;
    ;; Fontify anything at beginning of line as a declaration or definition.
    (list (concat "^\\(" c++-type-spec "[ \t*&]*\\)+")
          '(font-lock-match-c++-style-declaration-item-and-skip-to-next
            (goto-char (match-beginning 1))
            (goto-char (match-end 1))
            (1 (cond ((or (match-beginning 2) (match-beginning 4))
                      font-lock-type-face)
                     ((match-beginning 6) font-lock-function-name-face)
                     (t font-lock-variable-name-face)))
            (3 font-lock-type-face nil t)
            (5 (if (match-beginning 6)
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


(provide 'pesche-font-lock)

;; eof
