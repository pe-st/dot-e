;;; lingo-mode.el --- major mode for Macromedia Director Lingo
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

;;; todo
;; Font-Locking-Spezialfälle, auch noch irgendwie behandeln:
;; after before

;; Indenting:
;; - case-Konstrukt
;; - Kommentare


(defconst lingo-version "0.1"
  "`lingo-mode' version number.")

(defvar lingo-indent-offset 2
  "Default indentation per nesting level.")


(defconst lingo-handler-regexp "^[ \t]*[Oo]n[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
  "Regular expression to match the name of a handler.")


(defconst lingo-handler-end-regexp"^[ \t]*[Ee]nd\\([ \t]+.*\\)?$")

;; ;; all lines starting with "end" except "end if", "end case" and "end repeat"
;; (defconst lingo-handler-end-regexp
;;   (concat "^[ \t]*end[ \t]*\\("
;;           "[abd-hj-qs-z].*"
;;           ;; some lines missing here...
;;           "\\)?$"))

(defconst lingo-if-regexp "^[ \t]*[Ii]f[ \t]+.*")
(defconst lingo-ifthen-regexp "^[ \t]*[Ii]f.+\\<[Tt]hen\\>\\s-\\S-+")
(defconst lingo-else-regexp "^[ \t]*[Ee]lse\\([Ii]f\\)?")
(defconst lingo-endif-regexp "^[ \t]*[Ee]nd[ \t]*[Ii]f")

(defconst lingo-case-regexp "^[ \t]*[Cc]ase[ \t]+.*")
(defconst lingo-case-end-regexp "^[ \t]*[Ee]nd[ \t]*[Cc]ase")

(defconst lingo-repeat-regexp "^[ \t]*[Rr]epeat[ \t]+.*")
(defconst lingo-repeat-end-regexp "^[ \t]*[Ee]nd[ \t]*[Rr]epeat")

(defconst lingo-blank-regexp "^[ \t]*$")
(defconst lingo-comment-regexp "^[ \t]*\\s<.*$")
(defconst lingo-continuation-regexp "^.*[\\¬][ \t]*$")



(defvar lingo-font-lock-keywords
  (let ((lingo-keyword-list
         (mapconcat 'identity
                    '("beginRecording" "case" "castLib" "char" "down"
                      "else" "end"
                      "endRecording" "exit" "field" "global" "if" "item"
                      "in" "intersects" "line" "list" "loop" "member"
                      "menu" "next" "of" "on" "otherwise" "previous" "property"
                      "repeat" "return" "sprite" "the" "then" "to"
                      "version" "while" "window" "with" "within")
                    "\\|"))
        (lingo-operator-list
         (mapconcat 'identity
                    '("and" "contains")
                    "\\|"))
        (lingo-command-list
         (mapconcat 'identity
                    '("abort" "add" "addAt" "addProp" "addVertex" "alert"
                      "append" "appMinimize" "beep" "call" "callAncestor"
                      "cancelIdleLoad" "clearCache" "clearError" "clearFrame"
                      "clearGlobals" "close" "closeXlib" "copyToClipBoard"
                      "delay" "delete" "deleteAll" "deleteAt" "deleteFrame"
                      "deleteOne" "deleteProp" "deleteVertex" "do"
                      "downloadNetThing" "duplicateFrame" "duplicate"
                      "enableHotSpot" "erase" "externalEvent" "findPos"
                      "findPosNear" "finishIdleLoad" "flushInputEvents"
                      "getaProp" "getAt" "go" "gotoFrame" "gotoNetMovie"
                      "gotoNetPage")
                    "\\|"))
        (lingo-function-list
         (mapconcat 'identity
                    '("abs" "atan" "bitAnd" "bitNot" "bitOr" "bitXor"
                      "breakLoop" "cacheDocVerify" "cacheSize" "charPosToLoc"
                      "chars" "charToNum" "clickLoc" "clickOn" "commandDown"
                      "constrainH" "constrainV" "controlDown" "copyPixels"
                      "cos" "count" "createMask" "createMatte" "date"
                      "doubleClick" "draw" "endFrame" "exp" "externalParamCount"
                      "externalParamName" "externalParamValue" "extractAlpha"
                      "fadeIn" "fadeOut" "fadeTo" "fill" "findEmpty" "findLabel"
                      "flashToStage" "float" "floatP" "frameReady" "framesToHMS"
                      "freeBlock" "freeBytes" "getError" "getErrorString"
                      "getFlashProperty" "getFrameLabel" "getHotSpotRect"
                      "getLast" "getLatestNetID" "getNetText"
                      "getNthFileNameInFolder" "getOne" "getPixel" "getPlaylist"
                      "getPos" "getPref" "getProp" "getPropAt" "getStreamStatus"
                      "getVariable")
                    "\\|"))
        (lingo-property-list
         (mapconcat 'identity
                    '("actionsEnabled" "alignment" "alphaThreshold" "antiAlias"
                      "antiAliasThreshold" "autoMask" "autoTab" "backColor"
                      "backgroundColor" "bitmapSizes" "bitRate" "bitsPerSample"
                      "blend" "blendLevel" "border" "bottom" "bottomSpacing"
                      "boxDropShadow" "boxType" "broadcastProps" "bufferSize"
                      "buttonsEnabled" "buttonType" "bytesStreamed"
                      "castLibNum" "castMemberList" "center" "centerRegPoint"
                      "changeArea" "channelCount" "characterSet" "charSpacing"
                      "checkMark" "chunkSize" "clickMode" "closed" "color"
                      "comments" "constraint" "controller" "copyrightInfo"
                      "creationDate" "crop" "cuePointNames" "cuePointTimes"
                      "currentTime" "cursor" "cursorSize" "curve" "defaultRect"
                      "defaultRectMode" "depth" "digitalVideoType"
                      "directToStage" "dither" "drawRect" "dropShadow"
                      "duration" "editable" "elapsedTime" "enabled" "endColor"
                      "endTime" "eventPassMode" "fieldOfView" "fileName"
                      "fillColor" "fillCycles" "fillDirection" "filled"
                      "fillMode" "fillOffset" "fillScale" "firstIndent"
                      "fixedLineSpace" "fixedRate" "flashRect" "flipH" "flipV"
                      "font" "fontSize" "fontStyle" "foreColor" "forget" "frame"
                      "frameCount" "frameLabel" "framePalette" "frameRate"
                      "frameScript" "frameSound1" "frameSound2" "frameTempo"
                      "frameTransition" "gradientType"

                      "locH" "locV")
                    "\\|"))
        (lingo-object-property-list
         (mapconcat 'identity
                    '("ancestor")
                    "\\|"))
        (lingo-movie-property-list
         (mapconcat 'identity
                    '("activeWindow" "actorList" "allowCustomCaching"
                      "allowGraphicMenu" "allowSaveLocal"
                      "allowTransportControl" "allowVolumeControl"
                      "allowZooming" "beepOn" "buttonStyle" "centerStage"
                      "checkBoxAccess" "checkBoxType" "currentSpriteNum"
                      "editShortCutsEnabled" "exitLock" "fixStageSize"
                      "floatPrecision")
                    "\\|"))
        (lingo-system-property-list
         (mapconcat 'identity
                    '("activeCastLib" "alertHook" "applicationPath" "bgColor"
                      "browserName" "colorDepth" "cpuHogTicks" "desktopRectList"
                      "digitalVideoTimeScale" "emulateMultiButtonMouse"
                      "environment" "frontWindow" "globals")
                    "\\|"))
        (lingo-event-list
         (mapconcat 'identity
                    '("activateApplication" "activateWindow" "beginSprite"
                      "closeWindow" "cuePassed" "deactivateApplication"
                      "deactivateWindow" "endSprite" "enterFrame" "evalScript"
                      "exitFrame" "getBehaviorDescription" "getBehaviorTooltip"
                      "getPropertyDescriptionList"
                      "startMovie")
                    "\\|"))
        (lingo-constant-list
         (mapconcat 'identity
                    '("backspace" "empty" "enter" "false" "true")
                    "\\|"))
        )
    (list
     ;; keywords
     (cons (concat "\\b\\("
                   lingo-keyword-list lingo-operator-list
                   "\\)\\b[ \n\t(]") 1)
     ;; commands
     `(,(concat "\\b\\("
               lingo-command-list
               "\\)\\b[ \n\t(]") 1 font-lock-variable-name-face)
     ;; properties
     `(,(concat "\\b\\("
               lingo-property-list "\\|"
               lingo-object-property-list "\\|"
               lingo-movie-property-list "\\|"
               lingo-system-property-list
               "\\)\\b[ \n\t(]") 1 font-lock-variable-name-face)
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
  ;; GNU conventions say underscore should be symbol class, but most
  ;; users don't know them and expect `forward-word' and `backward-word'
  ;; to treat underscores the same as letters.
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


(defvar lingo-imenu-generic-expression
  (list (list nil lingo-handler-regexp 1))
  "Extract handler names for `imenu'.")

;; (defvar lingo-outline-regexp
;;   ...)


(define-derived-mode lingo-mode fundamental-mode "lingo"
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
  (make-local-variable 'indent-line-function)
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
        indent-line-function    'lingo-indent-line
        )
  (set (make-local-variable 'imenu-generic-expression)
       lingo-imenu-generic-expression)
  (use-local-map lingo-mode-map)

  ;; Run the mode hook.
  (run-hooks 'lingo-mode-hook)
  )


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


(defun lingo-previous-line-of-code ()
  (if (not (bobp))
      (forward-line -1))        ; previous-line depends on goal column
  (while (and (not (bobp))
              (or (looking-at lingo-blank-regexp)
                  (looking-at lingo-comment-regexp)))
    (forward-line -1)))


(defun lingo-find-original-statement ()
  "If the current line is a continuation, move back to the original stmt."
  (let ((here (point)))
    (lingo-previous-line-of-code)
    (while (and (not (bobp))
                (looking-at lingo-continuation-regexp))
      (setq here (point))
      (lingo-previous-line-of-code))
    (goto-char here)))

(defun lingo-find-matching-stmt (open-regexp close-regexp)
  ;; Searching backwards
  (let ((level 0))
    (while (and (>= level 0) (not (bobp)))
      (lingo-previous-line-of-code)
      (lingo-find-original-statement)
      (cond ((looking-at close-regexp)
             (setq level (+ level 1)))
            ((looking-at open-regexp)
             (setq level (- level 1)))))))

(defun lingo-find-matching-if ()
  (lingo-find-matching-stmt lingo-if-regexp
                            lingo-endif-regexp))

(defun lingo-find-matching-case ()
  (lingo-find-matching-stmt lingo-case-regexp
                            lingo-case-end-regexp))

(defun lingo-find-matching-repeat ()
  (lingo-find-matching-stmt lingo-repeat-regexp
                            lingo-repeat-end-regexp))


;;; If this fails it must return the indent of the line preceding the
;;; end not the first line because end without matching begin is a
;;; normal simple statement
(defun lingo-find-matching-begin ()
  (let ((original-point (point)))
    (lingo-find-matching-stmt lingo-begin-regexp
                                     lingo-end-begin-regexp)
    (if (bobp) ;failed to find a matching begin so assume that it is
               ;an end statement instead and use the indent of the
               ;preceding line.
        (progn (goto-char original-point)
               (lingo-previous-line-of-code)))))


;; borrowed from visual-basic-mode
(defun lingo-calculate-indent ()
  "calculate the indentation for the current line."
  (let ((original-point (point)))
    (save-excursion
      (beginning-of-line)
      (cond
       ;; end of control flow statements
       ((or (looking-at lingo-else-regexp)
            (looking-at lingo-endif-regexp))
        (lingo-find-matching-if)
        (current-indentation))

       ((looking-at lingo-case-end-regexp)
        (lingo-find-matching-case)
        (current-indentation))

       ((looking-at lingo-repeat-end-regexp)
        (lingo-find-matching-repeat)
        (current-indentation))

       ;; handlers start and end on first column
       ;; (this must be after the other "end" statements, because
       ;; lingo-handler-end-regexp matches other ends too
       ((or (looking-at lingo-handler-regexp)
            (looking-at lingo-handler-end-regexp))
        0)

       ;; all other cases depend on the previous line
       (t
        (lingo-previous-line-of-code)

        (cond
          ((looking-at lingo-continuation-regexp)
           (lingo-find-original-statement)

           ;; Indent continuation line under matching open paren,
           ;; or else one word in.
           (let* ((orig-stmt (point))
                  (matching-open-paren
                   (condition-case ()
                       (save-excursion
                         (goto-char original-point)
                         (beginning-of-line)
                         (backward-up-list 1)
                         ;; Only if point is now w/in cont. block.
                         (if (<= orig-stmt (point))
                             (current-column)))
                     (error nil))))
             (cond (matching-open-paren
                    (1+ matching-open-paren))
                   (t
                    ;; Else, after first word on original line.
                    (back-to-indentation)
                    (forward-word 1)
                    (while (looking-at "[ \t]")
                      (forward-char 1))
                    (current-column)))))
          (t
           (lingo-find-original-statement)
           (let ((indent (current-indentation)))
             ;; All the various +indent regexps.
             (cond ((looking-at lingo-handler-regexp)
                    (+ indent lingo-indent-offset))

                   ((and (or (looking-at lingo-if-regexp)
                             (looking-at lingo-else-regexp))
                         (not (looking-at lingo-ifthen-regexp)))
                    (+ indent lingo-indent-offset))

                   ((or (looking-at lingo-repeat-regexp)
                        (looking-at lingo-case-regexp))
                    (+ indent lingo-indent-offset))

                   (t
                    ;; By default, just copy indent from prev line.
                    indent))))))))))


(defun lingo-version ()
  "Echo the current version of `lingo-mode' in the minibuffer."
  (interactive)
  (message "Using `lingo-mode' version %s" lingo-version)
;;;  (lingo-keep-region-active)
  )


(provide 'lingo-mode)
;;; lingo-mode.el ends here
