;;; generic-pilrc.el -- An Emacs mode for editing PILRC resource files.
;;;
;;; Copyright (c) 1997 by Matthew Cravit.

;; Author: Matthew Cravit <mcravit@best.com>
;; Maintainer: Matthew Cravit <mcravit@best.com>
;; Created: Wed Jul 30 10:46:58 1997
;; $Id: generic-pilrc.el,v 1.4 1999/04/08 19:41:52 pesche Exp $

;; This file contains a generic mode (which requires generic-mode.el) for
;; editing PILRC files. PILRC is the resource compiler for applications to
;; run on the USR PalmPilot PDA. I've used generic-mode here primarily to
;; (somewhat) simplify the font-lock stuff; eventually, I'll rewrite this
;; as a proper major mode and eliminate the dependancies on generic-mode.
;; Meanwhile, generic-mode.el can be downloaded from:
;;
;; http://www.cs.washington.edu/homes/voelker/ntemacs/contrib/generic-mode.el

;; This mode also provides some functions for inserting resource templates
;; which can be edited as needed.

;; Bugs:
;; * The Keyword BITMAP is always font-locked as a Command (font-lock-type-face)
;;   though it can also be an Attribute. See the examples:
;;
;;   Command:
;;       BITMAP ID kidBitmap "pilrc.bmp"
;;
;;   Attribute:
;;       FORMBITMAP AT (10 15) BITMAP kidBitmap

(require 'generic)
(require 'font-lock)

(defun pilrc-setup nil
        (font-lock-mode 1)
        (font-lock-fontify-buffer))

(define-generic-mode 'pilrc-generic-mode
        (list "//")
        nil
        '(
          ;; Autovalues
          ; auto bottom center prevbottom prevheight prevleft
          ; prevright prevtop prevwidth right
          ("\\<\\(AUTO\\|BOTTOM\\|CENTER\\|PREV\\(BOTTOM\\|HEIGHT\\|LEFT\\|RIGHT\\|TOP\\|WIDTH\\)\\|RIGHT\\)\\>"
           1 'font-lock-reference-face)
          ;; Attributes
          ; at autoid autoshift begin boldframe checked columns columnwidths
          ; compress confirmation disabled dynamicsize editable end error font
          ; forcecompress frame group information leftalign leftanchor max
          ; maxchars min modal multiplelines nocompress noframe noneditable
          ; nonusable nosavebehind numeric pagesize rightalign rightanchor
          ; rows savebehind separator singleline underlined usable value
          ; visibleitems warning
          ("\\<\\(AT\\|AUTOID\\|AUTOSHIFT\\|BEGIN\\|BOLDFRAME\\|CHECKED\\|COLUMNS\\|COLUMNWIDTHS\\|COMPRESS\\|CONFIRMATION\\|DISABLED\\|DYNAMICSIZE\\|EDITABLE\\|END\\|ERROR\\|FONT\\|FORCECOMPRESS\\|FRAME\\|GROUP\\|INFORMATION\\|LEFTALIGN\\|LEFTANCHOR\\|MAX\\|MAXCHARS\\|MIN\\|MODAL\\|MULTIPLELINES\\|NOCOMPRESS\\|NOFRAME\\|NONEDITABLE\\|NONUSABLE\\|NOSAVEBEHIND\\|NUMERIC\\|PAGESIZE\\|RIGHTALIGN\\|RIGHTANCHOR\\|ROWS\\|SAVEBEHIND\\|SEPARATOR\\|SINGLELINE\\|UNDERLINED\\|USABLE\\|VALUE\\|VISIBLEITEMS\\|WARNING\\)\\>"
           1 'font-lock-keyword-face)
          ;; Commands
          ; alert application applicationiconname bitmap bitmapgray bitmapgrey
          ; form icon menu smallicon string translation trap version
          ;; Objects
          ; button buttons checkbox field formbitmap gadget
          ; graffitistateindicator label list menuitem message
          ; popuplist popuptrigger pulldown pushbutton
          ; repeatbutton scrollbar selectortrigger table title
          ("\\<\\(ALERT\\|APPLICATION\\|APPLICATIONICONNAME\\|BITMAP\\|BITMAPGRAY\\|BITMAPGREY\\|BUTTON\\|BUTTONS\\|CHECKBOX\\|FIELD\\|FORM\\|FORMBITMAP\\|GADGET\\|GRAFFITISTATEINDICATOR\\|ICON\\|LABEL\\|LIST\\|MENU\\|MENUITEM\\|MESSAGE\\|POPUPLIST\\|POPUPTRIGGER\\|PULLDOWN\\|PUSHBUTTON\\|REPEATBUTTON\\|SCROLLBAR\\|SELECTORTRIGGER\\|SMALLICON\\|STRING\\|TABLE\\|TITLE\\|TRANSLATION\\|TRAP\\|VERSION\\)\\>"
           1 'font-lock-type-face)
          ;; ID-Attributes (with an ID parameter)
          ; defaultbtnid id helpid menuid
          ("\\b\\(MENUID\\|ID\\|HELPID\\|DEFAULTBTNID\\)\\b\\s +\\b\\(\\(\\w\\|_\\)+\\)\\b"
           (1 'font-lock-keyword-face)
           (2 'font-lock-variable-name-face)
          ))
        (list "\\.rcp$")
        (list 'pilrc-setup)
        "Generic mode for pilrc files.")

(defun pilrc-insert-form nil
  (interactive)
  (insert "FORM ID identifier AT ( left top width height )\n"
          "MENUID identifier\n"
          "HELPID identifier\n"
          "BEGIN\n"
          "\tTITLE \"Title\"\n"
          "<objects>\n"
          "END\n\n"))

(defun pilrc-insert-menu nil
  (interactive)
  (insert "MENU ID identifier\n"
          "BEGIN\n"
          "\tPULLDOWN \"Title\"\n"
          "\tBEGIN\n"
          "\t\tMENUITEM \"Title\" identifier \"hotkey\"\n"
          "\tEND\n"
          "END\n\n"))

(defun pilrc-insert-alert nil
  (interactive)
  (insert "ALERT ID identifier\n"
          "type\n"
          "HELPID identifier\n"
          "BEGIN\n"
          "\tTITLE \"title\"\n"
          "\tMESSAGE \"message\"\n"
          "\tBUTTONS \"btntext\"\n"
          "END\n\n"))

(defun pilrc-insert-version nil
  (interactive)
  (insert "VERSION ID identifier \"version\"\n\n"))

(defun pilrc-insert-string nil
  (interactive)
  (insert "STRING ID identifier \"string\"\n\n"))

(defun pilrc-insert-icontitle nil
  (interactive)
  (insert "APPLICATIONICONNAME ID identifier \"title\"\n\n"))

(defun pilrc-insert-application nil
  (interactive)
  (insert "APPLICATION ID identifier \"appcode\"\n\n"))

(defun pilrc-insert-icon nil
  (interactive)
  (insert "ICON \"filename\"\n\n"))

(defun pilrc-insert-bitmap nil
  (interactive)
  (insert "BITMAP ID identifier \"filename\"\n\n"))

(defun pilrc-insert-translation nil
  (interactive)
  (insert "TRANSLATION \"language\"\n"
          "BEGIN\n"
          "\t\"original\" = \"translated\"\n"
          "END\n\n"))

(provide 'generic-pilrc)

; eof
