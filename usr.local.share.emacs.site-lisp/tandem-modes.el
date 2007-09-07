;; Tandem NSK Modes
;;
;;         $Id: $
;;     $Change: $
;;   $DateTime: 2007/05/09 12:19:44 $
;;     $Author: peter.steiner $
;;    $Created: 2007/06/19 $
;;  $Copyright: Peter Steiner <pesche@schlau.ch> $


;; cobol mode ------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.cob\\'" . nmcobol-mode))
(autoload 'nmcobol-mode "nmcobol-mode" "Major mode for editing Tandem Cobol files." t)

;; DDL mode --------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.ddl\\'" . ddl-mode))
(autoload 'ddl-mode "ddl-mode" "A major mode for editing DDL files." t)

;; tacl mode -------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.tacl\\'" . tacl-mode))
(autoload 'tacl-mode "tacl-mode" "Major mode for editing Tandem Advanced Command Language files." t)

;; tal mode --------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.tal\\'" . tal-mode))
(autoload 'tal-mode "tal-mode" "Major mode for editing TAL files." t)

;; tandem files ohne extension erkennen ----------------------------------------
(defadvice set-auto-mode
  (after my-determine-language last () activate)
  "When language is fundamental-mode; ACIMAKE, Cobol, DDL, TACL, & TAL modes are
   recognized if the standard ACI version line is present.
   Some other 1st line language indicators are also recognized."
  (if (eq major-mode 'fundamental-mode)
      (let ((mode nil))
        (save-excursion                ; don't trash bookmark entrances
          (goto-char (point-min))
          (if (looking-at "\\(\\*\\|!\\|#\\|{\\)\\*\\(SYNC\\|FIX.\\|SEQ.\\)\\.")
              (progn
                (goto-char (min (1- (point-max)) (+ (point) 34)))
                (cond ((looking-at "DDL ")
                       (setq mode 'ddl-mode))
                      ((looking-at "TAL ")
                       (setq mode 'tal-mode))
                      ((looking-at "COBOL ")
                       (setq mode 'nmcobol-mode))
                      ((looking-at "SCOBOL ")
                       (setq mode 'nmcobol-mode))
                      ((looking-at "TACL ")
                       (setq mode 'tacl-mode))
                      ((looking-at "MAKE ")
                       (setq mode 'makefile-mode))))
            (if (looking-at (concat "\\(\\?TACL \\|==\\|\\?SECTION +\\"
                                    "(\\S-+\\s-+\\"
                                    "(macro\\|routine\\|alias\\|text\\)\\)\\b\\)"))
                (setq mode 'tacl-mode)
              (if (looking-at "\\?SECTION +.+,TANDEM\\b")
                  (setq mode 'ddl-mode)
                (if (looking-at "\\?ANSI\\b")
                    (setq mode 'nmcobol-mode)
                  (if (looking-at "#[-*#=\n]")
                      (setq mode 'makefile-mode)
                    (if (looking-at "[!*] SCHEMA")
                        (setq mode 'ddl-mode)
                      )))))))
        (if mode (funcall mode))))
  )

;; Modul abschliessen ----------------------------------------------------------
(provide 'tandem-modes)

;; eof
