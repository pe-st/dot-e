;;     $Source: g:/archiv/cvsroot/site-lisp/subdirs.el,v $
;;   $Revision: 1.1 $
;;       $Date: 1999/08/10 12:15:33 $
;;     $Author: pesche $
;;
;; this is a workaround for NTEmacs 20.3 and 20.4 to obtain a behavior
;; documented in the NEWS file
;;
;; add all subdirectories of this file's directory to the load path
;; (if this file resides in a dir in load-path, it gets loaded
;; automatically; see normal-top-level in startup.el)
(normal-top-level-add-subdirs-to-load-path)

;; eof