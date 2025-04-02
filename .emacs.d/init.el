;; =================================
;; FTWynn's init.el
;; =================================

;; Mostly this has moved to the literate_init.org file
;; I'm leaving here anything to do with package management and getting to org for the translation
;; Namely straight.el, use-package, and org
;; The rest... to the org file!

;; 2023-03-11 straight.el installation from their website
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)
;; Remove the need to put straight: t in use use-package,
;; while also removes the need for ensure: t 
(setq straight-use-package-by-default t)

;; Let's get some packages
(straight-use-package 'org)

;; From Emacs bedrock... just copying this in case
(setq gc-cons-threshold (or bedrock--initial-gc-threshold 800000))

(require 'org)
(org-babel-load-file (expand-file-name (concat user-emacs-directory "literate_init.org")))
