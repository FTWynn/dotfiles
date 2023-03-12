;; =================================
;; David's init.el
;; =================================


;; 2023-03-11 straight.el installation
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
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
;; while still removing the need for ensure: t 
(setq straight-use-package-by-default t)

;; Let's get some packages
(straight-use-package 'org)

;; Chromebook specific
;; TODO: Need to detect chromebook environment before setting these
;; should be good enough on Windows for now
;(define-key org-mode-map (kbd "<prior>") 'org-metaup)
;(define-key org-mode-map (kbd "<next>") 'org-metadown)
; Actually they don't work complaining about org-mode-map... needs looking into

;; Start out all big like
(setq default-frame-alist '((fullscreen . maximized)))

;; Scratch please
(setq inhibit-startup-screen t)

;; 2023-03-11 Initial Configs
;; Testing C-M-x for live reload
(setq visible-bell t)

;; Turn off some unneeded UI elements
(menu-bar-mode -1)  ; Leave this one on if you're a beginner!
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Don't display line numbers in every buffer
(global-display-line-numbers-mode -1)

;; Configure the Modus Themes' appearance
;; There's a lot in here that borders on non-theme stuff
;; Rainbow parens, completions, etc
(setq modus-themes-mode-line '(accented borderless)
      modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-fringes 'subtle
      modus-themes-tabs-accented t
      modus-themes-paren-match '(bold intense)
      modus-themes-prompts '(bold intense)
      modus-themes-completions 'opinionated
      modus-themes-org-blocks 'tinted-background
      modus-themes-scale-headings t
      modus-themes-region '(bg-only)
      modus-themes-headings
      '((1 . (rainbow overline background 1.4))
        (2 . (rainbow background 1.3))
        (3 . (rainbow bold 1.2))
        (t . (semilight 1.1))))

(load-theme 'modus-vivendi)

;; These both get to the same place
;; Height is in 1/10th of a point... so add dat zero
;;(set-frame-font "Fira Code 18")
(set-face-attribute 'default nil :font "Fira Code" :height 180)

;; Make recent files a thing with M-x recentf-open-files
(recentf-mode 1)

;; Save what you enter into minibuffer prompts to cycle thorugh with M-p and M-n
(setq history-length 25)
(savehist-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Move automated customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Don't pop up UI dialogs when prompting, sticking to the more kweyboard native stuff
(setq use-dialog-box nil)

;; Revert buffers when the underlying file has changed
;; aka, watch files on disk and reload those changes if another program messes with them
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)
