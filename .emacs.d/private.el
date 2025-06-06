;;
;; FTWynn Customizations to Emacs Solo
;;

; ============================
; ========= Eshell ===========
; ============================

; ========================
; ====== Appearance ======
; ========================
;(set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 150)
;(set-face-attribute 'default nil :family "BlexMono Nerd Font" :height 150)
;(set-face-attribute 'default nil :family "FiraMono Nerd Font" :height 150)
(set-face-attribute 'default nil :family "DejaVuSansM Nerd Font" :height 150)

; https://lucidmanager.org/productivity/writing-prose-with-emacs/ has a good idea to only wrap text
; in text focused modes
(add-hook 'text-mode-hook 'visual-line-mode)

(blink-cursor-mode -1) ; Never realized how annoying this was until I removed it
(setq sentence-end-double-space nil) ; Two spaces should not be needed to mark a sentence

; Pulse highlight line on various buffer jump operations
; Pulled from https://karl-voit.at/2021/04/10/GLT21-emacs-org-features/
(defun my-pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(recenter-top-bottom other-window ace-window my-scroll-down-half my-scroll-up-half switch-to-buffer))
  (advice-add command :after #'my-pulse-line))


; ==========================
; ========= General ========
; ==========================
; Flyspell
; So much text writing... and I'm so bad at spelling...
; Opting for ispell since it seems easy to install and Flyspell uses it by default
; Don't forget to install it from the base package manager

; FINALLY
; So arcane...
; - Gemini was unaware of the ispell-local-dictionary-alist monstrosity that seems to need to be there
; - There is chatter on StackOverflow that the hunspell-dictionary-alist doesn't auto-populate correctly... so you need to write it from the ispell one
; - Then you need the extra paths as well for searching, since I couldn't change the command line version in the OS for some ungodly reason
; - Obviously this is only on Windows. I'm sure this'll be easier on nix.

(cond
 ((string-equal system-type "windows-nt")
  (progn
    (setq ispell-program-name "C:\\ProgramData\\chocolatey\\bin\\hunspell.exe")
                                        ;(setq ispell-hunspell-dict-paths-alist "C:\\Hunspell")
    (use-package rw-hunspell
      :ensure t)
    (setq ispell-local-dictionary-alist (quote ((nil "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8))))
    (setq ispell-hunspell-dict-paths-alist '(("en_US" . ("C:\\Users\\Remix\\Dictionaries\\en_US"))))
    (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)
    )))

(cond
 ((string-equal system-type "gnu/linux")
  (progn
    (setq-default ispell-program-name "aspell"
                  ispell-dictionary "en")
    ))
 )

(use-package flyspell
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))
  (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))
  )


; ====== Org =============

(defun ftwynn/org-mode-setup ()
  (org-indent-mode)
;;     ;(variable-pitch-mode 1)
;;     ;(god-local-mode)
  (auto-fill-mode 0)
  (visual-line-mode 1)
;;     ;(setq org-indent-indentation-per-level 3)
;;     ;(setq org-list-indent-offset 4)
;;     (setq org-descriptive-links nil)
;;     (setq org-M-RET-may-split-line nil)
;;     ;(setq line-spacing 0.2)
;;     ;(setq org-reverse-note-order t)
;;     (setq org-use-speed-commands t)
;;     (setq org-agenda-timegrid-use-ampm t)
;;     (setq org-tags-exclude-from-inheritance '("project" "epic"))
;;     (setq org-todo-keywords
;;           '((sequence "TODO(t)" "WAITING(w)" "|" "ABANDONED(a)" "DONE(d)")))

  ;; keybinding for editing source code blocks
  (local-set-key (kbd "C-c s e") 'org-edit-src-code)
  ;; keybinding for inserting code blocks
  (local-set-key (kbd "C-c s i") 'org-insert-src-block)

;;     ; Sets the src blocks to be fixed width
;;     ;; (mapc
;;     ;;  (lambda (face)
;;     ;;   (set-face-attribute
;;     ;;     face nil
;;     ;;     :inherit
;;     ;;     (my-adjoin-to-list-or-symbol
;;     ;;      'fixed-pitch
;;     ;;      (face-attribute face :inherit))))
;;     ;;  (list 'org-code 'org-block 'org-table))

;;     ;; Sets the faces for outline levels to the same font, so that org-sticky-header pulls the right font
;;     ;(set-face-attribute 'org-level-1 nil :family "DejaVu Sans Mono")
;;     ;(set-face-attribute 'org-level-2 nil :family "DejaVu Sans Mono")
;;     ;(set-face-attribute 'org-level-3 nil :family "DejaVu Sans Mono")
;;     ;(set-face-attribute 'org-level-4 nil :family "DejaVu Sans Mono")
;;     ;(set-face-attribute 'org-level-5 nil :family "DejaVu Sans Mono")
;;     ;(set-face-attribute 'org-level-6 nil :family "DejaVu Sans Mono")
;;     ;(set-face-attribute 'org-level-7 nil :family "DejaVu Sans Mono")
;;     ;(set-face-attribute 'org-level-8 nil :family "DejaVu Sans Mono")

  )

;; (setq ;org-ellipsis " ▾"
;;           org-ellipsis " [+]"
;;           ;;org-hide-emphasis-markers t
;;           org-src-fontify-natively t
;;           org-fontify-quote-and-verse-blocks t
;;           org-src-tab-acts-natively t
;;           org-edit-src-content-indentation 2
;;           org-hide-block-startup nil
;;           org-src-preserve-indentation nil
;;           org-startup-folded 'content
;;           org-cycle-separator-lines 2)

;; (setq org-refile-targets '((nil :maxlevel . 6)
;;                                ("~/org-roam-repo/brain.org" :maxlevel . 6)
;;                  ("~/org-roam-repo/journal.org" :maxlevel . 6)
;;                                ))

;;     (setq org-outline-path-complete-in-steps nil)
;;     (setq org-refile-use-outline-path t)


(add-hook 'org-mode-hook #'ftwynn/org-mode-setup)


; Setting up a customer org return function to be a little smoother
; An improved set of ideas around Org-return originally from here:
; https://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
(require 'org-inlinetask)

(defun ftwynn/item-list-trailing-whitespace-p ()
(and
 (org-in-item-p)
 (save-excursion (beginning-of-line) (looking-at "^[[:space:]]+$")) t)
)

(defun ftwynn/point-on-heading-or-item-p ()
  (or
   (org-at-item-checkbox-p)
   (and (org-in-item-p)
        (not (ftwynn/item-list-trailing-whitespace-p)))
   (org-at-heading-p)
   ))

(defun ftwynn/empty-item-or-heading-p ()
  (cond
   ((org-at-heading-p)
    (string= "" (org-element-property :title (org-element-context))))
   ((org-in-item-p)
    (not (save-excursion (beginning-of-line) (org-element-property :contents-begin (org-element-context)))))))


(defun ftwynn/org-return (&optional ignore)
  (interactive "P")
  (cond
   ;; Needs to be a valid item for us to replace
   ((ignore)
    (org-return))
   ((org-inlinetask-in-task-p)
    (org-return))
   ((not (ftwynn/point-on-heading-or-item-p))
    (org-return))
   ;; Clear if empty
   ((and (ftwynn/point-on-heading-or-item-p) (ftwynn/empty-item-or-heading-p))
    (beginning-of-line)
    (delete-region (line-beginning-position) (line-end-position)))
   ;; Add a checkbox after a checkbox
   ((org-at-item-checkbox-p)
    (org-insert-item t))
   ;; Add a list item after an item
   ((org-in-item-p)
    (org-meta-return))
   ;; Add a heading after a heading
   ((org-at-heading-p)
    (org-meta-return))
   ;; Default in case I forgot something
   (t
    (org-return)))
  )

(define-key org-mode-map (kbd "RET")
          'ftwynn/org-return)

; A "move to the next heading's content" function
(defun ftwynn/move-to-next-heading-or-content ()
  (interactive)
  (condition-case err
      (outline-next-visible-heading 1)
    (error
     (error "No next heading")))

  (end-of-line)

  (when
      (save-excursion
        (forward-line 1)
        (outline-on-heading-p))
    (error "No content"))

  (forward-line 1)
  (end-of-line))

(define-key org-mode-map (kbd "M-n")
          'ftwynn/move-to-next-heading-or-content)


; A function to dynamically create an agenda based on any linked files from the todo file


(defvar ftwynn/org-projects-file (expand-file-name "~/org-roam-repo/todo.org")
  "The main projects file to scan for linked org files.")

(defun ftwynn/org-get-dynamic-agenda-files ()
  "Dynamically generate the list of org-agenda-files.
Includes `ftwynn/org-projects-file` and any org files linked within it."
  (let ((project-file ftwynn/org-projects-file)
        (agenda-files '())) ;; Initialize empty list

    ;; 1. Add the main projects file itself, if it exists
    (when (file-exists-p project-file)
      (add-to-list 'agenda-files project-file))

    ;; 2. Find and add linked .org files from within the projects file
    (when (file-exists-p project-file)
      (let ((project-dir (file-name-directory project-file))
            ;; Regex to find [[file:*.org]] links. Captures the path.
        ;; TODO: update for denote links
            (org-link-regexp "\\[\\[file:\\([^]]+\\.org\\)\\]"))
        ;; Process the file content in a temporary buffer
        (with-temp-buffer
          (insert-file-contents project-file)
          (goto-char (point-min))
          ;; Search for links throughout the buffer
          (while (re-search-forward org-link-regexp nil t)
            (let* ((linked-file-relative (match-string 1))
                   ;; Resolve relative paths based on project-dir
                   (linked-file-absolute (expand-file-name linked-file-relative project-dir)))
              ;; Add the absolute path to the list if it exists
              (when (file-exists-p linked-file-absolute)
                (add-to-list 'agenda-files linked-file-absolute)))))))

    ;; Return the potentially modified list (duplicates already handled by add-to-list)
    ;; It's good practice to ensure the list contains *existing* files,
    ;; which we did inside the loop and when adding the main project file.
    ;; If you want to be extra robust, you could filter here, but it's likely redundant.
    ;; (seq-filter #'file-exists-p agenda-files)
    agenda-files))

;; Set org-agenda-files to *call* our function dynamically
;(setq org-agenda-files #'ftwynn/org-get-dynamic-agenda-files)
(setq org-agenda-files '("~/org-roam-repo/todo.org")) ;;... or don't

;; But on Windows we're in WSL, with a different HOME var
(when (and (eq system-type 'gnu/linux) (getenv "WSLENV"))
  (setq org-agenda-files '("mnt/c/Users/Remix/org-roam-repo/todo.org"))
)

;; Optional: Advise org-agenda-prepare-buffers to refresh if needed,
;; although calling the function directly usually suffices.
;; (advice-add 'org-agenda-prepare-buffers :before
;;             (lambda (&rest args) (setq org-agenda-files (my/org-get-dynamic-agenda-files))))
;; --> Note: The above advice is generally *not* needed when setting
;;     org-agenda-files to a function symbol, as Org mode handles calling it.
;;     Keeping it commented out is recommended unless you observe issues.

(message "Org agenda files set to dynamically update from %s" ftwynn/org-projects-file)

(global-set-key (kbd "C-c a") 'org-agenda)

; Stoic Daily Prompt Function
; So I couldn't for the life of me figure out how to do this in an associative array...
; at least not in the scratch buffer. Maybe it has elisp limits I'm unaware of. So, I split
; the doc strings out into individual variables and the function call now just concats and grabs
; the right date.

; Elegant? No.
; Good enough? Sure.
; Variables first.

  (setq
   ftwynn-stoic-prompt-01-01 "What things are truly in my control?"
   ftwynn-stoic-prompt-01-02 "What am I learning and studying for?"
   ftwynn-stoic-prompt-01-03 "What can I say no to so I can say yes to what matters?"
   ftwynn-stoic-prompt-01-04 "Am I seeing clearly? Acting generously? Accepting what I can't change?"
   ftwynn-stoic-prompt-01-05 "What is my purpose in life?"
   ftwynn-stoic-prompt-01-06 "Who am I and what do I stand for?"
   ftwynn-stoic-prompt-01-07 "How can I keep my mind clear from pollution?"
   ftwynn-stoic-prompt-01-08 "What am I addicted to?"
   ftwynn-stoic-prompt-01-09 "If I don't control what happens to me, what is left?"
   ftwynn-stoic-prompt-01-10 "Where can I find steadiness?"
   ftwynn-stoic-prompt-01-11 "What are sources of unsteadiness in my life?"
   ftwynn-stoic-prompt-01-12 "Where is my path to serenity?"
   ftwynn-stoic-prompt-01-13 "What can I put outside my circle of control?"
   ftwynn-stoic-prompt-01-14 "What jerks me around?"
   ftwynn-stoic-prompt-01-15 "Am I staying the course or being steered away?"
   ftwynn-stoic-prompt-01-16 "What assumptions have I left unquestioned?"
   ftwynn-stoic-prompt-01-17 "Am I doing work that matters?"
   ftwynn-stoic-prompt-01-18 "Can I find grace and harmony in places others overlook?"
   ftwynn-stoic-prompt-01-19 "Good or bad, high or low, do I still have choices?"
   ftwynn-stoic-prompt-01-20 "How can I rekindle my principles and start living today?"
   ftwynn-stoic-prompt-01-21 "What am I getting out of my journaling ritual?"
   ftwynn-stoic-prompt-01-22 "What bad habit did I curb today?"
   ftwynn-stoic-prompt-01-23 "Which of my possessions own me?"
   ftwynn-stoic-prompt-01-24 "Am I doing deep work?"
   ftwynn-stoic-prompt-01-25 "What do I truly prize?"
   ftwynn-stoic-prompt-01-26 "What is my mantra today?"
   ftwynn-stoic-prompt-01-27 "What am I studying, practicing, and training?"
   ftwynn-stoic-prompt-01-28 "What ruler do I measure myself against?"
   ftwynn-stoic-prompt-01-29 "Am I keeping a sturdy mind on the task at hand?"
   ftwynn-stoic-prompt-01-30 "Am I content to be clueless about the things that don't matter?"
   ftwynn-stoic-prompt-01-31 "What healing can philosophy help me find today?"
   ftwynn-stoic-prompt-02-01 "How can I conquer my temper?"
   ftwynn-stoic-prompt-02-02 "What impulses rob me of self-control?"
   ftwynn-stoic-prompt-02-03 "Am I in control or is my anxiety?"
   ftwynn-stoic-prompt-02-04 "Am I cultivating the invincibility of my power to choose?"
   ftwynn-stoic-prompt-02-05 "Am I thinking before I act?"
   ftwynn-stoic-prompt-02-06 "What needless conflict can I avoid?"
   ftwynn-stoic-prompt-02-07 "How can I conquer fear and worry--before they conquer me?"
   ftwynn-stoic-prompt-02-08 "Do my outbursts ever make things better?"
   ftwynn-stoic-prompt-02-09 "What if I didn't have an opinion about this?"
   ftwynn-stoic-prompt-02-10 "What parts of my life are driven by anger?"
   ftwynn-stoic-prompt-02-11 "Is my soul a good ruler or a tyrant?"
   ftwynn-stoic-prompt-02-12 "For what have I sold my peace of mind?"
   ftwynn-stoic-prompt-02-13 "Which of my pleasures are really punishments?"
   ftwynn-stoic-prompt-02-14 "How can I do a better job listening to the little voice inside me?"
   ftwynn-stoic-prompt-02-15 "Do these strong emotions even make sense?"
   ftwynn-stoic-prompt-02-16 "What am I making harder than it needs to be?"
   ftwynn-stoic-prompt-02-17 "What happiness am I putting off that I could have right now?"
   ftwynn-stoic-prompt-02-18 "Am I in rigorous training against false impressions?"
   ftwynn-stoic-prompt-02-19 "Am I happy with my portion at the banquet of life?"
   ftwynn-stoic-prompt-02-20 "Are the pleasures I'm chasing actually worth it?"
   ftwynn-stoic-prompt-02-21 "What can I stop yearning for?"
   ftwynn-stoic-prompt-02-22 "Am I certain what I want to say isn't better left unsaid?"
   ftwynn-stoic-prompt-02-23 "Why get angry at things, if anger doesn't change them?"
   ftwynn-stoic-prompt-02-24 "Why am I telling myself that I've been harmed?"
   ftwynn-stoic-prompt-02-25 "Will I even remember this fight in a few months?"
   ftwynn-stoic-prompt-02-26 "Why do I need to care that someone else screwed up?"
   ftwynn-stoic-prompt-02-27 "How can I cultivate indifference to unimportant things?"
   ftwynn-stoic-prompt-02-28 "What would happen if I took a second to cool down?"
   ftwynn-stoic-prompt-02-29 "You can't always be getting what you want"
   ftwynn-stoic-prompt-03-01 "How often do I question the things others take for granted?"
   ftwynn-stoic-prompt-03-02 "Do I see and assess myself accurately?"
   ftwynn-stoic-prompt-03-03 "Am I standing with the philosopher or the mob?"
   ftwynn-stoic-prompt-03-04 "How many of my limitations are really self-imposed?"
   ftwynn-stoic-prompt-03-05 "Do I really need these things I work so hard for?"
   ftwynn-stoic-prompt-03-06 "Where am I a loud mouth?"
   ftwynn-stoic-prompt-03-07 "Can I test my own opinion before trusting it?"
   ftwynn-stoic-prompt-03-08 "Am I protecting my time and attention?"
   ftwynn-stoic-prompt-03-09 "Does my social circle make me better or worse?"
   ftwynn-stoic-prompt-03-10 "Who is my role model? Why?"
   ftwynn-stoic-prompt-03-11 "Where have I traded away freedom? How can I get it back?"
   ftwynn-stoic-prompt-03-12 "What would I change if I looked for other people's good intentions?"
   ftwynn-stoic-prompt-03-13 "Instead of calling it bad luck) can I come to see it as inevitable?"
   ftwynn-stoic-prompt-03-14 "How is my arrogance preventing me from learning?"
   ftwynn-stoic-prompt-03-15 "What would it be like if I focused entirely on the present moment?"
   ftwynn-stoic-prompt-03-16 "Do I appreciate this mind I have been given?"
   ftwynn-stoic-prompt-03-17 "Are my choices beautiful?"
   ftwynn-stoic-prompt-03-18 "What bad assumptions can I cast out?"
   ftwynn-stoic-prompt-03-19 "What is the real cause of my irritations--external things or my opinions?"
   ftwynn-stoic-prompt-03-20 "Am I cultivating the virtue that makes adversity bearable?"
   ftwynn-stoic-prompt-03-21 "What if I sought peace where I am right now instead of in distant lands?"
   ftwynn-stoic-prompt-03-22 "Have I confused schooling and education?"
   ftwynn-stoic-prompt-03-23 "How can I treat my greedy vices? How can I heal my sickness?"
   ftwynn-stoic-prompt-03-24 "What philosophical lessons can I find in ordinary things?"
   ftwynn-stoic-prompt-03-25 "Would I feel wealthier if I decreased my wants?"
   ftwynn-stoic-prompt-03-26 "Am I keeping watch?"
   ftwynn-stoic-prompt-03-27 "What valuable things do I sell too cheaply?"
   ftwynn-stoic-prompt-03-28 "Is my training designed to help me rise to the occasion?"
   ftwynn-stoic-prompt-03-29 "Why do I care so much about impressing people?"
   ftwynn-stoic-prompt-03-30 "If I'm not ruled by reasons, what am I ruled by?"
   ftwynn-stoic-prompt-03-31 "Can I stop chasing the impossible today?"
   ftwynn-stoic-prompt-04-01 "What thoughts are coloring my world?"
   ftwynn-stoic-prompt-04-02 "What can I do today to keep drama away?"
   ftwynn-stoic-prompt-04-03 "Are my plans at war with my other plans?"
   ftwynn-stoic-prompt-04-04 "Can I fight to be the person philosophy wants me to be today?"
   ftwynn-stoic-prompt-04-05 "What would happen if I stopped to verify my options and initial reactions?"
   ftwynn-stoic-prompt-04-06 "Despite the worst things people do, can I love them anyway?"
   ftwynn-stoic-prompt-04-07 "Where are my opinions part of the problem?"
   ftwynn-stoic-prompt-04-08 "What bad assumptions, habits, or advice have I accepted?"
   ftwynn-stoic-prompt-04-09 "Can I step back and test my impressions? What would I find if I did?"
   ftwynn-stoic-prompt-04-10 "How do my judgments cause me anguish?"
   ftwynn-stoic-prompt-04-11 "Can I stop thinking I already know and learn something here?"
   ftwynn-stoic-prompt-04-12 "What's the truth about so-called 'honors' and 'riches'?"
   ftwynn-stoic-prompt-04-13 "What would /less/ look like?"
   ftwynn-stoic-prompt-04-14 "Do I balance my life better than the balance sheet of my business?"
   ftwynn-stoic-prompt-04-15 "Life is full of taxes--am I prepared to pay them?"
   ftwynn-stoic-prompt-04-16 "What can I pay closer attention to today?"
   ftwynn-stoic-prompt-04-17 "Can I stop feeling hurt by every little thing?"
   ftwynn-stoic-prompt-04-18 "Do I need to have an opinion about this?"
   ftwynn-stoic-prompt-04-19 "Am I leaving room for what might happen?"
   ftwynn-stoic-prompt-04-20 "What are the few real goods?"
   ftwynn-stoic-prompt-04-21 "How long can I go without letting my attention slide?"
   ftwynn-stoic-prompt-04-22 "Am I self-aware, self-critical, and self-determining?"
   ftwynn-stoic-prompt-04-23 "How am I caring for my mind?"
   ftwynn-stoic-prompt-04-24 "Nice cars, jewels, fine wine--what are these things really?"
   ftwynn-stoic-prompt-04-25 "Am I willing to admit when I'm wrong?"
   ftwynn-stoic-prompt-04-26 "How can I learn from my sparring partners?"
   ftwynn-stoic-prompt-04-27 "How long does praise really last anyway?"
   ftwynn-stoic-prompt-04-28 "What power does all my wanting take from me?"
   ftwynn-stoic-prompt-04-29 "What do I feel when I look up at the sky?"
   ftwynn-stoic-prompt-04-30 "Do my actions match my character?"
   ftwynn-stoic-prompt-05-01 "Do my actions--and my mind--match my philosophy?"
   ftwynn-stoic-prompt-05-02 "What kind of person do I want to be?"
   ftwynn-stoic-prompt-05-03 "Am I showing or telling?"
   ftwynn-stoic-prompt-05-04 "Where can I spend money to help others?"
   ftwynn-stoic-prompt-05-05 "Have I made myself a lifelong project?"
   ftwynn-stoic-prompt-05-06 "Am I seeking the beauty of human excellence?"
   ftwynn-stoic-prompt-05-07 "What is some good I can get from myself today?"
   ftwynn-stoic-prompt-05-08 "What evil comes from my own choices?"
   ftwynn-stoic-prompt-05-09 "Will I seize this day?"
   ftwynn-stoic-prompt-05-10 "What bold thing can I do today?"
   ftwynn-stoic-prompt-05-11 "Where does my lack of self-control create problems?"
   ftwynn-stoic-prompt-05-12 "What would happen if I responded with kindness, no matter what?"
   ftwynn-stoic-prompt-05-13 "Which bad habits am I fueling?"
   ftwynn-stoic-prompt-05-14 "Are my actions contributing to my well-being?"
   ftwynn-stoic-prompt-05-15 "What blessings can I count right now?"
   ftwynn-stoic-prompt-05-16 "How am I creating momentum for my good habits?"
   ftwynn-stoic-prompt-05-17 "Am I on the path to progress?"
   ftwynn-stoic-prompt-05-18 "Is my attention actually on the things at hand?"
   ftwynn-stoic-prompt-05-19 "Where am I doing the opposite of what I should?"
   ftwynn-stoic-prompt-05-20 "What are the seeds I'm planting and what will they grow?"
   ftwynn-stoic-prompt-05-21 "Can I take a blow and stay in the ring?"
   ftwynn-stoic-prompt-05-22 "Can I be a good person right here, right now?"
   ftwynn-stoic-prompt-05-23 "Can I start living right here, right now?"
   ftwynn-stoic-prompt-05-24 "How can I make my own good fortune?"
   ftwynn-stoic-prompt-05-25 "What kind of selfless things will bring me joy?"
   ftwynn-stoic-prompt-05-26 "What if I stopped caring what others thought?"
   ftwynn-stoic-prompt-05-27 "What small stuff should I sweat?"
   ftwynn-stoic-prompt-05-28 "What should I think about before I take action?"
   ftwynn-stoic-prompt-05-29 "What work nourishes my mind?"
   ftwynn-stoic-prompt-05-30 "Is my hard work for the right end?"
   ftwynn-stoic-prompt-05-31 "If my vocation is to be a good person, am I doing a good job?"
   ftwynn-stoic-prompt-06-01 "Do I have a backup operation in mind for all things?"
   ftwynn-stoic-prompt-06-02 "Where have I lost the forest for the trees?"
   ftwynn-stoic-prompt-06-03 "Do I have a backup plan for my backup plan?"
   ftwynn-stoic-prompt-06-04 "Do I realize how tough and strong I am capable of being?"
   ftwynn-stoic-prompt-06-05 "Can I blow my own nose--instead of asking someone to do it for me?"
   ftwynn-stoic-prompt-06-06 "Is this a time to stick or to quit?"
   ftwynn-stoic-prompt-06-07 "What mentors do I follow--alive or dead?"
   ftwynn-stoic-prompt-06-08 "If I took things patiently, step by step, what could I conquer?"
   ftwynn-stoic-prompt-06-09 "What do I need to nip in the bud right now?"
   ftwynn-stoic-prompt-06-10 "If someone else was strong enough to do it, why can't I?"
   ftwynn-stoic-prompt-06-11 "How often is anger more destructive than what caused it?"
   ftwynn-stoic-prompt-06-12 "Am I learning to be adaptable?"
   ftwynn-stoic-prompt-06-13 "Am I fulfilling my post in this campaign of life, or sleeping on duty?"
   ftwynn-stoic-prompt-06-14 "Do I have a hold on the right handle of this situation?"
   ftwynn-stoic-prompt-06-15 "Can I listen more and talk less today?"
   ftwynn-stoic-prompt-06-16 "Where do I need help? Who can I ask for it?"
   ftwynn-stoic-prompt-06-17 "What am I blaming on chance or luck that's really on me?"
   ftwynn-stoic-prompt-06-18 "Am I ready and able?"
   ftwynn-stoic-prompt-06-19 "How can I better keep myself in the present moment?"
   ftwynn-stoic-prompt-06-20 "Am I the calm one in the room or the one who needs to be calmed?"
   ftwynn-stoic-prompt-06-21 "How can I refresh my mind today?"
   ftwynn-stoic-prompt-06-22 "Am I actually learning from my failures?"
   ftwynn-stoic-prompt-06-23 "Where am I standing in my own way?"
   ftwynn-stoic-prompt-06-24 "Do I really need to argue and quarrel so much?"
   ftwynn-stoic-prompt-06-25 "Am I expecting the possible, and not just what I want?"
   ftwynn-stoic-prompt-06-26 "What thing do I always do that fails and what if I tried the opposite?"
   ftwynn-stoic-prompt-06-27 "What can this adversity show me?"
   ftwynn-stoic-prompt-06-28 "What can I stop beating myself up over?"
   ftwynn-stoic-prompt-06-29 "What can I stop making excuses for?"
   ftwynn-stoic-prompt-06-30 "How can I use this obstacle as an opportunity?"
   ftwynn-stoic-prompt-07-01 "As a Stoic, what is my job?"
   ftwynn-stoic-prompt-07-02 "What is the harder choice I'm avoiding?"
   ftwynn-stoic-prompt-07-03 "What if I saw opportunities instead of obligation?"
   ftwynn-stoic-prompt-07-04 "Am I keeping the flame of virtue burning?"
   ftwynn-stoic-prompt-07-05 "Am I doing the honorable thing?"
   ftwynn-stoic-prompt-07-06 "Am I dragging my feet, or am I doing my job as a human being?"
   ftwynn-stoic-prompt-07-07 "Can I show Odysses-like determination and perseverance?"
   ftwynn-stoic-prompt-07-08 "What painful things can I take responsibility for?"
   ftwynn-stoic-prompt-07-09 "Am I on the philosopher's path or winging it?"
   ftwynn-stoic-prompt-07-10 "Am I dedicated to my craft?"
   ftwynn-stoic-prompt-07-11 "How will I improve myself today?"
   ftwynn-stoic-prompt-07-12 "What principles govern my behavior?"
   ftwynn-stoic-prompt-07-13 "Am I ready to be a leader? Ready to do my job?"
   ftwynn-stoic-prompt-07-14 "Am I becoming more humble or less humble?"
   ftwynn-stoic-prompt-07-15 "Can I do the right thing--even without the promise of rewards?"
   ftwynn-stoic-prompt-07-16 "To what service am I committed?"
   ftwynn-stoic-prompt-07-17 "Where have I abandoned others?"
   ftwynn-stoic-prompt-07-18 "Can I mind my own business and not be distracted by others?"
   ftwynn-stoic-prompt-07-19 "What would forgiveness feel like?"
   ftwynn-stoic-prompt-07-20 "Am I living a just life?"
   ftwynn-stoic-prompt-07-21 "How can I work better with others?"
   ftwynn-stoic-prompt-07-22 "Am I acting nobly or grudgingly?"
   ftwynn-stoic-prompt-07-23 "How can I make sure none of it goes to my head--good or bad?"
   ftwynn-stoic-prompt-07-24 "Can I keep my cool when receiving disturbing news?"
   ftwynn-stoic-prompt-07-25 "Where do I let work diminish my quality of life?"
   ftwynn-stoic-prompt-07-26 "Where can I pitch in? How can I help?"
   ftwynn-stoic-prompt-07-27 "What is better than virtue?"
   ftwynn-stoic-prompt-07-28 "Where have I been privileged--and what am I doing with it?"
   ftwynn-stoic-prompt-07-29 "Where can I find confidence?"
   ftwynn-stoic-prompt-07-30 "Can I seek joy today in purpose, excellence, and duty?"
   ftwynn-stoic-prompt-07-31 "Am I neglecting the personal for the professional?"
   ftwynn-stoic-prompt-08-01 "Where does my idealism hold me back?"
   ftwynn-stoic-prompt-08-02 "How can I make do with the tough situations I face?"
   ftwynn-stoic-prompt-08-03 "Can I get the most out of where I am right here, right now?"
   ftwynn-stoic-prompt-08-04 "How can I avoid fruitless emotions today?"
   ftwynn-stoic-prompt-08-05 "Can I hold my tongue today?"
   ftwynn-stoic-prompt-08-06 "What small progress can I make today?"
   ftwynn-stoic-prompt-08-07 "Can I live well no matter how trying the environment?"
   ftwynn-stoic-prompt-08-08 "What's the smallest step I can take toward a big thing today?"
   ftwynn-stoic-prompt-08-09 "Can I keep things simple today? Straightforward?"
   ftwynn-stoic-prompt-08-10 "Where is perfectionism holding me back?"
   ftwynn-stoic-prompt-08-11 "Are my habits getting better?"
   ftwynn-stoic-prompt-08-12 "Am I making this philosophy my own by putting it into practice?"
   ftwynn-stoic-prompt-08-13 "What troubles can I solve in advance?"
   ftwynn-stoic-prompt-08-14 "How will philosophy help steer my course today?"
   ftwynn-stoic-prompt-08-15 "Will decisions I make today be based on true judgments?"
   ftwynn-stoic-prompt-08-16 "How will I turn today's adversities into advantages?"
   ftwynn-stoic-prompt-08-17 "Can I go a whole day without blaming others?"
   ftwynn-stoic-prompt-08-18 "Where can I better play to my strengths?"
   ftwynn-stoic-prompt-08-19 "What inessential things can I eliminate from my life?"
   ftwynn-stoic-prompt-08-20 "How well is my soul dressed?"
   ftwynn-stoic-prompt-08-21 "What if I stopped worrying about the future and enjoyed the present?"
   ftwynn-stoic-prompt-08-22 "What small stuff can I stop sweating?"
   ftwynn-stoic-prompt-08-23 "Where do I have too much of a good thing?"
   ftwynn-stoic-prompt-08-24 "What can I learn from others--even the people I don't like?"
   ftwynn-stoic-prompt-08-25 "What new path can I blaze today?"
   ftwynn-stoic-prompt-08-26 "What potential losses can I anticipate in advance?"
   ftwynn-stoic-prompt-08-27 "Where can I learn to laugh rather than cry?"
   ftwynn-stoic-prompt-08-28 "What luxuries can I practice not needing?"
   ftwynn-stoic-prompt-08-29 "What wants can I eliminate today?"
   ftwynn-stoic-prompt-08-30 "Can I do today's duties with both courage and confidence?"
   ftwynn-stoic-prompt-08-31 "Where have I done others wrong?"
   ftwynn-stoic-prompt-09-01 "Am I working to make my soul stronger than any Fortune?"
   ftwynn-stoic-prompt-09-02 "What's the most painful part of Stoicism for you?"
   ftwynn-stoic-prompt-09-03 "How am I preparing in the off-season for what is to come?"
   ftwynn-stoic-prompt-09-04 "How can I see these difficulties as a lesson and a test?"
   ftwynn-stoic-prompt-09-05 "What is truly mine?"
   ftwynn-stoic-prompt-09-06 "If I lost my freedom, would it break me?"
   ftwynn-stoic-prompt-09-07 "How will I use the power of choice today?"
   ftwynn-stoic-prompt-09-08 "Am I prepared for my bubble to be burst?"
   ftwynn-stoic-prompt-09-09 "Do I rule my fears, or do they rule me?"
   ftwynn-stoic-prompt-09-10 "How can I prepare for the losses I fear?"
   ftwynn-stoic-prompt-09-11 "Where can I do with less today?"
   ftwynn-stoic-prompt-09-12 "Where am I putting on airs?"
   ftwynn-stoic-prompt-09-13 "How strong is my Inner Citadel?"
   ftwynn-stoic-prompt-09-14 "Are you praying--or /demanding/?"
   ftwynn-stoic-prompt-09-15 "Are you sizzle or steak?"
   ftwynn-stoic-prompt-09-16 "Will I triumph over the disasters and panics of the day?"
   ftwynn-stoic-prompt-09-17 "Can I resist giving in to haters--and hating them in return?"
   ftwynn-stoic-prompt-09-18 "Can I let the pains of life pass without adding to them?"
   ftwynn-stoic-prompt-09-19 "Am I flexible enough to change my mind and accept feedback?"
   ftwynn-stoic-prompt-09-20 "How ready am I for unexpected attacks?"
   ftwynn-stoic-prompt-09-21 "Can I keep life's rhythm no matter the interruption?"
   ftwynn-stoic-prompt-09-22 "How will today's difficulty show my character?"
   ftwynn-stoic-prompt-09-23 "How is my training coming?"
   ftwynn-stoic-prompt-09-24 "Have I thought about /all/ that might happen?"
   ftwynn-stoic-prompt-09-25 "What am I slave to?"
   ftwynn-stoic-prompt-09-26 "What idle leisure can I replace with something more fulfilling?"
   ftwynn-stoic-prompt-09-27 "What do prosperity and difficulty each reveal about me?"
   ftwynn-stoic-prompt-09-28 "How will I respond to the things that happen today?"
   ftwynn-stoic-prompt-09-29 "Where are my eyes bigger than my stomach?"
   ftwynn-stoic-prompt-09-30 "How can I strengthen my Inner Citadel?"
   ftwynn-stoic-prompt-10-01 "How will I let my virtues shine today?"
   ftwynn-stoic-prompt-10-02 "If wisdom is the most valuable asset, how have I invested in it?"
   ftwynn-stoic-prompt-10-03 "Do I live as if we are all one--all part of the same whole?"
   ftwynn-stoic-prompt-10-04 "Will my actions today be good for all concerned?"
   ftwynn-stoic-prompt-10-05 "What do I say that's better left unsaid?"
   ftwynn-stoic-prompt-10-06 "Who else can I root for--other than myself?"
   ftwynn-stoic-prompt-10-07 "Why does my wrongdoing hurt me most of all?"
   ftwynn-stoic-prompt-10-08 "What is more pleasing than wisdom?"
   ftwynn-stoic-prompt-10-09 "Have I set my standards and am I using them?"
   ftwynn-stoic-prompt-10-10 "What do my principles tell me about persisting and resisting?"
   ftwynn-stoic-prompt-10-11 "Is honesty my default setting?"
   ftwynn-stoic-prompt-10-12 "Instead of seeking love can I give it first?"
   ftwynn-stoic-prompt-10-13 "Has revenge ever made anything better?"
   ftwynn-stoic-prompt-10-14 "What if instead of getting mad) I offered to help?"
   ftwynn-stoic-prompt-10-15 "Will I give people the benefit of the doubt?"
   ftwynn-stoic-prompt-10-16 "How can I share this philosophy that has helped me so much?"
   ftwynn-stoic-prompt-10-17 "Where can I show other people kindness?"
   ftwynn-stoic-prompt-10-18 "Am I avoiding false friendships and bad influences?"
   ftwynn-stoic-prompt-10-19 "Which good habit can I use today to drive out a bad one?"
   ftwynn-stoic-prompt-10-20 "Do my principles show themselves in my life?"
   ftwynn-stoic-prompt-10-21 "Can I do the right thing and not care about credit?"
   ftwynn-stoic-prompt-10-22 "Am I actually improving--or am I just chasing vanity?"
   ftwynn-stoic-prompt-10-23 "Am I displaying my best qualities?"
   ftwynn-stoic-prompt-10-24 "What goodness can I find inside myself? Can I bring it to the surface?"
   ftwynn-stoic-prompt-10-25 "What are my tasks in this life?"
   ftwynn-stoic-prompt-10-26 "Are my goals natural, moral, and rational?"
   ftwynn-stoic-prompt-10-27 "What bad behaviors or choices have come back to haunt me?"
   ftwynn-stoic-prompt-10-28 "What can I do to be part of something bigger than myself?"
   ftwynn-stoic-prompt-10-29 "How can I improve my character?"
   ftwynn-stoic-prompt-10-30 "What time can I claw back for myself--and how will I use it?"
   ftwynn-stoic-prompt-10-31 "What good turns can be done today?"
   ftwynn-stoic-prompt-11-01 "Can I love /everything/ that happens today?"
   ftwynn-stoic-prompt-11-02 "Can I make choices and accept whatever will be?"
   ftwynn-stoic-prompt-11-03 "How can this be exactly what I needed?"
   ftwynn-stoic-prompt-11-04 "Is change really so bad? Is the status quo really so good?"
   ftwynn-stoic-prompt-11-05 "Is my character producing a well-flowing life?"
   ftwynn-stoic-prompt-11-06 "Am I prepared for the randomness of fate and luck?"
   ftwynn-stoic-prompt-11-07 "Are you trying to master yourself--or other people?"
   ftwynn-stoic-prompt-11-08 "What's my role in the play of life?"
   ftwynn-stoic-prompt-11-09 "What principles will steer me through the flow of change?"
   ftwynn-stoic-prompt-11-10 "What will remain when all else passes away?"
   ftwynn-stoic-prompt-11-11 "What false judgment can I wipe away today?"
   ftwynn-stoic-prompt-11-12 "Can the buck stop with me today?"
   ftwynn-stoic-prompt-11-13 "Does complaining accomplish anything?"
   ftwynn-stoic-prompt-11-14 "Will I add negative thoughts on top of my troubles?"
   ftwynn-stoic-prompt-11-15 "Will I embrace the flow of change today?"
   ftwynn-stoic-prompt-11-16 "Can I cease both hoping for and fearing certain outcomes"
   ftwynn-stoic-prompt-11-17 "Is it really my place to judge other people?"
   ftwynn-stoic-prompt-11-18 "Am I practicing good Stoic thoughts?"
   ftwynn-stoic-prompt-11-19 "Will I accept the situation and still fight to do and be good?"
   ftwynn-stoic-prompt-11-20 "Where can I find timelessness in every moment?"
   ftwynn-stoic-prompt-11-21 "How can I make this minute--right now--be enough?"
   ftwynn-stoic-prompt-11-22 "What am I irrationally afraid of losing?"
   ftwynn-stoic-prompt-11-23 "Why is my power to choose so resilient and adaptable?"
   ftwynn-stoic-prompt-11-24 "How can I see my loved ones as gifts not possessions?"
   ftwynn-stoic-prompt-11-25 "Is more money really going to make things better?"
   ftwynn-stoic-prompt-11-26 "What petty comparisons am I bothering myself with?"
   ftwynn-stoic-prompt-11-27 "What sources of unrest can I tune out?"
   ftwynn-stoic-prompt-11-28 "What's bothering me that I haven't spoken up about?"
   ftwynn-stoic-prompt-11-29 "How can I be less agitated--and complain about it less, too?"
   ftwynn-stoic-prompt-11-30 "Am I ready to accept the pull of the universe?"
   ftwynn-stoic-prompt-12-01 "If I lived today as if it were my last) what would I do?"
   ftwynn-stoic-prompt-12-02 "How can I make my actions count?"
   ftwynn-stoic-prompt-12-03 "What practical problems am I solving with this philosophy?"
   ftwynn-stoic-prompt-12-04 "What do I truly own?"
   ftwynn-stoic-prompt-12-05 "What unpleasant thoughts can I face and use to my advantage?"
   ftwynn-stoic-prompt-12-06 "What can I do to /live/ now, while I still can?"
   ftwynn-stoic-prompt-12-07 "Can I love the hand Fate deals me?"
   ftwynn-stoic-prompt-12-08 "Are there any feelings I need to face?"
   ftwynn-stoic-prompt-12-09 "Are you saying no enough?"
   ftwynn-stoic-prompt-12-10 "What are you getting in return for all the time you spend so freely?"
   ftwynn-stoic-prompt-12-11 "Are you living with dignity and courage?"
   ftwynn-stoic-prompt-12-12 "Will I keep the rhythm of life) no matter the interruptions?"
   ftwynn-stoic-prompt-12-13 "Can I be grateful for the time I've been given?"
   ftwynn-stoic-prompt-12-14 "What will my life be a testament to?"
   ftwynn-stoic-prompt-12-15 "Am I going to get a little bit better today?"
   ftwynn-stoic-prompt-12-16 "What am I doing to build my self-confidence?"
   ftwynn-stoic-prompt-12-17 "How well do I really know myself?"
   ftwynn-stoic-prompt-12-18 "The end for us all is clear, but is my purpose?"
   ftwynn-stoic-prompt-12-19 "What can I focus on that is much) much bigger than me?"
   ftwynn-stoic-prompt-12-20 "What am I really so afraid of?"
   ftwynn-stoic-prompt-12-21 "How can I make the most of today--and in so doing, my life?"
   ftwynn-stoic-prompt-12-22 "What wisdom will I create today?"
   ftwynn-stoic-prompt-12-23 "If I relaxed my tight grip on life, what would happen?"
   ftwynn-stoic-prompt-12-24 "Can I consume less to make more room for virtue?"
   ftwynn-stoic-prompt-12-25 "Where can I find reinvigoration and balance?"
   ftwynn-stoic-prompt-12-26 "Where am I wasting life?"
   ftwynn-stoic-prompt-12-27 "Is my soul stronger than my body?"
   ftwynn-stoic-prompt-12-28 "In a hundred years, who will remember or be remembered?"
   ftwynn-stoic-prompt-12-29 "What am I grateful for?"
   ftwynn-stoic-prompt-12-30 "How can I bring a calm mind to tough situations?"
   ftwynn-stoic-prompt-12-31 "How will I turn these words into works?")

; Then the function itself. The string-to-symbol function has an odd name... shout out to:
; https://emacsredux.com/blog/2014/12/05/converting-between-symbols-and-strings/
(defun ftwynn/stoic-daily-prompt ()
  (interactive)
  (symbol-value (intern (concat "ftwynn-stoic-prompt-" (format-time-string "%m-%d"))))
  )


; org-capture templates
; Org-only capture templates can be inserted at point by preceding with =C-0=.
; Still tinkering with the right binding command, since the typical advice of =C-c c= is an
; extra space I don't want.. but boy is it proving to be a headache

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      '(("R" "New Recipe" entry (file+olp "~/org-roam-repo/Recipes.org" "Ideas to Try")
         (file "~/org-roam-repo/templates/new_recipe.org"))
        ("r" "Recipe Experiment" entry (file "~/org-roam-repo/mobile/Keep.org")
         (file "~/org-roam-repo/templates/new_recipe_experiment.org"))
        ("p" "Project Scaffold" entry (file+olp "~/org-roam-repo/PARA.org" "Projects" "Potential Projects")
         (file "~/org-roam-repo/templates/project_scaffold.org") :prepend t)
        ("c" "New Contact" entry (file+olp "~/org-roam-repo/mobile/contacts.org" "Inbox")
         (file "~/org-roam-repo/templates/new_contact.org") :jump-to-captured t)
        ("i" "Interstitial Journal" entry (file+datetree "~/org-roam-repo/journal.org")
         (file "~/org-roam-repo/templates/interstitial_journal.org"))
        ("m" "Morning Journal" entry (file+datetree "~/org-roam-repo/journal.org")
         (file "~/org-roam-repo/templates/morning_journal.org"))
        ("e" "Evening Journal" entry (file+datetree "~/org-roam-repo/journal.org")
         (file "~/org-roam-repo/templates/evening_journal.org"))
        ))
