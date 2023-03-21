
;; Trying to build a minor mode for rapid outlining
(define-minor-mode rapid-outliner-mode
  "v02 A mode for quick Org headlining."
  :keymap (make-sparse-keymap))

(evil-define-key '(normal insert) 'local "<return>" 'org-insert-heading)
(evil-define-key '(normal insert) 'local "M-<return>" 'org-return)

