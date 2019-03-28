(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(set-face-attribute  'org-level-1 nil :height 190)
(set-face-attribute  'org-level-2 nil :height 160)

(defun org-line-wrap ()
            (set-fill-column 100))
(add-hook 'org-mode-hook 'org-line-wrap)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-fill-column-mode)
(add-hook 'org-mode-hook 'org-show-block-all)

(use-package ido-vertical-mode
  :ensure t
  :init
  (ido-vertical-mode 1))

(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ;; binds keys after it initializes
  ("M-x" . smex))

(setq smex-save-file (concat user-emacs-directory ".smex-items"))

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
  :bind ((:map projectile-mode-map
              ("s-p" . 'projectile-command-map))
         (:map projectile-mode-map
              ("C-c p" . 'projectile-command-map))))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)
