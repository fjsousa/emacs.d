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

(define-key org-mode-map (kbd "M-<RET>") nil); remove old binding
(define-key org-mode-map (kbd "C-c n") 'org-insert-heading)

(define-key org-mode-map (kbd "M-S-<RET>") nil); remove old binding
(define-key org-mode-map (kbd "C-c c") 'org-insert-todo-heading); c for checkbox

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

(setq dired-dwim-target t)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c C-c") 'mc/edit-lines)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-,") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c >") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-c C-/") 'mc/unmark-next-like-this)

(defun fs/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-x C-y") 'eval-and-replace)

(defun fs/load-config-org ()
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

(add-hook 'org-mode-hook #'flyspell-mode)

(add-to-list 'exec-path "/usr/local/bin/")
(setq ispell-program-name "aspell")
;;(setq ispell-personal-dictionary "C:/path/to/your/.ispell")
(require 'ispell)

;; disabling as the popup timer should be enough
;;(define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct)

(use-package flyspell-popup
  :ensure t
  :config
  (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode))

(dolist (mode '(;emacs-lisp-mode-hook
                ;inferior-lisp-mode-hook
                clojure-mode-hook
                ;python-mode-hook
                ;js-mode-hook
                ;R-mode-hook
                ))
  (add-hook mode
            '(lambda ()
               (flyspell-prog-mode))))

(setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/4.5/libexec/languagetool-commandline.jar")
(require 'langtool)
(setq langtool-mother-tongue "en-GB"
      langtool-disabled-rules '("WHITESPACE_RULE"
                                "EN_UNPAIRED_BRACKETS"
                                "COMMA_PARENTHESIS_WHITESPACE"
                                "EN_QUOTES"))
