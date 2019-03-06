;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; dired listing options
(setq insert-directory-program "/usr/local/bin/gls")
(setq dired-listing-switches "-aBhl --group-directories-first")

;; Allow hash to be entered
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;;spell checker
(add-to-list 'exec-path "/usr/local/bin/")
(setq ispell-program-name "aspell")
;;(setq ispell-personal-dictionary "C:/path/to/your/.ispell")
(require 'ispell)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;;helm
(global-set-key (kbd "C-x C-h") 'helm-for-files)


;;org mode - should be moved to a seperate file or something

(defun org-line-wrap ()
            (set-fill-column 100))
(add-hook 'org-mode-hook 'org-line-wrap)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-fill-column-mode)
(add-hook 'org-mode-hook 'org-show-block-all)



