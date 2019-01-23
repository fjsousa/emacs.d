;; riped off from
;; https://emacs.cafe/emacs/javascript/setup/2017/04/23/emacs-setup-javascript.html
(require 'js2-mode)
(require 'js2-refactor)
(require 'xref-js2)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

;; highlight trailing white spaces. Any non nil value is fine
(add-hook 'js2-mode-hook (lambda () (setq show-trailing-whitespace "true")))

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)
;;(define-key esc-map "." #'xref-find-definitions)

(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; redefining the ignored dirs list to exclude "lib" as it was causing
;; issues with some of the repos
(setq xref-js2-ignored-dirs '("bower_components" "node_modules" "build"))
