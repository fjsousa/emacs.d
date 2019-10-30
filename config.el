(menu-bar-mode 1)

(global-linum-mode)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'tomorrow-night-bright t)

;; increase font size for better readability
(set-face-attribute 'default nil :height 140)

(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

(blink-cursor-mode 0)

(setq-default frame-title-format "%b (%f)")

;  (global-set-key (kbd "s-t") '(lambda () (interactive)))

(setq ring-bell-function 'ignore)



(add-hook 'sh-mode-hook 'flycheck-mode)

(use-package org-make-toc
:ensure t)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(set-face-attribute  'org-level-1 nil :height 190)
(set-face-attribute  'org-level-2 nil :height 160)

(add-hook 'org-mode-hook (lambda () (setq show-trailing-whitespace t)))

(defun org-line-wrap ()
(set-fill-column 100))
(add-hook 'org-mode-hook 'org-line-wrap)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-fill-column-mode)
(add-hook 'org-mode-hook 'org-show-block-all)

(require 'color)
(set-face-attribute 'org-block nil :background
                    (color-darken-name
                     (face-attribute 'default :background) 3))

(setq org-src-block-faces '(("emacs-lisp" (:background "#E3E3E3"))
                            ("python" (:background "#E3E3E3"))
                            ("javascript" (:background "#E3E3E3"))
                            ("json" (:background "#ffffff"))))

(autoload 'gfm-mode "markdown-mode.el" "Major mode for editing Markdown files" t)

(setq auto-mode-allist (append '(("\\.text$" . gfm-mode)
                                 ("\\.md$" . gfm-mode)
                                 ("\\.mdown$" . gfm-mode)
                                 ("\\.mdt$" . gfm-mode)) auto-mode-alist))

;;'(markdown-preview-style "/Users/fsousa/src/github-markdown-css/github-markdown.css")

(add-hook 'markdown-mode-hook '(lambda () (set-fill-column 100)))
(add-hook 'markdown-mode-hook 'visual-line-mode)

;wrap lines acording to fill-column
(add-hook 'markdown-mode-hook 'visual-fill-column-mode)

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

(use-package beacon
 :ensure t
 :config
 (progn
   (beacon-mode 1)
   (setq beacon-size 10)
   (setq beacon-color "#ca6768")
   (setq beacon-blink-duration 0.2)
   (setq beacon-blink-when-window-scrolls t)
   (setq beacon-blink-when-window-changes t)
   (setq beacon-blink-when-point-moves-horizontally 20)
   (setq beacon-blink-when-point-moves-vertically 10)))

(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

(require 'buffer-move)

(global-set-key (kbd "<C-s-up>")     'buf-move-up)
(global-set-key (kbd "<C-s-down>")   'buf-move-down)
(global-set-key (kbd "<C-s-left>")   'buf-move-left)
(global-set-key (kbd "<C-s-right>")  'buf-move-right)

(setq dired-dwim-target t)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c C-c") 'mc/edit-lines)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-,") 'mc/mark-all-like-this)
(global-set-key (kbd "C->") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-c C-/") 'mc/unmark-next-like-this)

(defun fs/sql-indent-string ()
  "Indents the string under the cursor as SQL."
  (interactive)
  (save-excursion
    (er/mark-inside-quotes)
    (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
           (pos (region-beginning))
           (column (progn (goto-char pos) (current-column)))
           (formatted-text (with-temp-buffer
                             (insert text)
                             (delete-trailing-whitespace)
                             (sql-indent-buffer)
                             (replace-string "\n" (concat "\n" (make-string column (string-to-char " "))) nil (point-min) (point-max))
                             (buffer-string))))
      (delete-region (region-beginning) (region-end))
      (goto-char pos)
      (insert formatted-text))))

(defun fs/sql-indent-region ()
  "Indents the region"
  (interactive)
  (save-excursion
    (let* ((beginning (region-beginning))
           (end (region-end))
           (text (buffer-substring-no-properties beginning end))
           (pos (region-beginning))
           (column (progn (goto-char pos) (current-column)))
           (formatted-text (with-temp-buffer
                             (insert text)
                             (delete-trailing-whitespace)
                             (sql-indent-buffer)
                             (replace-string "\n" (concat "\n" (make-string column (string-to-char " "))) nil (point-min) (point-max))
                             (buffer-string)
                             )))
      (delete-region beginning end)
      (goto-char pos)
      (insert formatted-text))))

(defun fs/put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun fs/unhex-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (decode-coding-string (url-unhex-string text) 'utf-8)))))

;; http://localhost:1212/well-search?terms=%22%22&limit=100&offset=100&rules=%5B%7B%3Aattribute%20%22basin%22%2C%20%3Avalue%20%22PERMIAN%20BASIN%22%2C%20%3Apredicate%20%22%3D%22%7D%5D&legend%3F=true&drilling-info%3F=true&name-only%3F=false

;;returns:

;;http://localhost:1212/well-search?terms=""&limit=100&offset=100&rules=[{:attribute "basin", :value "PERMIAN BASIN", :predicate "="}]&legend?=true&drilling-info?=true&name-only?=false

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

(defun fs/eval-config-org ()
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

(defun fs/delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

(defun fs/open-config-org ()
  (interactive)
  (find-file "~/.emacs.d/config.org"))

(defun fs/open-todo ()
  (interactive)
  (find-file "/home/fsousa/SpiderOak Hive/writeups/notes/todo.org"))

(defun fs/open-legend-org ()
  (interactive)
  (find-file "/home/fsousa/SpiderOak Hive/writeups/notes/legend.org"))

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

(define-key flyspell-mode-map (kbd "C-.") nil)

(dolist (mode '(;emacs-lisp-mode-hook
                ;inferior-lisp-mode-hook
                ;clojure-mode-hook
                ;python-mode-hook
                ;js-mode-hook
                ;R-mode-hook
                ))
  (add-hook mode
            '(lambda ()
               (flyspell-prog-mode))))

(if (eq system-type 'darwin)
    (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/4.5/libexec/languagetool-commandline.jar")
  (setq langtool-language-tool-jar "/home/fsousa/src/languagetool/languagetool-commandline.jar"))

(use-package langtool
  :ensure t
  :config
  (setq langtool-mother-tongue "en-GB"
        langtool-disabled-rules '("WHITESPACE_RULE"
                                  "EN_UNPAIRED_BRACKETS"
                                  ;;"COMMA_PARENTHESIS_WHITESPACE"
                                  "EN_QUOTES")))

;; riped off from
;; https://emacs.cafe/emacs/javascript/setup/2017/04/23/emacs-setup-javascript.html
(require 'js2-mode)
(require 'js2-refactor)
(require 'xref-js2)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

;;spell check in comments and
;; (add-hook 'js2-mode-hook #'flyspell-prog-mode)

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

;; so that you can run mocha tests.
;; emacs complains that your're setting variables in an unsafe way so you have to
;; do safe-local-variable-values
;; (add-hook 'js2-mode-hook
;;           (lambda ()
;;             (setq safe-local-variable-values
;;                   (quote
;;                    ((mocha-reporter . "spec")
;;                     (mocha-project-test-directory . "test/unit")
;;                     (mocha-options . " -b -R spec --timeout 100000")
;;                     (mocha-environment-variables . "NODE_ENV=test")
;;                     (mocha-command . "node_modules/.bin/mocha")
;;                     (mocha-which-node . "/Users/fsousa/.nvm/versions/node/v10.14.2/bin/node"))))))

;; (setq safe-local-variable-values
;;                   (quote
;;                    ((mocha-reporter . "spec")
;;                     (mocha-project-test-directory . "test/unit")
;;                     (mocha-options . " -b -R spec --timeout 100000")
;;                     (mocha-environment-variables . "NODE_ENV=test")
;;                     (mocha-command . "node_modules/.bin/mocha")
;;                     (mocha-which-node . "/Users/fsousa/.nvm/versions/node/v10.14.2/bin/node"))))

;; (require 'company)
;; (require 'company-tern)

;; (add-to-list 'company-backends 'company-tern)
;; (add-hook 'js2-mode-hook (lambda () (tern-mode) (company-mode)))

;; (define-key tern-mode-keymap (kbd "M-.") nil)
;; (define-key tern-mode-keymap (kbd "M-,") nil)

(require 'indium)
(add-hook 'js2-mode-hook #'indium-interaction-mode)

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :commands (smartparens-mode
             smartparens-strict-mode)
  :config
  (progn
    (require 'smartparens-config)
    (add-hook 'js2-mode-hook #'smartparens-mode)))

(setq sp-highlight-pair-overlay nil)
(setq sp-highlight-wrap-overlay t)
(setq sp-highlight-wrap-tag-overlay t)

;;(add-hook 'sql-mode-hook (lambda () (load-library "sql-indent"))) doesn't seem to work
(eval-after-load "sql"
  '(load-library "sql-indent"))
