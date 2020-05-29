(require 'package)

;; what's up with the three repos:
;; https://emacs.stackexchange.com/questions/10500/do-i-still-need-gnu-elpa-if-i-have-melpa/10501#10501
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))


;; Load and activate emacs packages.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; install use-package if not present
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
   (when (file-exists-p custom-file)
       (load custom-file))

(use-package better-defaults
  :ensure t)

(global-set-key (kbd "M-i") 'imenu)

;; moves around with shift
(windmove-default-keybindings)

;; recent files
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)

;; keep track of saved places in ~/.emacs.d/places
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

;; yes/no -> y/n
(fset 'yes-or-no-p 'y-or-n-p)

(setq create-lockfiles nil)

;; (use-package zenburn-theme
 ;;  :ensure t
 ;;  :config (load-theme 'zenburn t))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config (color-theme-sanityinc-tomorrow-night))

  ;;(load-theme color-theme-sanityinc-tomorrow-day t)
  ;;(load-theme color-theme-sanityinc-tomorrow-blue t)
  ;;(load-theme color-theme-sanityinc-tomorrow-bright t)
  ;;(load-theme color-theme-sanityinc-tomorrow-eighties t)

(set-face-attribute 'default nil :height 100 :weight 'bold)
;;(set-face-attribute 'default nil :height 130 :weight 'bold)

(setq-default frame-title-format "%b (%f)")

;; NOTE: you don't need fuzzy matchig, just hit space

(use-package ivy
  :ensure t
  :config (progn (ivy-mode 1)
           (setq ivy-use-virtual-buffers t)
           (setq enable-recursive-minibuffers t)
           (setq ivy-count-format "(%d/%d) "))
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)))

(use-package swiper
  :ensure t
  :config (setq search-default-mode #'char-fold-to-regexp)
  :bind ( "\C-s" . swiper))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h a" . counsel-apropos)
         ("C-h b" . counsel-descbinding)
         ("C-h i" . counsel-info-lookup-symbol)
         ("M-i" . counsel-imenu)
         ("C-c C-y" . counsel-yank-pop)
         ("C-c C-m" . counsel-mark-ring)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history)))

(use-package ivy-hydra
  :ensure t)

;; Fix
;; Warning (bytecomp): reference to free variable \‘ido-cur-item\’
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)

(use-package ido
  :config
  (ido-mode 1)
  (setq ido-everywhere t)
  (setq  ido-create-new-buffer 'always)
  (setq  ido-use-virtual-buffers t)
  (setq  ido-auto-merge-work-directories-length -1)
  (setq  ido-use-filename-at-point 'guess)
  (setq  ido-enable-flex-matching t))

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(use-package ido-ubiquitous
  :ensure t
  :config
  (ido-ubiquitous-mode 1))

(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex)
  :config (setq smex-save-file (concat user-emacs-directory ".smex-items")))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(if (eq system-type 'darwin)
  (setq insert-directory-program "/usr/local/bin/gls"))
(setq dired-listing-switches "-aBhl --group-directories-first")

(defun my-god-mode-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'bar
                      'box)))

(use-package god-mode
  :ensure t
  ;;:config (god-mode)
  :bind (("<escape>" . god-mode-all)
         ("C-x C-1" . delete-other-windows)
         ("C-x C-2" . split-window-below)
         ("C-x C-3" . split-window-right)
         ("C-x C-0" . delete-window)
         :map god-local-mode-map
         ("." . repeat)
         ("i" . god-local-mode))
  :hook ((god-mode-enabled . my-god-mode-update-cursor)
         (god-mode-disabled . my-god-mode-update-cursor)))

;;god-exempt-major-modes
;;god-exempt-predicates

(use-package rg
:ensure t
:config (rg-enable-default-bindings))

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
  :bind ((:map projectile-mode-map
              ("s-p" . 'projectile-command-map))
         (:map projectile-mode-map
              ("C-c p" . 'projectile-command-map))))

(use-package ace-jump-mode
  :ensure t
  :bind
  (("C-x C-." . ace-jump-mode)
   ("C-x SPC" . ace-jump-mode-pop-mark)))

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

;; C-h b or helm-descbinds to list these and others
(use-package multiple-cursors
  :ensure t
  :bind (("C-c C-c" . mc/edit-lines)
         ("C-." . mc/mark-next-like-this)
         ("C-," . mc/mark-previous-like-this)
         ("C-c C-," . mc/mark-all-like-this)
         ("C->" . mc/skip-to-next-like-this)
         ("C-c C-/" . 'mc/unmark-next-like-this)))
;;"C-v" mc/cycle-forward
;;"M-v" mc/cycle-backward

(use-package neotree
  :ensure t
  :bind ("<f8>" . neotree-toggle))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(defun fs/peer-clean-error ()
  "Paste peer error in new buffer"
  (interactive)
  (let (($buf (generate-new-buffer "peer-error")))
    (switch-to-buffer $buf)
    ;;(funcall initial-major-mode)
    ;;(setq buffer-offer-save t)
    (yank)
    (goto-char (point-min))
    (while (search-forward "\\n" nil t)
      (replace-match "\n"))
    $buf))

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

(defun fs/hexify-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (decode-coding-string (url-hexify-string text) 'utf-8)))))

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

(defun fs/open-config-org ()
  (interactive)
  (find-file "~/.emacs.d/config.org"))

  (defun fs/open-cheatsheet ()
  (interactive)
  (find-file "~/.emacs.d/cheatsheet.org"))

(defun fs/open-todo ()
  (interactive)
  (find-file "/home/fsousa/SpiderOak Hive/writeups/notes/todo.org"))

(defun fs/open-legend-org ()
  (interactive)
  (find-file "/home/fsousa/src/legend-docs/legend.org"))

(defun fs/seconds-to-human (timestamp)
  (format-time-string "<%Y-%m-%d %a %H:%M:%S>" (seconds-to-time timestamp)))


(defun fs/timestamp-to-human-date (arg)
  "converts timestamp in the region, if active; if not, use timestamp at point."
  (interactive "*p")
  (let* ((timestamp (buffer-substring (region-beginning) (region-end)))
         (string-size (length timestamp)))
    (cond ((= 10 string-size) (print (fs/seconds-to-human (string-to-number timestamp))))
          ((= 13 string-size) (print (fs/seconds-to-human (/ (string-to-number timestamp) 1000))))
          ('otherwise (print error)))))

;; select region on a timestamp and M-x fs/timestamp-to-human-date
;; 1588783092

(use-package paredit
  :ensure t
  :hook
  ((emacs-lisp-mode . paredit-mode)
   (lisp-interaction-mode . paredit-mode)
   (ielm-mode . paredit-mode)
   (lisp-mode . paredit-mode)
   (eval-expression-minibuffer-setup . paredit-mode)
   (clojure-mode . paredit-mode)
   (cider-repl-mode . paredit-mode)))

;;comes with emacs
(use-package eldoc
  :hook ((emacs-lisp-mode . eldoc-mode)
         (lisp-interaction-mode . eldoc-mode)
         (ielm-mode . eldoc-mode)))

(use-package git-link
  :ensure t)

(use-package clojure-mode
  :hook (clojure-mode . display-line-numbers-mode))

(use-package clojurescript-mode
  :hook (clojurescript-mode . display-line-numbers-mode))

(defun fs/legend-server-start ()
  (interactive)
  ;;(cider-jack-in '())
  (cider-interactive-eval "(legend.user/start)")
  (message "server running"))

(defun fs/legend-server-refresh ()
  (interactive)
  (cider-interactive-eval "(legend.user/refresh)")
  (message "refresh ok"))

(defun fs/legend-server-restart ()
    (interactive)
    (cider-interactive-eval  "(legend.user/reset)")
    (message "refresh and server restart ok"))

(use-package cider
  :ensure t
  :bind (:map clojure-mode-map
              ("C-c C-v" . fs/legend-server-restart))
  :config
  (setq cider-repl-pop-to-buffer-on-connect t)
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  (setq cider-repl-wrap-history t)
  (setq cider-repl-print-length 100))

;; TODO: put these in a single form
(add-to-list 'safe-local-variable-values
             '(cider-default-cljs-repl . shadow))

(add-to-list 'safe-local-variable-values
             '(cider-shadow-default-options . "app-with-login"))

(add-to-list 'safe-local-variable-values
             '(cider-custom-cljs-repl-init-form . "(legend.shadow-repl/cljs-repl)"))

(add-to-list 'safe-local-variable-values
             '(cider-lein-parameters . "with-profile dev,user repl :headless"))

(use-package clj-refactor
  :defer t
  :ensure t)

(use-package org
  :hook ((org-shiftup-final . windmove-up)
         (org-shiftleft-final . windmove-left)
         (org-shiftdown-final . windmove-down)
         (org-shiftright-final . windmove-right))
  :config (setq org-support-shift-select 'always))

;; display text in a column and wraps text around
(use-package visual-fill-column
  :hook (org-mode . (lambda () (progn
                                 ;; visual fill column mode works along side visual line mode
                                 ;; so we have to enable both
                                 (visual-line-mode)
                                 (visual-fill-column-mode)
                                 (setq visual-fill-column-width 100)))))
