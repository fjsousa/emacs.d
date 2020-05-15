(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'tomorrow-night-bright t)

;; increase font size for better readability
(set-face-attribute 'default nil :height 100 :weight 'bold)
;;(set-face-attribute 'default nil :height 130 :weight 'bold)

(setq-default frame-title-format "%b (%f)")

(add-hook 'sh-mode-hook 'flycheck-mode)
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; (use-package org-bullets
;;   :ensure t
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

;;(set-face-attribute  'org-level-1 nil :height 190)
;;(set-face-attribute  'org-level-2 nil :height 160)

(defun org-line-wrap ()
(set-fill-column 100))
(add-hook 'org-mode-hook 'org-line-wrap)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-fill-column-mode)
(add-hook 'org-mode-hook 'org-show-block-all)
(add-hook 'org-mode-hook (lambda () (setq show-trailing-whitespace t)))

(require 'color)
(set-face-attribute 'org-block nil :background
                    (color-darken-name
                     (face-attribute 'default :background) 3))

(setq org-src-block-faces '(("emacs-lisp" (:background "#2E2E2E"))
                            ("python" (:background "#2E2E2E"))
                            ("javascript" (:background "#E3E3E3"))
                            ("json" (:background "#E3E3E3"))))

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

;; http://www.emacswiki.org/emacs/InteractivelyDoThings

;; Fix
;; Warning (bytecomp): reference to free variable \‘ido-cur-item\’
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)

(setq ido-everywhere t)
(ido-mode 1)

;; Don't ask for permission. Other choices are prompt and never.
(setq ido-create-new-buffer 'always)

;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
(setq ido-enable-flex-matching t)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point 'guess)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not
;; open now
(setq ido-use-virtual-buffers t)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(ido-ubiquitous-mode 1)

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

(global-set-key (kbd "C-x C-b") 'ibuffer)

(windmove-default-keybindings)

(global-set-key (kbd "M-i") 'imenu)

(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

(global-set-key "\C-cy" '(lambda () (interactive) (popup-menu 'yank-menu)))

(fset 'yes-or-no-p 'y-or-n-p)

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

(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

(setq create-lockfiles nil)

;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(global-set-key (kbd "C-x g") 'magit-status)

(if (eq system-type 'darwin)
  (setq insert-directory-program "/usr/local/bin/gls"))
(setq dired-listing-switches "-aBhl --group-directories-first")

;;https://github.com/jwiegley/use-package#key-binding
(use-package multiple-cursors
  :ensure t
  ;;:config (... 1)
  :bind (("C-c C-c" . mc/edit-lines)
         ("C-." . mc/mark-next-like-this)
         ("C-," . mc/mark-previous-like-this)
         ("C-c C-," . mc/mark-all-like-this)
         ("C->" . mc/skip-to-next-like-this)
         ("C-c C-/" . 'mc/unmark-next-like-this)))
;;"C-v" mc/cycle-forward
;;"M-v" mc/cycle-backward

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

(defun fs/delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

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

;; javascript / html
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'html-mode-hook 'subword-mode)
(setq js-indent-level 2)
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))


;; coffeescript
(add-to-list 'auto-mode-alist '("\\.coffee.erb$" . coffee-mode))
(add-hook 'coffee-mode-hook 'subword-mode)
(add-hook 'coffee-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'coffee-mode-hook
          (defun coffee-mode-newline-and-indent ()
            (define-key coffee-mode-map "\C-j" 'coffee-newline-and-indent)
            (setq coffee-cleanup-whitespace nil)))
(custom-set-variables
 '(coffee-tab-width 2))

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

(setq python-shell-interpreter "python3")

(require 'direx)
(require 'popwin)
(push '(direx:direx-mode :position left :width 45 :dedicated t)
      popwin:special-display-config)
;;(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)

;; typescript
;; move to separate file
;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (company-mode +1))

;; (add-hook 'before-save-hook 'tide-format-before-save)
;; (add-hook 'typescript-mode-hook #'setup-tide-mode)
;; (put 'downcase-region 'disabled nil)
;; (put 'upcase-region 'disabled nil)

;; enhanced ruby mode

;;add enhanced mode to ruby files only
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))

;;add enhanced mode to all ruby related files
(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

;;feature-mode
(setq feature-step-search-path "spec/**/step_definitions/*.rb")
(setq feature-root-marker-file-name "Gemfile.lock")


;; notes
;; - jump to definition with robe mode fails to lunch a repl because of some pry cock up
;; Sorry, you can't use Pry without Readline or a compatible library.
;; Possible solutions:
;;  * Rebuild Ruby with Readline support using `--with-readline`
;;  * Use the rb-readline gem, which is a pure-Ruby port of Readline
;;  * Use the pry-coolline gem, a pure-ruby alternative to Readline
;;
;; last two didn't work, had to reinstall ruby 2.4:
;; https://stackoverflow.com/questions/19897045/how-to-compile-ruby-with-readline-support


;; robe mode

(add-hook 'enh-ruby-mode-hook 'robe-mode)
;; autocomplete for robe
;;(add-hook 'enh-ruby-mode-hook 'ac-robe-setup)
;;company mode for robe
;;(eval-after-load 'company '(push 'company-robe company-backends))


;; trailing white space
(add-hook 'enh-ruby-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; Automatically load paredit when editing a lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; trailing whitespaces
(add-hook 'emacs-list-mode-hook (lambda () (setq show-trailing-whitespace "true")))

;;;;
;; Clojure
;;;;

;; Enable paredit for Clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; adds trailing white space
(add-hook 'clojure-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; syntax hilighting for midje
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))
            (define-clojure-indent
              (context 1)
              (describe 1)
              (it 1)
              (with-redefs 1)
              (with 1)
              (around 1)
              (before 1)
              (fdef 1)
              (try 1))))

;;;;
;; Cider
;;;;

;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook 'eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;;autocomplete hooks
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

;; To make TAB complete, without losing the ability to manually indent, you can add this:
(add-hook 'clojure-mode (lambda ()
                          (local-set-key (kbd "TAB") #'company-indent-or-complete-common)))

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojurescript-mode))
(add-to-list 'auto-mode-alist '("lein-env" . clojure-mode))

;; key bindings
;; these help me out with the way I usually develop web apps
(defun fs/cider-server-restart ()
  (interactive)
  (cider-interactive-eval "(legend.repl/restart)"))

(defun fs/cider-switch-to-cljs-repl ()
  (interactive)
  (cider-interactive-eval "(figwheel-sidecar.repl-api/cljs-repl \"dev\")"))

(defun fs/cider-quit-cljs-repl ()
  (interactive)
  (cider-interactive-eval ":cljs/quit"))

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'fs/server-restart)))

(defun fs/cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(clojure.tools.namespace.repl/refresh)"))

;; setting cider output line to 100 char so that it doesn't break the repl
(setq cider-repl-print-length 100)

;;cljr need to use package on this
(require 'clj-refactor)

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "s-c"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;;(setq cider-lein-parameters "with-profile debug,dev repl :headless")
;;(setq cider-lein-parameters "with-profile +debug repl :headless")
