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
