;;; Markdown mode

;; don't I need a mode hook to enable this?
(autoload 'gfm-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.text$" . gfm-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md$" . gfm-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mdown$" . gfm-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mdt$" . gfm-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.markdown$" . gfm-mode) auto-mode-alist))

;; '(markdown-preview-style "/Users/fsousa/src/github-markdown-css/github-markdown.css")


