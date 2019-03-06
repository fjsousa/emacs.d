;; enhanced ruby mode

;;add enhanced mode to ruby files only
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))

;;add enhanced mode to all ruby related files
(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

(add-hook 'enh-ruby-mode-hook 'robe-mode)

;;feature-mode
(setq feature-step-search-path "spec/**/step_definitions/*.rb")
(setq feature-root-marker-file-name "Gemfile.lock")

