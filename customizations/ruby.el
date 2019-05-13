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

