;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)

;; what's up with the three repos:
;; https://emacs.stackexchange.com/questions/10500/do-i-still-need-gnu-elpa-if-i-have-melpa/10501#10501
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))


;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider
    cider-eval-sexp-fu

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ido-ubiquitous

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit

    ;; clj-refactor https://github.com/clojure-emacs/clj-refactor.el
    clj-refactor

    ;; github open browser tab in current file and stuff
    git-link

    ;; helm - incremental completion and selection
    ;;helm

    ;;Markdown
    markdown-mode

    ;;Omniscient debugger
    sayid

    ;;json validator etc (jsonlint)
    flymake-json
    json-mode
    json-navigator

    ;;better javascript mode, refactoring tools, jump to defs, etc
    indium ; repl
    js2-mode
    js2-refactor
    xref-js2
    ag;; xref-js2 uses ag in the backend
    mocha;; mocha tests with integration

    ;;minor mode colum wrap for visual line mode
    ;;(instead of having visual line wrapping around the edge of the buffer)
    visual-fill-column

    ;;ruby
    feature-mode  ;Emacs mode for editing Cucumber plain text stories.
    enh-ruby-mode ;Enhanced Ruby Mode replaces the emacs ruby mode that comes with ruby
    robe

    ;; misc
    use-package
   ))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(load "javascript.el")

;;markdown
(load "markdown.el")

(load "ruby.el")

;;org mode config
(org-babel-load-file (expand-file-name "config.org"))

;; save emacs custumizations to a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
   (when (file-exists-p custom-file)
       (load custom-file))

;; set cider-jack-in custom profile:
(set-variable 'cider-lein-parameters "with-profile +mirrors repl")

(put 'erase-buffer 'disabled nil)

;; need to move to some function soon
(format-time-string "<%Y-%m-%d %a %H:%M:%S>" (seconds-to-time (/ 1507456691120 1000)))

(current-time-zone)
