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
