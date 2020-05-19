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
(global-set-key (kbd "C-x C-b") 'ibuffer)

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

;;yank menu
(global-set-key "\C-cy" '(lambda () (interactive) (popup-menu 'yank-menu)))

;; yes/no -> y/n
(fset 'yes-or-no-p 'y-or-n-p)

(setq create-lockfiles nil)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'tomorrow-night-bright t)

(set-face-attribute 'default nil :height 100 :weight 'bold)
;;(set-face-attribute 'default nil :height 130 :weight 'bold)

(setq-default frame-title-format "%b (%f)")

;; Fix
;; Warning (bytecomp): reference to free variable \‘ido-cur-item\’
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)

(ido-mode 1)
(setq ido-everywhere t)


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
(use-package ido-ubiquitous
  :ensure t
  :config
  (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :ensure t
  :init
  (progn
    (ido-vertical-mode 1)
    (setq ido-vertical-define-keys 'C-n-and-C-p-only)))

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



(use-package helm
  :ensure t
  :config
  (helm-mode 0) ;;helm is not enabled everywhere
  :bind ("C-h a" . helm-apropos))

(use-package helm-descbinds
  :ensure t
  :init (helm-descbinds-mode)
  :bind ("C-h b" . helm-descbinds))

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
  :bind ((:map projectile-mode-map
              ("s-p" . 'projectile-command-map))
         (:map projectile-mode-map
              ("C-c p" . 'projectile-command-map))))

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
