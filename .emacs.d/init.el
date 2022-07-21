(setq visible-bell t) ; switch off annoying bell sound, instead bell is visible
(load-theme 'wombat) ; dark theme
(set-face-attribute 'default nil :family "Source Code Pro" :foundry "ADBO" :slant 'normal :weight 'normal :height 140 :width 'normal)

(column-number-mode)
(global-display-line-numbers-mode t)
(fringe-mode 10)

(size-indication-mode t)
(scroll-bar-mode -1)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize) ; activate all the packages

;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "00:00"))

;; Use keybindings when Ukrainian layout is on
(use-package reverse-im
  :custom
  (reverse-im-input-methods '("ukrainian-computer"))
  :config
  (reverse-im-mode t))

;; Helm config
(use-package helm
  :bind (
	 ("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-s" . helm-occur))
  :config
  (helm-mode 1))

;; Dev, mostly Java, config
(use-package markdown-mode)
(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp))
(use-package gradle-mode)
(use-package helm-lsp)
(use-package lsp-treemacs)
(use-package treemacs
  :bind (
	 ("C-t" . treemacs)))
