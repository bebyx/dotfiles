(setq visible-bell t) ; switch off annoying bell sound, instead bell is visible
(load-theme 'wombat) ; dark theme
(set-face-attribute 'default nil :family "Source Code Pro" :foundry "ADBO" :slant 'normal :weight 'normal :height 140 :width 'normal)

(global-linum-mode t)
(fringe-mode 15)

(column-number-mode t)
(size-indication-mode t)
(scroll-bar-mode -1)

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

(use-package reverse-im
  :custom
  (reverse-im-input-methods '("ukrainian-computer"))
  :config
  (reverse-im-mode t))

(use-package markdown-mode)
