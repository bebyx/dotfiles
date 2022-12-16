;;; init.el --- Initialization file for Emacs.
;;; Commentary:
;; bebyx initialization file for Emacs

;;; Code:

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(cua-mode)

(setq visible-bell t) ; switch off annoying bell sound, instead bell is visible

(set-face-attribute 'default nil
		    :family "Source Code Pro"
		    :foundry "ADBO"
		    :slant 'normal
		    :weight 'normal
		    :height 140
		    :width 'normal)

(column-number-mode)
(global-display-line-numbers-mode t)
(fringe-mode 10)

(size-indication-mode t)
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(menu-bar-mode -1)   ; Disable the menu bar

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
  (auto-package-update-at-time "21:00"))

;; Use keybindings when Ukrainian layout is on
(use-package reverse-im
  :custom
  (reverse-im-input-methods '("ukrainian-computer"))
  :config
  (reverse-im-mode t))

;; Helm config
(use-package helm
  :bind (
	 ("C-x b" . helm-buffers-list)
	 ("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-s" . helm-occur))
  :config
  (helm-mode 1))

;; Dev config
(use-package markdown-mode)
(use-package yaml-mode)
(use-package groovy-mode)
(use-package gradle-mode)
(use-package git-modes)
(use-package rainbow-delimiters
  :hook
  (haskell-mode . rainbow-delimiters-mode)
  (scheme-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode))
(use-package yasnippet)

(use-package lsp-mode
  :bind-keymap
  ("C-c l" . lsp-command-map)
  ;; Optional - enable lsp-mode automatically in scala files
  :hook
  (scala-mode . lsp)
  (lsp-mode . lsp-lens-mode)
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  ;; Tune lsp-mode performance according to
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold 100000000) ;; 100mb
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (defvar lsp-completion-provider :capf)
  (defvar lsp-prefer-flymake nil)
  ;; Makes LSP shutdown the metals server when all buffers in the project are closed.
  ;; https://emacs-lsp.github.io/lsp-mode/page/settings/mode/#lsp-keep-workspace-alive
  (setq lsp-keep-workspace-alive nil))
(use-package lsp-ui)

(use-package lsp-metals)
(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp))
(use-package helm-lsp)
(use-package lsp-treemacs)

(use-package treemacs
  :bind ("C-t" . treemacs)
  :config
  (setq treemacs-width 30))

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package helm-flycheck)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

;; Use company-capf as a completion provider.
(use-package company
  :hook
  (scala-mode . company-mode)
  :config
  (defvar lsp-completion-provider :capf))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'helm))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))
;; (use-package helm-projectile)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-checker-simple-format t))

;; Beautiful dark theme
(use-package solarized-theme
  :init
  (setq solarized-distinct-fringe-background t)
  (load-theme 'solarized-dark-high-contrast))

(use-package which-key
  :init
  (which-key-mode)
  (setq which-key-idle-delay 1))

(provide 'init)
;;; init.el ends here
