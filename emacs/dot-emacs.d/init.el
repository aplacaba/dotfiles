;;; package --- .emacs
;;; Commentary:
;;; Code:

;; My emacs configuration file
;; alacaba@fastmail.com

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

;; package installation
(defvar my-packages
  '(use-package))


(defun my-packages-installed-p ()
  (cl-loop for p in my-packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(defun file-extensions ()
  "Web mode support for the ff extensions."
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.eex?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.leex?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.org?\\'" . org-mode)))

(defun setup-eglot-lsp ()
  (add-to-list 'eglot-server-programs
               `(web-mode . ("javascript-typescript-stdio")))
  (add-to-list 'eglot-server-programs
               `(elixir-mode . ("~/Code/elixir/elixir-ls/release/language_server.sh")))
  (add-to-list 'eglot-server-programs
               `(rust-mode . ("rls"))))


(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))


(setq-default indent-tabs-mode nil)
(setq-default tab-stop-list 2)
(setq custom-safe-themes t)
(setq inhibit-startup-screen t)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq tab-always-indent 'complete)

(fset 'yes-or-no-p 'y-or-n-p)
(setq initial-scratch-message nil)
(setq initial-buffer-choice "~/Workspace")
(setq explicit-shell-file-name "/usr/bin/zsh") 

(global-set-key (kbd "C-x C-n") nil)
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))
(global-set-key (kbd "C-#") 'global-display-line-numbers-mode)


(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode 1)
  :init
  (require 'smartparens-config))


;; eglot
(use-package eglot
  :ensure t
  :hook
  (web-mode . eglot-ensure)
  (elixir-mode . eglot-ensure)
  (rust-mode . eglot-ensure))


;; magit
(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)))


;; company
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 10)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))


;; flycheck global
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))


(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle))


(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "M-g e") 'avy-goto-word-0)
  (global-set-key (kbd "C-c c-j") 'avy-resume))


;; org mode
(use-package org
  :ensure t
  :config
  (setq org-todo-keywords
        '((sequence "TODO" "DONE"))))


;; markdown
(use-package markdown-mode
  :ensure t)


;; emmet for web / html
(use-package emmet-mode
  :ensure t)

(use-package indent-guide
  :ensure t
  :config
  (set-face-background 'indent-guide-face "dimgray"))

;; fsharp
(use-package fsharp-mode
  :ensure t
  :init
  (require 'eglot-fsharp)
  :config
  (setq inferior-fsharp-program "dotent fsi")
  (add-hook 'fsharp-mode-hook 'indent-guide-mode)
  (add-hook 'fsharp-mode-hook #'smartparens-mode))


(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 10)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))


;; flycheck global
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))


(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle))


(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "M-g e") 'avy-goto-word-0)
  (global-set-key (kbd "C-c c-j") 'avy-resume))




;; elixir
(use-package elixir-mode
  :ensure t
  :commands elixir-mode
  :config
  (add-hook 'elixir-mode-hook 'company-mode))


;; web-mode
(use-package web-mode
  :ensure t
  :commands web-mode
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'company-mode)
  (add-hook 'web-mode-hook #'smartparens-mode)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))


(use-package racer
  :ensure t
  :config
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))


;; rust-mode
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  (setq rust-racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (add-hook 'rust-mode-hook #'smartparens-mode)
  (add-hook 'rust-mode-hook #'racer-mode))

;; json-mode
(use-package json-mode
  :ensure t)


(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

;; helm and projectile
(use-package helm-projectile
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-c C-p") 'helm-projectile-find-file)
  (global-set-key (kbd "C-c s") 'helm-projectile-rg)
  :init
  (helm-projectile-on))


(use-package helm-rg
  :ensure t)

;; modus themes
(use-package modus-vivendi-theme
  :ensure t)
(use-package modus-operandi-theme
  :ensure t)


;; elfeed
(use-package elfeed
  :ensure t
  :config
  (global-set-key (kbd "C-x w") 'elfeed)
  (setq elfeed-feeds
        '(("http://news.ycombinator.com/rss" HN)
          ("https://protesilaos.com/codelog.xml" prot))))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window))


(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Custom bindings
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "C-x #") 'global-display-line-numbers-mode)

;; Light for the day
(load-theme 'modus-operandi t t)
(run-at-time "05:00" (* 60 60 24) (lambda () (enable-theme 'modus-operandi)))

;; Dark for the night
(load-theme 'modus-vivendi t t)
(run-at-time "17:00" (* 60 60 24) (lambda () (enable-theme 'modus-vivendi)))

(add-hook 'after-init-hook 'global-company-mode)
(define-key global-map [remap list-buffers] 'helm-mini)
(file-extensions)
(setup-eglot-lsp)
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-10"))
(display-time-mode 1)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ace-window json-mode elfeed web-mode use-package smartparens rust-mode ruby-end pdf-tools neotree modus-vivendi-theme modus-operandi-theme markdown-mode magit indent-guide helm-rg helm-projectile fsharp-mode flycheck emmet-mode elixir-mode cycle-themes company base16-theme avy)))
 '(web-mode-code-indent-offset 2 t)
 '(web-mode-css-indent-offset 2 t)
 '(web-mode-markup-indent-offset 2 t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
