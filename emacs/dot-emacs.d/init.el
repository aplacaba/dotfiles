;;; package --- .emacs
;;; Commentary:
;;; Code:

;; My emacs configuration file
;; alacaba@fastmail.com

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

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
               `(web-mode . ("~/.asdf/shims/javascript-typescript-langserver")))
  (add-to-list 'eglot-server-programs
               `(elixir-mode . ("~/Code/elixir/elixir-ls/release/language_server.sh")))
  (add-to-list 'eglot-server-programs
               `(rust-mode . ("rls"))))

(setq
   ;; No need to see GNU agitprop.
   inhibit-startup-screen t
   ;; No need to remind me what a scratch buffer is.
   initial-scratch-message nil
   ;; Double-spaces after periods is morally wrong.
   sentence-end-double-space nil
   ;; Never ding at me, ever.
   ring-bell-function 'ignore
   ;; Prompts should go in the minibuffer, not in a GUI.
   use-dialog-box nil
   ;; Fix undo in commands affecting the mark.
   mark-even-if-inactive nil
   ;; Let C-k delete the whole line.
   kill-whole-line t
   ;; search should be case-sensitive by default
   case-fold-search nil

   column-number-mode t

   custom-safe-themes t
   tab-always-indent 'complete
   custom-safe-themes t

   backup-directory-alist `((".*" . ,temporary-file-directory))
   auto-save-file-name-transforms`((".*" ,temporary-file-directory t))
   frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b")))
   
   indent-tab-modes nil)

(defalias 'yes-or-no-p 'y-or-n-p) ; Accept 'y' in lieu of 'yes'.

(global-set-key (kbd "C-x C-n") nil)
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))
(global-set-key (kbd "C-#") 'global-display-line-numbers-mode)
(global-set-key (kbd "C-t") nil)
(global-set-key (kbd "C-x #") 'global-display-line-numbers-mode)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x <f2>") 'rename-buffer)
(define-key global-map [remap list-buffers] 'bs-show)

;; smartparens
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
  :defer t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :bind
  (("C-M-g" . magit-status)))

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
  (global-flycheck-mode))
  
;; avy
(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "M-g e") 'avy-goto-word-0)
  (global-set-key (kbd "C-c C-j") 'avy-resume)
  (global-set-key (kbd "C-:") 'avy-goto-char))


;; org mode
(use-package org
  :ensure t
  :config
  (setq org-todo-keywords
        '((sequence "TODO" "IN PROGRESS" "DONE"))))

;; markdown
(use-package markdown-mode
  :ensure t)

;; emmet 
(use-package emmet-mode
  :ensure t)

;; fsharp
(use-package fsharp-mode
  :ensure t
  :init
  (require 'eglot-fsharp)
  :config
  (setq inferior-fsharp-program "dotent fsi")
  (add-hook 'fsharp-mode-hook 'indent-guide-mode)
  (add-hook 'fsharp-mode-hook #'smartparens-mode))

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

;; elixir
(use-package elixir-mode
  :ensure t
  :commands elixir-mode
  :config
  (add-hook 'elixir-mode-hook 'company-mode))

;; clojure
(use-package cider
  :ensure t)

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

;; rust
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

;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


;; modus dark themes
(use-package modus-vivendi-theme
  :ensure t)

;; modus light theme
(use-package modus-operandi-theme
  :ensure t)

(use-package ripgrep
  :ensure t)

;; elfeed
(use-package elfeed
  :ensure t
  :config
  (global-set-key (kbd "C-x w") 'elfeed)
  (setq elfeed-feeds
        '(("http://news.ycombinator.com/rss" HN)
          ("https://protesilaos.com/codelog.xml" prot)
	  ("https://manila.craigslist.org/search/sof?format=rss" CL))))

;; window jump
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'ace-window))

;; terminal
(use-package vterm
  :ensure t
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t))

;; sane defaults
(use-package crux
  :ensure t
  :bind
  ("C-k" . crux-smart-kill-line)
  ("C-c n" . crux-cleanup-buffer-or-region)
  ("C-c f" . crux-recentf-find-file)
  ("C-a" . crux-move-beginning-of-line)) 

;; guide for key combinations
(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  (which-key-setup-side-window-right))

;; perspective
(use-package perspective
  :ensure t
  :custom
  (persp-mode-prefix-key (kbd "C-x z"))
  :config
  (global-set-key (kbd "s-z") 'persp-next)
  (persp-mode))

(use-package ibuffer-projectile
  :ensure t)

(use-package ibuffer
  :config
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))))
  
;; Custom bindings

(add-hook 'after-init-hook 'global-company-mode)
(defun dired-mode-buffers-p (buf)
  "Non-nil if buffer BUF is in `dired-mode'."
  (with-current-buffer buf
    (derived-mode-p 'dired-mode)))
(add-to-list 'ibuffer-never-show-predicates "^\\*helm")
(add-to-list 'ibuffer-never-show-predicates "^\\magit-process")
(add-to-list 'ibuffer-never-show-predicates "^\\magit")
(add-to-list 'ibuffer-never-show-predicates "^\\*")
(add-to-list 'ibuffer-never-show-predicates #'dired-mode-buffers-p)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(global-auto-revert-mode -1)
(load-theme 'modus-vivendi t)
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-10:book"))

(set-fill-column 120)
(file-extensions)
(setup-eglot-lsp)
(ido-mode)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ripgrep ibuffer-projectile perspective which-key web-mode vterm use-package smartparens ruby-end racer pdf-tools neotree modus-vivendi-theme modus-operandi-theme markdown-mode magit json-mode indent-guide helm-rg helm-projectile fsharp-mode flycheck emmet-mode elixir-mode elfeed doom-modeline cycle-themes crux company cider base16-theme ace-window)))
 '(persp-mode-prefix-key "z")
 '(web-mode-code-indent-offset 2 t)
 '(web-mode-css-indent-offset 2 t)
 '(web-mode-markup-indent-offset 2 t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
