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

(defun my/x220-laptop-p ()
  (equal (system-name) "aemacs"))

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

(when (my/x220-laptop-p)
      (require 'exwm)
      (require 'exwm-config)
      (exwm-config-default))

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

   display-time-24hr-format t
   display-time-format "%H:%M - %d %B %Y"
   
   indent-tab-modes nil)

(defalias 'yes-or-no-p 'y-or-n-p) ; Accept 'y' in lieu of 'yes'.

(set-fill-column 120)

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

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t
  :config
  (setq counsel-find-file-ignore-regexp "\\.elc\\'")
  (setq counsel-find-file-ignore-regexp "\\.pyc\\'")
  (setq counsel-find-file-ignore-regexp "\\.*~\\'"))

(use-package ivy
  :ensure t
  :config
  (setq ivy-ignore-buffers '("\\` " "\\`\\*"))
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c s") 'counsel-rg)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;; projectile
(use-package projectile
   :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


;; modus dark themes
(use-package modus-vivendi-theme
  :ensure t)

;; modus light theme
(use-package modus-operandi-theme
  :ensure t)

(use-package ripgrep
  :ensure t
  :defer)

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

;; diminish
(use-package diminish
  :ensure t
  :commands diminish
  :init
  (diminish 'flycheck)
  (diminish 'flymake)
  (diminish 'company)
  (diminish 'eldoc-mode)
  (diminish 'projectile-mode)
  (diminish 'sp-mode)
  (diminish 'wk-mode))

(use-package ibuffer-projectile
  :ensure t
  :defer)

(use-package ibuffer
  :config
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))))

(defun remaining-battery ()
  (concat "| Batt: " (shell-command-to-string "/bin/cat /sys/class/power_supply/BAT0/capacity 2>/dev/null")))


(use-package    feebleline
  :ensure       t
  :config       (setq feebleline-msg-functions
                      '((feebleline-line-number         :post "" :fmt "%5s")
                        (feebleline-column-number       :pre ":" :fmt "%-2s")
                        (feebleline-file-directory      :face feebleline-dir-face :post "")
                        (feebleline-file-or-buffer-name :face font-lock-keyword-face :post "")
                        (feebleline-file-modified-star  :face font-lock-warning-face :post "")
                        (feebleline-git-branch          :face feebleline-git-face :pre " : ")
			(feebleline-project-name        :align right)
			(remaining-battery              :align right)))
			
                (feebleline-mode 1))

 
(use-package pdf-tools
  :ensure t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Custom bindings
(global-set-key (kbd "C-x #") 'global-display-line-numbers-mode)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x <f2>") 'rename-buffer)
(global-set-key (kbd "s-l")
		(lambda () (interactive (start-process "" nil "slock"))))

(add-hook 'after-init-hook 'global-company-mode)
(define-key global-map [remap list-buffers] 'bs-show)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(defun dired-mode-buffers-p (buf)
  "Non-nil if buffer BUF is in `dired-mode'."
  (with-current-buffer buf
    (derived-mode-p 'dired-mode)))

(add-to-list 'ibuffer-never-show-predicates "^\\*helm")
(add-to-list 'ibuffer-never-show-predicates "^\\*elfeed-log")
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
             '(font . "DejaVu Sans Mono-9"))

(file-extensions)
(setup-eglot-lsp)
(ido-mode)

(provide 'init)
;;; init.el ends here
