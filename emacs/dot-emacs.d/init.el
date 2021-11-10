;;; init.el --- My Config
;;; Commentary:
;;; Emacs config
;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; customizations
(defalias 'yes-or-no-p 'y-or-n-p)

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil
                      :height 120
                      :weight 'regular
                      :font "Dejavu Sans Mono Book")
  (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq scroll-conservatively 101))

(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil
                      :family "Dejavu Sans Mono Book"
                      :weight 'regular
                      :height 90))

(setq-default line-spacing 0)
(setq-default indent-tabs-mode nil)

(setq
 custom-safe-themes t
 kill-whole-line t
 case-fold-search nil
 ring-bell-function 'ignore
 fill-column 120
 ;; no lock files
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 case-fold-search  nil
 initial-major-mode 'org-mode

 custom-file "~/.emacs.d/custom.el")

(add-hook 'before-save-hook
          'delete-trailing-whitespace)

(use-package exec-path-from-shell
  :ensure t)

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  (which-key-setup-side-window-bottom))

(use-package projectile
   :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package ripgrep
  :ensure t
  :defer)

(use-package swiper
  :ensure t
  :defer)

(use-package counsel
  :ensure t
  :defer
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

(use-package ibuffer
  :config
  (add-to-list 'ibuffer-never-show-predicates "^\\*elfeed-log")
  (add-to-list 'ibuffer-never-show-predicates "^\\magit-process")
  (add-to-list 'ibuffer-never-show-predicates "^\\magit"))

(use-package switch-window
  :ensure t
  :config
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-input-style 'minibuffer)
  (global-set-key (kbd "C-x o") 'switch-window))

(use-package diminish
  :ensure t
  :commands diminish
  :init
  (diminish 'auto-revert-mode)
  (diminish 'company-mode)
  (diminish 'flycheck-mode)
  (diminish 'eldoc-mode)
  (diminish 'projectile-mode)
  (diminish 'which-key-mode))

(use-package yaml-mode
  :ensure t)

;; themes

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; custom keybindings
(global-set-key (kbd "C-x C-n") nil)

;; split and go to buffer
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c r") 'rename-buffer)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)
(define-key global-map [remap list-buffers] 'ibuffer)
(global-set-key (kbd "C-c m m") 'modus-themes-toggle)
(global-set-key (kbd "C-x C-n") 'find-file)

;;; Programming

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 10)
  :hook
  (after-init . global-company-mode))

(use-package flycheck
  :ensure t
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  :hook
  (after-init . global-flycheck-mode))

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "M-g e") 'avy-goto-word-0)
  (global-set-key (kbd "C-c C-j") 'avy-resume)
  (global-set-key (kbd "C-:") 'avy-goto-char))

;; web

(use-package emmet-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

;; git

(use-package magit
  :ensure t
  :defer t
  ;;:config
  ;;(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :bind
  (("C-M-g" . magit-status)
   ("C-x g" . magit-status)))

;; javascript / typescript

(use-package json-mode
  :ensure t
  :config
  (setq js-indent-level 2))

(use-package web-mode
  :ensure t
  :commands web-mode
  :hook
  (web-mode . emmet-mode)
  (web-mode . company-mode)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package js2-mode
  :ensure t)

(use-package tide
  :ensure t
  :config
  (setq tide-format-options
	'(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
	  :placeOpenBraceOnNewLineForFunctions nil
	  :indentSize 2
	  :tabSize 2
	  :insertSpaceBeforeFunctionParenthesis t
	  :insertSpaceAfterOpeningAndBeforeClosingJsxExpressionBraces t))

  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (tide-hl-identifier-mode +1)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-attr-value-indent-offset 2)
  (setq typescript-indent-level 2)
  (setq flycheck-check-syntax-automatically '(save mode enabled))
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(setq company-tooltip-align-annotations t)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.eex?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.leex?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\dot-zshrc?\\'" . sh-mode))

(add-hook 'js2-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; clojure

(use-package cider
 :ensure t)

(use-package clojure-mode
  :ensure t
  :hook
  (clojure-mode . cider-mode))

;; ruby

(use-package robe
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  (ruby-end-mode +1))

;; dockerfile
(use-package dockerfile-mode
  :ensure t)

;; scheme
;; elixir

(use-package ruby-end
  :ensure t)


(use-package elixir-mode
  :ensure t
  :config
  (setq flycheck-elixir-credo-strict t))

(defvar my/home (getenv "HOME"))

(use-package mix
  :ensure t
  :config
  (setq mix-path-to-bin (concat my/home "/.asdf/shims/mix"))
  (setq compilation-scroll-output t)
  (add-hook 'elixir-mode-hook 'mix-minor-mode))

(setq flycheck-elixir-credo-strict t)

(add-to-list 'elixir-mode-hook
             (defun auto-activate-ruby-end-mode-for-elixir-mode ()
               (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
               (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
               (ruby-end-mode +1)))

(add-hook 'elixir-mode-hook 'flycheck-mode)
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
(add-hook 'elixir-format-hook (lambda ()
                                 (if (projectile-project-p)
                                      (setq elixir-format-arguments
                                            (list "--dot-formatter"
                                                  (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs")))
                                   (setq elixir-format-arguments nil))))

;; vterm
(use-package vterm
  :ensure t
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t))

(use-package vterm-toggle
  :ensure t
  :config
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (global-set-key (kbd "s-t") 'vterm-toggle)
  (global-set-key (kbd "C-M-t") 'vterm-toggle-cd)
  (define-key vterm-mode-map [(control return)]   #'vterm-toggle-insert-cd)
  (define-key vterm-mode-map (kbd "s-n")   'vterm-toggle-forward)
  (define-key vterm-mode-map (kbd "s-p")   'vterm-toggle-backward))


(require 'org-tempo)

(use-package ob-elixir
  :ensure t)

;; org
(use-package org
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda ()
                             (org-bullets-mode 1)
                             (org-indent-mode)))

  (setq org-agenda-files '("~/org/gtd/gtd.org"
                           "~/org/gtd/inbox.org"
                           "~/org/gtd/tickler.org"))
  (setq org-agenda-custom-commands
        '(("p" "Agenda and Personal tasks"
           ((agenda "")
            (tags-todo "@personal")
            (tags "books")))
          ("o" "Agenda and office tasks"
           ((agenda "")
            (tags-todo "@work")
            (tags "@projects")))))
  (setq org-capture-templates
        '(("t" "Todo [inbox]" entry
	   (file+headline "~/org/gtd/inbox.org" "Tasks")
           "* TODO %i%")
          ("T" "Tickler" entry
	   (file+headline "~/org/gtd/tickler.org" "Tickler")
           "* %i% \n %U")))
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c l") 'org-store-link)

  (setq org-todo-keywords '((sequence "TODO" "DOING" "WAITING" "DONE")))
  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t)
                               (ruby . t)
                               (python . t)
                               (elixir . t)
                               (js . t))))

(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir "~/org/journal"))

(use-package org-bullets
  :ensure t)

;; ws-butler

(use-package ws-butler
  :ensure t)

(use-package all-the-icons)

(global-set-key (kbd "C-c w b") #'windmove-left)
(global-set-key (kbd "C-c w f") #'windmove-right)
(global-set-key (kbd "C-c w n") #'windmove-down)
(global-set-key (kbd "C-c w p") #'windmove-up)

;; https://github.com/tumashu/ivy-posframe
;; makes minibuffer appear at center top
;; looking at the minibuffer using ultrawide screen is
;; so bad for eye focus, this module helps remedy that issue.
(use-package ivy-posframe
  :ensure t
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (ivy-posframe-mode +1))

;;; modes
(setq-default cursor-type 'box)

(setenv "PAGER" "cat")

(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(electric-pair-mode +1)
(projectile-mode +1)
(global-auto-revert-mode -1)
(global-hl-line-mode +1)
(ws-butler-global-mode +1)
(ido-mode +1)
;;(exec-path-from-shell-initialize)
(display-time-mode +1)

(provide 'init)
;;; init.el ends here
