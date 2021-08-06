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
  ;; iosevka's emac performance sucks
  (set-face-attribute 'default nil
                      :height 100
                      :font "Monaco-11")
  (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq scroll-conservatively 101)
  (exec-path-from-shell-initialize))


(when (eq system-type 'gnu/linux)
  (set-frame-parameter nil 'alpha '(85 . 85))
  (set-face-attribute 'default nil
                      :height 110
                      :font "CamingoCode-10"))
                      ;;:font "JetBrains Mono-9"))

(setq-default line-spacing 0.2)
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
 case-fold-search nil
 initial-major-mode 'org-mode

 custom-file "~/.emacs.d/custom.el")

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
  (add-to-list 'ibuffer-never-show-predicates "^\\magit")
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-projectile
  :ensure t
  :defer)

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

(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(defun my-doom-modeline--font-height ()
  "Calculate the actual char height of the mode-line."
  (+ (frame-char-height) 2))

(advice-add #'doom-modeline--font-height :override #'my-doom-modeline--font-height)

(setq doom-modeline-height 20)
(setq doom-modeline-icon (display-graphic-p))

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
(global-set-key (kbd "s-r") 'rename-buffer)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)
(define-key global-map [remap list-buffers] 'ibuffer)

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
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
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
  :hook
  (ruby-mode . 'robe-mode))

;; dockerfile
(use-package dockerfile-mode
  :ensure t)

;; scheme


;; elixir

(use-package ruby-end
  :ensure t)

(use-package elixir-mode
  :ensure t)

(defvar my/home (getenv "HOME"))

(use-package mix
  :ensure t
  :config
  (setq mix-path-to-bin (concat my/home "/.asdf/shims/mix"))
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
  (global-set-key (kbd "s-<tab>") 'vterm-toggle)
  (global-set-key (kbd "C-M-t") 'vterm-toggle-cd)
  (define-key vterm-mode-map [(control return)]   #'vterm-toggle-insert-cd)
  (define-key vterm-mode-map (kbd "s-n")   'vterm-toggle-forward)
  (define-key vterm-mode-map (kbd "s-p")   'vterm-toggle-backward))

;; org
(use-package org
  :ensure t
  :config
  (setq org-agenda-files '("~/gtd/gtd.org"
                           "~/gtd/inbox.org"
                           "~/gtd/tickler.org"))
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
	   (file+headline "~/gtd/indbox.org" "Tasks")
           "* TODO %i%")
          ("T" "Tickler" entry
	   (file+headline "~/gtd/tickler.org" "Tickler")
           "* %i% \n %U")))
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (setq org-todo-keywords '((sequence "TODO" "DOING" "DONE"))))

;; ws-butler

(use-package ws-butler
  :ensure t)

;; functions

(defun my/toggle-window-transparency ()
  "Toggle window transparency."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '(85 . 85) '(100 . 100)))))

(global-set-key (kbd "C-c t") #'my/toggle-window-transparency)

;;; modes
(setq-default cursor-type 'bar)

(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(electric-pair-mode +1)
(projectile-mode +1)
(global-auto-revert-mode -1)
(global-hl-line-mode +1)
(ws-butler-global-mode +1)
(ido-mode +1)

(provide 'init)
;;; init.el ends here
