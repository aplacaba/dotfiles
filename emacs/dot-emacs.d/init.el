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
                      :height 190
                      :weight 'regular
                      :font "Dejavu Sans Mono")
  (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq scroll-conservatively 101)
  (setq warning-minimum-level :emergency)
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq dired-listing-switches "-al --group-directories-first")
  (setq comp-async-report-warnings-errors nil))

(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil
                      :family "Dejavu Sans Mono"
                      :weight 'regular
                      :height 140))


(setq-default line-spacing 2)
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
 display-time-default-load-average nil
 display-time-format "%H:%M:%S %a,%d %b %Y"
 column-number-mode t

 custom-file "~/.emacs.d/custom.el")

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package exec-path-from-shell
  :ensure t)

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;; (use-package projectile
;;   :ensure t
;;   :config
;;   (setq projectile-completion-system 'vertico)
;;   (setq projectile-switch-project-action #'projectile-dired)
;;   (projectile-mode +1)
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;   :init
;;   (projectile-mode))

(use-package ripgrep
  :ensure t
  :defer)

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(defun consult-line-literal ()
  "Use this instead of isearch."
  (interactive)
  (let ((completion-styles '(substring))
        (completion-category-defaults nil)
        (completion-category-overrides nil))
    (consult-line)))

(use-package consult
  :ensure t
  :bind
  ("C-x b" . consult-buffer)
  ("C-c s" . consult-ripgrep)
  ("C-c M-x" . consult-mode-command)
  ("C-c h" . consult-history)
  ("C-c k" . consult-kmacro)
  ("C-c i" . consult-info)
  ("C-s" . consult-line-literal)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format))

(use-package ibuffer
  :ensure t)

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
;;  (diminish 'projectile-mode)
  (diminish 'which-key-mode))

;; themes

(use-package modus-themes
  :ensure nil
  :config
  (load-theme 'modus-vivendi))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; custom keybindings
(global-set-key (kbd "C-x C-n") nil)
(global-set-key (kbd "C-o") nil)

;; split and go to buffer
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c r") 'rename-buffer)
(global-set-key (kbd "C-c m m") 'modus-themes-toggle)
(global-set-key (kbd "C-x C-n") 'find-file)
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

;; elixir

(use-package ruby-end
  :ensure t)

;; vterm
(use-package vterm
  :ensure t
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t)
  (global-set-key (kbd "C-c t") 'vterm-copy-mode))

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

;; https://emacs.stackexchange.com/questions/39210/copy-paste-from-windows-clipboard-in-wsl-terminal
;; wsl-copy
(defun wsl-copy (start end)
  (interactive "r")
  (shell-command-on-region start end "clip.exe")
  (deactivate-mark))

(global-set-key (kbd "C-c y") #'wsl-copy)
(global-set-key (kbd "C-t") nil)

(use-package eglot
 :ensure nil
 :config (add-to-list 'eglot-server-programs '(elixir-ts-mode "language_server.sh")))

(use-package elixir-ts-mode
 :hook (elixir-ts-mode . eglot-ensure)
 (elixir-ts-mode
  .
  (lambda ()
    (push '(">=" . ?\u2265) prettify-symbols-alist)
    (push '("<=" . ?\u2264) prettify-symbols-alist)
    (push '("!=" . ?\u2260) prettify-symbols-alist)
    (push '("==" . ?\u2A75) prettify-symbols-alist)
    (push '("=~" . ?\u2245) prettify-symbols-alist)
    (push '("<-" . ?\u2190) prettify-symbols-alist)
    (push '("->" . ?\u2192) prettify-symbols-alist)
    (push '("<-" . ?\u2190) prettify-symbols-alist)
    (push '("|>" . ?\u25B7) prettify-symbols-alist)))
 (before-save . eglot-format))

(add-to-list 'elixir-ts-mode-hook
             (defun auto-activate-ruby-end-mode-for-elixir-mode ()
               (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
               (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
               (ruby-end-mode +1)))

(defvar my/home (getenv "HOME"))

(use-package mix
  :ensure t
  :config
  (setq mix-path-to-bin (concat my/home "/.asdf/shims/mix"))
  (setq compilation-scroll-output t)
  (add-hook 'elixir-ts-mode-hook 'mix-minor-mode))

;; mode grammars

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
     (docker "https://github.com/camdencheek/tree-sitter-dockerfile")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; rerun on list update
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (elixir-mode . elixir-ts-mode)
   (python-mode . python-ts-mode)))

;; colemak c-t to c-x
(define-key key-translation-map [?\C-t] [?\C-x])

;; commonlisp setup
(add-to-list 'load-path "~/workspace/slime")
(require 'slime-autoloads)
(setq inferior-lisp-program "sbcl")

(setq-default cursor-type 'box)
(pixel-scroll-precision-mode)

(setenv "PAGER" "cat")

(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(electric-pair-mode +1)
(global-auto-revert-mode -1)
(global-hl-line-mode +1)
(ws-butler-global-mode +1)
(exec-path-from-shell-initialize)
(display-time-mode +1)

(provide 'init)
;;; init.el ends here
