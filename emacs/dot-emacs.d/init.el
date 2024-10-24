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

;;;; emacs config

(defalias 'yes-or-no-p 'y-or-n-p)

(set-face-attribute 'default nil
                    :family "Iosevka Comfy"
                    ;;:family "JetBrains Mono"
                    :weight 'regular
                    :height 110)

(setq-default line-spacing 2)
(setq-default indent-tabs-mode nil)
(load-theme 'ef-maris-dark t)

;; editor config

(setq
 custom-safe-themes t
 kill-whole-line t
 case-fold-search nil
 ring-bell-function 'ignore
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 case-fold-search  nil
 initial-major-mode 'org-mode
 display-time-default-load-average nil
 display-time-format "%H:%M:%S %a,%d %b %Y"
 column-number-mode t
 warning-minimum-level :emergency tab-always-indent 'complete
 custom-file "~/.emacs.d/custom.el")

(fringe-mode 1)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; enabled modes and other vars
;;(require 'slime-autoloads)

(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(setq-default cursor-type 'box)
(setq-default fill-column 120)
(pixel-scroll-precision-mode)
(setenv "PAGER" "cat")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(electric-pair-mode +1)
(global-auto-revert-mode -1)
(global-hl-line-mode +1)
(fringe-mode +1)
(display-time-mode 0)

;;;; hooks

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;; keybindings

(global-set-key (kbd "C-x C-n") nil)
(global-set-key (kbd "C-o") nil)
(global-set-key "\C-x2"
                (lambda ()
                  (interactive)
                  (split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda ()
                          (interactive)
                          (split-window-horizontally)
                          (other-window 1)))
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c r") 'rename-buffer)
(global-set-key (kbd "C-c m m") 'modus-themes-toggle)
(global-set-key (kbd "C-x C-n") 'find-file)
(global-set-key (kbd "C-c n n") 'denote)
(define-key global-map [remap list-buffers] 'ibuffer)
(define-key key-translation-map [?\C-t] [?\C-x])


;;; QoL packages

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

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

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

(use-package ripgrep
  :ensure t
  :defer)

(use-package vertico
  :init
  (vertico-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode))

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
  (diminish 'flycheck-mode)
  (diminish 'eldoc-mode)
  (diminish 'which-key-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "M-g e") 'avy-goto-word-0)
  (global-set-key (kbd "C-c C-j") 'avy-resume)
  (global-set-key (kbd "C-:") 'avy-goto-char))


;;; Programming dev and modes

(use-package flycheck
  :ensure t
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'cfn-lint          'cfn-yaml-mode)
  :hook
  (after-init . global-flycheck-mode))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq transient-default-level 7)
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
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package js2-mode
  :ensure t)

(use-package emmet-mode
  :ensure t)

;; file ext mode support
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.heex?\\'" . heex-ts-mode))
(add-to-list 'auto-mode-alist '("\\.leex?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\dot-zshrc?\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\Dockerfile?\\'" . dockerfile-mode))

(setq auto-mode-alist
      (append
       '(("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.css?\\'" . web-mode)
         ("\\.scss?\\'" . web-mode)
         ("\\.erb?\\'" . web-mode)
         ("\\.heex\\'" . web-mode)
         ("\\dot-zshrc?\\'" . sh-mode)
         ("\\.go\\'" . go-ts-mode)
         ("\\Dockerfile?\\'" . dockerfile-ts-mode))
       auto-mode-alist))

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

(use-package elixir-ts-mode
  :ensure t)

(use-package ruby-ts-mode
  :ensure t
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :bind (:map ruby-ts-mode-map
              ("C-c r b" . 'treesit-beginning-of-defun)
              ("C-c r e" . 'treesit-end-of-defun))
  :hook
  (ruby-ts-mode . ruby-end-mode))

(add-to-list 'elixir-ts-mode-hook
             (defun auto-activate-ruby-end-mode-for-elixir-mode ()
               (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
               (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
               (ruby-end-mode +1)))

(use-package terraform-mode
  :ensure
;; language server

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-enable-snippet nil)
  (setq lsp-keymap-prefix "C-c C-l")
  (setq lsp-completion-provider :none)
  (defun corfu-lsp-setup ()
    (setq-local completion-styles '(orderless)
                completion-category-defaults nil))
  (add-hook 'lsp-mode-hook #'corfu-lsp-setup)
  :hook
  (ruby-ts-mode . lsp-deferred)
  (elixir-ts-mode . lsp-deferred)
  (terraform-mode . lsp-deferred)
  :commands
  lsp)

(use-package dockerfile-mode
  :ensure t)

;;;; note taking

(require 'org-tempo)

(use-package ob-elixir
  :ensure t)

(use-package org
  :ensure t
  :config
  (setq org-startup-indented t)
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
  (global-set-key (kbd "C-x C-g") 'org-todo)


  (setq org-todo-keywords '((sequence "TODO" "DOING" "WAITING" "DONE")))
  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t)
                               (ruby . t)
                               (python . t)
                               (elixir . t)
                               (js . t))))

(use-package yaml-pro
  :ensure t)

(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir "~/org/journal"))

(use-package org-bullets
  :ensure t)

(use-package denote
  :ensure t
  :config
  (setq denote-directory (expand-file-name "~/.notes/"))
  (setq denote-sort-keywords t)
  (setq denote-known-keywords '("work" "personal" "finance" "urgent"))
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories))


;;; Windows
;;; https://emacs.stackexchange.com/questions/39210/copy-paste-from-windows-clipboard-in-wsl-terminal

(defun wsl-copy (start end)
  (interactive "r")
  (shell-command-on-region start end "clip.exe")
  (deactivate-mark))

(global-set-key (kbd "C-c y") #'wsl-copy)
(global-set-key (kbd "C-SPC") #'set-mark-command)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; tressitter mode grammars

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
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
     (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
     (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

;; uncomment and eval block below on treesitter list update
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(setq major-mode-remap-alist
 '((bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (elixir-mode . elixir-ts-mode)
   (dockerfile-mode . dockerfile-ts-mode)
   (ruby-mode . ruby-ts-mode)
   (python-mode . python-ts-mode)))


;; rice

(use-package company
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.3)
  (global-company-mode t))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons
  :if (display-graphic-p))

;; -nw configs and packages

(global-unset-key (kbd "C-<down-mouse-1>"))

(unless (display-graphic-p)
  ;; corfu setup
  (unless (package-installed-p 'quelpa)
    (with-temp-buffer
      (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
      (eval-buffer)
      (quelpa-self-upgrade)))

  (quelpa '(corfu-terminal
            :fetcher git
            :url "https://codeberg.org/akib/emacs-corfu-terminal.git"))

  (corfu-terminal-mode 1)

  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; devops

;; cfn-lint
;; Set up a mode for JSON based templates
(use-package yaml-mode
  :ensure t)

(use-package cfn-mode
  :ensure t)

(define-derived-mode cfn-json-mode js-mode
    "CFN-JSON"
    "Simple mode to edit CloudFormation template in JSON format."
    (setq js-indent-level 2))

(add-to-list 'magic-mode-alist
             '("\\({\n *\\)? *[\"']AWSTemplateFormatVersion" . cfn-json-mode))

;; Set up a mode for YAML based templates if yaml-mode is installed
;; Get yaml-mode here https://github.com/yoshiki/yaml-mode
(when (featurep 'yaml-mode)

  (define-derived-mode cfn-yaml-mode yaml-mode
    "CFN-YAML"
    "Simple mode to edit CloudFormation template in YAML format.")

  (add-to-list 'magic-mode-alist
               '("\\(---\n\\)?AWSTemplateFormatVersion:" . cfn-yaml-mode)))

;; Set up cfn-lint integration if flycheck is installed
;; Get flycheck here https://www.flycheck.org/
(when (featurep 'flycheck)
  (flycheck-define-checker cfn-lint
    "AWS CloudFormation linter using cfn-lint.

Install cfn-lint first: pip install cfn-lint

See `https://github.com/aws-cloudformation/cfn-python-lint'."

    :command ("~/.local/bin/cfn-lint" "-f" "parseable" source)
    :error-patterns ((warning line-start (file-name) ":" line ":" column
                              ":" (one-or-more digit) ":" (one-or-more digit) ":"
                              (id "W" (one-or-more digit)) ":" (message) line-end)
                     (error line-start (file-name) ":" line ":" column
                            ":" (one-or-more digit) ":" (one-or-more digit) ":"
                            (id "E" (one-or-more digit)) ":" (message) line-end))
    :modes (cfn-json-mode cfn-yaml-mode))

  (add-to-list 'flycheck-checkers 'cfn-lint)
  (add-hook 'cfn-json-mode-hook 'flycheck-mode)
  (add-hook 'cfn-yaml-mode-hook 'flycheck-mode))

;; (setq mac-option-key-is-meta nil
;;       mac-command-key-is-meta t
;;       mac-command-modifier 'meta
;;       mac-option-modifier 'none)
(put 'magit-clean 'disabled nil)

(provide 'init)
;;; init.el ends here
