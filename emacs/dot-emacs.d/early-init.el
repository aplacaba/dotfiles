;;; early-init.el --- Summary
;;; Commentary:
;;; Emacs early init setup
;;; Code:
(setopt package-enable-at-startup t)

(defvar package-quickstart)

(setopt frame-inhibit-implied-resize t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setopt
 inhibit-splash-screen t
 use-dialog-box t
 use-file-dialog nil
 inhibit-startup-screen t
 gc-cons-threshold most-positive-fixnum
 read-process-output-max (* 1024 1024))

(defun restore-gc-cons-threshold ()
  (setopt gc-cons-threshold (* 16 1024 1024)
    gc-cons-percentage 0.1))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook #'restore-gc-cons-threshold 105)

;;; early-init.el ends here
