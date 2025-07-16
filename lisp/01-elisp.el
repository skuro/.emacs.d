;;; cs-elisp.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Carlo Sciolla

;; Author: Carlo Sciolla <carlo.sciolla@gmail.com>
;; Keywords: emacs, lisp, elisp

;;; Commentary:

;; This file configures my elisp editing

;;; Code:

;;; Elisp

(require 'validate)
(require 'comint)

(use-package ielm                       ; Emacs Lisp REPL
  :bind ("C-c d i" . ielm)
  :config (bind-key "C-c C-q" #'comint-send-eof inferior-emacs-lisp-mode-map))

(use-package elisp-mode                 ; Emacs Lisp editing
  :interpreter ("emacs" . emacs-lisp-mode)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-k" . eval-buffer)
              ("C-c C-c" . eval-last-sexp)
              ("C-c m e b" . eval-buffer)
              ("C-c m e f" . eval-defun)
              ("C-c m e e" . eval-last-sexp)
              ("C-c m e r" . eval-region))
  :config
  (defconst skuro/use-package-imenu-expression
    `("Use Package" ,(rx "(use-package" (optional "-with-elapsed-timer")
                         symbol-end (1+ (syntax whitespace)) symbol-start
                         (group-n 1 (1+ (or (syntax word) (syntax symbol))))
                         symbol-end) 1)
    "IMenu expression for `use-package' declarations.")

  (defun skuro/add-use-package-to-imenu ()
    "Add `use-package' declarations to `imenu'."
    (add-to-list 'imenu-generic-expression skuro/use-package-imenu-expression))

  (add-hook 'emacs-lisp-mode-hook #'skuro/add-use-package-to-imenu))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-miniuffer-setup-hook #'paredit-mode))

;; Use `emacs-lisp-mode' instead of `lisp-interaction-mode' for scratch buffer
(validate-setq initial-major-mode 'emacs-lisp-mode)

(use-package dash
  :ensure t
  :config
  (global-dash-fontify-mode))

(provide '01-elisp)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 01-elisp.el ends here
