;;; 03-java.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Carlo Sciolla

;; Author: Carlo Sciolla <carlo.sciolla@gmail.com>
;; Keywords: clojure

;;; Commentary:

;; This file configures my Java IDE

;;; Code:

;;; Env stuff

(use-package lsp-mode                   ; Language Server Protocol
  :ensure t)

(use-package company-lsp                ; Completion for LSP
  :ensure t)

(use-package lsp-ui                     ; LSP ui elements (sideline, menus, etc.)
  :ensure t)

(use-package lsp-java                   ; LSP for Java
  :ensure t
  :after  lsp
  :config
  (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(provide '03-java)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 03-java.el ends here
