;;; 03-java.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Carlo Sciolla

;; Author: Carlo Sciolla <carlo.sciolla@gmail.com>
;; Keywords: clojure

;;; Commentary:

;; This file configures LSP services

;;; Code:

;;; Env stuff

(use-package lsp-java                   ; LSP for Java
  :ensure t
  :after  lsp-mode
  :config
  (add-hook 'java-mode-hook 'lsp))

(provide '03-java)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 03-java.el ends here
