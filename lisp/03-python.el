;;; 03-python.el --- Part of my Emacs startup configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Carlo Sciolla

;; Author: Carlo Sciolla <skuro@margiani>
;; Keywords: convenience

;;; Commentary:

;; Python configuration

;;; Code:

(use-package pyvenv
  :ensure t)

(setq-default python-shell-interpreter "python3")

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(provide '03-python)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 03-python.el ends here
