;;; 03-haskell.el --- Part of my Emacs startup configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Carlo Sciolla

;; Author: Carlo Sciolla <carlo.sciolla@gmail.com>
;; Keywords: convenience

;;; Commentary:

;; Haskellconfiguration

;;; Code:

;; Autocomplete for cabal
(use-package company-cabal
  :defer t)

;; Autocomplete through ghci
(use-package company-ghci
  :defer t)

;; Autocomplete through ghci
(use-package company-ghc
  :defer t)

(use-package ghc
  :defer t)

(use-package haskell-mode
  :defer t
  :init
  (setq
   ;; Remove annoying error popups
   haskell-interactive-popup-errors nil
   ;; Better import handling
   haskell-process-suggest-remove-import-lines t
   haskell-process-auto-import-loaded-modules t))

(provide '03-haskell)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 03-haskell.el ends here
