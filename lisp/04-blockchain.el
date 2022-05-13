;;; 04-blockchain.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Carlo Sciolla

;; Author: Carlo Sciolla <carlo.sciolla@gmail.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for fiddling with blockchain stuff.

;;; Code:

(use-package solidity-mode
  :ensure t
  :defer t)

(use-package solidity-flycheck
  :ensure t
  :defer t)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 04-blockchain.el ends here
