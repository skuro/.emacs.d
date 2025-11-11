;;; 02-lsp.el --- Part of my Emacs setup             -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Carlo Sciolla

;; Author: Carlo Sciolla <skuro@lepori>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file configures LSP services

;;; Code:

(require 'validate)

(use-package eglot
  :config
  (validate-setq eglot-connect-timeout 90)
  (defun skuro/eglot-format-buffer-before-save ()
    "Add a local hook for formatting the current buffer with eglot before saving."
    (add-hook 'before-save-hook #'eglot-format-buffer nil t))

  (add-to-list 'eglot-server-programs
               '(astro-mode . ("astro-ls" "--stdio"
                               :initializationOptions
                               (:typescript (:tsdk "./node_modules/typescript/lib"))))))

(provide '02-lsp)
;;; 02-lsp.el ends here
