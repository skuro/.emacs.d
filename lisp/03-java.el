;;; 03-java.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Carlo Sciolla

;; Author: Carlo Sciolla <carlo.sciolla@gmail.com>
;; Keywords: clojure

;;; Commentary:

;; This file configures LSP services

;;; Code:

;;; Env stuff

(require 'no-littering)
(require 'eglot)

;; eglot configuration
(defun skuro/java-eglot-server-programs-concat (_interactive project)
  "Function to return the CONTACT for the java `eglot-server-programs'.
INTERACTIVE is t when calling `eglot' interactively.
PROJECT is whatever Eglot discovered via `project-find-functions'."
  (let* ((project-root (cdr project))
         (jdtls-bin (expand-file-name "jdtls" "~/.local/bin"))
         (config-path (expand-file-name "jdtls/config_mac" no-littering-var-directory))
         (data-path (expand-file-name ".lsp" project-root)))
    (message (concat "Starting " jdtls-bin " with config " config-path " and data stored in " data-path))
    (list jdtls-bin
          "-configuration" config-path
          "-data" data-path)))

(add-to-list 'eglot-server-programs
             `(java-mode . skuro/java-eglot-server-programs-concat))

(provide '03-java)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 03-java.el ends here
