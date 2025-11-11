;;; 05-ai.el --- AI related tools and libraries      -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Carlo Sciolla

;; Author: Carlo Sciolla <skuro@arraxu.local>
;; Keywords: tools

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

;;

;;; Code:

(require 'validate)

(use-package gptel
  :ensure t
  :defer t
  :pin melpa-unstable
  :config
  (require 'gptel-integrations)
  (validate-setq gptel-backend
                 (gptel-make-anthropic "Claude" ;Any name you want
                   :stream t                    ;Streaming responses
                   :key (lambda ()
                          (gptel-api-key-from-auth-source "api.anthropic.com" "apiKey")))))

(use-package mcp
  :ensure t
  :after gptel
  :custom (mcp-hub-servers
           `(("cockpit" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "/Users/skuro/Development/HonestWOZ/cockpit")))
             ("puml" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "/Users/skuro/Development/Sandbox/plantuml-mode")))
             ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))))
  :config (require 'mcp-hub)
  :hook (after-init . mcp-hub-start-all-server))

(use-package eca                               ; Emacs Code Assistant, by Eric Dallo
  :ensure t
  :pin melpa-unstable)

(use-package monet                             ; Claude Code IDE API implementation
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  ;; use vterm instead of eat
  (validate-setq claude-code-terminal-backend 'eat)

  ;; IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions
            #'monet-start-server-function)

  (monet-mode 1)

  (claude-code-mode)
  :bind-keymap ("C-c a" . claude-code-command-map)

  :bind
  ;; Make "M" cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

(provide '05-ai)
;;; 05-ai.el ends here
