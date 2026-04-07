;;; 03-various.el --- Various programming languages support -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Carlo Sciolla

;; Author: Carlo Sciolla <skuro@arraxu.local>
;; Keywords:

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

(use-package uiua-ts-mode               ; a stack based array language, see https://uiua.org
  :mode "\\.ua\\'"
  :ensure t)

(use-package kubed                      ; useful mode to interact with Kubernetes
  :ensure t
  :config
  (keymap-global-set "M-s-k" 'kubed-prefix-map))

(use-package sly-asdf
  :ensure t
  :defer t)

(use-package sly-quicklisp
  :ensure t
  :defer t)

(use-package sly
  :ensure t
  :defer t
  :after (sly-asdf sly-quicklisp)
  :custom
  (inferior-lisp-program (executable-find "sbcl")))

(provide '03-various)
;;; 03-various.el ends here
