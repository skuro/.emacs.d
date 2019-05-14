;;; cs-web.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Carlo Sciolla

;; Author: Carlo Sciolla <carlo.sciolla@gmail.com>
;; Keywords: javascript, html, css, web

;;; Commentary:

;; This file configures web-related technologies

;;; Code:

;;; Javascript
(use-package web-mode                   ; Major mode for editing web templates
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.php\\'"   . web-mode)
         ("\\.tpl\\'"   . web-mode))
  :config
  ;; Better JSX syntax-highlighting in web-mode
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it)))

(use-package rjsx-mode                  ; Powerful JavaScript mode
  :ensure t
  :mode (("\\.js\\'" . rjsx-mode)
         ("\\.jsx\\'" . rjsx-mode))
  :hook (rjxs-mode . js2-imenu-extras-mode)
  :config
  (validate-setq
   ;; Disable parser errors and strict warnings
   js2-mode-show-parse-errors nil
   js2-mode-show-strict-warnings nil
   js-indent-level 2)

  ;; Try to highlight most ECMA built-ins
  (validate-setq js2-highlight-level 3))

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)
         ("\\.json.template\\'" . json-mode))
  :hook ((json-mode . enable-paredit-mode)
         (json-mode . hs-minor-mode))
  :config
  (bind-key "{" #'paredit-open-curly json-mode-map)
  (bind-key "}" #'paredit-close-curly json-mode-map))

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

(use-package tide
  :ensure t
  :bind (("C-c C-d C-d" . tide-documentation-at-point))
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (rjsx-mode . tide-setup)
         (before-save . tide-format-before-save)))

(provide '04-web)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; cs-web.el ends here
