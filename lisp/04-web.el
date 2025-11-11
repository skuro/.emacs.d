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
         ("\\.tpl\\'"   . web-mode))
  :config
  ;; Better JSX syntax-highlighting in web-mode
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it)))

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)
         ("\\.json.template\\'" . json-mode))
  :hook ((json-mode . enable-paredit-mode)
         (json-mode . hs-minor-mode))
  :bind (("C-c C-c" . json-pretty-print))
  :config
  (bind-key "{" #'paredit-open-curly json-mode-map)
  (bind-key "}" #'paredit-close-curly json-mode-map))

(use-package tide
  :ensure t
  :bind (("C-c C-d C-d" . tide-documentation-at-point))
  :after (typescript-mode typescript-ts-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-ts-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (typescript-ts-mode . tide-hl-identifier-mode)
         (rjsx-mode . tide-setup)
         (before-save . tide-format-before-save)))

(use-package php-ts-mode
  :mode (("\\.php" . php-ts-mode)))

(use-package typescript-ts-mode
  :mode (("\\.mtx" . typescript-ts-mode)
         ("\\.tsx" . typescript-ts-mode)))

;;
;; Astro
;;
(define-derived-mode astro-mode web-mode "astro")
(setq auto-mode-alist
      (append '((".*\\.astro\\'" . astro-mode))
              auto-mode-alist))

(provide '04-web)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 04-web.el ends here
