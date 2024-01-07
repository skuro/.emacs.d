;;; 02-completion.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018 Carlo Sciolla

;; Author: Carlo Sciolla <carlo.sciolla@gmail.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for autocompletion tools.

;;; Code:

(use-package pcomplete                  ; Programmable completion
  :config
  (defvar pcomplete-man-user-commands
    (split-string
     (shell-command-to-string
      "apropos -s 1 .|while read -r a b; do echo \" $a\";done;"))
    "p-completion candidates for `man' command")

  (defun pcomplete/man ()
    "Completion rules for the `man' command."
    (pcomplete-here pcomplete-man-user-commands)))

(use-package pcmpl-git                  ; pcomplete for git
  :ensure t
  :after pcomplete)

(use-package autoinsert                 ; Auto insert custom text
  :init
  (auto-insert-mode)
  (define-auto-insert
    '("\\.org\\'" . "Org files skeleton")
    '("Startup: "
      "#+startup: showall\n"
      > _ \n \n))
  (define-auto-insert '("/_posts/.*\\.md\\'" . "Blog posts skeleton")
    '("Header: "
      "---\n"
      "layout:     post\n"
      "title:\n"
      "date:       " (format-time-string "%Y-%m-%d") "\n"
      "summary:\n"
      "categories:\n"
      "---\n"
      > _ \n \n))
  :config (validate-setq auto-insert-query nil))

(use-package yasnippet                  ; Snippets
  :ensure t
  :config
  (validate-setq
   yas-verbosity 1                      ; No need to be so verbose
   yas-wrap-around-region t)

  (with-eval-after-load 'yasnippet
    (validate-setq yas-snippet-dirs '(yasnippet-snippets-dir)))

  (yas-reload-all)
  (yas-global-mode))

(use-package yasnippet-snippets         ; Collection of snippets
  :ensure t)

(use-package abbrev                     ; Save abbreviations
  :init (abbrev-mode)
  :config (validate-setq save-abbrevs t))

;; In `completion-at-point', do not pop up completion buffers for less
;; than five candidates. Cycle instead.
(validate-setq completion-cycle-threshold 5)

(use-package hippie-exp                 ; Powerful expansion and completion
  :bind ("C-c /" . hippie-expand)
  :config
  (validate-setq
   hippie-expand-try-functions-list
   '(try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     try-expand-list
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol)))

(add-to-list 'completion-styles 'initials t)

(use-package company                    ; Auto-completion
  :ensure t
  :init (global-company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("M-/" . company-other-backend))
  :config
  (validate-setq
   company-tooltip-align-annotations t
   company-tooltip-flip-when-above t
   ;; Easy navigation to candidates with M-<n>
   company-show-quick-access ''left)

  (setq-default company-tooltip-align-annotations t))

(use-package company-dabbrev            ; dabbrev-like Company backend
  :after company
  :config
  (validate-setq
   ;; Ignore case
   company-dabbrev-ignore-case t
   ;; Do not downcase completion
   company-dabbrev-downcase nil))

(use-package company-statistics         ; Show likelier candidates on top
  :ensure t
  :after company
  :config (company-statistics-mode))

(use-package company-quickhelp          ; Show help in tooltip
  :ensure t
  :after company
  :config (company-quickhelp-mode))

(use-package company-math               ; Backends for math symbols
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-math-symbols-latex))

(use-package company-web                ; Backend for web development
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-web-html))

(use-package company-shell              ; Company support for shell functions
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-shell)
  (add-to-list 'company-backends 'company-fish-shell)
  (add-to-list 'company-backends 'company-shell-env))

(use-package company-restclient         ; Company support for restclient
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-restclient))

(use-package company-ansible            ; Company support for ansible
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-ansible))

(provide '02-completion)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 02-completion.el ends here
