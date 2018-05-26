;;; cs-style.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Carlo Sciolla

;; Author: Carlo Sciolla <carlo.sciolla@gmail.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my look'n'feel configuration.

;;; Code:

;;; Theme
(validate-setq custom-safe-themes nil)    ; Treat themes as safe

;; I really love zenburn
(use-package zenburn-theme
             :ensure t
             :config
             (load-theme 'zenburn t))

;;; Interface
(use-package frame                      ; Frames
             :bind ("C-c w f" . toggle-frame-fullscreen)
             :init
             ;; Kill `suspend-frame'
             (unbind-key "C-x C-z")
             :config (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(validate-setq echo-keystrokes 0.1)     ; Faster echo keystrokes

;; Avoid showing ?? in the mode line when we have long lines.
(validate-setq line-number-display-limit-width 10000)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ; disable the toolbar

;; Do not pop up *Messages* when clicking on the minibuffer
(bind-key [mouse-1] #'ignore minibuffer-inactive-mode-map)

;; Turn off annoying settings
(blink-cursor-mode -1)
(tooltip-mode -1)

;; Cursor stretches to the current glyph's width
(validate-setq x-stretch-cursor t)

;; Disable annoying prompts
(fset 'yes-or-no-p 'y-or-n-p)
(validate-setq kill-buffer-query-functions
               (remq 'process-kill-buffer-query-function
                     kill-buffer-query-functions))

;; Disable startup messages
(validate-setq
 ring-bell-function #'ignore
 inhibit-startup-screen t
 initial-scratch-message nil)

;; Disable startup echo area message
(fset 'display-startup-echo-area-message #'ignore)

(validate-setq history-length 1000)           ; Store more history
(setq-default line-spacing 0.2)         ; Increase line-spacing (default 0)

;; Configure a reasonable fill column and enable automatic filling
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)
(diminish 'auto-fill-function)

;; Hide the cursor in inactive windows
(setq-default cursor-in-non-selected-windows nil)

(use-package iso-transl                 ; Fix dead characters
             :demand t)

(use-package page-break-lines           ; Better looking break lines
             :ensure t
             :defer t
             :init (global-page-break-lines-mode))

(use-package prog-mode
             ;; Prettify symbols
             :config
             (global-prettify-symbols-mode 1)

             ;; Unprettify symbols with point on them and next to them
             (validate-setq prettify-symbols-unprettify-at-point 'right-edge))

(use-package ansi-color                 ; Colorize ANSI escape sequences
             :defer t
             :config
             (defun skuro/colorize-compilation ()
               "Colourize from `compilation-filter-start' to `point-max'."
               (let ((inhibit-read-only t))
                 (ansi-color-apply-on-region compilation-filter-start (point-max))))

             (add-hook 'compilation-filter-hook #'skuro/colorize-compilation))

;; Underline below the font bottomline instead of the baseline
(validate-setq x-underline-at-descent-line t)

;;; The mode line
(line-number-mode)
(column-number-mode)

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position
                (vc-mode vc-mode)
                (multiple-cursors-mode mc/mode-line)
                " " mode-line-modes
                mode-line-end-spaces))

(defmacro skuro/rename-modeline (package-name mode new-name)
  "Rename PACKAGE-NAME with MODE into NEW-NAME in the mode line."
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(skuro/rename-modeline "js2-mode" js2-mode "JS2")

(use-package minions                    ; A minor-mode menu for the mode line
             :ensure t
             :init (minions-mode)
             :config (validate-setq minions-direct '(cider-mode
                                                     flycheck-mode
                                                     overwrite-mode)))

(use-package moody                      ; Tabs and ribbons for the mode line
             :ensure t
             :config
             (moody-replace-mode-line-buffer-identification)
             (moody-replace-vc-mode))

(use-package paren                      ; Highlight paired delimiters
  :init (show-paren-mode))

(use-package diff-hl                    ; Show changes in fringe
  :ensure t
  :hook ((prog-mode          . diff-hl-mode)
         (dired-mode         . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package symbol-overlay             ; Highlight symbols
  :ensure t
  :bind (:map symbol-overlay-mode-map
              ("M-h" . symbol-overlay-put)
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev))
  :hook ((prog-mode . symbol-overlay-mode)
         (html-mode . symbol-overlay-mode)
         (css-mode  . symbol-overlay-mode)
         (yaml-mode . symbol-overlay-mode)
         (conf-mode . symbol-overlay-mode)))

(use-package hl-todo                    ; Highlight TODO and similar keywords
  :ensure t
  :hook (prog-mode . hl-todo-mode))

(use-package highlight-numbers          ; Fontify number literals
  :ensure t
  :hook (prog-mode . highlight-numbers-mode))

(use-package rainbow-mode               ; Highlight colors
  :ensure t
  :bind ("C-c t R" . rainbow-mode)
  :hook (css-mode . rainbow-mode))

(use-package rainbow-delimiters         ; Highlight parens
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package hi-lock                    ; Custom regexp highlights
  :init (global-hi-lock-mode))

(use-package fontify-face               ; Fontify symbols with that their face
  :ensure t
  :commands fontify-face-mode)

(provide '02-style)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; cs-style.el ends here
