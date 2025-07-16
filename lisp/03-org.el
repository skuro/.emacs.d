;;; 03-org.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Carlo Sciolla

;; Author: Carlo Sciolla <carlo.sciolla@gmail.com>
;; Keywords: convenience

;;; Commentary:

;; Org mode configuration.

;;; Code:

(require 'plantuml-mode)
(require 'validate)

(use-package org                        ; The almighty Org
  :ensure t
  :bind (("C-c o l" . org-store-link)
         ("C-c w" . org-refile))
  :config
  (validate-setq
   org-src-fontify-natively t
   org-log-done 'time
   org-hide-emphasis-markers t
   org-return-follows-link t
   org-directory (expand-file-name "~/Dropbox/org")
   org-default-notes-file (expand-file-name "notes.org" org-directory)
   org-startup-with-inline-images t)

  ;; Use Org tables in message mode
  (add-hook 'message-mode-hook #'turn-on-orgtbl)

  ;; Define TODO workflow states
  (validate-setq
   org-todo-keywords '("TODO(t)" "PROCESSING(p)" "|" "CANCELLED(c)" "DONE(d)"))

  ;; Define Agenda files for GTD
  (validate-setq org-agenda-files (list (expand-file-name "~/Dropbox/org")))

  ;; Archive in same file
  (validate-setq org-archive-location "::* Archive")

  ;; Evaluate code blocks
  (validate-setq org-babel-load-languages
                 '((emacs-lisp . t)
                   (clojure . t)
                   (shell . t)
                   (python . t)))

  (defun skuro/org-ispell ()
    "Configure `ispell-skip-region-alist' for `org-mode'."
    (make-local-variable 'ispell-skip-region-alist)
    (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
    (add-to-list 'ispell-skip-region-alist '("~" "~"))
    (add-to-list 'ispell-skip-region-alist '("=" "="))
    (add-to-list 'ispell-skip-region-alist
                 '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))

  (add-hook 'org-mode-hook #'skuro/org-ispell)

  (defun skuro/update-clocktable-at-clock-out ()
    "Automatically update the previous `clocktable' block at clock-out."
    ;; note: when running as an `org-clock-out-hook' function, `point' is at the current clock-out headline
    (save-excursion
      (let ((clocktable (re-search-backward "BEGIN:.*clocktable" nil t)))
        (goto-char clocktable)
        (org-ctrl-c-ctrl-c))))

  (add-hook 'org-clock-out-hook #'skuro/update-clocktable-at-clock-out)

  (unbind-key "C-'" org-mode-map)        ; Free C-' (see: 00-editing.el)
  (unbind-key "S-<return>" org-mode-map) ; Free S-RET (see: 00-editing.el)

  ;; load org-babel backends
  (require 'ob-python)
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

  (validate-setq org-plantuml-jar-path plantuml-jar-path))

(use-package org-agenda                 ; Dynamic task and appointment lists
  :after org
  :bind (("C-c o a" . org-agenda)
         ("C-c o l" . org-agenda-list)
         ("C-c o s" . org-search-view)
         ("C-c o t" . org-todo-list))
  :config
  (validate-setq org-agenda-restore-windows-after-quit t))

(use-package org-clock
  :after org
  :config
  ;; Play sound when timer ends
  (when (file-exists-p "/System/Library/Sounds/Bottle.aiff")
    (validate-setq org-clock-sound "/System/Library/Sounds/Bottle.aiff"))

  (add-hook 'org-clock-out-hook
            #'(lambda ()
                (setq org-mode-line-string nil)
                (force-mode-line-update)))
  (validate-setq org-duration-format '(("h" . t) ("min" . t))))

(use-package org-capture                ; Fast note taking
  :after org
  :bind ("C-c c" . org-capture)
  :config
  (setq
   org-capture-templates `(("i" "GTD inbox" entry (file+headline ,(expand-file-name "~/Dropbox/org/notes.org") "Inbox")
                            "* TODO %^{Headline} %^g\n  Added: %U\n %i %?")
                           ("n" "GTD next action" entry (file+headline ,(expand-file-name "~/Dropbox/org/notes.org") "Next")
                            "* TODO %^{Headline} %^g\n  Added: %U\n %i %?")))
  (setq org-agenda-custom-commands
        '(
          ;; Only show TODOs with priority A
          ("H" "Daily review of current items, GTD style"
           ((agenda "" ((org-agenda-ndays 1)))
            (tags-todo "+PRIORITY=\"A\"")
            (todo "TODO")))

          ;; Show PROJECT | MAYBE | BLOCKED entries
          ("W" "Weekly review of pending items, GTD style"
           ((agenda "" ((org-agenda-ndays 14)))
            (todo "PROJECT")
            (todo "MAYBE")
            (todo "BLOCKED"))))))

(use-package org-faces                  ; Faces definitions
  :after org
  :config
  ;; Force title font size to override theme setting
  (set-face-attribute 'org-document-title nil :height 1.0))

(use-package ox
  :ensure org
  :config (validate-setq org-export-with-smart-quotes t))

(use-package ox-html
  :ensure org
  :config
  (validate-setq
   ;; Turn off preamble and postamble in HTML export
   org-html-preamble nil
   org-html-postamble nil))

(use-package org-indent                 ; Dynamic indentation for Org-mode
  :ensure org
  :bind ("C-c t o" . org-indent-mode)
  :init (add-hook 'org-mode-hook #'org-indent-mode))

(use-package org-bullets                ; Bullets as UTF-8 characters
  :ensure t
  :init (add-hook 'org-mode-hook #'org-bullets-mode)
  :config (validate-setq org-bullets-bullet-list '("◉" "○" "●" "►" "◇" "◎")))

(use-package org-cliplink               ; Insert links from the clipboard
  :ensure t
  :bind ("C-c o i" . org-cliplink))

(use-package deft                       ; Search through collections of text files
  :ensure t
  :custom
  (deft-extensions '("org" "md" "txt"))
  (deft-directory "~/Dropbox/zettelkast")
  (deft-use-filename-as-title t))

(use-package zetteldeft                 ; A Zettelkast implementation on top of `deft'
  :ensure t
  :pin "melpa-unstable"
  :after deft
  :config (zetteldeft-set-classic-keybindings))

(provide '03-org)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 03-org.el ends here
