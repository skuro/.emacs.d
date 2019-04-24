;;; 03-org.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Carlo Sciolla

;; Author: Carlo Sciolla <carlo.sciolla@gmail.com>
;; Keywords: convenience

;;; Commentary:

;; Org mode configuration.

;;; Code:

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
   org-duration-format ((special . h:mm))
   org-default-notes-file (expand-file-name "notes.org" org-directory)
   org-startup-with-inline-images t)

  ;; Use Org structures and tables in message mode
  (add-hook 'message-mode-hook #'turn-on-orgtbl)
  (add-hook 'message-mode-hook #'turn-on-orgstruct++)

  ;; Define TODO workflow states
  (validate-setq
   org-todo-keywords '("TODO(t)" "WAITING(w)" "|" "CANCELLED(c)" "DONE(d)"))

  ;; Define Agenda files for GTD
  (validate-setq org-agenda-files '("/Users/skuro/Dropbox/org"
                                    "/Users/skuro/Dropbox/org/synple"))

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

  (unbind-key "C-'" org-mode-map)       ; Free C-' (see: 00-editing.el)
  (unbind-key "S-<return>" org-mode-map) ; Free S-RET (see: 00-editing.el)

  ;; load org-babel backends
  (require 'ob-python))

(use-package org-agenda                 ; Dynamic task and appointment lists
  :after org
  :bind (("C-c o a" . org-agenda)
         ("C-c o l" . org-agenda-list)
         ("C-c o s" . org-search-view)
         ("C-c o t" . org-todo-list))
  :config
  (validate-setq org-agenda-restore-windows-after-quit t))

(use-package org-capture                ; Fast note taking
  :after org
  :bind ("C-c c" . org-capture)
  :config
  (setq
   org-capture-templates '(("i" "GTD inbox" entry (file+headline "/Users/skuro/Dropbox/org/notes.org" "Inbox")
                            "* TODO %^{Headline} %^g\n  Added: %U\n %i %?")
                           ("n" "GTD next action" entry (file+headline "/Users/skuro/Dropbox/org/notes.org" "Next")
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

(use-package org-indent ; Dynamic indentation for Org-mode
  :ensure org
  :bind ("C-c t o" . org-indent-mode)
  :init (add-hook 'org-mode-hook #'org-indent-mode))

(use-package org-bullets                ; Bullets as UTF-8 characters
  :ensure t
  :init (add-hook 'org-mode-hook #'org-bullets-mode)
  :config (validate-setq org-bullets-bullet-list '("◉" "○" "●" "►" "◇" "◎")))

(use-package org-pdfview                ; Link to PDF files
  :ensure t
  :after org)

(use-package org-cliplink               ; Insert links from the clipboard
  :ensure t
  :bind ("C-c o i" . org-cliplink))

(provide '03-org)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 03-org.el ends here
