;;; 03-net.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Carlo Sciolla

;; Author: Carlo Sciolla <carlo.sciolla@gmail.com>
;; Keywords: convenience

;;; Commentary:

;; Dealing with network services.

;;; Code:

(use-package tramp                      ; Remote editing
  :config
  ;; Without this change, tramp ends up sending hundreds of shell commands to
  ;; the remote side to ask what the temporary directory is.
  (put 'temporary-file-directory 'standard-value '("/tmp"))

  (validate-setq
   tramp-verbose 1                      ; Reduce verbosity
   tramp-default-method "ssh"
   tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*"
   auto-save-file-name-transforms nil)

  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil)))

(use-package paradox                    ; Better package manager interface
  :ensure t
  :bind (("C-c p p" . paradox-list-packages)
         ("C-c p P" . paradox-upgrade-packages))
  :config

  (validate-setq
   paradox-github-token t             ; Don't ask for a token
   paradox-execute-asynchronously nil ; No async updates
   paradox-use-homepage-buttons nil   ; Hide download button
   paradox-automatically-star nil     ; Don't star packages automatically
   paradox-display-download-count t   ; Show all possible counts
   paradox-display-star-count t)

  ;; Don't need paradox report
  (remove-hook 'paradox-after-execute-functions
               #'paradox--report-buffer-print)
  (remove-hook 'paradox-after-execute-functions
               #'paradox--report-buffer-display-if-noquery))

(defun skuro/open-with-chrome
    (url &optional _new-window)
  "Opens URL with Google Chrome."
  (shell-command (concat "open -a \"Google Chrome\" " url)))

(use-package browse-url                 ; Browse URLs
  :config
  (validate-setq
   browse-url-browser-function 'skuro/open-with-chrome))

(use-package goto-addr                  ; Make links clickable
  :defer t
  :bind (("C-c t a" . goto-address-mode)
         ("C-c t A" . goto-address-prog-mode))
  :hook ((prog-mode . goto-address-prog-mode)
         (text-mode . goto-address-mode)
         (org-mode  . (lambda () (goto-address-mode -1)))))

(provide '03-net)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 03-net.el ends here
