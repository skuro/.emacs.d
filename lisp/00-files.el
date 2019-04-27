;;; 00-files.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Carlo Sciolla

;; Author: Carlo Sciolla <carlo.sciolla@gmail.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for navigating and managing files.

;;; Code:

(use-package files                      ; Core commands for files
  :bind (("<f5>" . revert-buffer)
         ("C-c f l" . find-file-literally)))

(use-package recentf                    ; Manage recent files
  :demand t
  :config
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")

  (validate-setq
   recentf-max-saved-items 200
   recentf-max-menu-items 15))

(validate-setq view-read-only t)                 ; View read-only
(validate-setq large-file-warning-threshold nil) ; No large file warning

(use-package ffap                       ; Find files at point
  :defer t
  ;; Do not ping random hosts
  :config (validate-setq ffap-machine-p-known 'reject))

(use-package ignoramus                  ; Ignore uninteresting files everywhere
  :ensure t
  :config
  ;; Ignore some additional directories and file extensions
  (dolist (name '(".cask"))
    ;; Ignore some additional directories
    (add-to-list 'ignoramus-file-basename-exact-names name))

  (dolist (ext '(".fls" ".out" ".aux"))
    (add-to-list 'ignoramus-file-endings ext))

  (validate-setq ignoramus-file-basename-beginnings
                 '(
                   ".#"                                   ; emacs
                   "._"                                   ; thumbnails
                   ))

  (ignoramus-setup))

(use-package hardhat                    ; Protect user-writable files
  :ensure t
  :init (global-hardhat-mode))

(use-package sudo-edit                  ; Edit files as root, through Tramp
  :ensure t
  :bind ("C-c f s" . sudo-edit))

(use-package archive-mode                   ; Browse archive files
  :mode ("\\.\\(cbr\\)\\'" . archive-mode)) ; Enable .cbr support

(use-package csv-mode                   ; Better .csv files editing
  :ensure t
  :no-require t
  :mode "\\.csv\\'")

(use-package image-file                 ; Visit images as images
  :init (auto-image-file-mode))

(use-package rst                        ; ReStructuredText
  :defer t
  :bind (:map rst-mode-map
              ("C-="     . nil)
              ;; For similarity with AUCTeX and Markdown
              ("C-c C-j" . rst-insert-list)
              ("M-RET"   . rst-insert-list)))

(use-package markdown-mode              ; Edit markdown files
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :hook (markdown-mode . auto-fill-mode)
  :config
  (validate-setq markdown-fontify-code-blocks-natively t)

  ;; Don't change font in code blocks
  (set-face-attribute 'markdown-code-face nil
                      :inherit nil))

(use-package dockerfile-mode            ; Edit docker's Dockerfiles
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package docker-compose-mode        ; Edit docker-compose files
  :ensure t
  :mode ("docker-compose.yml\\'". docker-compose-mode))

(use-package docker-tramp              ; TRAMP integration for docker containers
  :ensure t)

(use-package nov                        ; Featureful EPUB reader mode
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

;;; Utilities and key bindings
(defun skuro/current-file ()
  "Gets the \"file\" of the current buffer.
The file is the buffer's file name, or the `default-directory' in
`dired-mode'."
  (if (derived-mode-p 'dired-mode)
      default-directory
    (buffer-file-name)))

;;;###autoload
(defun skuro/copy-filename-as-kill (&optional arg)
  "Copy the name of the currently visited file to kill ring.
With a zero prefix arg, copy the absolute file name.  With
\\[universal-argument] ARG, copy the file name relative to the
current Projectile project, or to the current buffer's
`default-directory', if the file is not part of any project.
Otherwise copy the non-directory part only."
  (interactive "P")
  (if-let* ((file-name (skuro/current-file))
            (name-to-copy
             (cond
              ((zerop (prefix-numeric-value arg)) file-name)
              ((consp arg)
               (let* ((projectile-require-project-root nil)
                      (directory (and (fboundp 'projectile-project-root)
                                      (projectile-project-root))))
                 (file-relative-name file-name directory)))
              (t (file-name-nondirectory file-name)))))
      (progn
        (kill-new name-to-copy)
        (message "%s" name-to-copy))
    (user-error "This buffer is not visiting a file")))

;;;###autoload
(defun skuro/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;;;###autoload
(defun skuro/rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))

(bind-key "C-c f D" #'skuro/delete-this-file)
(bind-key "C-c f R" #'skuro/rename-this-file-and-buffer)
(bind-key "C-c f w" #'skuro/copy-filename-as-kill)

;; Additional bindings for built-ins
(bind-key "C-c f v d" #'add-dir-local-variable)
(bind-key "C-c f v l" #'add-file-local-variable)
(bind-key "C-c f v p" #'add-file-local-variable-prop-line)

(defun skuro/reload-dir-locals-for-current-buffer ()
  "Reload dir locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun skuro/reload-dir-locals-for-all-buffers-in-this-directory ()
  "Reload dir-locals for all buffers in current buffer's `default-directory'."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (skuro/reload-dir-locals-for-current-buffer)))))

(bind-key "C-c f v r" #'skuro/reload-dir-locals-for-current-buffer)
(bind-key "C-c f v r" #'skuro/reload-dir-locals-for-all-buffers-in-this-directory)

(use-package dired                      ; File manager
  :defer t
  :bind (("<C-return>" . skuro/open-in-external-app)
         ("C-c f g"    . skuro/dired-get-size)
         ("C-c f f"    . find-name-dired))
  :bind (:map dired-mode-map
              ("<backspace>" . skuro/dired-up)
              ("M-n"         . skuro/dired-down)
              ("RET"         . skuro/find-file-reuse-dir-buffer)
              ("!"           . skuro/sudired)
              ("<prior>"     . beginend-dired-mode-goto-beginning)
              ("<next>"      . beginend-dired-mode-goto-end))
  :config
  (validate-setq
   dired-auto-revert-buffer t           ; Revert buffers on revisiting
   dired-listing-switches "-lFaGh1v"
   dired-dwim-target t                  ; Use other pane as target
   dired-recursive-copies 'always       ; Copy dirs recursively
   dired-recursive-deletes ' always     ; Delete dirs recursively
   dired-ls-F-marks-symlinks t)         ; -F marks links with @

  ;; Enable dired-find-alternate-file
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Handle long file names
  (add-hook 'dired-mode-hook #'toggle-truncate-lines)

  (defun skuro/dired-up ()
    "Go to previous directory."
    (interactive)
    (find-alternate-file ".."))

  (defun skuro/dired-down ()
    "Enter directory."
    (interactive)
    (dired-find-alternate-file))

  (defun skuro/open-in-external-app ()
    "Open the file(s) at point with an external application."
    (interactive)
    (let* ((file-list
            (dired-get-marked-files)))
      (mapc
       (lambda (file-path)
         (let ((process-connection-type nil))
           (start-process "" nil "open" file-path))) file-list)))

  (defun skuro/find-file-reuse-dir-buffer ()
    "Like `dired-find-file', but reuse Dired buffers."
    (interactive)
    (set-buffer-modified-p nil)
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (find-alternate-file file)
        (find-file file))))

  (defun skuro/sudired ()
    "Open directory with sudo in Dired."
    (interactive)
    (require 'tramp)
    (let ((dir (expand-file-name default-directory)))
      (if (string-match "^/sudo:" dir)
          (user-error "Already in sudo")
        (dired (concat "/sudo::" dir)))))

  (defun skuro/dired-get-size ()
    "Quick and easy way to get file size in Dired."
    (interactive)
    (let ((files (dired-get-marked-files)))
      (with-temp-buffer
        (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
        (message
         "Size of all marked files: %s"
         (progn
           (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
           (match-string 1)))))))

(use-package find-dired                 ; Run `find' in Dired
  :config
  ;; Prefer case-insensitive search
  (validate-setq find-name-arg "-iname")

  (defun skuro/find-by-date (dir args)
    "Find file in DIR with given ARGS and sort the result by date."
    (interactive (list (read-directory-name "Run find in directory: " nil "" t)
                       (read-string "Run find (with args): " find-args
                                    '(find-args-history . 1))))
    (validate-setq
     find-ls-option '("-exec ls -lt {} + | cut -d ' ' -f5-" . "-lt"))
    (find-dired dir args)
    (validate-setq find-ls-option '("-ls" . "-dilsb")))

  (defun skuro/find-by-size (dir args)
    "Find file in DIR with given ARGS and sort the result by size."
    (interactive (list (read-directory-name "Run find in directory: " nil "" t)
                       (read-string "Run find (with args): " find-args
                                    '(find-args-history . 1))))
    (validate-setq
     find-ls-option '("-exec ls -lSr {} + | cut -d ' ' -f5-" . "-lSr"))
    (find-dired dir args)
    (validate-setq find-ls-option '("-ls" . "-dilsb"))))

(use-package dired-aux                  ; Other Dired customizations
  :after dired
  :config
  (validate-setq
   ;; Search only file names when point is on a file name
   dired-isearch-filenames 'dwim))

(use-package dired-x                    ; Enable some nice Dired features
  :bind ("C-x C-j" . dired-jump)
  :config
  (validate-setq
   ;; Be less verbose, Dired
   dired-omit-verbose nil
   ;; Do not ask for confirmation when killing deleted buffers
   dired-clean-confirm-killing-deleted-buffers nil
   ;; Omit dotfiles with C-x M-o
   dired-omit-files (concat dired-omit-files "\\|^\\.+$\\|^\\..+$"))

  (add-hook 'dired-mode-hook #'dired-omit-mode))

(use-package dired-narrow               ; Live-narrowing of search results
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package diredfl                    ; Add colours to Dired
  :ensure t
  :config (diredfl-global-mode))

(use-package dired-rsync                ; Allow rsync from dired buffers
  :ensure t
  :bind (:map dired-mode-map
              ("C-c C-r" . dired-rsync)))

(use-package dired-du
  :ensure t
  :defer t
  :config
  (validate-setq
   dired-du-size-format t
   dired-du-update-headers t))

(use-package projectile                 ; Project management
  :ensure t
  :init (projectile-mode)
  :bind-keymap  ("C-c p" . projectile-command-map)
  :config
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  (validate-setq
   projectile-completion-system 'ivy
   projectile-find-dir-includes-top-level t
   projectile-mode-line-function '(lambda () (format " Proj[%s]" (projectile-project-name))))

  (projectile-register-project-type 'lein-clj '("project.clj")
                                    :compile "lein uberjar"
                                    :test    "lein test"))

(use-package counsel-projectile         ; Ivy integration for Projectile
  :ensure t
  :bind (:map projectile-command-map
              ("p" . counsel-projectile-switch-project)
              ("r" . counsel-projectile-rg))
  :pin "melpa-unstable"                 ; see https://github.com/ericdanan/counsel-projectile/issues/119
  :init (counsel-projectile-mode))

(provide '00-files)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 00-files.el ends here
