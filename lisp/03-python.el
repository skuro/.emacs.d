;;; 03-python.el --- Part of my Emacs startup configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Carlo Sciolla

;; Author: Carlo Sciolla <skuro@margiani>
;; Keywords: convenience

;;; Commentary:

;; Python configuration

;;; Code:

(use-package python
  :ensure nil
  :hook (python-mode . eglot-ensure)
  :config
  (setq-default python-shell-interpreter "python3")

  (defun skuro/setup-python-interpreter ()
    "Set up Python interpreter, preferring uv's managed Python."
    (let ((uv-python (executable-find "uv")))
      (when uv-python
        ;; Try to get python from current uv environment
        (let ((uv-python-path
               (string-trim
                (shell-command-to-string "uv run which python 2>/dev/null || echo ''"))))
          (when (and (not (string-empty-p uv-python-path))
                     (file-executable-p uv-python-path))
            (setq-local python-shell-interpreter uv-python-path)
            (setq-local python-shell-interpreter-args ""))))))

  (add-hook 'python-mode-hook #'skuro/setup-python-interpreter))

(use-package pyvenv
  :ensure t
  :config
  (defun skuro/auto-activate-uv-venv ()
    "Automatically activate virtual environment using uv."
    (when (and (executable-find "uv")
               (locate-dominating-file default-directory "pyproject.toml"))
      (let ((venv-path (string-trim
                        (shell-command-to-string "uv venv --quiet && uv run python -c 'import sys; print(sys.prefix)' 2>/dev/null || echo ''"))))
        (when (and (not (string-empty-p venv-path))
                   (file-directory-p venv-path))
          (pyvenv-activate venv-path)))))

  (add-hook 'python-mode-hook #'skuro/auto-activate-uv-venv))

(provide '03-python)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 03-python.el ends here
