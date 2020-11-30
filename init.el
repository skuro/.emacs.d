;;; init.el --- Emacs configuration of Carlo Sciolla -*- lexical-binding: t; -*-

;; Copyright  (c) 2018-2019 Carlo Sciolla

;; Author: Carlo Sciolla carlo.sciolla@gmail.com
;; URL: https://github.com/skuro/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This is the GNU Emacs configuration of Carlo Sciolla.

;;; Code:

;;; Package setup
(require 'package)

(setq debug-on-error t)                ; Always provide full error details

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

(setq package-archives
      '(("gnu"            . "https://elpa.gnu.org/packages/")
        ("melpa"          . "https://stable.melpa.org/packages/")
        ("melpa-unstable" . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("melpa"          . 50)
        ("gnu"            . 10)
        ("melpa-unstable" . 0)))

;; Reconsider when this issue is solved and a new release is out:
;; https://github.com/jwiegley/use-package/issues/602
(add-to-list 'package-pinned-packages
             '(use-package . "melpa-unstable") t)

;; Prepare for Emacs 27 when package initialization happens before init
(when (< emacs-major-version 27)
  (package-initialize))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(setq load-prefer-newer t)              ; Always load newer compiled files
(setq ad-redefinition-action 'accept)   ; Silence advice redefinition warnings
(setq message-log-max 10000)            ; Debugging
(setq enable-local-variables :all)      ; Always enable all local variables

;; Allow more than 800Kb cache during init
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defun skuro/set-gc-threshold ()
  "Reset `gc-cons-threshold' and `gc-cons-percentage' to their default values."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

(use-package diminish                   ; Hide modes in the mode-line
  :ensure t)

(use-package dash                       ; A modern list library
  :ensure t)

(require 'subr-x)
(require 'time-date)

;;; Initialization
(setq inhibit-default-init t)           ; Disable the site default settings

;; Ensure resizing Emacs window doesn't cause display problems
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;;; Validation
(use-package validate                   ; Validate options
  :ensure t)

(defun skuro/batch-mode-p ()
  "Return non-nil if Emacs is start in batch mode."
  noninteractive)

(use-package exec-path-from-shell       ; Set up environment variables
  :ensure t
  :if (not (skuro/batch-mode-p))
  :config
  (validate-setq exec-path-from-shell-variables
                 '("PATH"               ; Full path
                   "JAVA_OPTS"          ; Options for Java processes
                   ))
  :init
  (exec-path-from-shell-initialize))

;; Set separate custom file for the customize interface
(defconst skuro/custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit                   ; Set up custom.el
  :defer t
  :config
  (validate-setq
   custom-file skuro/custom-file
   custom-buffer-done-kill nil          ; Kill when existing
   custom-buffer-verbose-help nil       ; Remove redundant help text
   custom-unlispify-tag-names nil       ; Show me the real variable name
   custom-unlispify-menu-entries nil)
  :init (load skuro/custom-file 'no-error 'no-message))

(use-package no-littering               ; Keep .emacs.d clean
  :ensure t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)

  (validate-setq
   create-lockfiles nil
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

  (validate-setq
   backup-directory-alist
   `((".*" . ,(no-littering-expand-var-file-name "backup/")))
   auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; Confirm before quitting Emacs
(validate-setq confirm-kill-emacs #'y-or-n-p)

(defconst skuro/personal-packages-folder (expand-file-name "lisp" user-emacs-directory))

;;;
;;; Split elisp configuration in files under ~/.emacs.d/lisp and automatically
;;; load all of them in lexicographical order
;;;
(add-to-list 'load-path skuro/personal-packages-folder)

(defun skuro/personal-packages ()
  "List all packages declared in the personal `lisp' folder."
  (mapcar 'file-name-sans-extension
          (directory-files skuro/personal-packages-folder nil ".*\.el")))

(defmacro skuro/use-personal-packages ()
  "Ensure and defer PACKAGES using `use-package'."
  (declare (indent defun))
  (let ((packages (skuro/personal-packages)))
    (macroexp-progn
     (mapcar (lambda (package)
               `(use-package ,package))
             packages))))

(skuro/use-personal-packages)

;; Reset default values
(add-hook 'emacs-startup-hook #'skuro/set-gc-threshold)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
