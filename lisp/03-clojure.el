;;; 03-clojure.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Carlo Sciolla

;; Author: Carlo Sciolla <carlo.sciolla@gmail.com>
;; Keywords: clojure

;;; Commentary:

;; This file configures my Clojure IDE

;;; Code:

;;; Env stuff
(setenv "JAVA_HOME" ;; lein won't work with Java 9+
        "/Library/Java/JavaVirtualMachines/jdk1.8.0_161.jdk/Contents/Home")

;;; Clojure
(use-package cider                      ; Clojure development environment
  :ensure t
  :hook (cider-mode . eldoc-mode)
  :config (validate-setq cider-offer-to-open-cljs-app-in-browser nil))

(use-package cider-mode                 ; CIDER mode for REPL interaction
  :ensure cider
  :defer t
  :bind (:map cider-mode-map
              ("C-c m l" . cider-load-all-project-ns))
  :config
  (require 'cider-client)

  (defun skuro/cider-mode-line-info ()
    "Simplify CIDER mode-line indicator."
    (if-let* ((current-connection (ignore-errors (cider-current-connection))))
        (with-current-buffer current-connection
          (format
           "%s:%s" cider-repl-type (or (cider--project-name nrepl-project-dir) "<no project>")))
      "-"))

  (validate-setq
   cider-mode-line '(:eval (format " CIDER[%s]" (skuro/cider-mode-line-info))))
  (validate-setq
   clojure-toplevel-inside-comment-form t))

(use-package flycheck-clj-kondo         ; Requires `clj-kondo' to be installed and available on `exec-path'
  :ensure t)

(use-package clojure-mode               ; Major mode for Clojure files
  :ensure t
  :mode (("\\.boot$" . clojure-mode)
         ("\\.clj$"  . clojure-mode)
         ("\\.cljc$"  . clojure-mode)
         ("\\.cljs$" . clojurescript-mode)
         ("\\.edn$"  . clojure-mode)
         ("\\.bb$" . clojure-mode))
  :hook ((clojure-mode . cider-mode)
         (clojure-mode . subword-mode)
         (clojure-mode . paredit-mode)
         (clojure-mode . hs-minor-mode)
         (clojure-mode . skuro/prog-mode-prettifies))
  :config
  ;; Fix indentation of some common macros
  (define-clojure-indent
    (for-all 1)
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)
    (reporting 1))

  ;; Add linting
  (require 'flycheck-clj-kondo))

(use-package clojure-mode-extra-font-locking ; Extra font-locking for Clojure
  :ensure t)

(use-package nrepl-client               ; Client for Clojure nREPL
  :ensure cider
  :defer t
  :config (validate-setq nrepl-hide-special-buffers t))

(use-package cider-repl                 ; REPL interactions with CIDER
  :ensure cider
  :bind (:map cider-repl-mode-map
              ("C-c M-o" . cider-repl-clear-buffer)
              ("C-c C-o" . cider-repl-switch-to-other)
              ("C-c t p" . cider-toggle-pretty-printing))
  :hook ((cider-repl-mode . company-mode)
         (cider-repl-mode . eldoc-mode)
         (cider-repl-mode . subword-mode)
         (cider-repl-mode . paredit-mode))
  :config
  (validate-setq
   cider-repl-wrap-history t
   cider-repl-history-size 1000
   cider-repl-history-file (locate-user-emacs-file "cider-repl-history")
   cider-repl-display-help-banner nil
   cider-repl-result-prefix ";; => "
   cider-repl-use-pretty-printing t))

(use-package cider-stacktrace           ; Navigate stacktrace
  :ensure cider
  :defer t)

(use-package cider-util                 ; Common utilities
  :ensure cider)

(use-package clj-refactor               ; Refactoring utilities
  :ensure t
  :hook (clojure-mode . (lambda ()
                          (clj-refactor-mode 1)
                          (yas-minor-mode 1)
                          (cljr-add-keybindings-with-prefix "C-c RET")))
  :config
  (validate-setq
   cljr-suppress-middleware-warnings t
   cljr-add-ns-to-blank-clj-files t
   cljr-auto-sort-ns t
   cljr-favor-prefix-notation nil
   cljr-favor-private-functions nil
   cljr-warn-on-eval nil
   cljr-hotload-dependencies t
   cljr-inject-dependencies-at-jack-in nil)

  (validate-setq
   cljr-clojure-test-declaration "[clojure.test :refer :all]"
   cljr-cljs-clojure-test-declaration
   "[cljs.test :refer-macros [deftest is use-fixtures]]")

  (advice-add 'cljr-add-require-to-ns :after
              (lambda (&rest _)
                (yas-next-field)
                (yas-next-field))))

(provide '03-clojure)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 03-clojure.el ends here
