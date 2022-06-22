;;; 00-editing.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Carlo Sciolla

;; Author: Carlo Sciolla <carlo.sciolla@gmail.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for text editing tools.

;;; Code:

(prefer-coding-system 'utf-8)

(defun skuro/to-beginning-or-indentation ()
  "Jumps back to either beginning of the line or indentation."
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(bind-key "C-1" 'recenter)
(bind-key "C-<tab>" 'hs-toggle-hiding)
(bind-key "s-<left>" 'skuro/to-beginning-or-indentation)
(bind-key "s-<right>" 'end-of-line)
(bind-key "s-<home>" 'previous-buffer)
(bind-key "s-<end>" 'next-buffer)
(bind-key "C-M-<backspace>" 'window-toggle-side-windows)

(use-package electric                   ; Electric modes package
  :config (add-hook 'after-init-hook #'electric-indent-mode))

(use-package aggressive-indent          ; Automatically indent code
  :ensure t
  :bind ("C-c t i" . aggressive-indent-mode)
  :hook ((lisp-mode       . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (clojure-mode    . aggressive-indent-mode))
  :config
  ;; Free C-c C-q, used in Org and in CIDER
  (unbind-key "C-c C-q" aggressive-indent-mode-map)

  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode))

(use-package whitespace-cleanup-mode    ; Cleanup whitespace in buffers
  :ensure t
  :bind (("C-c t w" . whitespace-cleanup-mode)
         ("C-c x w" . whitespace-cleanup))
  :hook ((prog-mode . whitespace-cleanup-mode)
         (text-mode . whitespace-cleanup-mode)
         (conf-mode . whitespace-cleanup-mode)))

(use-package delsel                     ; Replace the selection upon insert
  :defer t
  :init (delete-selection-mode))

(use-package subword                    ; Handle capitalized subwords
  :defer t
  ;; Do not override `transpose-words', it should not transpose subwords
  :bind (:map subword-mode-map
              ([remap transpose-words] . nil))
  :init (global-subword-mode 1))

(use-package expand-region      ; Increase the selected region by semantic units
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package change-inner              ; Change contents based on semantic units
  :ensure t
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(use-package easy-kill                  ; Better kill text
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp]      . easy-mark)))

(use-package avy
  :ensure t
  :bind (("s-r s" . avy-goto-char-timer)
         ("s-r l" . avy-goto-line)))

;; Free C-m and make it different from RET
(when (display-graphic-p) (define-key input-decode-map [?\C-m] [C-m]))

(use-package multiple-cursors        ; Easily place multiple cursors in a buffer
  :ensure t
  :bind (("C-'"         . set-rectangular-region-anchor)
         ("<C-m> ^"     . mc/edit-beginnings-of-lines)
         ("<C-m> $"     . mc/edit-ends-of-lines)
         ("<C-m> '"     . mc/edit-ends-of-lines)
         ("<C-m> R"     . mc/reverse-regions)
         ("<C-m> S"     . mc/sort-regions)
         ("<C-m> W"     . mc/mark-all-words-like-this)
         ("<C-m> Y"     . mc/mark-all-symbols-like-this)
         ("<C-m> a"     . mc/mark-all-like-this-dwim)
         ("<C-m> c"     . mc/mark-all-dwim)
         ("<C-m> l"     . mc/insert-letters)
         ("<C-m> n"     . mc/insert-numbers)
         ("<C-m> r"     . mc/mark-all-in-region-regexp)
         ("<C-m> t"     . mc/mark-sgml-tag-pair)
         ("<C-m> w"     . mc/mark-next-like-this-word)
         ("<C-m> x"     . mc/mark-more-like-this-extended)
         ("<C-m> y"     . mc/mark-next-like-this-symbol)
         ("<C-m> C-SPC" . mc/mark-pop)
         ("<C-m> ("     . mc/mark-all-symbols-like-this-in-defun)
         ("<C-m> C-("   . mc/mark-all-words-like-this-in-defun)
         ("<C-m> M-("   . mc/mark-all-like-this-in-defun)
         ("<C-m> ["     . mc/vertical-align-with-space)
         ("<C-m> {"     . mc/vertical-align))
  :bind (:map selected-keymap
              ("C-'" . mc/edit-lines)
              ("c"   . mc/edit-lines)
              ("."   . mc/mark-next-like-this)
              ("<"   . mc/unmark-next-like-this)
              ("C->" . mc/skip-to-next-like-this)
              (","   . mc/mark-previous-like-this)
              (">"   . mc/unmark-previous-like-this)
              ("C-<" . mc/skip-to-previous-like-this)
              ("y"   . mc/mark-next-symbol-like-this)
              ("Y"   . mc/mark-previous-symbol-like-this)
              ("w"   . mc/mark-next-word-like-this)
              ("W"   . mc/mark-previous-word-like-this))
  :init
  (setq mc/mode-line
        ;; Simplify the MC mode line indicator
        '(:propertize (:eval (concat " " (number-to-string
                                          (mc/num-cursors))))
                      face font-lock-warning-face)))

(use-package mc-extras                  ; Extra functions for multiple-cursors
  :ensure t
  :bind (("C-. M-C-f" . mc/mark-next-sexps)
         ("C-. M-C-b" . mc/mark-previous-sexps)
         ("C-. <"     . mc/mark-all-above)
         ("C-. >"     . mc/mark-all-below)
         ("C-. C-d"   . mc/remove-current-cursor)
         ("C-. C-k"   . mc/remove-cursors-at-eol)
         ("C-. M-d"   . mc/remove-duplicated-cursors)
         ("C-. |"     . mc/move-to-column)
         ("C-. ~"     . mc/compare-chars)))

(use-package mc-freeze
  :ensure mc-extras
  :bind ("C-. C-f" . mc/freeze-fake-cursors-dwim))

(use-package mc-rect
  :ensure mc-extras
  :bind ("C-\"" . mc/rect-rectangle-to-multiple-cursors))

(use-package saveplace                  ; Save point position in files
  :init (save-place-mode 1))

(use-package super-save                 ; Autosave buffers when they lose focus
  :ensure t
  :init (super-save-mode)
  :config (validate-setq super-save-auto-save-when-idle t))

(use-package autorevert                 ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode)
  :bind ("C-c t t" . auto-revert-tail-mode)
  :config
  (validate-setq
   auto-revert-verbose nil
   ;; Revert Dired buffers, too
   global-auto-revert-non-file-buffers t
   ;; Auto-revert files opened via TRAMP
   auto-revert-remote-files t))

;; Disable tabs, but given them proper width
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Make Tab complete if the line is indented
(validate-setq tab-always-indent 'complete)

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(validate-setq
 indicate-empty-lines t
 require-final-newline t)

(validate-setq
 kill-ring-max 200                      ; More killed items
 kill-do-not-save-duplicates t          ; No duplicates in kill ring
 ;; Save the contents of the clipboard to kill ring before killing
 save-interprogram-paste-before-kill t
 mouse-yank-at-point t)

;;;###autoload
(defun skuro/just-one-space-in-region (beg end)
  "Replace all whitespace in the region from BEG to END with single spaces."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))

;;;###autoload
(defun skuro/duplicate-line ()
  "Duplicate the line containing point."
  (interactive)
  (save-excursion
    (let (line-text)
      (goto-char (line-beginning-position))
      (let ((beg (point)))
        (goto-char (line-end-position))
        (setq line-text (buffer-substring beg (point))))
      (if (eobp)
          (insert ?\n)
        (forward-line))
      (open-line 1)
      (insert line-text))))

;;;###autoload
(defun skuro/move-line-up ()
  "Move up the current line, courtesy of mr Batsov."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

;;;###autoload
(defun skuro/move-line-down ()
  "Move down the current line, courtesy of mr Batsov."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;;;###autoload
(defun skuro/font-+ ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (+ (face-attribute 'default :height) 10)))
;;;===autoload
(defun skuro/font-- ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (- (face-attribute 'default :height) 10)))

(bind-key "C-c x d" 'skuro/duplicate-line)    ; Duplicate line at point
(bind-key "S-s-<up>" 'skuro/move-line-up)     ; Move current line up
(bind-key "C-s-<up>" 'skuro/move-line-up)     ; Move current line up (alternate keybinding)
(bind-key "S-s-<down>" 'skuro/move-line-down) ; Move current line down
(bind-key "C-s-<down>" 'skuro/move-line-down) ; Move current line down (alternate keybinding)
(global-set-key (kbd "C-<mouse-5>") 'skuro/font-+) ; Enlarge fonts with your mouse
(global-set-key (kbd "C-<mouse-4>") 'skuro/font--) ; Shrink fonts with your mouse

;; Join line with the next one
(bind-key "C-j" (lambda ()
                  (interactive)
                  (join-line -1)))

;;;###autoload
(defun skuro/flush-kill-lines (regex)
  "Flush lines matching REGEX and append to kill ring.  Restrict to region\
if active."
  (interactive "sFlush kill regex: ")
  (save-excursion
    (save-restriction
      (when (use-region-p)
        (narrow-to-region (point) (mark))
        (goto-char 0))
      (while (search-forward-regexp regex nil t)
        (move-beginning-of-line nil)
        (kill-whole-line)))))

(bind-key [remap just-one-space] #'cycle-spacing)

;;;###autoload
(defun skuro/untabify-buffer ()
  "Apply `untabify' to the entire buffer."
  (interactive)
  (untabify (point-min) (point-max)))

;;;###autoload
(defun skuro/indent-buffer ()
  "Apply `indent-region' to the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;;;###autoload
(defun skuro/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including `indent-buffer', which should not be called automatically on save."
  (interactive)
  (skuro/untabify-buffer)
  (delete-trailing-whitespace)
  (skuro/indent-buffer))

(bind-key "C-c t c" #'skuro/cleanup-buffer)

;;;###autoload
(defun skuro/smart-kill-whole-line (&optional arg)
  "Kill whole line and move back to indentation.
Kill the whole line with function `kill-whole-line' and then move
`back-to-indentation'.
With prefix ARG, kill that many lines."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))

;;;###autoload
(defun skuro/smart-backward-kill-line ()
  "Kill line backwards and re-indent."
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

;;;###autoload
(defun skuro/smart-open-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

;;;###autoload
(defun skuro/back-to-indentation-or-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;;;###autoload
(defun skuro/insert-current-date (iso)
  "Insert the current date at point.
When ISO is non-nil, insert the date in ISO 8601 format.
Otherwise insert the date as Mar 04, 2014."
  (interactive "P")
  (insert (format-time-string (if iso "%F" "%b %d, %Y"))))

;;;###autoload
(defun skuro/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(bind-keys
 ([remap kill-whole-line]        . skuro/smart-kill-whole-line)
 ([remap move-beginning-of-line] . skuro/back-to-indentation-or-beginning-of-line)
 ("RET"                          . newline-and-indent)
 ("S-<return>"                   . skuro/smart-open-line)
 ("C-o"                          . skuro/open-line-with-reindent)
 ("C-s-<backspace>"              . skuro/smart-backward-kill-line)
 ("M-<delete>"                   . kill-word)
 ("<backtab>"                    . skuro/toggle-hiding))

(use-package which-key                  ; Show help popups for prefix keys
  :ensure t
  :init (which-key-mode)
  :config
  (validate-setq
   which-key-idle-delay 0.4
   which-key-sort-order 'which-key-prefix-then-key-order))

(use-package hydra                      ; Make bindings that stick
  :pin    melpa-unstable ;; https://github.com/abo-abo/hydra/issues/323
  :ensure t)

(defhydra hydra-zoom ()
  "zoom"
  ("+" text-scale-increase "in")
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("_" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset")
  ("q" nil "quit" :color blue))

(bind-keys ("C-x C-0" . hydra-zoom/body)
           ("C-x C-=" . hydra-zoom/body)
           ("C-x C--" . hydra-zoom/body)
           ("C-x C-+" . hydra-zoom/body))

(bind-keys :map prog-mode-map
           ("C-c d c" . comment-region)
           ("C-c d u" . uncomment-region))

;; Enable upcase-/downcase-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Don't let the cursor go into minibuffer prompt
(let ((default (eval (car (get 'minibuffer-prompt-properties 'standard-value))))
      (dont-touch-prompt-prop '(cursor-intangible t)))
  (setq minibuffer-prompt-properties
        (append default dont-touch-prompt-prop))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; Allow to read from minibuffer while in minibuffer.
(validate-setq enable-recursive-minibuffers t)

;; Show the minibuffer depth (when larger than 1)
(minibuffer-depth-indicate-mode 1)

(validate-setq
 ;; Never use dialogs for minibuffer input
 use-dialog-box nil
 ;; Store more history
 history-length 1000)

(use-package savehist                   ; Save minibuffer history
  :init (savehist-mode t)
  :config
  (validate-setq
   savehist-save-minibuffer-history t
   savehist-autosave-interval 180))

(validate-setq
 frame-resize-pixelwise t               ; Resize by pixels
 frame-title-format '(:eval (concat "[Emacs] " (if (buffer-file-name)
                                                   (abbreviate-file-name (buffer-file-name))
                                                 "%b"))))

;; Configure `display-buffer' behaviour for some special buffers
(validate-setq
 display-buffer-alist
 `(
   ;; Messages, errors, processes, Calendar and REPLs in the bottom side window
   (,(rx bos (or "*Apropos"             ; Apropos buffers
                 "*Man"                 ; Man buffers
                 "*Help"                ; Help buffers
                 "*Warnings*"           ; Emacs warnings
                 "*Process List*"       ; Processes
                 "*Proced"              ; Proced processes list
                 "*Compile-Log*"        ; Emacs byte compiler log
                 "*compilation"         ; Compilation buffers
                 "*Flycheck errors*"    ; Flycheck error list
                 "*Calendar"            ; Calendar window
                 "*version"             ; Emacs version from my custom function
                 "*cider-repl"          ; CIDER REPL
                 "*intero"              ; Intero REPL
                 "*idris-repl"          ; Idris REPL
                 "*ielm"                ; IELM REPL
                 "*SQL"                 ; SQL REPL
                 "*Cargo"               ; Cargo process buffers
                 ;; AUCTeX command output
                 (and (1+ nonl) " output*")))
    (display-buffer-reuse-window display-buffer-in-side-window)
    (side . bottom)
    (reusable-frames . visible)
    (window-height . 0.45))
   ;; Open shell in a single window
   (,(rx bos "*shell")
    (display-buffer-same-window)
    (reusable-frames . nil))
   ;; Open PDFs in the right side window
   (,(rx bos "*pdf")
    (display-buffer-reuse-window display-buffer-in-side-window)
    (side . right)
    (reusable-frames . visible)
    (window-width . 0.5))
   ;; Let `display-buffer' reuse visible frames for all buffers.  This must
   ;; be the last entry in `display-buffer-alist', because it overrides any
   ;; previous entry with more specific actions.
   ("." nil (reusable-frames . visible))))

(use-package ibuf-ext                   ; Extensions for Ibuffer
  :config
  ;; Do not show empty groups
  (validate-setq ibuffer-show-empty-filter-groups nil))

(use-package fullframe                 ; Generalized execution in a single frame
  :ensure t
  :defer t)

(use-package ibuffer                    ; Buffer management
  :bind (("C-x C-b" . ibuffer)
         ([remap list-buffers] . ibuffer))
  :config
  (validate-setq
   ibuffer-expert t              ; Do not prompt when on kill buffers operations
   ibuffer-filter-group-name-face 'font-lock-doc-face)

  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  (validate-setq ibuffer-formats
                 '((mark modified read-only " "
                         (name 18 18 :left :elide)
                         " "
                         (size-h 9 -1 :right)
                         " "
                         (mode 16 16 :left :elide)
                         " "
                         filename-and-process)
                   (mark modified read-only " "
                         (name 18 18 :left :elide)
                         " "
                         (size 9 -1 :right)
                         " "
                         (mode 16 16 :left :elide)
                         " "
                         (vc-status 16 16 :left)
                         " "
                         filename-and-process))))

(use-package ibuffer-vc                 ; Group buffers by VC project and status
  :ensure t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'filename/process)
                      (ibuffer-do-sort-by-filename/process)))))

;;; Utilities and key bindings
;; Don't kill the important buffers
(defconst skuro/do-not-kill-buffer-names '("*scratch*" "*Messages*")
  "Names of buffers that should not be killed.")

;;;###autoload
(defun skuro/do-not-kill-important-buffers ()
  "Inhibit killing of important buffers.
Add this to `kill-buffer-query-functions'."
  (if (not (member (buffer-name) skuro/do-not-kill-buffer-names))
      t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (bury-buffer)
    nil))

;; Don't kill important buffers
(add-hook 'kill-buffer-query-functions #'skuro/do-not-kill-important-buffers)

;; Trim whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(bind-key "C-x C-k" #'kill-this-buffer)  ; Kill only the current buffer

(validate-setq window-combination-resize t) ; Size new windows proportionally

(use-package eyebrowse                  ; Easy workspaces creation and switching
  :ensure t
  :init (eyebrowse-mode t)
  :config
  (validate-setq
   eyebrowse-mode-line-separator " "
   eyebrowse-mode-line-style 'always
   eyebrowse-new-workspace t
   eyebrowse-wrap-around t))

(validate-setq
 scroll-conservatively 1000
 ;; Move to beg/end of buffer before signalling an error
 scroll-error-top-bottom t
 ;; Ensure M-v always undoes C-v
 scroll-preserve-screen-position 'always
 ;; Start recentre from top
 recenter-positions '(top middle bottom)
 ;; Disable mouse scrolling acceleration
 mouse-wheel-progressive-speed nil)

(use-package beginend                   ; Redefine M-< and M-> for some modes
  :ensure t
  :config (beginend-global-mode))

(use-package dumb-jump                  ; Jump to definitions
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (validate-setq dumb-jump-selector 'ivy))

(use-package windmove                   ; Quickly move between windows
  :bind (("S-<up>"    . windmove-up)
         ("S-<down>"  . windmove-down)
         ("S-<left>"  . windmove-left)
         ("S-<right>" . windmove-right)))

(use-package elisp-def             ; Macro-aware go-to-definition for Emacs Lisp
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("M-." . elisp-def)
              ("M-," . xref-pop-marker-stack)))

(use-package macrostep                  ; Navigate through macros
  :ensure t
  :after lisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-c m m e" . macrostep-expand))
  :bind (:map lisp-interaction-mode-map
              ("C-c m m e" . macrostep-expand)))

;; Quickly pop the mark several times with C-u C-SPC C-SPC
(validate-setq set-mark-command-repeat-pop t)

;; Focus new help windows when opened
(setq-default help-window-select t)

(use-package flycheck                   ; On-the-fly syntax checker
  :ensure t
  :bind ("C-c t e" . flycheck-mode)
  :pin melpa-unstable                  ; https://github.com/flycheck/flycheck/issues/1754
  :init
  (global-flycheck-mode))

(use-package flycheck-package          ; Check package conventions with Flycheck
  :ensure t
  :defer t
  :after flycheck
  :config (flycheck-package-setup))

(use-package plantuml-mode             ; UML graphs
  :ensure t
  :mode (("\\.puml" . plantuml-mode)
         ("\\.plantuml" . plantuml-mode)
         ("\\.iuml" . plantuml-mode))
  :config (validate-setq plantuml-output-type "png"
                         plantuml-jar-path    (expand-file-name "~/.plantuml/plantuml.jar")))

(use-package image+ :ensure t :after 'image-mode
  :init
  (add-hook 'image-mode-hook
            '(lambda ()
               (require 'image+)
               (imagex-sticky-mode)))
  :bind (:map image-mode-map
              ("=" . imagex-sticky-zoom-in)
              ("-" . imagex-sticky-zoom-out)
              ("m" . imagex-sticky-maximize)
              ("0" . imagex-sticky-restore-original)
              ("S" . imagex-sticky-save-image)
              ("r" . imagex-sticky-rotate-right)
              ("l" . imagex-sticky-rotate-left)
              ("/" . imagex-sticky-binding/body)))

(use-package jinja2-mode               ; python / GCP templates
  :ensure t)

(use-package rust-mode                 ; rust
  :ensure t)

(use-package restclient
  :ensure t
  :mode (("\\.http" . restclient-mode)))

(use-package adoc-mode
  :ensure t
  :mode (("\\.adoc" . adoc-mode)
         ("\\.asciidoc" . adoc-mode)))

(use-package terraform-mode
  :ensure t)

(use-package ini-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package mustache-mode
  :ensure t
  :mode ("\\.tpl\\'" . mustache-mode))

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

;;;###autoload
(defun skuro/open-buffer-file-with-app ()
  "Open the file associated with the current buffer using an external application."
  (interactive)
  (let ((file (buffer-file-name))
        (process-connection-type nil)
        (open-cmd (cond
                   ((string-equal system-type "windows-nt")
                    (message "Microsoft Windows not supported"))
                   ((string-equal system-type "darwin")
                    "/usr/bin/open")
                   ((string-equal system-type "gnu/linux")
                    "/usr/bin/xdg-open"))))
    (when open-cmd
      (start-process "" nil open-cmd file))))

;;;###autoload
(defun skuro/toggle-selective-display (column)
  "Switch on `selective-display' based on indentation to the column COLUMN."
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

;;;###autoload
(defun skuro/toggle-hiding (column)
  "Hide and show blocks based on indentation to the column COLUMN."
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          (hs-show-all))
    (skuro/toggle-selective-display column)))

;;;###autoload
(defun skuro/kill-matching-lines (regexp &optional rstart rend interactive)
  "Kill lines containing matching REGEXP.

Operates on either the full buffer or within the region delimited
by RSTART and REND.

Can be interatively executed in which case it's set as INTERACTIVE.

See `flush-lines' or `keep-lines' for behavior of this command.

If the buffer is read-only, Emacs will beep and refrain from deleting
the line, but put the line in the kill ring anyway.  This means that
you can use this command to copy text from a read-only buffer.
\(If the variable `kill-read-only-ok' is non-nil, then this won't
even beep.)"
  (interactive
   (keep-lines-read-args "Kill lines containing match for regexp"))
  (let ((buffer-file-name nil)) ;; HACK for `clone-buffer'
    (with-current-buffer (clone-buffer nil nil)
      (let ((inhibit-read-only t))
        (keep-lines regexp rstart rend interactive)
        (kill-region (or rstart (line-beginning-position))
                     (or rend (point-max))))
      (kill-buffer)))
  (unless (and buffer-read-only kill-read-only-ok)
    ;; Delete lines or make the "Buffer is read-only" error.
    (flush-lines regexp rstart rend interactive)))

(provide '00-editing)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 00-editing.el ends here
