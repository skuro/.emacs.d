;;; 00-wm.el --- Window management for my own Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Carlo Sciolla

;; Author: Carlo Sciolla <skuro@arraxu.local>
;; Keywords: frames

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Thanks Mikey and his great article:
;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager

;;; Code:
(require 'validate)

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

(setq switch-to-buffer-obey-display-actions t)   ; Do not skip rules when C-x b
(setq switch-to-buffer-in-dedicated-window 'pop) ; Pick the first available window for buffer when window is dedicated

(defun skuro/toggle-window-dedication ()
  "Toggle window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p (selected-window)
     (not (window-dedicated-p (selected-window)))))


(provide '00-wm)
;;; 00-wm.el ends here
