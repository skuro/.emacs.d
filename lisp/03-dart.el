;;; 03-dart.el --- Dart and flutter development configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Carlo Sciolla

;; Author: Carlo Sciolla <skuro@lepori>
;; Keywords:

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

;; This file configures Dart and Flutter development tools.

;;; Code:

(use-package dart-mode
  :ensure t)

(defun skuro/find-dart-flutter-sdk-dir ()
  "Find the Dart Flutter SDK directory.
Credits to @Walheimat https://github.com/emacs-lsp/lsp-dart/issues/107#issuecomment-944959495."
  (when-let* ((flutter-bin (executable-find "flutter"))
              (sdk-dir (string-trim (shell-command-to-string "flutter sdk-path"))))
    sdk-dir))

(use-package lsp-dart
  :after lsp-mode
  :ensure t
  :hook (dart-mode . lsp)
  :custom
  (lsp-dart-flutter-sdk-dir (skuro/find-dart-flutter-sdk-dir)))

(provide '03-dart)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 03-dart.el ends here
