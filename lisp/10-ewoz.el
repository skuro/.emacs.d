;;; 10-ewoz.el --- Utilities for EWOZ stuff          -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Carlo Sciolla

;; Author: Carlo Sciolla <skuro@arraxu.local>
;; Keywords: ewoz

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

;;

;;; Code:

(require 'project)

(defun ewoz/new-sql-migration ()
  "Create a new SQL migration file."
  (interactive)
  (let* ((migration-purpose (read-string "Migration purpose (e.g. 'add column foo to table bar'): "))
         (migration-purpose-file (thread-last migration-purpose
                                              (downcase)
                                              (replace-regexp-in-string "[[:space:]]+" " " )
                                              (replace-regexp-in-string "[^a-zA-Z0-9]" "-")))
         (prefix (format-time-string "%Y%m%d%H%M"))
         (suffix ".up.sql")
         (migrations-path (expand-file-name "resources/migrations" (project-root (project-current))))
         (migration-filename (concat prefix "-" migration-purpose-file suffix))
         (migration-full-path (expand-file-name migration-filename migrations-path)))
    (find-file migration-full-path)))

(provide '10-ewoz)
;;; 10-ewoz.el ends here
