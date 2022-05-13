;;; 03-sql.el --- Utilities for SQL stuff  -*- lexical-binding: t; -*-

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

;; This file contains packages and custom functions to deal with SQL.

;;; Code:

(require 'sql)

(defun skuro/load-psql-connections
    ()
  "Connects to a given PostgreSQL instance leveraging `auth-sources'."
  (when-let* ((connections (seq-filter (lambda (elem) (plist-member elem :sql-connection))
                                       (auth-source-search :max 100))))
    (dolist (entry connections)
      (message (plist-get entry :sql-connection))
      (setq sql-connection-alist (map-insert sql-connection-alist
                                             (plist-get entry :sql-connection)
                                             (list (list 'sql-product ''postgres)
                                                   (list 'sql-server (plist-get entry :host))
                                                   (list 'sql-user (plist-get entry :user))
                                                   (list 'sql-database (plist-get entry :db))
                                                   (list 'sql-port (or (when-let ((port-spec (plist-get entry :port)))
                                                                         (string-to-number port-spec))
                                                                       5432))
                                                   (list 'sql-password (funcall (plist-get entry :secret)))))))))

(provide '03-sql)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 03-sql.el ends here
