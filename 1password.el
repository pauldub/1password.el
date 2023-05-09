;;; 1password.el --- Retrive password from 1Password  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Homepage: https://github.com/xuchunyang/1password.el
;; Package-Requires: ((emacs "25.1"))
;; Created: 2019年5月15日 晚饭后

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

;; This package needs the command op, it is the 1Password command line tool
;; <https://support.1password.com/command-line/>.

;;; Code:

;; The macro `let-alist' is already autoloaded, this expression is not
;; needed. However, for Emacs without let-alist.el, this expression fails
;; immediately and produces a clean reason.
(eval-when-compile (require 'let-alist))

(require 'json)

(defun 1password--get-command-output (command &rest args)
  "Execute COMMAND with ARGS and return its output as a string."
  (with-output-to-string
    (apply 'call-process command nil standard-output nil args)))

(defgroup 1password nil
  "Use 1Password from Emacs."
  :group 'tools)

(defcustom 1password-op-executable "op"
  "The 1Password command-line tool."
  :group '1password
  :type 'string)

(defvar 1password-items nil)

(defun 1password--json-read (string)
    (condition-case err
        (json-parse-string string)
      (error
       (error "JSON parsing error: %s" (error-message-string err)))))

(defun 1password-items ()
  "Cache of 'op item list'."
  (or 1password-items
      (let ((output (1password--get-command-output 1password-op-executable "item" "list" "--format" "json")))
        (if output
            (setq 1password-items (1password--json-read output))
          (error "'op item list' failed")))))

(defun 1password--read-name ()
  (let ((completion-ignore-case t))
    (completing-read "Name: "
                     (mapcar (lambda (item) (let-alist item .overview.title))
                             (1password-items))
                     nil t)))

(defvar 1password--get-item-cache nil
  "Cache for `1password-get-item'.

1Password is soooooo slow from here.")

(defun 1password-get-item (name)
  "Return json object for the NAME item."
  (or (assoc-string name 1password--get-item-cache 'ignore-case)
      (with-temp-buffer
        (if (zerop (call-process 1password-op-executable nil t nil "item" "get" name))
            (progn
              (goto-char (point-min))
              (let ((item (1password--json-read)))
                (push (cons (downcase name) item) 1password--get-item-cache)
                item))
          (error "'op list items' failed: %s" (buffer-string))))))

;;;###autoload
(defun 1password-get-field (name field &optional copy)
  "Return field of the NAME item."
  (interactive (list (1password--read-name) t))
  (when (string= "" name)
    (user-error "Name can't be emtpy"))
  (when (string= "" field)
    (user-error "Field can't be emtpy"))
  (catch 'getfield
    (dolist (field (let-alist (1password-get-item name) .details.fields))
      (let-alist field
        (when (string= .name field)
          (when copy
            (kill-new .value)
            (message "Field %s of %s copied: %s" field name .value))
          (throw 'getfield .value))))))

;;;###autoload
(defun 1password-get-password (name &optional copy)
  "Return password of the NAME item."
  (1password-get-field name "password" copy))

(provide '1password)
;;; 1password.el ends here
