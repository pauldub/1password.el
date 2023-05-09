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

(defun 1password--json-read (string)
    (condition-case err
        (json-parse-string string)
      (error
       (error "JSON parsing error: %s" (error-message-string err)))))

(defvar 1password--get-item-cache nil
  "Cache for `1password-get-item'.
  
1Password is soooooo slow from here.")

;;;###autoload
(defun 1password-get-item (name)
  "Return json object for the NAME item."
  (let ((cached-item (assoc (downcase name) 1password--get-item-cache)))
    (if cached-item
        (cdr cached-item)
      (with-temp-buffer
        (if (zerop (call-process 1password-op-executable nil t nil "item" "get" name "--format" "json"))
            (progn
              (goto-char (point-min))
              (let ((item (1password--json-read (buffer-string))))
                (push (cons (downcase name) item) 1password--get-item-cache)
                item))
          (error "'op list items' failed: %s" (buffer-string)))))))

(defun 1password-get-fields (name)
  "Return the fields of the 1Password item with the given NAME."
  (let ((item (1password-get-item name)))
    (when (string= "" name)
      (user-error "Name can't be empty"))
    (append (gethash "fields" item) nil)))

;;;###autoload
(defun 1password-get-field (name field &optional copy)
  "Return the value of the specified FIELD in the 1Password item with the given NAME."
  (let ((fields (1password-get-fields name)))
    (when (string= "" name)
      (user-error "Name can't be empty"))
    (when (string= "" field)
      (user-error "Field can't be empty"))
    (catch 'getfield
      (dolist (field-item fields)
        (let ((field-label (gethash "label" field-item))
              (field-value (gethash "value" field-item)))
          (when (string= field field-label)
            (when copy
              (kill-new field-value)
              (message "Field %s of %s copied: %s" field name field-value))
            (throw 'getfield field-value))))
      (error "Field '%s' not found in item '%s'" field name))))

;;;###autoload
(defun 1password-get-password (name &optional copy)
  "Return password of the NAME item."
  (1password-get-field name "password" copy))

(provide '1password)
;;; 1password.el ends here
