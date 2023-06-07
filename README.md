# 1password.el

Forked from https://github.com/xuchunyang/1password.el to update to the latest 1Password CLI version, add support to get arbitrary fields and add auth-source backend.

## Usage

### `M-x 1password-get-password NAME`

Get password of item `NAME`, e.g., `github`.

### `M-x 1password-get-field NAME FIELD`

Get field with label `FIELD` of item `NAME`, e.g., `github` `api_key`.

### `M-x 1password-auth-source-enable`

To enable the auth-source backend.

## Requirements

- Emacs 25.1 or later
- `op` / [1Password command line tool](https://support.1password.com/command-line/)


## Installation

For emacs-doom:

```emacs-elisp
;; in packages.el

(package! 1password
  :recipe (:host github :repo "kamushadenes/1password.el" :files ("*.el")))
```

```emacs-elisp
;; in config.el

(use-package! 1password
  :demand t
  :init
  (message "Enabling 1password ...")
  :config
  (1password-auth-source-enable))
```
