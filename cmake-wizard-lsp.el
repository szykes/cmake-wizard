;;; cmake-wizard-lsp.el --- eglot / lsp-mode glue for cmake-wizard -*- lexical-binding: t; -*-

;; Author: Ádám Szikes
;; Maintainer: Ádám Szikes
;; Version: 0.2.0
;; Package-Requires: ((emacs "31.1") (cmake-wizard "0.2.0"))
;; Keywords: tools, c, cmake
;; URL: https://github.com/szikes-adam/cmake-wizard

;;; Commentary:

;; Starts an LSP client (`eglot', built into Emacs 29+, or `lsp-mode')
;; against `clangd' in a project's live C/C++ buffers once its
;; compile_commands.json has been wired up to the project root by
;; cmake-wizard-compdb.el, so clangd sees the exact flags/includes/
;; defines cmake used (REQ-2.1, REQ-2.3).

;;; Code:

(require 'cmake-wizard-project)
(require 'cmake-wizard-compile)
(require 'cmake-wizard-compdb)
(require 'eglot)

(declare-function lsp-deferred "lsp-mode")
(require 'lsp-mode nil t)

(defcustom cmake-wizard-lsp-client 'auto
  "Which LSP client cmake-wizard should drive.
One of `eglot', `lsp-mode', `auto', or `none'.  `auto' prefers `eglot'
\(built into Emacs\) and falls back to `lsp-mode' if `eglot' is
unavailable and `lsp-mode' is installed."
  :group 'cmake-wizard
  :type '(choice (const eglot) (const lsp-mode) (const auto) (const none)))

(defun cmake-wizard-lsp--effective-client ()
  "Return the LSP client symbol actually usable, honoring the user's choice."
  (pcase cmake-wizard-lsp-client
    ('none nil)
    ('eglot (and (fboundp 'eglot-ensure) 'eglot))
    ('lsp-mode (and (fboundp 'lsp-deferred) 'lsp-mode))
    ('auto (cond ((fboundp 'eglot-ensure) 'eglot)
                 ((fboundp 'lsp-deferred) 'lsp-mode)))))

(defun cmake-wizard-lsp-start-in-buffer (&optional buffer)
  "Start the configured LSP client in BUFFER (default: current buffer)."
  (with-current-buffer (or buffer (current-buffer))
    (pcase (cmake-wizard-lsp--effective-client)
      ('eglot (eglot-ensure))
      ('lsp-mode (lsp-deferred)))))

(defun cmake-wizard-lsp-maybe-start (root)
  "After ROOT is configured, start the LSP client in its live C/C++ buffers."
  (when (and (cmake-wizard-lsp--effective-client)
             (file-exists-p (cmake-wizard-compdb-link-path root)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and buffer-file-name
                   (derived-mode-p 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode)
                   (file-in-directory-p buffer-file-name root))
          (cmake-wizard-lsp-start-in-buffer buf))))))

(add-hook 'cmake-wizard-configure-success-functions #'cmake-wizard-lsp-maybe-start 10)

(provide 'cmake-wizard-lsp)
;;; cmake-wizard-lsp.el ends here
