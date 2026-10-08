;;; cmake-wizard-compdb.el --- compile_commands.json wiring for clangd -*- lexical-binding: t; -*-

;; Author: Ádám Szikes
;; Maintainer: Ádám Szikes
;; Version: 0.2.0
;; Package-Requires: ((emacs "31.1") (cmake-wizard "0.2.0"))
;; Keywords: tools, c, cmake
;; URL: https://github.com/szikes-adam/cmake-wizard

;;; Commentary:

;; cmake-wizard always configures with
;; -DCMAKE_EXPORT_COMPILE_COMMANDS=ON (see cmake-wizard-compile.el),
;; which makes cmake write compile_commands.json into the build
;; directory.  clangd only looks for it in the project root (or an
;; ancestor), so after every successful configure we expose it there -
;; via a symlink where possible, falling back to a plain copy on
;; platforms (chiefly Windows without Developer Mode / admin rights)
;; where symlinking isn't permitted.

;;; Code:

(require 'cmake-wizard-project)
(require 'cmake-wizard-profile)
(require 'cmake-wizard-toolchain)
(require 'cmake-wizard-compile)

(defcustom cmake-wizard-compdb-filename "compile_commands.json"
  "Filename cmake writes the compilation database to."
  :group 'cmake-wizard
  :type 'string)

(defun cmake-wizard-compdb-source-path (root)
  "Return the path to ROOT's compile db inside its current build directory."
  (let* ((root (cmake-wizard--canonical-root root))
         (profile (cmake-wizard-current-profile root))
         (toolchain (cmake-wizard-current-toolchain root)))
    (expand-file-name cmake-wizard-compdb-filename
                       (cmake-wizard-build-directory root profile toolchain))))

(defun cmake-wizard-compdb-link-path (root)
  "Return the path where ROOT's compile db should be exposed for clangd."
  (expand-file-name cmake-wizard-compdb-filename (cmake-wizard--canonical-root root)))

(defun cmake-wizard-compdb-sync (root)
  "Expose ROOT's compile db at the project root (REQ-2.3).
Symlinks the build directory's compile_commands.json into ROOT,
falling back to copying the file when symlinking isn't available."
  (let ((source (cmake-wizard-compdb-source-path root))
        (link (cmake-wizard-compdb-link-path root)))
    (if (not (file-exists-p source))
        (message "cmake-wizard: %s not found, skipping clangd wiring" source)
      (when (or (file-symlink-p link) (file-exists-p link))
        (delete-file link))
      (condition-case err
          (make-symbolic-link source link t)
        (file-error
         (message "cmake-wizard: symlink failed (%s), copying instead"
                   (error-message-string err))
         (copy-file source link t))))))

(add-hook 'cmake-wizard-configure-success-functions #'cmake-wizard-compdb-sync -10)

(provide 'cmake-wizard-compdb)
;;; cmake-wizard-compdb.el ends here
