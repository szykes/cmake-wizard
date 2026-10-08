;;; cmake-wizard.el --- VSCode-CMake-Tools-like CMake workflow -*- lexical-binding: t; -*-

;; Author: Ádám Szikes
;; Maintainer: Ádám Szikes
;; Version: 0.2.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: tools, c, cmake
;; URL: https://github.com/szikes-adam/cmake-wizard

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; cmake-wizard brings a VSCode-CMake-Tools-like workflow to Emacs:
;; configure/build via `compile.el' (so `next-error' and the
;; minibuffer command-editing you already know from `M-x compile'
;; just work), a Clang-first toolchain with cross-compilation support,
;; build-profile (Debug/Release/...) switching, automatic
;; compile_commands.json wiring for `clangd' (via `eglot' or
;; `lsp-mode'), `ctest'/`cpack' integration, and `projectile'
;; project-type registration.
;;
;; This file is the facade: `(require 'cmake-wizard)' pulls in every
;; submodule.  Each submodule (cmake-wizard-project.el,
;; cmake-wizard-profile.el, cmake-wizard-toolchain.el,
;; cmake-wizard-compile.el, cmake-wizard-compdb.el, cmake-wizard-lsp.el,
;; cmake-wizard-ctest.el, cmake-wizard-cpack.el,
;; cmake-wizard-projectile.el, cmake-wizard-dir-locals.el) can also be
;; required on its own.
;;
;; See docs/high-level.md for usage and docs/low-level.md for
;; internals.  Targets GNU Emacs 31.1+ and CMake 4.0+ only.

;;; Code:

(require 'cmake-wizard-project)
(require 'cmake-wizard-profile)
(require 'cmake-wizard-toolchain)
(require 'cmake-wizard-compile)
(require 'cmake-wizard-compdb)
(require 'cmake-wizard-lsp)
(require 'cmake-wizard-ctest)
(require 'cmake-wizard-cpack)
(require 'cmake-wizard-dir-locals)
(require 'cmake-wizard-projectile nil t)
(require 'cmake-mode nil t)

(defvar cmake-wizard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c c") #'cmake-wizard-configure)
    (define-key map (kbd "C-c c b") #'cmake-wizard-build)
    (define-key map (kbd "C-c c m") #'cmake-wizard-compile)
    (define-key map (kbd "C-c c t") #'cmake-wizard-test)
    (define-key map (kbd "C-c c P") #'cmake-wizard-package)
    (define-key map (kbd "C-c c p") #'cmake-wizard-select-profile)
    (define-key map (kbd "C-c c k") #'cmake-wizard-select-toolchain)
    (define-key map (kbd "C-c c s") #'cmake-wizard-save-settings-to-dir-locals)
    map)
  "Keymap for `cmake-wizard-mode'.")

;;;###autoload
(define-minor-mode cmake-wizard-mode
  "Minor mode providing cmake-wizard's keybindings in the current buffer."
  :lighter " CMakeWizard"
  :keymap cmake-wizard-mode-map
  :group 'cmake-wizard)

(defun cmake-wizard--maybe-enable (&rest _)
  "Enable `cmake-wizard-mode' if the current buffer is inside a CMake project."
  (when (ignore-errors (cmake-wizard-project-root))
    (cmake-wizard-mode 1)))

;;;###autoload
(dolist (hook '(c-mode-hook c++-mode-hook c-ts-mode-hook c++-ts-mode-hook
                cmake-mode-hook))
  (add-hook hook #'cmake-wizard--maybe-enable))

(provide 'cmake-wizard)
;;; cmake-wizard.el ends here
