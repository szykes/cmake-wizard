;;; cmake-wizard-projectile.el --- projectile glue for cmake-wizard -*- lexical-binding: t; -*-

;; Author: Ádám Szikes
;; Maintainer: Ádám Szikes
;; Version: 0.2.0
;; Package-Requires: ((emacs "31.1") (cmake-wizard "0.2.0") (projectile "2.0"))
;; Keywords: tools, c, cmake
;; URL: https://github.com/szikes-adam/cmake-wizard

;;; Commentary:

;; Registers a `projectile' project type keyed on CMakeLists.txt, so
;; `projectile-configure-project' and `projectile-compile-project' use
;; cmake-wizard's commands out of the box (REQ-4.3).  `projectile'
;; itself expects `:configure'/`:compile' to be either a literal shell
;; command string or a function that *returns* one (it still runs the
;; command through its own `compile'-based machinery) - so these
;; wrapper functions build and return the command string, they don't
;; run it themselves.

;;; Code:

(require 'cmake-wizard-project)
(require 'cmake-wizard-compile)

(declare-function projectile-register-project-type "projectile")

(defun cmake-wizard-projectile--configure-command ()
  "Return the cmake-wizard configure command for the current project."
  (cmake-wizard-configure-command (cmake-wizard-project-root)))

(defun cmake-wizard-projectile--compile-command ()
  "Return the cmake-wizard build command for the current project."
  (cmake-wizard-build-command (cmake-wizard-project-root)))

(defun cmake-wizard-projectile-setup ()
  "Register cmake-wizard as projectile's handler for CMake projects."
  (when (fboundp 'projectile-register-project-type)
    (projectile-register-project-type
     'cmake-wizard '("CMakeLists.txt")
     :configure #'cmake-wizard-projectile--configure-command
     :compile #'cmake-wizard-projectile--compile-command)))

(with-eval-after-load 'projectile
  (cmake-wizard-projectile-setup))

(provide 'cmake-wizard-projectile)
;;; cmake-wizard-projectile.el ends here
