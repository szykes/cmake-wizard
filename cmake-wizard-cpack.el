;;; cmake-wizard-cpack.el --- CPack integration for cmake-wizard -*- lexical-binding: t; -*-

;; Author: Ádám Szikes
;; Maintainer: Ádám Szikes
;; Version: 0.2.0
;; Package-Requires: ((emacs "31.1") (cmake-wizard "0.2.0"))
;; Keywords: tools, c, cmake
;; URL: https://github.com/szikes-adam/cmake-wizard

;;; Commentary:

;; Runs `cpack' against the current project's build directory to
;; produce distributable packages (TGZ/ZIP/DEB/RPM/...).  Requires the
;; project to call `include(CPack)'; which generators actually run is
;; controlled by CPACK_GENERATOR in the project's own CMakeLists.txt,
;; not by cmake-wizard.
;;
;; Invoked as `cpack --config <build-dir>/CPackConfig.cmake', which
;; works from any `default-directory' and - like `cmake-wizard-ctest's
;; use of `ctest --test-dir' - sidesteps relying on a generator-
;; specific "package" build-target name.

;;; Code:

(require 'seq)
(require 'cmake-wizard-project)
(require 'cmake-wizard-profile)
(require 'cmake-wizard-toolchain)
(require 'cmake-wizard-compile)

(defcustom cmake-wizard-cpack-executable "cpack"
  "Path to the cpack executable, or a name resolved via `exec-path'."
  :group 'cmake-wizard
  :type 'string
  :safe #'stringp)

(defcustom cmake-wizard-cpack-arguments nil
  "Extra literal arguments passed to every `cpack' invocation.
For example `(\"-G\" \"ZIP\")' to override the project's default
CPACK_GENERATOR for a single run."
  :group 'cmake-wizard
  :type '(repeat string)
  :safe (lambda (v) (and (listp v) (seq-every-p #'stringp v))))

(defun cmake-wizard-cpack-config-path (root)
  "Return the path to ROOT's generated CPackConfig.cmake."
  (let* ((root (cmake-wizard--canonical-root root))
         (profile (cmake-wizard-current-profile root))
         (toolchain (cmake-wizard-current-toolchain root)))
    (expand-file-name "CPackConfig.cmake"
                       (cmake-wizard-build-directory root profile toolchain))))

(defun cmake-wizard-package-command (root)
  "Return the shell command string that packages ROOT via cpack."
  (setq root (cmake-wizard-ensure-cmake-project root))
  (unless (cmake-wizard--configured-p root)
    (user-error "Project not configured yet; run `cmake-wizard-configure' first"))
  (let* ((profile (cmake-wizard-current-profile root))
         (toolchain (cmake-wizard-current-toolchain root))
         (build-dir (cmake-wizard-build-directory root profile toolchain))
         (config (cmake-wizard-cpack-config-path root))
         (args (append
                (list cmake-wizard-cpack-executable
                      "--config" (shell-quote-argument config)
                      "-B" (shell-quote-argument (directory-file-name build-dir)))
                cmake-wizard-cpack-arguments)))
    (mapconcat #'identity args " ")))

;;;###autoload
(defun cmake-wizard-package ()
  "Build distributable packages for the current project via `cpack'.
Signals a `user-error' with a hint if the project's CMakeLists.txt
never called `include(CPack)' (so no CPackConfig.cmake was ever
generated), rather than letting the user decode cpack's own, less
immediately visible \"Cannot find CPack config file\" error."
  (interactive)
  (let ((root (cmake-wizard-project-root)))
    (unless (file-exists-p (cmake-wizard-cpack-config-path root))
      (user-error
       "No CPackConfig.cmake in the build directory; does CMakeLists.txt call include(CPack)?"))
    (cmake-wizard--run root (cmake-wizard-package-command root))))

(provide 'cmake-wizard-cpack)
;;; cmake-wizard-cpack.el ends here
