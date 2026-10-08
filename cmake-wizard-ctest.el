;;; cmake-wizard-ctest.el --- CTest integration for cmake-wizard -*- lexical-binding: t; -*-

;; Author: Ádám Szikes
;; Maintainer: Ádám Szikes
;; Version: 0.2.0
;; Package-Requires: ((emacs "31.1") (cmake-wizard "0.2.0"))
;; Keywords: tools, c, cmake
;; URL: https://github.com/szikes-adam/cmake-wizard

;;; Commentary:

;; Runs `ctest' against the current project's build directory.
;;
;; This deliberately shells out to `ctest --test-dir <build-dir>'
;; rather than `cmake --build <build-dir> --target test': that build
;; target's very *name* is generator-dependent (it's "test" with
;; Make/Ninja but "RUN_TESTS" with the Visual Studio or Xcode
;; generators), so going through `cmake --build' would silently stop
;; working the moment a user picks a different toolchain/generator.
;; `ctest --test-dir' reads the build directory's CTestTestfile.cmake
;; directly and works identically regardless of which native build
;; tool produced it.
;;
;; Requires the project to call `enable_testing()'/`include(CTest)'
;; and register tests with `add_test'; if it hasn't, `ctest' itself
;; reports "No tests were found!!!" (exit 0) - cmake-wizard does not
;; duplicate that check.

;;; Code:

(require 'seq)
(require 'cmake-wizard-project)
(require 'cmake-wizard-profile)
(require 'cmake-wizard-toolchain)
(require 'cmake-wizard-compile)

(defcustom cmake-wizard-ctest-executable "ctest"
  "Path to the ctest executable, or a name resolved via `exec-path'."
  :group 'cmake-wizard
  :type 'string
  :safe #'stringp)

(defcustom cmake-wizard-ctest-arguments '("--output-on-failure")
  "Extra literal arguments passed to every `ctest' invocation.
The default asks ctest to print a failing test's own output inline,
instead of only pointing at a log file to go read separately."
  :group 'cmake-wizard
  :type '(repeat string)
  :safe (lambda (v) (and (listp v) (seq-every-p #'stringp v))))

(defun cmake-wizard-test-command (root)
  "Return the shell command string that runs ROOT's tests via ctest."
  (setq root (cmake-wizard-ensure-cmake-project root))
  (unless (cmake-wizard--configured-p root)
    (user-error "Project not configured yet; run `cmake-wizard-configure' first"))
  (let* ((profile (cmake-wizard-current-profile root))
         (toolchain (cmake-wizard-current-toolchain root))
         (build-dir (cmake-wizard-build-directory root profile toolchain))
         (args (append
                (list cmake-wizard-ctest-executable
                      "--test-dir" (shell-quote-argument (directory-file-name build-dir)))
                cmake-wizard-ctest-arguments)))
    (mapconcat #'identity args " ")))

;;;###autoload
(defun cmake-wizard-test ()
  "Run the current project's tests via `ctest'."
  (interactive)
  (let ((root (cmake-wizard-project-root)))
    (cmake-wizard--run root (cmake-wizard-test-command root))))

(provide 'cmake-wizard-ctest)
;;; cmake-wizard-ctest.el ends here
