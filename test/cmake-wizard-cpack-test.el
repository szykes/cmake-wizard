;;; cmake-wizard-cpack-test.el --- Tests for cmake-wizard-cpack.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'cmake-wizard-cpack)
(require 'cmake-wizard-test-util)

(ert-deftest cmake-wizard-test-cpack-config-path-is-in-build-dir ()
  (cmake-wizard-test-with-temp-project root nil
    (should (string-match-p "build/Debug-clang/CPackConfig\\.cmake\\'"
                              (cmake-wizard-cpack-config-path root)))))

(ert-deftest cmake-wizard-test-package-command-errors-when-unconfigured ()
  (cmake-wizard-test-with-temp-project root nil
    (should-error (cmake-wizard-package-command root) :type 'user-error)))

(ert-deftest cmake-wizard-test-package-command-uses-config-not-build-target ()
  "Must use `cpack --config', not `cmake --build --target package':
CPack's own \"package\" target name happens to be generator-agnostic
today, but going through `cpack' directly also works when a project
has no native build target for it at all (e.g. header-only configs)."
  (cmake-wizard-test-with-temp-project root nil
    (let ((build-dir (cmake-wizard-build-directory
                       root (cmake-wizard-current-profile root)
                       (cmake-wizard-current-toolchain root))))
      (write-region "" nil (expand-file-name "CMakeCache.txt" build-dir)))
    (let ((args (split-string-and-unquote (cmake-wizard-package-command root))))
      (should (equal (car args) cmake-wizard-cpack-executable))
      (should (member "--config" args))
      (should-not (member "--target" args)))))

(ert-deftest cmake-wizard-test-package-command-honors-custom-arguments ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((build-dir (cmake-wizard-build-directory
                       root (cmake-wizard-current-profile root)
                       (cmake-wizard-current-toolchain root))))
      (write-region "" nil (expand-file-name "CMakeCache.txt" build-dir)))
    (let ((cmake-wizard-cpack-arguments '("-G" "ZIP")))
      (let ((args (split-string-and-unquote (cmake-wizard-package-command root))))
        (should (member "ZIP" args))))))

(ert-deftest cmake-wizard-test-package-errors-with-hint-when-no-cpack-config ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((build-dir (cmake-wizard-build-directory
                       root (cmake-wizard-current-profile root)
                       (cmake-wizard-current-toolchain root))))
      ;; configured, but CMakeLists.txt never called include(CPack):
      ;; no CPackConfig.cmake was ever generated.
      (write-region "" nil (expand-file-name "CMakeCache.txt" build-dir)))
    (cl-letf (((symbol-function 'cmake-wizard-project-root) (lambda (&rest _) root)))
      (should-error (cmake-wizard-package) :type 'user-error))))

(ert-deftest cmake-wizard-test-package-dispatches-through-engine-when-configured ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((build-dir (cmake-wizard-build-directory
                       root (cmake-wizard-current-profile root)
                       (cmake-wizard-current-toolchain root))))
      (write-region "" nil (expand-file-name "CMakeCache.txt" build-dir))
      (write-region "" nil (expand-file-name "CPackConfig.cmake" build-dir)))
    (let (ran)
      (cl-letf (((symbol-function 'cmake-wizard-project-root) (lambda (&rest _) root))
                ((symbol-function 'cmake-wizard--run)
                 (lambda (r command) (setq ran (cons r command)) (generate-new-buffer "*fake*"))))
        (cmake-wizard-package)
        (should (equal (car ran) root))
        (should (string-match-p "\\`cpack " (cdr ran)))))))

(provide 'cmake-wizard-cpack-test)
;;; cmake-wizard-cpack-test.el ends here
