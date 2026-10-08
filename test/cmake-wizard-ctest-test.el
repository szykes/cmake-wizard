;;; cmake-wizard-ctest-test.el --- Tests for cmake-wizard-ctest.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'cmake-wizard-ctest)
(require 'cmake-wizard-test-util)

(ert-deftest cmake-wizard-test-ctest-command-errors-when-unconfigured ()
  (cmake-wizard-test-with-temp-project root nil
    (should-error (cmake-wizard-test-command root) :type 'user-error)))

(ert-deftest cmake-wizard-test-ctest-command-uses-test-dir-not-build-target ()
  "Must use `ctest --test-dir', not `cmake --build --target test':
that build-target's name is generator-dependent (\"test\" on Make/Ninja,
\"RUN_TESTS\" on Visual Studio/Xcode) while `ctest --test-dir' is not."
  (cmake-wizard-test-with-temp-project root nil
    (let ((build-dir (cmake-wizard-build-directory
                       root (cmake-wizard-current-profile root)
                       (cmake-wizard-current-toolchain root))))
      (write-region "" nil (expand-file-name "CMakeCache.txt" build-dir)))
    (let ((args (split-string-and-unquote (cmake-wizard-test-command root))))
      (should (equal (car args) cmake-wizard-ctest-executable))
      (should (member "--test-dir" args))
      (should-not (member "--build" args))
      (should-not (member "--target" args)))))

(ert-deftest cmake-wizard-test-ctest-command-includes-output-on-failure-by-default ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((build-dir (cmake-wizard-build-directory
                       root (cmake-wizard-current-profile root)
                       (cmake-wizard-current-toolchain root))))
      (write-region "" nil (expand-file-name "CMakeCache.txt" build-dir)))
    (should (member "--output-on-failure"
                      (split-string-and-unquote (cmake-wizard-test-command root))))))

(ert-deftest cmake-wizard-test-ctest-command-honors-custom-arguments ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((build-dir (cmake-wizard-build-directory
                       root (cmake-wizard-current-profile root)
                       (cmake-wizard-current-toolchain root))))
      (write-region "" nil (expand-file-name "CMakeCache.txt" build-dir)))
    (let ((cmake-wizard-ctest-arguments '("-j" "4" "-R" "unit")))
      (let ((args (split-string-and-unquote (cmake-wizard-test-command root))))
        (should (member "-j" args))
        (should (member "unit" args))))))

(ert-deftest cmake-wizard-test-run-tests-dispatches-through-engine ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((build-dir (cmake-wizard-build-directory
                       root (cmake-wizard-current-profile root)
                       (cmake-wizard-current-toolchain root))))
      (write-region "" nil (expand-file-name "CMakeCache.txt" build-dir)))
    (let (ran)
      (cl-letf (((symbol-function 'cmake-wizard-project-root) (lambda (&rest _) root))
                ((symbol-function 'cmake-wizard--run)
                 (lambda (r command) (setq ran (cons r command)) (generate-new-buffer "*fake*"))))
        (cmake-wizard-test)
        (should (equal (car ran) root))
        (should (string-match-p "\\`ctest " (cdr ran)))))))

(provide 'cmake-wizard-ctest-test)
;;; cmake-wizard-ctest-test.el ends here
