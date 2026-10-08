;;; cmake-wizard-toolchain-test.el --- Tests for cmake-wizard-toolchain.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'cmake-wizard-toolchain)
(require 'cmake-wizard-test-util)

(ert-deftest cmake-wizard-test-current-toolchain-defaults-to-clang ()
  (cmake-wizard-test-with-temp-project root nil
    (should (equal (cmake-wizard-current-toolchain root) "clang"))))

(ert-deftest cmake-wizard-test-toolchain-cache-args-default-is-clang ()
  (cmake-wizard-test-with-temp-project root nil
    (should (equal (cmake-wizard-toolchain-cache-args root)
                    '("-DCMAKE_C_COMPILER=clang" "-DCMAKE_CXX_COMPILER=clang++")))))

(ert-deftest cmake-wizard-test-toolchain-cache-args-for-gcc ()
  (cmake-wizard-test-with-temp-project root nil
    (cmake-wizard-set-toolchain root "gcc")
    (should (equal (cmake-wizard-toolchain-cache-args root)
                    '("-DCMAKE_C_COMPILER=gcc" "-DCMAKE_CXX_COMPILER=g++")))))

(ert-deftest cmake-wizard-test-toolchain-cache-args-include-toolchain-file ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((cmake-wizard-toolchains
           '(("arm64-cross" . (:toolchain-file "~/toolchains/arm64.cmake"
                                :extra-args ("-DCMAKE_SYSTEM_PROCESSOR=aarch64"))))))
      (cmake-wizard-set-toolchain root "arm64-cross")
      (let ((args (cmake-wizard-toolchain-cache-args root)))
        (should (string-match-p "\\`-DCMAKE_TOOLCHAIN_FILE=" (car args)))
        (should (member "-DCMAKE_SYSTEM_PROCESSOR=aarch64" args))))))

(ert-deftest cmake-wizard-test-set-toolchain-rejects-unknown-name ()
  (cmake-wizard-test-with-temp-project root nil
    (should-error (cmake-wizard-set-toolchain root "msvc-but-unregistered") :type 'user-error)))

(ert-deftest cmake-wizard-test-select-toolchain-prompts-and-sets ()
  (cmake-wizard-test-with-temp-project root nil
    (cl-letf (((symbol-function 'cmake-wizard-project-root) (lambda (&rest _) root))
              ((symbol-function 'completing-read) (lambda (&rest _) "gcc")))
      (cmake-wizard-select-toolchain)
      (should (equal (cmake-wizard-current-toolchain root) "gcc")))))

(provide 'cmake-wizard-toolchain-test)
;;; cmake-wizard-toolchain-test.el ends here
