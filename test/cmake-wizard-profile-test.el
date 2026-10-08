;;; cmake-wizard-profile-test.el --- Tests for cmake-wizard-profile.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'cmake-wizard-profile)
(require 'cmake-wizard-test-util)

(ert-deftest cmake-wizard-test-current-profile-defaults ()
  (cmake-wizard-test-with-temp-project root nil
    (should (equal (cmake-wizard-current-profile root) cmake-wizard-default-profile))))

(ert-deftest cmake-wizard-test-set-profile-updates-current ()
  (cmake-wizard-test-with-temp-project root nil
    (cmake-wizard-set-profile root "Release")
    (should (equal (cmake-wizard-current-profile root) "Release"))))

(ert-deftest cmake-wizard-test-set-profile-rejects-unknown-name ()
  (cmake-wizard-test-with-temp-project root nil
    (should-error (cmake-wizard-set-profile root "NoSuchProfile") :type 'user-error)))

(ert-deftest cmake-wizard-test-profile-cache-args-for-debug ()
  (cmake-wizard-test-with-temp-project root nil
    (cmake-wizard-set-profile root "Debug")
    (should (equal (cmake-wizard-profile-cache-args root) '("-DCMAKE_BUILD_TYPE=Debug")))))

(ert-deftest cmake-wizard-test-profile-cache-args-for-release ()
  (cmake-wizard-test-with-temp-project root nil
    (cmake-wizard-set-profile root "Release")
    (should (equal (cmake-wizard-profile-cache-args root) '("-DCMAKE_BUILD_TYPE=Release")))))

(ert-deftest cmake-wizard-test-profile-cache-args-respects-custom-profiles ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((cmake-wizard-profiles
           '(("Coverage" . (("CMAKE_BUILD_TYPE" . "Debug")
                             ("CMAKE_CXX_FLAGS" . "--coverage"))))))
      (cmake-wizard-set-profile root "Coverage")
      (should (equal (cmake-wizard-profile-cache-args root)
                      '("-DCMAKE_BUILD_TYPE=Debug" "-DCMAKE_CXX_FLAGS=--coverage"))))))

(ert-deftest cmake-wizard-test-select-profile-prompts-and-sets ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((default-directory root))
      (cl-letf (((symbol-function 'cmake-wizard-project-root) (lambda (&rest _) root))
                ((symbol-function 'completing-read) (lambda (&rest _) "RelWithDebInfo")))
        (cmake-wizard-select-profile)
        (should (equal (cmake-wizard-current-profile root) "RelWithDebInfo"))))))

(provide 'cmake-wizard-profile-test)
;;; cmake-wizard-profile-test.el ends here
