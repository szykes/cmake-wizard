;;; cmake-wizard-project-test.el --- Tests for cmake-wizard-project.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'cmake-wizard-project)
(require 'cmake-wizard-test-util)

(ert-deftest cmake-wizard-test-canonical-root-adds-trailing-slash ()
  (should (string-suffix-p "/" (cmake-wizard--canonical-root "/tmp/foo"))))

(ert-deftest cmake-wizard-test-project-root-via-locate-dominating-file ()
  ;; projectile isn't installed in the test environment, so the
  ;; projectile branch is already skipped via `fboundp'; stub
  ;; `project-current' too so this exercises the final fallback.
  (cmake-wizard-test-with-temp-project root nil
    (let* ((subdir (expand-file-name "src/" root)))
      (make-directory subdir t)
      (with-temp-buffer
        (setq default-directory subdir)
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
          (should (equal (cmake-wizard-project-root) root)))))))

(ert-deftest cmake-wizard-test-project-root-signals-when-not-found ()
  (let ((default-directory (file-name-as-directory (make-temp-file "cmake-wizard-none" t))))
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
            (should-error (cmake-wizard-project-root) :type 'user-error)))
      (delete-directory default-directory t))))

(ert-deftest cmake-wizard-test-ensure-cmake-project-errors-without-cmakelists ()
  (let ((dir (file-name-as-directory (make-temp-file "cmake-wizard-empty" t))))
    (unwind-protect
        (should-error (cmake-wizard-ensure-cmake-project dir) :type 'user-error)
      (delete-directory dir t))))

(ert-deftest cmake-wizard-test-ensure-cmake-project-returns-canonical-root ()
  (cmake-wizard-test-with-temp-project root nil
    (should (equal (cmake-wizard-ensure-cmake-project root) root))))

(ert-deftest cmake-wizard-test-project-name-is-directory-basename ()
  (cmake-wizard-test-with-temp-project root nil
    (should (equal (cmake-wizard-project-name root)
                    (file-name-nondirectory (directory-file-name root))))))

(ert-deftest cmake-wizard-test-project-get-put-roundtrip ()
  (cmake-wizard-test-with-temp-project root nil
    (should (eq (cmake-wizard-project-get root :missing-key 'fallback) 'fallback))
    (cmake-wizard-project-put root :profile "Release")
    (should (equal (cmake-wizard-project-get root :profile) "Release"))
    (should (equal (cmake-wizard-project-get root :profile "Debug") "Release"))))

(ert-deftest cmake-wizard-test-build-directory-is-created-and-namespaced ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((dir (cmake-wizard-build-directory root "Debug" "clang")))
      (should (file-directory-p dir))
      (should (string-match-p "build/Debug-clang/\\'" dir)))))

(ert-deftest cmake-wizard-test-build-directory-function-is-customizable ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((cmake-wizard-build-directory-function
           (lambda (root profile toolchain)
             (file-name-concat root (format "out-%s-%s" profile toolchain)))))
      (let ((dir (cmake-wizard-build-directory root "Release" "gcc")))
        (should (file-directory-p dir))
        (should (string-match-p "out-Release-gcc/?\\'" dir))))))

(ert-deftest cmake-wizard-test-installed-cmake-version-parses-real-cmake ()
  (skip-unless (executable-find cmake-wizard-cmake-executable))
  (should (stringp (cmake-wizard-installed-cmake-version)))
  (should (string-match-p "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\'"
                           (cmake-wizard-installed-cmake-version))))

(ert-deftest cmake-wizard-test-ensure-supported-cmake-version-rejects-old ()
  (cl-letf (((symbol-function 'cmake-wizard-installed-cmake-version) (lambda () "3.9.0")))
    (should-error (cmake-wizard-ensure-supported-cmake-version) :type 'user-error)))

(ert-deftest cmake-wizard-test-ensure-supported-cmake-version-accepts-current ()
  (cl-letf (((symbol-function 'cmake-wizard-installed-cmake-version) (lambda () "4.4.4")))
    (should (equal (cmake-wizard-ensure-supported-cmake-version) "4.4.4"))))

(ert-deftest cmake-wizard-test-ensure-supported-cmake-version-errors-when-missing ()
  (cl-letf (((symbol-function 'cmake-wizard-installed-cmake-version) (lambda () nil)))
    (should-error (cmake-wizard-ensure-supported-cmake-version) :type 'user-error)))

(ert-deftest cmake-wizard-test-check-cmake-lists-minimum-version-warns-on-old ()
  (cmake-wizard-test-with-temp-project root "cmake_minimum_required(VERSION 3.10)\n"
    (let ((messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
        (cmake-wizard-check-cmake-lists-minimum-version root))
      (should (seq-some (lambda (m) (string-match-p "3.10" m)) messages)))))

(ert-deftest cmake-wizard-test-check-cmake-lists-minimum-version-silent-on-current ()
  (cmake-wizard-test-with-temp-project root "cmake_minimum_required(VERSION 4.0)\n"
    (let ((messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
        (cmake-wizard-check-cmake-lists-minimum-version root))
      (should (null messages)))))

(provide 'cmake-wizard-project-test)
;;; cmake-wizard-project-test.el ends here
