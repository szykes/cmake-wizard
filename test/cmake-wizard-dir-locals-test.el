;;; cmake-wizard-dir-locals-test.el --- Tests for cmake-wizard-dir-locals.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'cmake-wizard-dir-locals)
(require 'cmake-wizard-test-util)

(ert-deftest cmake-wizard-test-dir-locals-variables-are-marked-safe ()
  (should (eq (get 'cmake-wizard-default-profile 'safe-local-variable) #'stringp))
  (should (functionp (get 'cmake-wizard-cmake-global-arguments 'safe-local-variable))))

(ert-deftest cmake-wizard-test-read-dir-locals-returns-nil-when-absent ()
  (cmake-wizard-test-with-temp-project root nil
    (should (null (cmake-wizard--read-dir-locals root)))))

(ert-deftest cmake-wizard-test-write-then-read-dir-locals-roundtrip ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((alist '((nil . ((compile-command . "cmake --build build"))))))
      (cmake-wizard--write-dir-locals root alist)
      (should (equal (cmake-wizard--read-dir-locals root) alist)))))

(ert-deftest cmake-wizard-test-dir-locals-merge-adds-new-mode-class ()
  (let ((merged (cmake-wizard--dir-locals-merge nil nil '((foo . 1)))))
    (should (equal merged '((nil . ((foo . 1))))))))

(ert-deftest cmake-wizard-test-dir-locals-merge-overwrites-conflicting-key-only ()
  (let* ((existing '((nil . ((foo . "old") (bar . "kept")))))
         (merged (cmake-wizard--dir-locals-merge existing nil '((foo . "new")))))
    (should (equal (cdr (assoc nil merged)) '((foo . "new") (bar . "kept"))))))

(ert-deftest cmake-wizard-test-dir-locals-merge-preserves-other-mode-classes ()
  (let* ((existing '((c++-mode . ((indent-tabs-mode . nil)))))
         (merged (cmake-wizard--dir-locals-merge existing nil '((foo . 1)))))
    (should (equal (cdr (assoc 'c++-mode merged)) '((indent-tabs-mode . nil))))
    (should (equal (cdr (assoc nil merged)) '((foo . 1))))))

(ert-deftest cmake-wizard-test-save-settings-to-dir-locals-writes-expected-values ()
  (cmake-wizard-test-with-temp-project root nil
    (cmake-wizard-set-profile root "Release")
    (cmake-wizard-set-toolchain root "gcc")
    (cl-letf (((symbol-function 'cmake-wizard-project-root) (lambda (&rest _) root)))
      (cmake-wizard-save-settings-to-dir-locals))
    (let* ((alist (cmake-wizard--read-dir-locals root))
           (bindings (cdr (assoc nil alist))))
      (should (equal (cdr (assq 'cmake-wizard-default-profile bindings)) "Release"))
      (should (equal (cdr (assq 'cmake-wizard-default-toolchain bindings)) "gcc"))
      (should (member "-DCMAKE_C_COMPILER=gcc"
                        (split-string-and-unquote (cdr (assq 'compile-command bindings))))))))

(ert-deftest cmake-wizard-test-save-settings-preserves-existing-mode-class ()
  (cmake-wizard-test-with-temp-project root nil
    (cmake-wizard--write-dir-locals root '((c++-mode . ((indent-tabs-mode . nil)))))
    (cl-letf (((symbol-function 'cmake-wizard-project-root) (lambda (&rest _) root)))
      (cmake-wizard-save-settings-to-dir-locals))
    (let ((alist (cmake-wizard--read-dir-locals root)))
      (should (equal (cdr (assoc 'c++-mode alist)) '((indent-tabs-mode . nil)))))))

(provide 'cmake-wizard-dir-locals-test)
;;; cmake-wizard-dir-locals-test.el ends here
