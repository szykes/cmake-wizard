;;; cmake-wizard-compdb-test.el --- Tests for cmake-wizard-compdb.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'cmake-wizard-compdb)
(require 'cmake-wizard-test-util)

(ert-deftest cmake-wizard-test-compdb-source-path-is-in-build-dir ()
  (cmake-wizard-test-with-temp-project root nil
    (should (string-match-p "build/Debug-clang/compile_commands.json\\'"
                              (cmake-wizard-compdb-source-path root)))))

(ert-deftest cmake-wizard-test-compdb-link-path-is-in-project-root ()
  (cmake-wizard-test-with-temp-project root nil
    (should (equal (cmake-wizard-compdb-link-path root)
                    (expand-file-name "compile_commands.json" root)))))

(ert-deftest cmake-wizard-test-compdb-sync-noop-when-source-missing ()
  (cmake-wizard-test-with-temp-project root nil
    (cmake-wizard-compdb-sync root)
    (should-not (file-exists-p (cmake-wizard-compdb-link-path root)))))

(ert-deftest cmake-wizard-test-compdb-sync-symlinks-when-possible ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((source (cmake-wizard-compdb-source-path root)))
      (make-directory (file-name-directory source) t)
      (write-region "[]" nil source))
    (cmake-wizard-compdb-sync root)
    (let ((link (cmake-wizard-compdb-link-path root)))
      (should (file-exists-p link))
      (when (file-symlink-p link)
        (should (equal (file-truename link) (file-truename (cmake-wizard-compdb-source-path root))))))))

(ert-deftest cmake-wizard-test-compdb-sync-falls-back-to-copy-on-symlink-failure ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((source (cmake-wizard-compdb-source-path root)))
      (make-directory (file-name-directory source) t)
      (write-region "[]" nil source))
    (cl-letf (((symbol-function 'make-symbolic-link)
               (lambda (&rest _) (signal 'file-error '("no symlinks here")))))
      (cmake-wizard-compdb-sync root))
    (let ((link (cmake-wizard-compdb-link-path root)))
      (should (file-exists-p link))
      (should-not (file-symlink-p link))
      (should (equal (with-temp-buffer (insert-file-contents link) (buffer-string)) "[]")))))

(ert-deftest cmake-wizard-test-compdb-sync-replaces-stale-link ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((source (cmake-wizard-compdb-source-path root))
          (link (cmake-wizard-compdb-link-path root)))
      (make-directory (file-name-directory source) t)
      (write-region "[]" nil source)
      (write-region "stale" nil link)
      (cmake-wizard-compdb-sync root)
      (should (equal (with-temp-buffer (insert-file-contents (file-truename link)) (buffer-string))
                      "[]")))))

(provide 'cmake-wizard-compdb-test)
;;; cmake-wizard-compdb-test.el ends here
