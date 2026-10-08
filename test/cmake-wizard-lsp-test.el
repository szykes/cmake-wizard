;;; cmake-wizard-lsp-test.el --- Tests for cmake-wizard-lsp.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'cmake-wizard-lsp)
(require 'cmake-wizard-test-util)

(ert-deftest cmake-wizard-test-effective-client-none ()
  (let ((cmake-wizard-lsp-client 'none))
    (should-not (cmake-wizard-lsp--effective-client))))

(ert-deftest cmake-wizard-test-effective-client-eglot-when-available ()
  ;; eglot is built into Emacs 29+, always present on the targeted 31.1.
  (should (fboundp 'eglot-ensure))
  (let ((cmake-wizard-lsp-client 'eglot))
    (should (eq (cmake-wizard-lsp--effective-client) 'eglot))))

(ert-deftest cmake-wizard-test-effective-client-auto-prefers-eglot ()
  (let ((cmake-wizard-lsp-client 'auto))
    (should (eq (cmake-wizard-lsp--effective-client) 'eglot))))

(ert-deftest cmake-wizard-test-effective-client-lsp-mode-when-unavailable ()
  ;; lsp-mode isn't installed in the test environment.
  (should-not (fboundp 'lsp-deferred))
  (let ((cmake-wizard-lsp-client 'lsp-mode))
    (should-not (cmake-wizard-lsp--effective-client))))

(ert-deftest cmake-wizard-test-maybe-start-skips-when-no-compdb ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((called nil))
      (cl-letf (((symbol-function 'cmake-wizard-lsp-start-in-buffer) (lambda (&rest _) (setq called t))))
        (cmake-wizard-lsp-maybe-start root))
      (should-not called))))

(ert-deftest cmake-wizard-test-maybe-start-starts-client-in-project-buffers ()
  (cmake-wizard-test-with-temp-project root nil
    (write-region "[]" nil (cmake-wizard-compdb-link-path root))
    (let* ((file (expand-file-name "main.c" root))
           (started-buffers nil))
      (write-region "int main(void) { return 0; }" nil file)
      (let ((buf (find-file-noselect file)))
        (unwind-protect
            (progn
              (with-current-buffer buf (c-mode))
              (cl-letf (((symbol-function 'cmake-wizard-lsp-start-in-buffer)
                         (lambda (&optional b) (push (or b (current-buffer)) started-buffers))))
                (cmake-wizard-lsp-maybe-start root))
              (should (memq buf started-buffers)))
          (kill-buffer buf))))))

(provide 'cmake-wizard-lsp-test)
;;; cmake-wizard-lsp-test.el ends here
