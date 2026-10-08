;;; cmake-wizard-integration-test.el --- Cross-module integration tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercises properties that span more than one module: that two
;; different projects never share state, and that the thin `M-x
;; compile' front door (`cmake-wizard-compile') wires `compile-command'
;; correctly before deferring to real `compile'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'cmake-wizard)
(require 'cmake-wizard-test-util)

(ert-deftest cmake-wizard-test-two-projects-have-independent-profile-and-toolchain ()
  (cmake-wizard-test-with-temp-project root-a nil
    (cmake-wizard-test-with-temp-project root-b nil
      (cmake-wizard-set-profile root-a "Release")
      (cmake-wizard-set-toolchain root-a "gcc")
      ;; root-b was never touched: it must still see the defaults,
      ;; not root-a's selections.
      (should (equal (cmake-wizard-current-profile root-b) cmake-wizard-default-profile))
      (should (equal (cmake-wizard-current-toolchain root-b) cmake-wizard-default-toolchain))
      (should (equal (cmake-wizard-current-profile root-a) "Release"))
      (should (equal (cmake-wizard-current-toolchain root-a) "gcc")))))

(ert-deftest cmake-wizard-test-two-projects-build-in-different-directories ()
  (cmake-wizard-test-with-temp-project root-a nil
    (cmake-wizard-test-with-temp-project root-b nil
      (cmake-wizard-set-profile root-a "Release")
      (should-not (equal (cmake-wizard-build-directory
                           root-a (cmake-wizard-current-profile root-a)
                           (cmake-wizard-current-toolchain root-a))
                          (cmake-wizard-build-directory
                           root-b (cmake-wizard-current-profile root-b)
                           (cmake-wizard-current-toolchain root-b)))))))

(ert-deftest cmake-wizard-test-compile-sets-compile-command-and-calls-compile ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((file (expand-file-name "main.c" root))
          (compile-called nil))
      (write-region "int main(void) { return 0; }" nil file)
      (let ((buf (find-file-noselect file)))
        (unwind-protect
            (with-current-buffer buf
              (cl-letf (((symbol-function 'cmake-wizard-project-root) (lambda (&rest _) root))
                        ;; Mimic real `compile': read the buffer-local
                        ;; `compile-command' it's handed, rather than
                        ;; arguments threaded through `call-interactively'.
                        ((symbol-function 'compile)
                         (lambda () (interactive) (setq compile-called compile-command))))
                (cmake-wizard-compile))
              (should (equal compile-called (cmake-wizard-configure-command root))))
          (kill-buffer buf))))))

(provide 'cmake-wizard-integration-test)
;;; cmake-wizard-integration-test.el ends here
