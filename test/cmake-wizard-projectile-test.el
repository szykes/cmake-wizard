;;; cmake-wizard-projectile-test.el --- Tests for cmake-wizard-projectile.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'cmake-wizard-projectile)
(require 'cmake-wizard-test-util)

(ert-deftest cmake-wizard-test-projectile-configure-command-matches-engine ()
  (cmake-wizard-test-with-temp-project root nil
    (cl-letf (((symbol-function 'cmake-wizard-project-root) (lambda (&rest _) root)))
      (should (equal (cmake-wizard-projectile--configure-command)
                      (cmake-wizard-configure-command root))))))

(ert-deftest cmake-wizard-test-projectile-compile-command-matches-engine ()
  (cmake-wizard-test-with-temp-project root nil
    (cl-letf (((symbol-function 'cmake-wizard-project-root) (lambda (&rest _) root)))
      (should (equal (cmake-wizard-projectile--compile-command)
                      (cmake-wizard-build-command root))))))

(ert-deftest cmake-wizard-test-projectile-setup-is-noop-without-projectile ()
  ;; projectile isn't installed in the test environment; setup must not error.
  (should-not (fboundp 'projectile-register-project-type))
  (should (null (cmake-wizard-projectile-setup))))

(ert-deftest cmake-wizard-test-projectile-setup-registers-cmake-type ()
  (let (registered-args)
    (cl-letf (((symbol-function 'projectile-register-project-type)
               (lambda (&rest args) (setq registered-args args))))
      ;; `fboundp' now sees our stub, so setup should call it.
      (cmake-wizard-projectile-setup)
      (should (eq (nth 0 registered-args) 'cmake-wizard))
      (should (equal (nth 1 registered-args) '("CMakeLists.txt")))
      (should (eq (plist-get (cddr registered-args) :configure)
                   #'cmake-wizard-projectile--configure-command))
      (should (eq (plist-get (cddr registered-args) :compile)
                   #'cmake-wizard-projectile--compile-command)))))

(provide 'cmake-wizard-projectile-test)
;;; cmake-wizard-projectile-test.el ends here
