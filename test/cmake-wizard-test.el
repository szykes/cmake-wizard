;;; cmake-wizard-test.el --- Tests for the cmake-wizard.el facade -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'cmake-wizard)
(require 'cmake-wizard-test-util)

(ert-deftest cmake-wizard-test-mode-map-has-core-bindings ()
  (should (eq (lookup-key cmake-wizard-mode-map (kbd "C-c c c")) #'cmake-wizard-configure))
  (should (eq (lookup-key cmake-wizard-mode-map (kbd "C-c c b")) #'cmake-wizard-build))
  (should (eq (lookup-key cmake-wizard-mode-map (kbd "C-c c t")) #'cmake-wizard-test))
  (should (eq (lookup-key cmake-wizard-mode-map (kbd "C-c c P")) #'cmake-wizard-package))
  (should (eq (lookup-key cmake-wizard-mode-map (kbd "C-c c p")) #'cmake-wizard-select-profile))
  (should (eq (lookup-key cmake-wizard-mode-map (kbd "C-c c k")) #'cmake-wizard-select-toolchain)))

(ert-deftest cmake-wizard-test-maybe-enable-turns-mode-on-in-cmake-project ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((file (expand-file-name "main.c" root)))
      (write-region "int main(void) { return 0; }" nil file)
      (let ((buf (find-file-noselect file)))
        (unwind-protect
            (with-current-buffer buf
              (cmake-wizard-mode -1)
              (cmake-wizard--maybe-enable)
              (should cmake-wizard-mode))
          (kill-buffer buf))))))

(ert-deftest cmake-wizard-test-maybe-enable-stays-off-outside-project ()
  (let ((dir (file-name-as-directory (make-temp-file "cmake-wizard-outside" t))))
    (unwind-protect
        (let* ((file (expand-file-name "scratch.c" dir)))
          (write-region "" nil file)
          (let ((buf (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buf
                  (cmake-wizard-mode -1)
                  (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
                    (cmake-wizard--maybe-enable))
                  (should-not cmake-wizard-mode))
              (kill-buffer buf))))
      (delete-directory dir t))))

(provide 'cmake-wizard-test)
;;; cmake-wizard-test.el ends here
