;;; cmake-wizard-e2e-test.el --- Real end-to-end cmake-wizard test -*- lexical-binding: t; -*-

;;; Commentary:

;; Unlike the other test files, this one invokes a real `cmake' and a
;; real C compiler against test/fixtures/sample-project, exercising
;; the full configure -> build -> compile_commands.json pipeline
;; (REQ-2.2, REQ-2.3) exactly as a user would.  It skips itself if
;; `cmake' or a C compiler isn't available on `PATH'.

;;; Code:

(require 'ert)
(require 'cmake-wizard)

(defconst cmake-wizard-e2e--fixture-root
  (file-name-as-directory
   (expand-file-name "fixtures/sample-project"
                      (file-name-directory (or load-file-name buffer-file-name))))
  "The sample CMake project used for the end-to-end test.")

(defun cmake-wizard-e2e--wait-for (buffer &optional timeout)
  "Block until BUFFER's process exits or TIMEOUT (seconds, default 120) passes."
  (let ((deadline (+ (float-time) (or timeout 120))))
    (while (and (get-buffer-process buffer)
                (process-live-p (get-buffer-process buffer))
                (< (float-time) deadline))
      (accept-process-output (get-buffer-process buffer) 1))))

(defun cmake-wizard-e2e--executable-path (root profile toolchain)
  "Return the expected path of the fixture's built executable."
  (expand-file-name (if (eq system-type 'windows-nt) "sample.exe" "sample")
                     (cmake-wizard-build-directory root profile toolchain)))

(ert-deftest cmake-wizard-test-e2e-configure-build-and-wire-compdb ()
  (skip-unless (and (executable-find cmake-wizard-cmake-executable)
                     (executable-find "clang")))
  (let* ((root cmake-wizard-e2e--fixture-root)
         (cmake-wizard-default-toolchain "clang")
         (cmake-wizard-default-profile "Debug"))
    (unwind-protect
        (progn
          (cmake-wizard-e2e--wait-for (cmake-wizard--run root (cmake-wizard-configure-command root)))
          (should (cmake-wizard--configured-p root))
          (cmake-wizard-e2e--wait-for (cmake-wizard--run root (cmake-wizard-build-command root)))
          (should (file-exists-p (cmake-wizard-e2e--executable-path root "Debug" "clang")))
          (cmake-wizard-compdb-sync root)
          (should (file-exists-p (cmake-wizard-compdb-link-path root))))
      (ignore-errors (delete-directory (expand-file-name "build" root) t))
      (ignore-errors (delete-file (cmake-wizard-compdb-link-path root))))))

(ert-deftest cmake-wizard-test-e2e-ctest-and-cpack ()
  (skip-unless (and (executable-find cmake-wizard-cmake-executable)
                     (executable-find "clang")
                     (executable-find cmake-wizard-ctest-executable)
                     (executable-find cmake-wizard-cpack-executable)))
  (let* ((root cmake-wizard-e2e--fixture-root)
         (cmake-wizard-default-toolchain "clang")
         (cmake-wizard-default-profile "Debug"))
    (unwind-protect
        (progn
          (cmake-wizard-e2e--wait-for (cmake-wizard--run root (cmake-wizard-configure-command root)))
          (should (cmake-wizard--configured-p root))
          (cmake-wizard-e2e--wait-for (cmake-wizard--run root (cmake-wizard-build-command root)))
          (should (file-exists-p (cmake-wizard-e2e--executable-path root "Debug" "clang")))
          ;; `sample_runs' (CMakeLists.txt) just execs the built binary,
          ;; so a real `ctest' run through our command must pass.
          (let* ((buf (cmake-wizard--run root (cmake-wizard-test-command root)))
                 (proc (get-buffer-process buf)))
            (cmake-wizard-e2e--wait-for buf)
            (should (zerop (process-exit-status proc))))
          (should (file-exists-p (cmake-wizard-cpack-config-path root)))
          (let* ((buf (cmake-wizard--run root (cmake-wizard-package-command root)))
                 (proc (get-buffer-process buf)))
            (cmake-wizard-e2e--wait-for buf)
            (should (zerop (process-exit-status proc)))))
      (ignore-errors (delete-directory (expand-file-name "build" root) t))
      (ignore-errors (delete-file (cmake-wizard-compdb-link-path root))))))

(provide 'cmake-wizard-e2e-test)
;;; cmake-wizard-e2e-test.el ends here
