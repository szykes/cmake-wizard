;;; cmake-wizard-compile-test.el --- Tests for cmake-wizard-compile.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'compile)
(require 'cmake-wizard-compile)
(require 'cmake-wizard-test-util)

(ert-deftest cmake-wizard-test-configure-command-contains-required-flags ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((args (split-string-and-unquote (cmake-wizard-configure-command root))))
      (should (member "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON" args))
      (should (member "-DCMAKE_C_COMPILER=clang" args))
      (should (member "-DCMAKE_CXX_COMPILER=clang++" args))
      (should (member "-DCMAKE_BUILD_TYPE=Debug" args))
      (should (member "-S" args))
      (should (member "-B" args))
      (should (string-match-p "build/Debug-clang" (nth (1+ (seq-position args "-B")) args))))))

(ert-deftest cmake-wizard-test-configure-command-honors-global-arguments ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((cmake-wizard-cmake-global-arguments '("-DBUILD_TESTING=ON")))
      (should (member "-DBUILD_TESTING=ON"
                        (split-string-and-unquote (cmake-wizard-configure-command root)))))))

(ert-deftest cmake-wizard-test-configure-command-errors-without-cmakelists ()
  (let ((dir (file-name-as-directory (make-temp-file "cmake-wizard-nocl" t))))
    (unwind-protect
        (should-error (cmake-wizard-configure-command dir) :type 'user-error)
      (delete-directory dir t))))

(ert-deftest cmake-wizard-test-build-command-without-target ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((cmd (cmake-wizard-build-command root)))
      (should (string-match-p "--build" cmd))
      (should-not (string-match-p "--target" cmd)))))

(ert-deftest cmake-wizard-test-build-command-with-target ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((cmd (cmake-wizard-build-command root "my-target")))
      (should (string-match-p "--target my-target" cmd)))))

(ert-deftest cmake-wizard-test-configured-p-false-before-configure ()
  (cmake-wizard-test-with-temp-project root nil
    (should-not (cmake-wizard--configured-p root))))

(ert-deftest cmake-wizard-test-configured-p-true-once-cache-exists ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((build-dir (cmake-wizard-build-directory
                       root (cmake-wizard-current-profile root)
                       (cmake-wizard-current-toolchain root))))
      (write-region "" nil (expand-file-name "CMakeCache.txt" build-dir)))
    (should (cmake-wizard--configured-p root))))

(ert-deftest cmake-wizard-test-build-targets-errors-when-unconfigured ()
  (cmake-wizard-test-with-temp-project root nil
    (should-error (cmake-wizard-build-targets root) :type 'user-error)))

(ert-deftest cmake-wizard-test-build-targets-parses-cmake-help-output ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((build-dir (cmake-wizard-build-directory
                       root (cmake-wizard-current-profile root)
                       (cmake-wizard-current-toolchain root))))
      (write-region "" nil (expand-file-name "CMakeCache.txt" build-dir)))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _)
                 (insert "... all (the default if no target is provided)\n")
                 (insert "... clean\n")
                 (insert "... my-app\n")
                 0)))
      (should (equal (cmake-wizard-build-targets root) '("all" "clean" "my-app"))))))

(ert-deftest cmake-wizard-test-compilation-regexp-is-registered ()
  (should (assq 'cmake-wizard compilation-error-regexp-alist-alist))
  (should (memq 'cmake-wizard compilation-error-regexp-alist)))

(ert-deftest cmake-wizard-test-compilation-regexp-matches-cmake-error ()
  (let* ((entry (assq 'cmake-wizard compilation-error-regexp-alist-alist))
         (regexp (nth 1 entry)))
    (with-temp-buffer
      (insert "CMake Error at CMakeLists.txt:12 (message):\n  boom\n")
      (goto-char (point-min))
      (should (re-search-forward regexp nil t))
      (should (equal (match-string 1) "CMakeLists.txt"))
      (should (equal (match-string 2) "12")))))

(ert-deftest cmake-wizard-test-run-starts-compilation-in-dedicated-buffer ()
  (cmake-wizard-test-with-temp-project root nil
    (let (started-command started-dir)
      (cl-letf (((symbol-function 'compilation-start)
                 (lambda (command &optional _mode name-function &rest _)
                   (setq started-command command)
                   (setq started-dir default-directory)
                   (get-buffer-create (funcall name-function nil)))))
        (cmake-wizard--run root "echo hi")
        (should (equal started-command "echo hi"))
        (should (equal started-dir root))
        (should (get-buffer (cmake-wizard--buffer-name root)))))))

(ert-deftest cmake-wizard-test-finish-handler-runs-hook-for-configure-command ()
  (let ((ran nil))
    (with-temp-buffer
      (setq-local compile-command "cmake -S . -B build -DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
      (let ((default-directory (expand-file-name default-directory))
            (cmake-wizard-configure-success-functions
             (list (lambda (_root) (setq ran t)))))
        (cmake-wizard--compilation-finish-handler (current-buffer) "finished\n")))
    (should ran)))

(ert-deftest cmake-wizard-test-finish-handler-ignores-non-configure-command ()
  (let ((ran nil))
    (with-temp-buffer
      (setq-local compile-command "make all")
      (let ((cmake-wizard-configure-success-functions
             (list (lambda (_root) (setq ran t)))))
        (cmake-wizard--compilation-finish-handler (current-buffer) "finished\n")))
    (should-not ran)))

(ert-deftest cmake-wizard-test-finish-handler-ignores-failed-compile ()
  (let ((ran nil))
    (with-temp-buffer
      (setq-local compile-command "cmake -S . -B build -DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
      (let ((cmake-wizard-configure-success-functions
             (list (lambda (_root) (setq ran t)))))
        (cmake-wizard--compilation-finish-handler (current-buffer) "exited abnormally")))
    (should-not ran)))

(ert-deftest cmake-wizard-test-build-auto-configures-when-unconfigured ()
  (cmake-wizard-test-with-temp-project root nil
    (let (ran-commands)
      (cl-letf (((symbol-function 'cmake-wizard-project-root) (lambda (&rest _) root))
                ((symbol-function 'cmake-wizard-ensure-supported-cmake-version) (lambda () "4.4.4"))
                ((symbol-function 'cmake-wizard--run)
                 (lambda (r command)
                   (push command ran-commands)
                   (let ((buf (generate-new-buffer (cmake-wizard--buffer-name r))))
                     (with-current-buffer buf (setq default-directory r))
                     buf))))
        (cmake-wizard-build)
        ;; Only the configure has run so far - the build is deferred
        ;; until the configure buffer reports success.
        (should (= (length ran-commands) 1))
        (should (string-match-p "CMAKE_EXPORT_COMPILE_COMMANDS=ON" (car ran-commands)))
        (let ((buf (get-buffer (cmake-wizard--buffer-name root))))
          (with-current-buffer buf
            (run-hook-with-args 'compilation-finish-functions buf "finished\n")))
        (should (= (length ran-commands) 2))
        (should (string-match-p "--build" (car ran-commands)))))))

(ert-deftest cmake-wizard-test-build-chain-does-not-leak-into-public-hook ()
  "A failed configure must not leave anything on the global success hook
\(regression test: see docs/low-level.md\)."
  (cmake-wizard-test-with-temp-project root nil
    (let ((before (length cmake-wizard-configure-success-functions)))
      (cl-letf (((symbol-function 'cmake-wizard-project-root) (lambda (&rest _) root))
                ((symbol-function 'cmake-wizard-ensure-supported-cmake-version) (lambda () "4.4.4"))
                ((symbol-function 'cmake-wizard--run)
                 (lambda (r command)
                   (ignore command)
                   (let ((buf (generate-new-buffer (cmake-wizard--buffer-name r))))
                     (with-current-buffer buf (setq default-directory r))
                     buf))))
        (cmake-wizard-build)
        (let ((buf (get-buffer (cmake-wizard--buffer-name root))))
          (with-current-buffer buf
            ;; Simulate a FAILED configure.
            (run-hook-with-args 'compilation-finish-functions buf "exited abnormally with code 1\n"))))
      (should (= (length cmake-wizard-configure-success-functions) before)))))

(ert-deftest cmake-wizard-test-build-when-already-configured-skips-configure ()
  (cmake-wizard-test-with-temp-project root nil
    (let ((build-dir (cmake-wizard-build-directory
                       root (cmake-wizard-current-profile root)
                       (cmake-wizard-current-toolchain root))))
      (write-region "" nil (expand-file-name "CMakeCache.txt" build-dir)))
    (let (ran-commands)
      (cl-letf (((symbol-function 'cmake-wizard-project-root) (lambda (&rest _) root))
                ((symbol-function 'cmake-wizard--run)
                 (lambda (_r command) (push command ran-commands) (generate-new-buffer "*fake*"))))
        (cmake-wizard-build)
        (should (= (length ran-commands) 1))
        (should (string-match-p "--build" (car ran-commands)))
        (should-not (string-match-p "CMAKE_EXPORT_COMPILE_COMMANDS" (car ran-commands)))))))

(ert-deftest cmake-wizard-test-two-projects-with-same-basename-get-distinct-buffers ()
  (cmake-wizard-test-with-temp-project parent-a nil
    (cmake-wizard-test-with-temp-project parent-b nil
      (let ((root-a (file-name-as-directory (expand-file-name "same-name" parent-a)))
            (root-b (file-name-as-directory (expand-file-name "same-name" parent-b))))
        (make-directory root-a t)
        (make-directory root-b t)
        (should (equal (cmake-wizard-project-name root-a) (cmake-wizard-project-name root-b)))
        (should-not (equal (cmake-wizard--buffer-name root-a) (cmake-wizard--buffer-name root-b)))))))

(provide 'cmake-wizard-compile-test)
;;; cmake-wizard-compile-test.el ends here
