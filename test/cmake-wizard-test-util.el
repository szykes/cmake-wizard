;;; cmake-wizard-test-util.el --- Shared helpers for cmake-wizard's ERT suite -*- lexical-binding: t; -*-

;;; Commentary:

;; Not a cmake-wizard module; not autoloaded, not part of the package.
;; `test/*-test.el' files `require' this for temp-project scaffolding
;; and so they don't each re-invent process/package mocking.

;;; Code:

(require 'cl-lib)

(defun cmake-wizard-test--make-temp-project (&optional cmakelists-content)
  "Create a temp directory with a CMakeLists.txt and return its canonical root."
  (let ((root (file-name-as-directory (make-temp-file "cmake-wizard-test" t))))
    (with-temp-file (expand-file-name "CMakeLists.txt" root)
      (insert (or cmakelists-content
                  "cmake_minimum_required(VERSION 4.0)\nproject(test C)\n")))
    root))

(defmacro cmake-wizard-test-with-temp-project (root-var cmakelists-content &rest body)
  "Bind ROOT-VAR to a fresh temp project root around BODY, then delete it."
  (declare (indent 2))
  `(let ((,root-var (cmake-wizard-test--make-temp-project ,cmakelists-content)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,root-var t))))

(provide 'cmake-wizard-test-util)
;;; cmake-wizard-test-util.el ends here
