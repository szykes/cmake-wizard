;;; cmake-wizard-project.el --- Project discovery and state for cmake-wizard -*- lexical-binding: t; -*-

;; Author: Ádám Szikes
;; Maintainer: Ádám Szikes
;; Version: 0.2.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: tools, c, cmake
;; URL: https://github.com/szikes-adam/cmake-wizard

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Project-root discovery (projectile -> project.el -> plain directory
;; walk), per-project-root state storage shared by every cmake-wizard
;; module, out-of-source build-directory layout, and the cmake / CMake
;; version sanity checks that keep this package scoped to CMake 4.0+.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)

(defgroup cmake-wizard nil
  "VSCode-CMake-Tools-like CMake workflow for Emacs."
  :group 'tools
  :link '(url-link :tag "GitHub" "https://github.com/szikes-adam/cmake-wizard"))

(defcustom cmake-wizard-cmake-executable "cmake"
  "Path to the cmake executable, or a name resolved via `exec-path'."
  :group 'cmake-wizard
  :type 'string
  :safe #'stringp)

(defcustom cmake-wizard-minimum-cmake-version "4.0.0"
  "Lowest cmake version this package is designed to support."
  :group 'cmake-wizard
  :type 'string)

(defcustom cmake-wizard-cmake-lists-filename "CMakeLists.txt"
  "Filename that marks a directory as a CMake project root."
  :group 'cmake-wizard
  :type 'string)

(defcustom cmake-wizard-build-directory-name "build"
  "Name of the directory (under the project root) that holds build trees."
  :group 'cmake-wizard
  :type 'string
  :safe #'stringp)

(defcustom cmake-wizard-build-directory-function
  #'cmake-wizard--default-build-directory
  "Function returning the build directory for a project, profile and toolchain.
Called with (ROOT PROFILE-NAME TOOLCHAIN-NAME); must return a directory path.
The default lays out one build tree per profile+toolchain combination, e.g.
\"<root>/build/Debug-clang/\", so switching toolchains never reuses a
CMakeCache.txt configured for a different compiler."
  :group 'cmake-wizard
  :type 'function)

(defvar cmake-wizard--project-state (make-hash-table :test #'equal)
  "Per-project-root plist storage shared by all cmake-wizard modules.
Keys are canonicalized project roots; values are plists such as
\(:profile \"Debug\" :toolchain \"clang\"\).")

(defun cmake-wizard--canonical-root (root)
  "Return a canonicalized, slash-terminated absolute path for ROOT."
  (file-name-as-directory (expand-file-name root)))

(defun cmake-wizard--project-root-via-projectile ()
  "Return the project root via `projectile', if available and found."
  (when (fboundp 'projectile-project-root)
    (ignore-errors (projectile-project-root))))

(defun cmake-wizard--project-root-via-project.el ()
  "Return the project root via the built-in `project.el', if found."
  (when-let* ((proj (project-current nil default-directory)))
    (project-root proj)))

(defun cmake-wizard-project-root (&optional buffer)
  "Return the CMake project root for BUFFER (default: current buffer).
Tries, in order: `projectile', `project.el', then walking up directories
looking for `cmake-wizard-cmake-lists-filename'.  Signals a `user-error'
when none of them find a root."
  (let* ((buffer (or buffer (current-buffer)))
         (default-directory (buffer-local-value 'default-directory buffer))
         (root (or (cmake-wizard--project-root-via-projectile)
                   (cmake-wizard--project-root-via-project.el)
                   (locate-dominating-file default-directory
                                            cmake-wizard-cmake-lists-filename))))
    (unless root
      (user-error "No %s found above %s"
                  cmake-wizard-cmake-lists-filename default-directory))
    (cmake-wizard--canonical-root root)))

(defun cmake-wizard-ensure-cmake-project (root)
  "Signal a `user-error' unless ROOT contains a CMakeLists.txt.
Returns ROOT, canonicalized."
  (let ((root (cmake-wizard--canonical-root root)))
    (unless (file-exists-p (expand-file-name cmake-wizard-cmake-lists-filename root))
      (user-error "%s not found in %s" cmake-wizard-cmake-lists-filename root))
    root))

(defun cmake-wizard-project-name (root)
  "Return a human-readable project name for ROOT."
  (file-name-nondirectory (directory-file-name (cmake-wizard--canonical-root root))))

(defun cmake-wizard-project-get (root key &optional default)
  "Return KEY from ROOT's state plist, or DEFAULT when unset."
  (let* ((root (cmake-wizard--canonical-root root))
         (state (gethash root cmake-wizard--project-state)))
    (if (plist-member state key)
        (plist-get state key)
      default)))

(defun cmake-wizard-project-put (root key value)
  "Set KEY to VALUE in ROOT's state plist and return VALUE."
  (let* ((root (cmake-wizard--canonical-root root))
         (state (gethash root cmake-wizard--project-state)))
    (puthash root (plist-put state key value) cmake-wizard--project-state)
    value))

(defun cmake-wizard--default-build-directory (root profile-name toolchain-name)
  "Default build directory layout: ROOT/build/PROFILE-NAME-TOOLCHAIN-NAME/."
  (file-name-as-directory
   (file-name-concat root
                      cmake-wizard-build-directory-name
                      (format "%s-%s" profile-name toolchain-name))))

(defun cmake-wizard-build-directory (root profile-name toolchain-name)
  "Return (creating it if needed) the build directory for ROOT.
PROFILE-NAME and TOOLCHAIN-NAME together select the out-of-source build
tree, see `cmake-wizard-build-directory-function'."
  (let ((dir (funcall cmake-wizard-build-directory-function
                       (cmake-wizard--canonical-root root)
                       profile-name
                       toolchain-name)))
    (make-directory dir t)
    (file-name-as-directory dir)))

(defun cmake-wizard-installed-cmake-version ()
  "Return the installed cmake's version string, or nil if it can't be run."
  (when (executable-find cmake-wizard-cmake-executable)
    (with-temp-buffer
      (when (zerop (call-process cmake-wizard-cmake-executable nil t nil "--version"))
        (goto-char (point-min))
        (when (re-search-forward "[0-9]+\\.[0-9]+\\.[0-9]+" nil t)
          (match-string 0))))))

(defun cmake-wizard-ensure-supported-cmake-version ()
  "Signal a `user-error' if the installed cmake is older than supported.
Returns the detected version string otherwise."
  (let ((version (cmake-wizard-installed-cmake-version)))
    (unless version
      (user-error "Could not determine `%s' version; is it installed?"
                  cmake-wizard-cmake-executable))
    (when (version< version cmake-wizard-minimum-cmake-version)
      (user-error "cmake-wizard requires cmake >= %s, found %s"
                  cmake-wizard-minimum-cmake-version version))
    version))

(defun cmake-wizard-check-cmake-lists-minimum-version (root)
  "Warn (non-fatally) if ROOT's CMakeLists.txt targets an old CMake.
This package only targets CMake 4.0+ semantics; projects declaring an
older `cmake_minimum_required' are not guaranteed to work correctly."
  (let ((file (expand-file-name cmake-wizard-cmake-lists-filename root)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((case-fold-search t))
          (when (re-search-forward
                 "cmake_minimum_required[ \t\n]*([ \t\n]*VERSION[ \t\n]+\\([0-9]+\\(?:\\.[0-9]+\\)*\\)"
                 nil t)
            (let ((declared (match-string 1)))
              (when (version< declared cmake-wizard-minimum-cmake-version)
                (message
                 "cmake-wizard: %s declares cmake_minimum_required VERSION %s (< %s); this package targets %s+"
                 cmake-wizard-cmake-lists-filename declared
                 cmake-wizard-minimum-cmake-version
                 cmake-wizard-minimum-cmake-version)))))))))

(provide 'cmake-wizard-project)
;;; cmake-wizard-project.el ends here
