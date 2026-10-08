;;; cmake-wizard-profile.el --- Build-profile ("build type") switching -*- lexical-binding: t; -*-

;; Author: Ádám Szikes
;; Maintainer: Ádám Szikes
;; Version: 0.2.0
;; Package-Requires: ((emacs "31.1") (cmake-wizard "0.2.0"))
;; Keywords: tools, c, cmake
;; URL: https://github.com/szikes-adam/cmake-wizard

;;; Commentary:

;; Build profiles are cmake-wizard's analogue of VSCode CMake Tools'
;; "variants": a named set of -D cache variables (CMAKE_BUILD_TYPE and
;; friends) that can be switched per-project at runtime (REQ-4.4) or
;; persisted via `.dir-locals.el' (see cmake-wizard-dir-locals.el).

;;; Code:

(require 'cmake-wizard-project)

(defcustom cmake-wizard-profiles
  '(("Debug"          . (("CMAKE_BUILD_TYPE" . "Debug")))
    ("Release"        . (("CMAKE_BUILD_TYPE" . "Release")))
    ("RelWithDebInfo" . (("CMAKE_BUILD_TYPE" . "RelWithDebInfo")))
    ("MinSizeRel"     . (("CMAKE_BUILD_TYPE" . "MinSizeRel"))))
  "Alist of profile name to an alist of CMake cache variables to define.
Add your own entries to introduce project-specific profiles, e.g. a
\"Coverage\" profile that also defines CMAKE_CXX_FLAGS."
  :group 'cmake-wizard
  :type '(alist :key-type string
                :value-type (alist :key-type string :value-type string)))

(defcustom cmake-wizard-default-profile "Debug"
  "Profile used for a project that has not explicitly selected one.
Settable per-directory via `.dir-locals.el'."
  :group 'cmake-wizard
  :type 'string
  :safe #'stringp)

(defun cmake-wizard-current-profile (root)
  "Return the currently selected profile name for ROOT."
  (cmake-wizard-project-get root :profile cmake-wizard-default-profile))

(defun cmake-wizard-set-profile (root name)
  "Set the current profile for ROOT to NAME."
  (unless (assoc name cmake-wizard-profiles)
    (user-error "Unknown cmake-wizard profile: %s" name))
  (cmake-wizard-project-put root :profile name))

;;;###autoload
(defun cmake-wizard-select-profile ()
  "Interactively select the build profile for the current project."
  (interactive)
  (let* ((root (cmake-wizard-project-root))
         (name (completing-read "cmake-wizard profile: "
                                 (mapcar #'car cmake-wizard-profiles)
                                 nil t nil nil
                                 (cmake-wizard-current-profile root))))
    (cmake-wizard-set-profile root name)
    (message "cmake-wizard: profile set to %s" name)))

(defun cmake-wizard-profile-cache-args (root)
  "Return the list of \"-DVAR=VAL\" strings for ROOT's current profile.
Only VAL is shell-quoted, so a value containing a space (e.g. extra
compiler flags) stays a single cmake argument."
  (let* ((name (cmake-wizard-current-profile root))
         (vars (cdr (assoc name cmake-wizard-profiles))))
    (mapcar (lambda (kv) (format "-D%s=%s" (car kv) (shell-quote-argument (cdr kv))))
            vars)))

(provide 'cmake-wizard-profile)
;;; cmake-wizard-profile.el ends here
