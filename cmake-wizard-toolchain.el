;;; cmake-wizard-toolchain.el --- Compiler/toolchain ("kit") selection -*- lexical-binding: t; -*-

;; Author: Ádám Szikes
;; Maintainer: Ádám Szikes
;; Version: 0.2.0
;; Package-Requires: ((emacs "31.1") (cmake-wizard "0.2.0"))
;; Keywords: tools, c, cmake
;; URL: https://github.com/szikes-adam/cmake-wizard

;;; Commentary:

;; Toolchains ("kits", in VSCode CMake Tools terms) describe which
;; compiler cmake should configure with, and optionally a
;; `-DCMAKE_TOOLCHAIN_FILE=' for cross-compiling to another
;; architecture (arm32/arm64/mips/...) from the host.  Clang is the
;; default per cmake-wizard's toolchain requirements.
;;
;; Example cross-compilation entry, added to `cmake-wizard-toolchains':
;;
;;   (\"arm64-clang\" . (:toolchain-file \"~/toolchains/arm64-clang.cmake\"))

;;; Code:

(require 'subr-x)
(require 'cmake-wizard-project)

(defcustom cmake-wizard-toolchains
  '(("clang" . (:c-compiler "clang" :cxx-compiler "clang++"))
    ("gcc"   . (:c-compiler "gcc"   :cxx-compiler "g++")))
  "Alist of toolchain (\"kit\") name to a plist describing it.
Supported plist keys:
  :c-compiler     value for -DCMAKE_C_COMPILER
  :cxx-compiler   value for -DCMAKE_CXX_COMPILER
  :toolchain-file value for -DCMAKE_TOOLCHAIN_FILE (cross-compilation)
  :extra-args     list of extra literal \"-DX=Y\" strings"
  :group 'cmake-wizard
  :type '(alist :key-type string :value-type plist))

(defcustom cmake-wizard-default-toolchain "clang"
  "Toolchain used for a project that has not explicitly selected one.
Defaults to Clang, per cmake-wizard's toolchain requirements."
  :group 'cmake-wizard
  :type 'string
  :safe #'stringp)

(defun cmake-wizard-current-toolchain (root)
  "Return the currently selected toolchain name for ROOT."
  (cmake-wizard-project-get root :toolchain cmake-wizard-default-toolchain))

(defun cmake-wizard-set-toolchain (root name)
  "Set the current toolchain for ROOT to NAME."
  (unless (assoc name cmake-wizard-toolchains)
    (user-error "Unknown cmake-wizard toolchain: %s" name))
  (cmake-wizard-project-put root :toolchain name))

;;;###autoload
(defun cmake-wizard-select-toolchain ()
  "Interactively select the toolchain (kit) for the current project."
  (interactive)
  (let* ((root (cmake-wizard-project-root))
         (name (completing-read "cmake-wizard toolchain: "
                                 (mapcar #'car cmake-wizard-toolchains)
                                 nil t nil nil
                                 (cmake-wizard-current-toolchain root))))
    (cmake-wizard-set-toolchain root name)
    (message "cmake-wizard: toolchain set to %s" name)))

(defun cmake-wizard-toolchain-cache-args (root)
  "Return the list of \"-DVAR=VAL\" strings for ROOT's current toolchain.
The compiler name/path is used verbatim (compiler names rarely need
shell quoting, and quoting would make the common case needlessly ugly
in the minibuffer); the toolchain-file path, more often an absolute
path, is quoted since it may contain spaces."
  (let* ((name (cmake-wizard-current-toolchain root))
         (plist (cdr (assoc name cmake-wizard-toolchains)))
         (args nil))
    (when-let* ((v (plist-get plist :c-compiler)))
      (push (format "-DCMAKE_C_COMPILER=%s" v) args))
    (when-let* ((v (plist-get plist :cxx-compiler)))
      (push (format "-DCMAKE_CXX_COMPILER=%s" v) args))
    (when-let* ((v (plist-get plist :toolchain-file)))
      (push (format "-DCMAKE_TOOLCHAIN_FILE=%s"
                     (shell-quote-argument (expand-file-name v)))
            args))
    (append (nreverse args) (plist-get plist :extra-args))))

(provide 'cmake-wizard-toolchain)
;;; cmake-wizard-toolchain.el ends here
