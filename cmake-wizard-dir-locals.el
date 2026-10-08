;;; cmake-wizard-dir-locals.el --- .dir-locals.el persistence for cmake-wizard -*- lexical-binding: t; -*-

;; Author: Ádám Szikes
;; Maintainer: Ádám Szikes
;; Version: 0.2.0
;; Package-Requires: ((emacs "31.1") (cmake-wizard "0.2.0"))
;; Keywords: tools, c, cmake
;; URL: https://github.com/szikes-adam/cmake-wizard

;;; Commentary:

;; Project-specific cmake-wizard options are meant to be persisted in
;; the project's `.dir-locals.el' (REQ-3.1), the same mechanism Emacs
;; itself uses for per-project variables such as `compile-command'
;; (REQ-3.2).  This module marks cmake-wizard's variables as safe to
;; set that way and provides a command to write the current in-memory
;; selection out to `.dir-locals.el'.

;;; Code:

(require 'seq)
(require 'cmake-wizard-project)
(require 'cmake-wizard-profile)
(require 'cmake-wizard-toolchain)
(require 'cmake-wizard-compile)

(dolist (var '(cmake-wizard-default-profile
               cmake-wizard-default-toolchain
               cmake-wizard-build-directory-name))
  (put var 'safe-local-variable #'stringp))

(put 'cmake-wizard-cmake-global-arguments 'safe-local-variable
     (lambda (v) (and (listp v) (seq-every-p #'stringp v))))

(defun cmake-wizard--dir-locals-file (root)
  "Return the `.dir-locals.el' path for project ROOT."
  (expand-file-name ".dir-locals.el" root))

(defun cmake-wizard--read-dir-locals (root)
  "Return the alist stored in ROOT's `.dir-locals.el', or nil if absent."
  (let ((file (cmake-wizard--dir-locals-file root)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (read (current-buffer))))))

(defun cmake-wizard--write-dir-locals (root alist)
  "Write ALIST as ROOT's `.dir-locals.el', pretty-printed."
  (with-temp-file (cmake-wizard--dir-locals-file root)
    (let ((print-length nil) (print-level nil))
      (pp alist (current-buffer)))))

(defun cmake-wizard--dir-locals-merge (alist mode-class bindings)
  "Return ALIST with MODE-CLASS's BINDINGS merged in.
BINDINGS wins on key conflicts; other mode-classes and other variables
within MODE-CLASS are left untouched."
  (let* ((existing (cdr (assoc mode-class alist)))
         (kept (seq-remove (lambda (kv) (assq (car kv) bindings)) existing))
         (merged (append bindings kept)))
    (cons (cons mode-class merged)
          (seq-remove (lambda (entry) (equal (car entry) mode-class)) alist))))

;;;###autoload
(defun cmake-wizard-save-settings-to-dir-locals ()
  "Persist the current project's profile/toolchain/compile command.
Writes them into the project's `.dir-locals.el' so they survive across
Emacs sessions (REQ-3.1, REQ-4.4)."
  (interactive)
  (let* ((root (cmake-wizard-project-root))
         (bindings `((cmake-wizard-default-profile . ,(cmake-wizard-current-profile root))
                     (cmake-wizard-default-toolchain . ,(cmake-wizard-current-toolchain root))
                     (compile-command . ,(cmake-wizard-configure-command root))))
         (alist (cmake-wizard--dir-locals-merge
                 (cmake-wizard--read-dir-locals root) nil bindings)))
    (cmake-wizard--write-dir-locals root alist)
    (message "cmake-wizard: wrote settings to %s" (cmake-wizard--dir-locals-file root))))

(provide 'cmake-wizard-dir-locals)
;;; cmake-wizard-dir-locals.el ends here
