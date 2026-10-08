;;; cmake-wizard-compile.el --- compile.el-backed configure/build engine -*- lexical-binding: t; -*-

;; Author: Ádám Szikes
;; Maintainer: Ádám Szikes
;; Version: 0.2.0
;; Package-Requires: ((emacs "31.1") (cmake-wizard "0.2.0"))
;; Keywords: tools, c, cmake
;; URL: https://github.com/szikes-adam/cmake-wizard

;;; Commentary:

;; This is cmake-wizard's execution engine.  Rather than hand-rolling
;; process/buffer management, every command here goes through Emacs'
;; own `compile.el' (`compilation-start'/`compile'), which is what
;; REQ-1.1 explicitly asks for and what gives us, for free:
;;
;;  - asynchronous subprocesses (REQ-1.2)
;;  - a dedicated, interactive *compilation*-derived buffer (REQ-1.3)
;;  - `next-error'/`previous-error' navigation (REQ-1.4), once we
;;    register a regexp for CMake's own "CMake Error/Warning at
;;    FILE:LINE (...):" diagnostics (compiler errors are already
;;    covered by the builtin `gnu' regexp)
;;  - `compile-command' as a normal, dir-local-able variable (REQ-3.2)
;;  - minibuffer editing of the command on `M-x compile' (REQ-3.3),
;;    via `compilation-read-command'
;;
;; Extensibility: `cmake-wizard-configure-success-functions' is run
;; after ANY successful cmake-wizard configure command finishes -
;; whether it was started by `cmake-wizard-configure', by projectile
;; (cmake-wizard-projectile.el), or by a plain `M-x compile' that
;; happens to run the same command.  cmake-wizard-compdb.el and
;; cmake-wizard-lsp.el hook into it to wire up clangd; third-party code
;; can hook into it too.

;;; Code:

(require 'compile)
(require 'seq)
(require 'cmake-wizard-project)
(require 'cmake-wizard-profile)
(require 'cmake-wizard-toolchain)

(defcustom cmake-wizard-cmake-global-arguments nil
  "Extra literal arguments appended to every cmake configure invocation.
Analogous to a global `cmake-ide-cmake-opts' style override."
  :group 'cmake-wizard
  :type '(repeat string)
  :safe (lambda (v) (and (listp v) (seq-every-p #'stringp v))))

(defconst cmake-wizard--configure-marker "CMAKE_EXPORT_COMPILE_COMMANDS=ON"
  "Substring fingerprinting a shell command as a cmake-wizard configure run.
Used by `cmake-wizard--compilation-finish-handler' to recognize our own
configure commands regardless of how they were launched (our own
commands, projectile, or a hand-typed `M-x compile').")

(defconst cmake-wizard--compilation-regexp
  '(cmake-wizard
    "^CMake \\(?:Error\\|Warning\\)\\(?: (dev)\\)? at \\([^:\n]+\\):\\([0-9]+\\)[ \t]*\\(?:([^)\n]*)\\)?:"
    1 2)
  "`compilation-error-regexp-alist-alist' entry for CMake's own diagnostics.
Matches lines such as \"CMake Error at CMakeLists.txt:10 (message):\".")

(defun cmake-wizard--register-compilation-regexp ()
  "Register the CMake diagnostic regexp with `compile.el', once."
  (unless (assq 'cmake-wizard compilation-error-regexp-alist-alist)
    (push cmake-wizard--compilation-regexp compilation-error-regexp-alist-alist))
  (add-to-list 'compilation-error-regexp-alist 'cmake-wizard))

(cmake-wizard--register-compilation-regexp)

(defvar cmake-wizard-configure-success-functions nil
  "Abnormal hook run as (FN ROOT) after a cmake-wizard configure succeeds.")

(defun cmake-wizard--compilation-finish-handler (buf msg)
  "Run `cmake-wizard-configure-success-functions' for a finished configure.
BUF and MSG are as passed by `compilation-finish-functions'."
  (when (string-match-p "\\`finished" msg)
    (with-current-buffer buf
      (when (and (stringp compile-command)
                 (string-match-p (regexp-quote cmake-wizard--configure-marker)
                                  compile-command))
        (run-hook-with-args 'cmake-wizard-configure-success-functions
                             (cmake-wizard--canonical-root default-directory))))))

(add-hook 'compilation-finish-functions #'cmake-wizard--compilation-finish-handler)

(defun cmake-wizard--buffer-name (root)
  "Return the dedicated compilation buffer name for project ROOT.
Uses ROOT's full (abbreviated) path rather than just
`cmake-wizard-project-name', so two unrelated projects that happen to
share a directory basename (\"build\", \"app\", ...) never collide on
the same compilation buffer."
  (format "*cmake-wizard:%s*" (abbreviate-file-name (directory-file-name root))))

(defun cmake-wizard--run (root command)
  "Run COMMAND for ROOT via `compilation-start' in its dedicated buffer.
Returns the compilation buffer."
  (let ((default-directory root))
    (compilation-start command nil (lambda (_mode) (cmake-wizard--buffer-name root)))))

(defun cmake-wizard-configure-command (root)
  "Return the shell command string that configures ROOT.
Only path/value arguments are shell-quoted (not whole \"-DVAR=VAL\"
tokens), so the \"=\" in cache-variable flags is never escaped - that
keeps `compile-command' readable for REQ-3.3's minibuffer editing and
keeps `cmake-wizard--configure-marker' matching reliable."
  (setq root (cmake-wizard-ensure-cmake-project root))
  (let* ((profile (cmake-wizard-current-profile root))
         (toolchain (cmake-wizard-current-toolchain root))
         (build-dir (cmake-wizard-build-directory root profile toolchain))
         (args (append
                (list cmake-wizard-cmake-executable
                      "-S" (shell-quote-argument (directory-file-name root))
                      "-B" (shell-quote-argument (directory-file-name build-dir))
                      "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
                (cmake-wizard-toolchain-cache-args root)
                (cmake-wizard-profile-cache-args root)
                cmake-wizard-cmake-global-arguments)))
    (mapconcat #'identity args " ")))

(defun cmake-wizard-build-command (root &optional target)
  "Return the shell command string that builds ROOT, optionally for TARGET."
  (setq root (cmake-wizard-ensure-cmake-project root))
  (let* ((profile (cmake-wizard-current-profile root))
         (toolchain (cmake-wizard-current-toolchain root))
         (build-dir (cmake-wizard-build-directory root profile toolchain))
         (args (append
                (list cmake-wizard-cmake-executable
                      "--build" (shell-quote-argument (directory-file-name build-dir)))
                (when target (list "--target" (shell-quote-argument target))))))
    (mapconcat #'identity args " ")))

(defun cmake-wizard--configured-p (root)
  "Return non-nil if ROOT's current profile+toolchain has been configured."
  (let* ((profile (cmake-wizard-current-profile root))
         (toolchain (cmake-wizard-current-toolchain root))
         (build-dir (cmake-wizard-build-directory root profile toolchain)))
    (file-exists-p (expand-file-name "CMakeCache.txt" build-dir))))

(defun cmake-wizard-build-targets (root)
  "Return the list of build target names for ROOT.
Signals a `user-error' if ROOT hasn't been configured yet."
  (unless (cmake-wizard--configured-p root)
    (user-error "Project not configured yet; run `cmake-wizard-configure' first"))
  (let* ((profile (cmake-wizard-current-profile root))
         (toolchain (cmake-wizard-current-toolchain root))
         (build-dir (cmake-wizard-build-directory root profile toolchain)))
    (with-temp-buffer
      (call-process cmake-wizard-cmake-executable nil t nil
                     "--build" (directory-file-name build-dir)
                     "--target" "help")
      (goto-char (point-min))
      (let (targets)
        (while (re-search-forward "^\\.\\.\\. \\([!-~]+\\)" nil t)
          (push (match-string 1) targets))
        (nreverse targets)))))

;;;###autoload
(defun cmake-wizard-configure ()
  "Configure (generate the build system for) the current cmake-wizard project."
  (interactive)
  (let ((root (cmake-wizard-project-root)))
    (cmake-wizard-ensure-cmake-project root)
    (cmake-wizard-ensure-supported-cmake-version)
    (cmake-wizard-check-cmake-lists-minimum-version root)
    (cmake-wizard--run root (cmake-wizard-configure-command root))))

;;;###autoload
(defun cmake-wizard-build (&optional target)
  "Build TARGET (default: all targets) for the current project.
Configures the project first if its current profile+toolchain has no
build directory yet.  With a prefix argument, prompts for TARGET among
the project's known build targets."
  (interactive
   (list (when current-prefix-arg
           (completing-read
            "Target: "
            (ignore-errors (cmake-wizard-build-targets (cmake-wizard-project-root)))))))
  (let ((root (cmake-wizard-project-root)))
    (cmake-wizard-ensure-cmake-project root)
    (if (cmake-wizard--configured-p root)
        (cmake-wizard--run root (cmake-wizard-build-command root target))
      (cmake-wizard-ensure-supported-cmake-version)
      (cmake-wizard--configure-then-build target))))

(defun cmake-wizard--configure-then-build (target)
  "Configure the current project, then build TARGET iff that succeeds.
Attaches a buffer-local, self-removing `compilation-finish-functions'
entry to the one configure buffer this call starts, instead of using
the public `cmake-wizard-configure-success-functions' hook: that hook
is global, so a one-shot function added to it would stay registered
forever - and fire against a later, unrelated project's configure -
if this configure happens to fail (see docs/low-level.md)."
  (let ((buf (cmake-wizard-configure)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let (fn)
          (setq fn
                (lambda (finished-buf msg)
                  (remove-hook 'compilation-finish-functions fn t)
                  (when (string-match-p "\\`finished" msg)
                    (with-current-buffer finished-buf
                      (cmake-wizard--run default-directory
                                          (cmake-wizard-build-command
                                           default-directory target))))))
          (add-hook 'compilation-finish-functions fn nil t))))))

;;;###autoload
(defun cmake-wizard-compile ()
  "Populate `compile-command' and run `compile' interactively.
This is cmake-wizard's thinnest front door: it sets the buffer-local
`compile-command' to the project's configure command and then calls
`compile' exactly like a user invoking `M-x compile' would, so the
minibuffer prompts to edit the command first (REQ-3.3)."
  (interactive)
  (let ((root (cmake-wizard-project-root)))
    (cmake-wizard-ensure-cmake-project root)
    (setq-local compile-command (cmake-wizard-configure-command root))
    (call-interactively #'compile)))

(provide 'cmake-wizard-compile)
;;; cmake-wizard-compile.el ends here
