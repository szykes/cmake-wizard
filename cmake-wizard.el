;;; cmake-wizard.el --- TODO

;;; Commentary:

;; General requirements:
;; - The default run command shall be just: cmake [project path]
;; - The cmake output shall have a separated buffer
;; - The cmake shall run in /tmp/cmake-wizard/[project-hash]
;; - More build config shall be added easily (eg: -DFLAVOR=release [compiler config file])
;; - The build config shall be changeable by using its name
;; - The pre-build and post-build tasks shall be added (how to set variables? etc., post-build like running unit test)
;; - The build config shall be stored within the project as eg: .cmake-wizard-config
;; - The building shall be done by the compile function (the cmake-wizard shall give a list of targets)
;; - The gtest shall be supported (is it really needed?)
;; - The projectile shall be supported
;; - The lsp-mode shall be supported
;; - The helm shall be supported
;; - The ivy shall be supported
;; - The rtags shall be supported

;;; Code:

;;; Interface:

(defgroup cmake-wizard nil
  "Use cmake and build targets easily."
  :group 'tools
  :link '(url-link :tag "GitHub" "https://github.com/szikes-adam/cmake-wizard"))

(defcustom cmake-wizard-cmake-global-arugments nil
  "Commonly used cmake arguments in all projects."
  :group 'cmake-wizard
  :type 'string)

(defcustom cmake-wizard-cmake-build-directory-path "/tmp/"
  "Specify the directory where the 'cmake-wizard-cmake-build-directory-name' is.
The nil means it is in the project root."
  :group 'cmake-wizard
  :type 'directory)

(defcustom cmake-wizard-cmake-build-directory-name "cmake-build/"
  "Specify name of directory where the generated files are.
Slash is required at the end."
  :group 'cmake-wiazrd
  :type 'string)

(defcustom cmake-wizard-cmake-executable-path nil
  "The path of cmake executable.
The nil means $PATH."
  :group 'cmake-wizard
  :type '(file :must-match t))

(defun cmake-wizard-print-cmake-version ()
  "Print cmake executable version."
  (interactive)
  (cmake-wizard--print-cmake-version))

(defun cmake-wizard-generate-build-project-system ()
  "Run cmake."
  (interactive)
  (cmake-wizard--generate-project-build-system))

(defun cmake-wizard-build-target ()
  "Run make."
  (interactive))

;; (defun cmake-wizard-add-build-config ()
;;   "Add new build config to the project."
;;   (interactive))

;; (defun cmake-wizard-remove-build-config ()
;;   "Remove the existing build config from the project."
;;   (interactive))

;;; Core:

(require 'projectile)

(defconst cmake-wizard--cmake-executable-name "cmake")

(defconst cmake-wizard--process-name "cmake")

(defconst cmake-wizard--buffer-name " *cmake-wizard log*")

(defconst cmake-wizard--cmake-lists-file "CMakeLists.txt")

(defun cmake-wizard--do-with-buffer (do)
  "Switch to buffer, do the DO then switch back."
  (let ((current-buffer (current-buffer)))
    (set-buffer (cmake-wizard--get-buffer))
    (read-only-mode -1)
    (goto-char (point-max))
    (funcall do)
    (read-only-mode 1)
    (set-buffer current-buffer)))

(defun cmake-wizard--build-message (string &rest objects)
  "Build message based on STRING and OBJECTS."
  (let ((base-string (format "cmake-wizard [%s]" (current-time-string)))
	(got-string (apply 'format string objects)))
    (concat base-string " " got-string)))

(defun cmake-wizard--print-message-and-buffer (string &rest objects)
  "Print STRING and OBJECTS in *Messgae* and own buffer."
  (let ((built-string (apply 'cmake-wizard--build-message string objects)))
    (apply 'message string objects)
    (cmake-wizard--do-with-buffer (lambda () (insert built-string "\n")))))

(defun cmake-wizard--print-buffer (string &rest objects)
  "Print STRING and OBJECTS in only own buffer."
  (let ((built-string (apply 'cmake-wizard--build-message string objects)))
    (cmake-wizard--do-with-buffer (lambda () (insert built-string "\n")))))

(defun cmake-wizard--throw-error (string &rest objects)
  "Throw an error with STRING and OBJECTS."
  (apply 'cmake-wizard--print-buffer string objects)
  (apply 'error string objects))

(defun cmake-wizard--generate-buffer (buffer-name)
  "Generate buffer with BUFFER-NAME."
  (let ((current-buffer (current-buffer))
        (new-buffer (get-buffer-create buffer-name)))
    (set-buffer new-buffer)
    (read-only-mode 1)
    (set-buffer current-buffer)))

(defun cmake-wizard--get-buffer ()
  "Get buffer and generate it, if it does not exist."
  (when (not (get-buffer cmake-wizard--buffer-name))
    (cmake-wizard--generate-buffer cmake-wizard--buffer-name))
  (get-buffer cmake-wizard--buffer-name))

(defun cmake-wizard--get-project-root-dir ()
  "Return with the project root directory based on the current position."
  (let ((root (projectile-project-root)))
    (when (not root)
      (cmake-wizard--throw-error "Project has not found based on buffer %s (%s)"
				 (buffer-name)
				 (buffer-file-name)))
    (cmake-wizard--print-buffer "Project has found with name %s (%s)"
				(cmake-wizard--get-project-name)
				root)
    root))

(defun cmake-wizard--test-whether-cmake-project ()
  "Test wheter the current prject is cmake project."
  (let ((path (concat
	       (cmake-wizard--get-project-root-dir)
	       cmake-wizard--cmake-lists-file)))
    (when (not (file-exists-p path))
      (cmake-wizard--throw-error "The project %s (%s) is not cmake project"
				 (cmake-wizard--get-project-name)
				 (cmake-wizard--get-project-root-dir)))))

(defun cmake-wizard--get-build-root-path ()
  "Return with the full path of the build root."
  (concat (or
	   cmake-wizard-cmake-build-directory-path
	   (cmake-wizard--get-project-root-dir))
	  cmake-wizard-cmake-build-directory-name))

(defun cmake-wizard--get-build-project-path ()
  "Return with the full path of the project specific build directory."
  (let* ((project-path (cmake-wizard--get-project-root-dir))
	 (directory (replace-regexp-in-string "/" "_" project-path))
	 (root-path (cmake-wizard--get-build-root-path))
	 (build-path (concat root-path directory)))
    (when (not (file-directory-p build-path))
      (make-directory build-path root-path))
    build-path))

(defun cmake-wizard--get-project-name ()
  "Return with the project name based on the current position."
  (projectile-project-name))

(defun cmake-wizard--register-callback (process-name callback)
  "Register CALLBACK on process of PROCESS-NAME."
  (set-process-sentinel (get-process process-name) callback))

(defun cmake-wizard--run-program (process-name program &rest program-args)
  "Run PROGRAM with PROCESS-NAME and PROGRAM-ARGS."
  (when (get-process process-name)
    (cmake-wizard--throw-error "Another process of %s is running with name: %s (ID: %d)"
			       program
			       (get-process process-name)
			       (process-id (get-process process-name))))
  (cmake-wizard--print-buffer (concat
			       "Run: "
			       program
			       " "
			       (mapconcat 'identity program-args " ")))
  (apply
   'start-process
   process-name
   (cmake-wizard--get-buffer)
   program
   program-args))

(defun cmake-wizard--get-cmake-executable ()
  "Return with cmake executable."
  (or cmake-wizard-cmake-executable-path cmake-wizard--cmake-executable-name))

(defun cmake-wizard--run-cmake (callback &rest program-args)
  "Run cmake with PROGRAM-ARGS then register CALLBACK."
  (apply 'cmake-wizard--run-program
	 cmake-wizard--process-name
	 (cmake-wizard--get-cmake-executable)
	 program-args)
  (cmake-wizard--register-callback cmake-wizard--process-name callback))

(defun cmake-wizard--filter (proc string)
  "PROC and STRING."
  )

(defun cmake-wizard--print-cmake-version ()
  "Print cmake version into buffer.
cmake --version"
  (cmake-wizard--run-cmake nil "-version")
  (set-process-filter (get-process cmake-wizard--process-name) 'cmake-wizard--filter))

(defun cmake-wizard--generate-project-build-system (&rest options)
  "Run cmake on project to generate the build system with OPTIONS.
cmake <OPTIONS> -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -S <project path> -B <build path>"
  (cmake-wizard--test-whether-cmake-project)
  (cmake-wizard--run-cmake
   (lambda (process event)
     (let ((status (process-exit-status process)))
     (when (not (= 0 status))
       (cmake-wizard--throw-error "The cmake has failed (exit code: %d), see the %s"
				  status
				  cmake-wizard--buffer-name))
     (cmake-wizard--print-message-and-buffer "Success")))
   (mapconcat 'identity options " ")
   "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
   "-S"
   (cmake-wizard--get-project-root-dir)
   "-B"
   (cmake-wizard--get-build-project-path)))

(defun cmake-wizard--build-target (target &rest options)
  "Run cmake to build TARGET with build OPTIONS for native tool.
cmake --build <build path> --target <TARGET> -- <OPTIONS>"
  (cmake-wizard--test-whether-cmake-project)
  (apply 'cmake-wizard--run-cmake
   (lambda (process event)
     (let ((status (process-exit-status process)))
     (when (not (= 0 status))
       (cmake-wizard--throw-error "The cmake has failed (exit code: %d), see the %s"
				  status
				  cmake-wizard--buffer-name))
     (cmake-wizard--print-message-and-buffer "Success")))
   "--build"
   (cmake-wizard--get-build-project-path)
   "--target"
   target
   "--"
   options))

(provide 'cmake-wizard)
;;; cmake-wizard.el ends here
