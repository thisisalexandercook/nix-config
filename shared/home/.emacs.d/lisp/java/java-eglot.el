;;; java-eglot.el --- Java Eglot/JDT LS integration -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'eglot)
(require 'jsonrpc)
(require 'java-extra-jars)
(require 'pp)
(require 'project)
(require 'url-parse)

(defclass my/eglot-java-server (eglot-lsp-server) ()
  "JDT LS server with classfile content support enabled.")

(defgroup my/java nil
  "Local Java tooling customizations."
  :group 'tools)

(defcustom my/java-eglot-gradle-init-script
  (expand-file-name "eglot/extra-jars.init.gradle" user-emacs-directory)
  "Path to the Gradle init script used by JDT LS imports."
  :type 'file
  :group 'my/java)

(defcustom my/java-eglot-javac-export-packages
  '("com.sun.tools.javac.api"
    "com.sun.tools.javac.code"
    "com.sun.tools.javac.comp"
    "com.sun.tools.javac.file"
    "com.sun.tools.javac.jvm"
    "com.sun.tools.javac.main"
    "com.sun.tools.javac.model"
    "com.sun.tools.javac.parser"
    "com.sun.tools.javac.processing"
    "com.sun.tools.javac.tree"
    "com.sun.tools.javac.util")
  "JDK compiler packages to export to JDT LS."
  :type '(repeat string)
  :group 'my/java)

(defun my/java-eglot--project-root ()
  "Return current project root directory, or nil when unavailable."
  (when-let ((project (project-current nil)))
    (project-root project)))

(defun my/java-eglot--current-build-root ()
  "Return the current Gradle subproject root when available."
  (let* ((anchor (or buffer-file-name default-directory))
         (gradle-root
          (or (and anchor (locate-dominating-file anchor "build.gradle"))
              (and anchor (locate-dominating-file anchor "build.gradle.kts")))))
    (or gradle-root (my/java-eglot--project-root))))

(defun my/java-eglot--jdtls-jvm-args ()
  "Return JVM args passed to jdtls."
  (mapcar (lambda (pkg)
            (format "--jvm-arg=--add-exports=jdk.compiler/%s=ALL-UNNAMED" pkg))
          my/java-eglot-javac-export-packages))

(defun my/java-eglot--ensure-gradle-init-script (&optional extra-jars)
  "Write/update the Gradle init script used to inject extra jars."
  (let* ((script-path (expand-file-name my/java-eglot-gradle-init-script))
         (extra-jars (or extra-jars (my/java-extra-jars-expanded)))
         (script-dir (file-name-directory script-path))
         (escaped-jars
          (mapcar (lambda (jar)
                    (replace-regexp-in-string "'" "\\\\'" jar t t))
                  extra-jars))
         (script-content
          (format
           (concat
            "def extraJars = [\n%s\n]\n"
            "allprojects { p ->\n"
            "  p.plugins.withId('java') {\n"
            "    def existing = extraJars.findAll { it.exists() }\n"
            "    if (!existing.isEmpty()) {\n"
            "      p.dependencies.add('compileOnly', p.files(existing))\n"
            "    }\n"
            "  }\n"
            "}\n")
           (if escaped-jars
               (mapconcat (lambda (jar) (format "  new File('%s')" jar))
                          escaped-jars
                          ",\n")
             ""))))
    (unless (file-directory-p script-dir)
      (make-directory script-dir t))
    (with-temp-file script-path
      (insert script-content))
    script-path))

(defun my/java-eglot-workspace-configuration ()
  "Return Eglot/JDT LS workspace settings."
  (let* ((build-root (my/java-eglot--current-build-root))
         (extra-jars (my/java-extra-jars-for-project build-root))
         (gradle-init-script (my/java-eglot--ensure-gradle-init-script extra-jars))
         (extra-libs (vconcat extra-jars))
         (java-project-settings
          (append
           '(:resourceFilters ["node_modules" ".git" "src/test/resources"])
           (when (> (length extra-libs) 0)
             `(:referencedLibraries ,extra-libs)))))
    `(:java (:import (:exclusions ["**/src/test/resources/**"
                                   "**/build/generated/**/annotated-jdk/**"]
                      :gradle (:arguments ,(format "-I%s" gradle-init-script)))
             :project ,java-project-settings))))

(defun my/java-eglot-show-extra-jars ()
  "Show effective extra jar configuration sent to JDT LS."
  (interactive)
  (let* ((configured (mapcar #'my/java-extra-jars--normalize-entry my/java-extra-jars))
         (build-root (my/java-eglot--current-build-root))
         (resolved (my/java-extra-jars-expanded))
         (filtered (my/java-extra-jars-for-project build-root))
         (referenced (append (vconcat filtered) nil))
         (gradle-init-script (my/java-eglot--ensure-gradle-init-script filtered))
         (workspace-config (my/java-eglot-workspace-configuration)))
    (with-current-buffer (get-buffer-create "*Java Eglot Extra Jars*")
      (erase-buffer)
      (insert "Configured entries (my/java-extra-jars):\n")
      (dolist (entry configured)
        (insert "  - " entry "\n"))
      (insert "\nResolved existing jars:\n")
      (if resolved
          (dolist (jar resolved)
            (insert "  - " jar "\n"))
        (insert "  (none)\n"))
      (insert "\nFiltered jars for current project:\n")
      (if filtered
          (dolist (jar filtered)
            (insert "  - " jar "\n"))
        (insert "  (none)\n"))
      (insert "\nJDTLS java.project.referencedLibraries entries:\n")
      (if referenced
          (dolist (entry referenced)
            (insert "  - " entry "\n"))
        (insert "  (none)\n"))
      (insert "\nGradle init script:\n")
      (insert "  " gradle-init-script "\n")
      (insert "Gradle args:\n")
      (insert "  -I" gradle-init-script "\n")
      (insert "\nWorkspace config payload:\n")
      (pp workspace-config (current-buffer))
      (goto-char (point-min))
      (view-mode 1)
      (display-buffer (current-buffer)))))

(cl-defmethod eglot-initialization-options ((server my/eglot-java-server))
  "Augment initialization options for JDT LS classfile navigation."
  (let ((opts (cl-call-next-method)))
    (cond
     ((hash-table-p opts)
      (let ((caps (make-hash-table :test 'equal)))
        (puthash "classFileContentsSupport" t caps)
        (puthash "extendedClientCapabilities" caps opts)
        (puthash "settings" (my/java-eglot-workspace-configuration) opts))
      opts)
     ((listp opts)
      (setq opts (plist-put opts :extendedClientCapabilities
                            '(:classFileContentsSupport t)))
      (setq opts (plist-put opts :settings (my/java-eglot-workspace-configuration)))
      opts)
     (t
      `(:extendedClientCapabilities (:classFileContentsSupport t)
        :settings ,(my/java-eglot-workspace-configuration))))))

(defun my/java-lsp-eligible-file-p (file)
  "Return non-nil when FILE should be managed by Java LSP."
  (let ((f (file-truename file)))
    (and (not (string-match-p "/src/test/resources/" f))
         (or (string-match-p "/src/main/java/" f)
             (string-match-p "/src/test/java/" f)))))

(defun my/java-eglot-maybe-enable ()
  "Enable Eglot only for Java source files."
  (when (and buffer-file-name
             (my/java-lsp-eligible-file-p buffer-file-name))
    (eglot-ensure)))

(defun my/eglot-java--find-server ()
  "Find a running Java Eglot server for the current project."
  (when-let* ((project (project-current))
              (servers (gethash project eglot--servers-by-project)))
    (cl-find-if (lambda (server)
                  (object-of-class-p server 'my/eglot-java-server))
                servers)))

(defun my/eglot-jdt-uri-handler (_operation &rest args)
  "Resolve JDT LS `jdt://` URIs into cached source files."
  (let* ((uri (car args))
         (cache-dir (expand-file-name ".eglot-jdt" (project-root (project-current t))))
         (source-file
          (expand-file-name
           (or (save-match-data
                 (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\\.class\\?" uri)
                   (format "%s.java"
                           (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))
               "unknown.java")
           cache-dir)))
    (unless (file-readable-p source-file)
      (let ((server (or (my/eglot-java--find-server)
                        (eglot-current-server)))
            (metadata-file (format "%s.%s.metadata"
                                   (file-name-directory source-file)
                                   (file-name-base source-file))))
        (unless (file-directory-p (file-name-directory source-file))
          (make-directory (file-name-directory source-file) t))
        (with-temp-file source-file
          (insert (jsonrpc-request server :java/classFileContents (list :uri uri))))
        (with-temp-file metadata-file
          (insert uri))))
    source-file))

(defun my/eglot-wrap-uri-to-path (original-fn &rest args)
  "Preserve `jdt://` URIs for file-name handlers."
  (let ((uri (car args)))
    (if (and (stringp uri)
             (string= "jdt" (url-type (url-generic-parse-url uri))))
        uri
      (apply original-fn args))))

(defun my/eglot-wrap-path-to-uri (original-fn &rest args)
  "Avoid converting `jdt://` pseudo-paths."
  (let ((path (car args)))
    (if (and (stringp path)
             (string= "jdt" (url-type (url-generic-parse-url path))))
        path
      (apply original-fn args))))

(defun my/eglot-jdtls-contact (_interactive)
  "Return contact tuple for plain jdtls with custom server class."
  (let* ((project-root-path (project-root (project-current t)))
         (workspace-root (expand-file-name "eglot-jdtls-workspaces" user-emacs-directory))
         (workspace-dir (expand-file-name (md5 project-root-path) workspace-root)))
    (unless (file-directory-p workspace-dir)
      (make-directory workspace-dir t))
    (cons 'my/eglot-java-server
          (append (list "jdtls")
                  (my/java-eglot--jdtls-jvm-args)
                  (list "-data" workspace-dir)))))

(defun my/java-eglot-setup ()
  "Configure Eglot for Java projects."
  (interactive)
  (setq-default eglot-workspace-configuration (my/java-eglot-workspace-configuration))
  (add-to-list 'eglot-stay-out-of 'imenu)
  (setq eglot-report-progress nil)
  (setq eglot-connect-timeout 120)
  (add-to-list 'eglot-ignored-server-capabilities :workspace/didChangeWorkspaceFolders)
  (add-hook 'java-mode-hook #'my/java-eglot-maybe-enable)
  (add-hook 'java-ts-mode-hook #'my/java-eglot-maybe-enable)
  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode) . my/eglot-jdtls-contact))
  (add-to-list 'file-name-handler-alist '("\\`jdt://" . my/eglot-jdt-uri-handler))
  (unless (advice-member-p #'my/eglot-wrap-uri-to-path 'eglot--uri-to-path)
    (advice-add 'eglot--uri-to-path :around #'my/eglot-wrap-uri-to-path))
  (unless (advice-member-p #'my/eglot-wrap-path-to-uri 'eglot--path-to-uri)
    (advice-add 'eglot--path-to-uri :around #'my/eglot-wrap-path-to-uri)))

(provide 'java-eglot)
;;; java-eglot.el ends here
