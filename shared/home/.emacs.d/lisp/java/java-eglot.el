;;; java-eglot.el --- Java Eglot/JDT LS integration -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'eglot)
(require 'jsonrpc)
(require 'project)
(require 'url-parse)

(defclass my/eglot-java-server (eglot-lsp-server) ()
  "JDT LS server with classfile content support enabled.")

(defgroup my/java nil
  "Local Java tooling customizations."
  :group 'tools)
;;
(defcustom my/java-checker-framework-root
  (expand-file-name "~/eisop/checker-framework")
  "Root directory of a local Checker Framework checkout."
  :type 'directory
  :group 'my/java)

(defcustom my/java-eglot-gradle-init-script
  (expand-file-name "eglot/checker-framework.init.gradle" user-emacs-directory)
  "Path to the Gradle init script used by JDT LS imports."
  :type 'file
  :group 'my/java)

(defun my/java-checker-framework-referenced-libraries ()
  "Return Checker Framework library globs for JDT LS."
  (let* ((root (expand-file-name my/java-checker-framework-root))
         (checker-dist (expand-file-name "checker/dist" root)))
    (if (file-directory-p checker-dist)
        (vector (expand-file-name "*.jar" checker-dist))
      [])))

(defun my/java--checker-qual-jar ()
  "Return absolute path to local checker-qual jar."
  (expand-file-name "checker/dist/checker-qual.jar"
                    (expand-file-name my/java-checker-framework-root)))

(defun my/java-eglot--ensure-gradle-init-script ()
  "Write/update the Gradle init script used to inject local CF jars."
  (let* ((script-path (expand-file-name my/java-eglot-gradle-init-script))
         (checker-qual-jar (my/java--checker-qual-jar))
         (script-dir (file-name-directory script-path))
         (script-content
          (format
           (concat
            "def checkerQualJar = new File('%s')\n"
            "allprojects { p ->\n"
            "  p.plugins.withId('java') {\n"
            "    if (checkerQualJar.exists()) {\n"
            "      p.dependencies.add('compileOnly', p.files(checkerQualJar))\n"
            "    }\n"
            "  }\n"
            "}\n")
           (replace-regexp-in-string "'" "\\\\'" checker-qual-jar t t))))
    (unless (file-directory-p script-dir)
      (make-directory script-dir t))
    (with-temp-file script-path
      (insert script-content))
    script-path))

(defun my/java-eglot-workspace-configuration ()
  "Return Eglot/JDT LS workspace settings."
  (let* ((gradle-init-script (my/java-eglot--ensure-gradle-init-script))
         (checker-framework-libs (my/java-checker-framework-referenced-libraries))
         (java-project-settings
          (append
           '(:resourceFilters ["node_modules" ".git" "src/test/resources"])
           (when (> (length checker-framework-libs) 0)
             `(:referencedLibraries ,checker-framework-libs)))))
    `(:java (:import (:exclusions ["**/src/test/resources/**"]
                      :gradle (:arguments ,(format "-I%s" gradle-init-script)))
             :project ,java-project-settings))))

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
          (list "jdtls" "-data" workspace-dir))))

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
