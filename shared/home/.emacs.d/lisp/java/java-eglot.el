;;; java-eglot.el --- Java Eglot/JDT LS integration -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'eglot)
(require 'jsonrpc)
(require 'project)
(require 'url-parse)

(defclass my/eglot-java-server (eglot-lsp-server) ()
  "JDT LS server with classfile content support enabled.")

(cl-defmethod eglot-initialization-options ((server my/eglot-java-server))
  "Augment initialization options for JDT LS classfile navigation."
  (let ((opts (cl-call-next-method)))
    (cond
     ((hash-table-p opts)
      (let ((caps (make-hash-table :test 'equal)))
        (puthash "classFileContentsSupport" t caps)
        (puthash "extendedClientCapabilities" caps opts))
      opts)
     ((listp opts)
      (plist-put opts :extendedClientCapabilities
                 '(:classFileContentsSupport t)))
     (t
      '(:extendedClientCapabilities (:classFileContentsSupport t))))))

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
  (add-to-list 'eglot-stay-out-of 'imenu)
  (setq eglot-report-progress nil)
  (setq eglot-connect-timeout 120)
  (setq-default eglot-workspace-configuration
                '(:java (:import (:exclusions ["**/src/test/resources/**"])
                         :project (:resourceFilters ["node_modules" ".git" "src/test/resources"]))))
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
