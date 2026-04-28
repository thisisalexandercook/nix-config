;;; java-dumb-jump.el --- Java Dumb Jump integration -*- lexical-binding: t; -*-

(require 'seq)
(require 'subr-x)

(defgroup my/java nil
  "Local Java tooling customizations."
  :group 'tools)

(defcustom my/java-dumb-jump-jdk-source-root
  (expand-file-name "~/jdk-sources")
  "Directory containing unpacked JDK source trees named jdkVERSION."
  :type 'directory
  :group 'my/java)

(defcustom my/java-dumb-jump-default-jdk-version "21"
  "JDK source version to use when the current project does not identify one."
  :type 'string
  :group 'my/java)

(defun my/java-dumb-jump--read-version-from-file (file)
  "Return the first plausible Java major version found in FILE."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file nil 0 512)
      (goto-char (point-min))
      (when (re-search-forward "\\b\\(?:1\\.\\)?\\([0-9][0-9]?\\)\\(?:\\.[0-9]\\|\\b\\)" nil t)
        (match-string 1)))))

(defun my/java-dumb-jump--read-tool-versions-file (file)
  "Return the Java major version declared in asdf/mise FILE."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file nil 0 2048)
      (goto-char (point-min))
      (when (re-search-forward "^java[ \t]+.*?\\b\\(?:1\\.\\)?\\([0-9][0-9]?\\)\\(?:\\.[0-9]\\|\\b\\)" nil t)
        (match-string 1)))))

(defun my/java-dumb-jump--version-from-java-home ()
  "Return the major JDK version described by JAVA_HOME."
  (when-let ((java-home (getenv "JAVA_HOME")))
    (let* ((home (file-name-as-directory java-home))
           (release-files (list (expand-file-name "release" home)
                                (expand-file-name "lib/openjdk/release" home))))
      (or (seq-some #'my/java-dumb-jump--read-version-from-file release-files)
          (when (string-match "\\(?:openjdk\\|jdk\\)[^0-9]*\\([0-9][0-9]?\\)" java-home)
            (match-string 1 java-home))))))

(defun my/java-dumb-jump--version-from-project (project-root)
  "Return the Java version requested by PROJECT-ROOT, if declared."
  (when project-root
    (or (my/java-dumb-jump--read-version-from-file
         (expand-file-name ".java-version" project-root))
        (my/java-dumb-jump--read-tool-versions-file
         (expand-file-name ".tool-versions" project-root)))))

(defun my/java-dumb-jump--jdk-source-dir (version)
  "Return the JDK source directory for VERSION, if it exists."
  (when (and version (file-directory-p my/java-dumb-jump-jdk-source-root))
    (let ((dir (expand-file-name (format "jdk%s" version)
                                 my/java-dumb-jump-jdk-source-root)))
      (when (file-directory-p dir)
        dir))))

(defun my/java-dumb-jump-extra-search-paths (lang project-root)
  "Return JDK source paths for Java Dumb Jump lookups."
  (when (equal lang "java")
    (when-let ((jdk-source-dir
                (my/java-dumb-jump--jdk-source-dir
                 (or (my/java-dumb-jump--version-from-project project-root)
                     (my/java-dumb-jump--version-from-java-home)
                     my/java-dumb-jump-default-jdk-version))))
      (list jdk-source-dir))))

(defun my/java-xref-prefer-dumb-jump ()
  "Prefer Dumb Jump in Java buffers and avoid etags prompts."
  (setq-local xref-backend-functions
              (cons #'dumb-jump-xref-activate
                    (remove #'etags--xref-backend xref-backend-functions))))

(defun my/java-dumb-jump-setup ()
  "Set up Java-specific Dumb Jump behavior."
  (add-hook 'java-mode-hook #'my/java-xref-prefer-dumb-jump)
  (add-hook 'java-ts-mode-hook #'my/java-xref-prefer-dumb-jump))

(provide 'java-dumb-jump)
;;; java-dumb-jump.el ends here
