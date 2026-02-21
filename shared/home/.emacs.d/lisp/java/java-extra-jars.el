;;; java-extra-jars.el --- Extra Java jars for tooling -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)

(defgroup my/java-extra-jars nil
  "Extra jars to expose to Java tooling (JDT LS, Gradle import)."
  :group 'tools)

(defcustom my/java-extra-jars
  '("~/eisop/checker-framework/checker/dist/*.jar")
  "List of extra jar entries for Java tooling.
Each entry may be:
- a direct jar file path, or
- a glob pattern that resolves to jar files (for example: \"~/lib/*.jar\")."
  :type '(repeat string)
  :group 'my/java-extra-jars)

(defcustom my/java-extra-jars-self-allowlist
  '("javac.jar")
  "Jar basenames to keep even when they are under the current project root."
  :type '(repeat string)
  :group 'my/java-extra-jars)

(defun my/java-extra-jars--normalize-entry (entry)
  "Return ENTRY with home-relative segments expanded."
  (expand-file-name entry))

(defun my/java-extra-jars--glob-entry-p (entry)
  "Return non-nil if ENTRY appears to be a glob pattern."
  (string-match-p "[*?\\[]" entry))

(defun my/java-extra-jars-referenced-libraries ()
  "Return JDT LS `java.project.referencedLibraries' entries as a vector."
  (vconcat
   (mapcar #'my/java-extra-jars--normalize-entry my/java-extra-jars)))

(defun my/java-extra-jars-expanded ()
  "Return sorted list of existing jar files resolved from `my/java-extra-jars'."
  (sort
   (delete-dups
    (cl-mapcan
     (lambda (entry)
       (let ((normalized (my/java-extra-jars--normalize-entry entry)))
         (cond
          ((my/java-extra-jars--glob-entry-p entry)
           (file-expand-wildcards normalized t))
          ((file-readable-p normalized)
           (list normalized))
          (t nil))))
     my/java-extra-jars))
   #'string-lessp))

(defun my/java-extra-jars--within-directory-p (path dir)
  "Return non-nil when PATH is inside DIR."
  (let ((path-truename (file-truename path))
        (dir-name (file-name-as-directory (file-truename dir))))
    (string-prefix-p dir-name path-truename)))

(defun my/java-extra-jars-for-project (project-root)
  "Return extra jars excluding jars that belong to PROJECT-ROOT.
When PROJECT-ROOT is nil, return all expanded jars."
  (let ((expanded (my/java-extra-jars-expanded)))
    (if (not project-root)
        expanded
      (seq-remove (lambda (jar)
                    (and (my/java-extra-jars--within-directory-p jar project-root)
                         (not (member (file-name-nondirectory jar)
                                      my/java-extra-jars-self-allowlist))))
                  expanded))))

(provide 'java-extra-jars)
;;; java-extra-jars.el ends here
