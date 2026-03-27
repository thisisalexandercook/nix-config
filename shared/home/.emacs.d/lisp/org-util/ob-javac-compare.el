;;; ob-javac-compare.el --- Org Babel support for javac variants -*- lexical-binding: t; -*-

(require 'org)
(require 'ob)
(require 'subr-x)

(defgroup my/org-javac nil
  "Org Babel helpers for Java compilation experiments."
  :group 'org)

(defcustom my/org-babel-javac-command "javac"
  "Shell command used for plain Java compilation blocks."
  :type 'string
  :group 'my/org-javac)

(defcustom my/org-babel-checker-javac-command
  (shell-quote-argument
   (expand-file-name "~/eisop/checker-framework/checker/bin/javac"))
  "Shell command used for checker-enabled Java compilation blocks."
  :type 'string
  :group 'my/org-javac)

(defvar org-babel-header-args:javac
  '((:classname . :any)
    (:dir . :any)
    (:sources . :any)
    (:cmd . :any)
    (:flags . :any))
  "Javac-specific Babel header arguments.")

(defvar org-babel-header-args:checker-javac
  '((:classname . :any)
    (:dir . :any)
    (:sources . :any)
    (:cmd . :any)
    (:flags . :any))
  "Checker-javac-specific Babel header arguments.")

(defun my/org-babel--infer-java-classname (body)
  "Infer Java public type name from BODY, or nil."
  (when (string-match
         "\\bpublic\\s-+\\(?:class\\|interface\\|enum\\|record\\)\\s-+\\([[:word:]_]+\\)"
         body)
    (match-string 1 body)))

(defun my/org-babel--header (key params)
  "Return header value for KEY from PARAMS."
  (cdr (assq key params)))

(defun my/org-babel--render-command (cmd src flags)
  "Build shell command from CMD, SRC file path, and FLAGS."
  (let* ((flags (or flags ""))
         (cmd-with-flags (if (string-empty-p flags) cmd (concat cmd " " flags))))
    (if (string-match-p "%f" cmd-with-flags)
        (replace-regexp-in-string "%f" (shell-quote-argument src) cmd-with-flags t t)
      (concat cmd-with-flags " " (shell-quote-argument src)))))

(defun my/org-babel--normalize-sources (sources)
  "Normalize SOURCES header argument to a list of file paths."
  (cond
   ((null sources) nil)
   ((stringp sources)
    (split-string (string-trim sources) "[[:space:]\n,]+" t))
   ((listp sources)
    (mapcar (lambda (x) (format "%s" x)) sources))
   (t (list (format "%s" sources)))))

(defun my/org-babel--compile-java (body params default-cmd)
  "Compile BODY according to PARAMS using DEFAULT-CMD.
Return compiler output string for Org Babel results."
  (let* ((classname (or (my/org-babel--header :classname params)
                        (my/org-babel--infer-java-classname body)
                        "Main"))
         (dir-param (my/org-babel--header :dir params))
         (shared-dir
          (cond
           ((stringp dir-param) (expand-file-name dir-param))
           ((and (consp dir-param) (stringp (car dir-param)))
            (expand-file-name (car dir-param)))
           (t nil)))
         (cmd (or (my/org-babel--header :cmd params) default-cmd))
         (flags (my/org-babel--header :flags params))
         (extra-sources (my/org-babel--normalize-sources
                         (my/org-babel--header :sources params)))
         (workdir (or shared-dir (make-temp-file "ob-javac-" t)))
         (src (expand-file-name (format "%s.java" classname) workdir))
         (default-directory workdir)
         (extra-args
          (mapconcat
           (lambda (f)
             (shell-quote-argument
              (if (file-name-absolute-p f) f (expand-file-name f workdir))))
           extra-sources
           " "))
         (shell-cmd
          (concat (my/org-babel--render-command cmd src flags)
                  (if (string-empty-p extra-args) "" (concat " " extra-args))
                  " 2>&1"))
         (output "")
         (exit-code 0))
    (unless (file-directory-p workdir)
      (make-directory workdir t))
    (with-temp-file src
      (insert body))
    (with-temp-buffer
      (setq exit-code (call-process-shell-command shell-cmd nil t))
      (setq output (string-trim-right (buffer-string))))
    (if (= exit-code 0)
        (if (string-empty-p output)
            (format "OK: %s" shell-cmd)
          output)
      (format "FAILED (%s)\n%s" shell-cmd output))))

(defun org-babel-execute:javac (body params)
  "Compile Java BODY with plain javac."
  (my/org-babel--compile-java body params my/org-babel-javac-command))

(defun org-babel-execute:checker-javac (body params)
  "Compile Java BODY with checker-enabled javac."
  (my/org-babel--compile-java body params my/org-babel-checker-javac-command))

(provide 'ob-javac-compare)
;;; ob-javac-compare.el ends here
