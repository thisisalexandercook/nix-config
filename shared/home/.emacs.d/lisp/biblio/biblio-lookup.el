(require 'biblio)
(require 'biblio-dblp)
(require 'subr-x)


(defun biblio-dblp--url (query)
  "Create a DBLP url to look up QUERY."
  (format "https://dblp.uni-trier.de/search/publ/api?q=%s&format=xml" (url-encode-url query)))

(defun biblio-dblp--forward-bibtex (metadata forward-to)
  "Forward BibTeX for DBLP entry METADATA to FORWARD-TO, forcing Trier mirror."
  (let* ((source-url (biblio-alist-get 'url metadata))
         (url (replace-regexp-in-string
               "https?://dblp.org" "https://dblp.uni-trier.de"
               (replace-regexp-in-string "/rec/" "/rec/bib2/" source-url t t))))
    (biblio-url-retrieve url (biblio-generic-url-callback
                              `(lambda ()
                                 (funcall ,forward-to
                                          (biblio-response-as-utf-8)))))))

(defvar my/biblio-current-file nil
  "Variable to pass current file context to the cleanup function.")

(defvar my/biblio-target-key nil
  "Variable to pass the desired BibTeX key to the cleanup function.")

(defun my/biblio-cleanup (autokey)
  "Rename file to match DBLP key, add file field, remove crossref."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^[ 	]*crossref[ 	]*=" nil t)
      (bibtex-kill-field))

    (goto-char (point-min))
    (let* ((key (when (re-search-forward "@[a-zA-Z]+{\\([^,]+\\)," nil t)
                  (match-string 1)))
           (new-bibtex-key key))
      (when (and key my/biblio-current-file (file-exists-p my/biblio-current-file))
        (let* ((parts (split-string key "[:/]"))
               (short-key (car (last parts)))
               (ext (file-name-extension my/biblio-current-file))
               (dir (file-name-directory my/biblio-current-file))
               (candidate-name (concat short-key "." ext))
               (candidate-path (expand-file-name candidate-name dir))
               (final-path candidate-path)
               (counter 97))

          (while (and (file-exists-p final-path)
                      (not (string= (file-truename my/biblio-current-file)
                                    (file-truename final-path))))
            (setq final-path (expand-file-name (format "%s%c.%s" short-key counter ext) dir))
            (setq counter (1+ counter)))

          (unless (string= (file-truename my/biblio-current-file)
                           (file-truename final-path))
            (rename-file my/biblio-current-file final-path 1)
            (setq my/biblio-current-file final-path)
            (message "Renamed file to: %s" (file-name-nondirectory final-path)))

          (setq new-bibtex-key (file-name-base final-path))))

      (goto-char (point-min))
      (when (and key (not (string= key new-bibtex-key))
                 (search-forward key nil t))
        (replace-match new-bibtex-key t t))

      (goto-char (point-min))
      (bibtex-end-of-entry)
      (backward-char 1)
      (bibtex-make-field `("file" nil ,(file-name-nondirectory my/biblio-current-file))))

    (goto-char (point-min))
    (bibtex-end-of-entry)
    (delete-region (point) (point-max)))

  (biblio--cleanup-bibtex autokey))

(defvar my/biblio-default-bibliography "~/codex/papers/main.bib"
  "Default bibliography file for biblio-lookup insertion.")

(defun my/get-pdf-title (file)
  "Attempt to get the PDF title using pdf-tools metadata."
  (when (and (string-suffix-p ".pdf" file t)
             (require 'pdf-tools nil t))
    (let ((title (condition-case nil
                     (cdr (assoc 'title (pdf-info-metadata file)))
                   (error nil))))
      (when title
        (setq title (string-trim title)))
      (if (and title (not (string-empty-p title)))
          title
        nil))))

(defun my/dired-biblio-lookup ()
  "Search DBLP for the file at point, prompting for a query.
Inserts result into `my/biblio-default-bibliography`."
  (interactive)
  (let* ((file (dired-get-filename nil t))
         (name (or (my/get-pdf-title file) (file-name-base file)))
         (query (read-string "DBLP Query: " name))
         (bib-file (expand-file-name my/biblio-default-bibliography)))
    (setq my/biblio-current-file file)
    (setq my/biblio-target-key nil)

    (find-file bib-file)
    (goto-char (point-max))

    (biblio-lookup 'biblio-dblp-backend query)))

(provide 'biblio-lookup)
