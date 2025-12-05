(defun my/instant-project-checkin ()
  "Take the project at point, and move it into the journal"
  (interactive)
  (let* ((marker (or (org-get-at-bol 'org-marker) (point-marker)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker))
         (headline "")
         (id "")
         (journal-file (denote-journal-path-to-new-or-existing-entry)))

    (with-current-buffer buffer
      (goto-char pos)
      (setq headline (org-get-heading t t t t))
      (setq id (org-id-get-create)))

    (find-file journal-file)

    (goto-char (point-min))
    (if (re-search-forward "^\\* Time-block" nil t)
        (org-end-of-subtree) ;; Append inside Time-block
      (goto-char (point-max))) ;; Or just append to end of file

    (insert (format "\n* BLOCK [[id:%s][%s]]\n:PROPERTIES:\n:CREATED: %s\n:END:\n"
                    id
                    headline
                    (format-time-string "[%Y-%m-%d %a %H:%M]")))

    (backward-char 1)
    (message "Checked in to '%s'!" headline)))
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "k") 'my/instant-project-checkin))

;; --- 1. FILE FINDERS ---

(defun my/get-training-file-path ()
  "Find the Denote file tagged 'training'."
  (let ((files (directory-files org-directory t "__training.*\\.org$")))
    (if files (car files) (error "No training file found!"))))

;; --- 2. SNAPSHOT GENERATOR ---

(defun my/get-snapshot-maxes ()
  "Read global_maxes from training.org and return a string for the template."
  (let ((file (my/get-training-file-path))
        (maxes-alist '()))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "#\\+NAME: global_maxes" nil t)
          (forward-line 1)
          (let ((table (org-table-to-lisp)))
            (dolist (row table)
              (when (and (listp row) (stringp (car row)))
                (push (cons (car row) (cadr row)) maxes-alist)))))))
    ;; Return the formatted table string
    (format "#+NAME: %%<%%Y-%%m-%%d>-maxes\n| Lift | Max |\n|----------+--------|\n| bench | %s |\n| squat | %s |\n| deadlift | %s |\n| press | %s |"
            (cdr (assoc "bench" maxes-alist))
            (cdr (assoc "squat" maxes-alist))
            (cdr (assoc "deadlift" maxes-alist))
            (cdr (assoc "press" maxes-alist)))))

;; --- 3. THE "HOOK" LOGIC (Link to Journal) ---

(defvar my/just-captured-training-p nil "Flag to track if we are capturing training.")

(defun my/start-training-session ()
  "Interactive command to start the training workflow."
  (interactive)
  (setq my/just-captured-training-p t)
  (org-capture nil "t"))

(defun my/link-training-to-journal ()
  "Hook: Run after capture finishes. Link the new training entry to today's journal."
  (when (and my/just-captured-training-p
             (not org-note-abort))
    (let* ((headline "")
           (id "")
           (journal-file (denote-journal-extras-path-to-new-or-existing-entry)))

      ;; A. Get ID from the new Training Entry
      (with-current-buffer (org-capture-get :buffer)
        (save-excursion
          (goto-char (org-capture-get :begin-marker))
          (setq headline (org-get-heading t t t t))
          (setq id (org-id-get-create)) ;; Create ID in training.org so we can link to it
          (save-buffer)))

      ;; B. Create Block in Journal
      (find-file journal-file)
      (goto-char (point-max))
      (insert (format "\n* BLOCK Training Session: %s [[id:%s][Link]]\n:PROPERTIES:\n:CREATED: %s\n:END:\n"                      headline id (format-time-string "[%Y-%m-%d %a %H:%M]")))

      ;; C. Clock In
      (backward-char 1)
      (org-clock-in)
      (message "Training linked and clock started!")))

  (setq my/just-captured-training-p nil))

(add-hook 'org-capture-after-finalize-hook 'my/link-training-to-journal)

(provide 'pull-block)
