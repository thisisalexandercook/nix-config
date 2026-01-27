;;; pull-block.el --- Helper for pulling project blocks into daily journal -*- lexical-binding: t; -*-

(require 'org)
(require 'org-agenda)
(require 'org-id)
(require 'denote)

(defun my/get-training-file-path ()
  "Find the Denote file tagged 'training'."
  (let ((files (directory-files org-directory t "__.*training.*\\.org$")))
    (if files
        (car files)
      (error "Could not find a file tagged 'training' in %s" org-directory))))

(defun my/get-snapshot-maxes ()
  "Read max-lifts from training log and return a string for the template."
  (let ((file (my/get-training-file-path))
        (maxes-alist '()))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "#\\+NAME: max-lifts" nil t)
            (progn
              (forward-line 1)
              (let ((table (org-table-to-lisp)))
                (dolist (row table)
                  (when (and (listp row) (stringp (car row)))
                    (push (cons (car row) (cadr row)) maxes-alist)))))
          (error "Could not find #+NAME: max-lifts in training.org"))))
    (format "#+NAME: Maxes_%s\n| Lift | Max |\n|----------+--------|\n| bench | %s |\n| squat | %s |\n| deadlift | %s |\n| press | %s |"
            (format-time-string "%Y%m%d")
            (cdr (assoc "bench" maxes-alist))
            (cdr (assoc "squat" maxes-alist))
            (cdr (assoc "deadlift" maxes-alist))
            (cdr (assoc "press" maxes-alist)))))

(defun my/instant-project-checkin ()
  "Take the project at point, create a Journal entry for it, and clock in immediately."
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
    (delete-other-windows)
    (split-window-horizontally)
    (find-file journal-file)

    (goto-char (point-min))
    (if (re-search-forward "^\\* Time-block" nil t)
        (org-end-of-subtree)
      (goto-char (point-max)))

    (insert (format "\n** BLOCK [[id:%s][%s]]\n:PROPERTIES:\n:CREATED: %s\n:END:\n"
                    id
                    headline
                    (format-time-string "[%Y-%m-%d %a %H:%M]")))

    (save-buffer)))

(defvar my/just-captured-training-p nil "Flag: Are we in a training capture?")
(defvar my/last-training-data nil "Storage: (headline . id) from the capture.")

(defun my/start-training-session ()
  "Start the training workflow."
  (interactive)
  (setq my/just-captured-training-p t)
  (setq my/last-training-data nil) ;; Reset storage
  (org-capture nil "t"))

(defun my/capture-training-prepare ()
  "Run BEFORE finalize to grab the ID while the buffer is still alive."
  (when (and my/just-captured-training-p
             (not org-note-abort))
    (save-excursion
      (goto-char (point-min))
      (let ((headline (org-get-heading t t t t))
            (id (org-id-get-create)))
        (setq my/last-training-data (cons headline id))))))

(defun my/capture-training-finalize ()
  "Run AFTER finalize to write to the journal using the stored data."
  (when (and my/just-captured-training-p
             (not org-note-abort)
             my/last-training-data)
    (let ((headline (car my/last-training-data))
          (id (cdr my/last-training-data))
          (journal-file (denote-journal-path-to-new-or-existing-entry)))

      (find-file journal-file)
      (goto-char (point-max))
      (insert (format "\n** BLOCK Training Session: %s [[id:%s][Link]]\n:PROPERTIES:\n:CREATED: %s\n:END:\n"
                      headline id (format-time-string "[%Y-%m-%d %a %H:%M]")))))


  (setq my/just-captured-training-p nil)
  (setq my/last-training-data nil)
  (save-buffer))

(defvar my/captured-event-time nil "Temporary storage for the event timestamp.")

(defun my/get-journal-file-for-date ()
  "Ask user for a date, set the global variable, and return the Denote path."
  (let* ((time (org-read-date nil t nil "Event Date/Time: "))
         (date-string (format-time-string "%Y-%m-%d" time)))
    (setq my/captured-event-time time)
    (denote-journal-path-to-new-or-existing-entry date-string)))

(defun my/find-event-target-location ()
  "Prompt for Work/Personal and move point to the end of that subtree."
  (let* ((choice (read-char-choice "Category: [w]ork, [p]ersonal? " '(?w ?p)))
         (header-name (if (eq choice ?w) "Work" "Personal"))
         (tag-name    (if (eq choice ?w) ":work:" ":personal:")))

    (goto-char (point-min))
    (if (re-search-forward (format "^\\* %s" header-name) nil t)
        (org-end-of-subtree)
      (goto-char (point-max))
      (insert (format "\n* %s %s\n" header-name tag-name)))))

(defun my/get-todo-file-path ()
  (let ((files (directory-files org-directory t "__.*todos.*\\.org$")))
    (if files
        (car files)
      (error "Could not find a file tagged 'todos' in %s" org-directory))))

(add-hook 'org-capture-prepare-finalize-hook 'my/capture-training-prepare)

(add-hook 'org-capture-after-finalize-hook 'my/capture-training-finalize)

;; Keybinding
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "k") 'my/instant-project-checkin))

(provide 'pull-block)
