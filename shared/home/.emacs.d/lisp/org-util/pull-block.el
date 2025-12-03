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

(provide 'pull-block)
