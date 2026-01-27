(require 'project)
(require 'vterm)

;;;###autoload
(defun my/project-gemini ()
  "Launch Gemini CLI in a vterm buffer for the current project."
  (interactive)
  (let* ((project (project-current t))
         (root (project-root project))
         (project-name (file-name-nondirectory (directory-file-name root)))
         (buffer-name (format "*gemini:%s*" project-name)))
    (if (get-buffer buffer-name)
        (pop-to-buffer buffer-name)
      (let ((default-directory root))
        (vterm-other-window buffer-name)
        (vterm-send-string "gemini\n")))))

(provide 'gemini-integration)

