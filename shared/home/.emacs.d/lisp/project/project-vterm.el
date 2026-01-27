(require 'project)
(require 'vterm)

;;;###autoload
(defun my/project-vterm ()
  "Launch vterm in a buffer for the current project."
  (interactive)
  (let* ((project (project-current t))
         (root (project-root project))
         (project-name (file-name-nondirectory (directory-file-name root)))
         (buffer-name (format "*vterm:%s*" project-name)))
    (if (get-buffer buffer-name)
        (pop-to-buffer buffer-name)
      (let ((default-directory root))
        (vterm-other-window buffer-name)))))

(provide 'project-vterm)
