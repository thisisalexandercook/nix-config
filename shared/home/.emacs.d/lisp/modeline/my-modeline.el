;;; my-modeline.el --- Minimal, extensible modeline -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'vc)

(defgroup my/modeline nil
  "Minimal modeline customizations."
  :group 'convenience)

(defface my/modeline-status-clean
  '((t :foreground "forest green" :weight bold))
  "Face for clean file status indicator."
  :group 'my/modeline)

(defface my/modeline-status-modified
  '((t :foreground "red3" :weight bold))
  "Face for modified file status indicator."
  :group 'my/modeline)

(defface my/modeline-status-read-only
  '((t :foreground "goldenrod3" :weight bold))
  "Face for read-only status indicator."
  :group 'my/modeline)

(defface my/modeline-buffer-name
  '((t :inherit mode-line-buffer-id :weight bold))
  "Face for modeline buffer name."
  :group 'my/modeline)

(defface my/modeline-vcs
  '((t :inherit mode-line :slant italic))
  "Face for modeline version-control segment."
  :group 'my/modeline)

(defcustom my/modeline-segments '(status buffer vcs)
  "Ordered modeline segment identifiers."
  :type '(repeat symbol)
  :group 'my/modeline)

(defcustom my/modeline-separator "  "
  "Separator used between segments."
  :type 'string
  :group 'my/modeline)

(defun my/modeline-segment-status ()
  "Return modified/read-only/clean indicator segment."
  (cond
   (buffer-read-only
    (propertize "🔒" 'face 'my/modeline-status-read-only))
   ((buffer-modified-p)
    (propertize "●" 'face 'my/modeline-status-modified))
   (t
    (propertize "●" 'face 'my/modeline-status-clean))))

(defun my/modeline-segment-buffer ()
  "Return buffer name segment."
  (propertize (format-mode-line "%b") 'face 'my/modeline-buffer-name))

(defun my/modeline-segment-vcs ()
  "Return VCS segment for the current buffer, or nil."
  (when vc-mode
    (propertize (string-trim (format-mode-line '(vc-mode vc-mode)))
                'face 'my/modeline-vcs)))

(defvar my/modeline-segment-functions
  '((status . my/modeline-segment-status)
    (buffer . my/modeline-segment-buffer)
    (vcs . my/modeline-segment-vcs))
  "Mapping of modeline segment symbols to functions.")

(defun my/modeline-render ()
  "Render modeline string from `my/modeline-segments'."
  (string-join
   (delq nil
         (mapcar (lambda (segment)
                   (when-let ((fn (alist-get segment my/modeline-segment-functions)))
                     (funcall fn)))
                 my/modeline-segments))
   my/modeline-separator))

(defun my/modeline-enable ()
  "Enable the minimal modeline globally."
  (interactive)
  (setq-default mode-line-format '((:eval (my/modeline-render)))))

(my/modeline-enable)

(provide 'my-modeline)
;;; my-modeline.el ends here
